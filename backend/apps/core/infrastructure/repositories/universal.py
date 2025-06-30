from typing import Any, Dict, List, Sequence, Type, Set

from django.contrib.auth import get_user_model
from django.db import connection
from django.db.models import QuerySet

from backend.apps.core.infrastructure.logging.logger import debug_print
from core.models import Universal
from core.domain.universal.enums.aggregation_type import AggregationType
from core.domain.universal.enums.frequency_type import FrequencyType
from core.domain.universal.enums.operator_type import OperatorType
from core.domain.universal.enums.univ_columns import UniversalColumn

from core.infrastructure.orm.operator_lookup import OPERATOR_LOOKUPS
from core.infrastructure.orm.agg_type_to_func import AGGREGATION_FUNCTIONS

import backend.apps.core.infrastructure.repositories.universal_util as universal_util

User = get_user_model()

class UniversalRepository:

    # -------------------- Data Retrieval --------------------

    def get_full_data(self, user_id: int) -> QuerySet[Universal]:
        """Retrieve all Universal rows belonging to a user."""
        user = User.objects.get(pk=user_id)
        qs = Universal.objects.filter(user=user)
        debug_print(f"Retrieved {qs.count()} rows for user {user_id}")
        return qs

    def filter_data(
            self,
            qs: QuerySet[Universal],
            column: UniversalColumn,
            value: Any,
            operator: OperatorType
        ) -> QuerySet[Universal]:
        """
        Apply a filter or exclusion based on OperatorType lookup.
        """
        # Handle ArrayField contains
        if operator in {OperatorType.ARRAY_CONTAINS, OperatorType.ARRAY_NOT_CONTAINS}:
            if not isinstance(value, (list, tuple)):
                value = [value]

        lookup, exclude = OPERATOR_LOOKUPS[operator]
        kwargs = {f"{column}{lookup}": value}
        return qs.exclude(**kwargs) if exclude else qs.filter(**kwargs)
    
# --------- Graph Traversal ---------

    def get_neighbors(
        self,
        qs: QuerySet[Universal],
        graph_cols: Sequence[UniversalColumn]
    ) -> Dict[Any, List[Any]]:
        """
        Bulk-fetch neighbor lists for graph traversal.
        """
        mapping: Dict[Any, List[Any]] = {}
        for entry_id, *lists in qs.values_list('entry_id', *graph_cols):
            for lst in lists:
                if lst:
                    mapping.setdefault(entry_id, []).extend(lst)
        return mapping

    def get_rows_by_ids(
        self,
        qs: QuerySet[Universal],
        ids: Sequence[Any]
    ) -> QuerySet[Universal]:
        """Return rows whose entry_id is in the provided list."""
        lookup_suffix, _ = OPERATOR_LOOKUPS[OperatorType.IN]
        kwargs = {f"{UniversalColumn.ENTRY_ID.value}{lookup_suffix}":ids}
        return qs.filter(**kwargs)
    
    
    
    # -------------------- Unique Value Extraction --------------------

    def get_unique_values(
        self,
        qs: QuerySet[Universal],
        column: UniversalColumn
    ) -> List[Any]:
        """Distinct values for any column, handling JSON/Array extraction."""
        qs2, field = universal_util.adapt_column_for_processing(qs, column)
        return list(qs2.values_list(field, flat=True).distinct())

    def get_unique_json_keys(self, qs: QuerySet[Universal]) -> list[str]:
        """All distinct JSON keys in the 'fields' JSON column."""
        qs2, alias = universal_util.create_col_for_all_json_keys(qs)
        return list(qs2.values_list(alias, flat=True).distinct())

    def get_unique_json_key_values(
        self,
        qs: QuerySet[Universal],
        key: str
    ) -> Set[Any]:
        """Distinct values for a specific JSON key."""
        kwargs = {f"fields__has_key": key}
        qs2 = qs.filter(**kwargs)
        qs3, alias = universal_util.create_col_for_json_key(qs2, key)
        return set(qs3.values_list(alias, flat=True).distinct())

    def get_unique_json_values(self, qs: QuerySet[Universal]) -> Set[Any]:
        """
        All distinct values across all JSON keys, via per-key extraction.
        """
        all_vals = set()
        keys = self.get_unique_json_keys(qs)
        for key in keys:
            kwargs = {f"{UniversalColumn.FIELDS.value}{OperatorType.HAS_KEY.value}":{key}}
            fields_expr = f"{UniversalColumn.FIELDS.value}__{key}"
            vals_for_key = qs.filter(**kwargs).values_list(fields_expr, flat=True).distinct()
            all_vals.update(v for v in vals_for_key if v is not None)
        return all_vals

    # -------------------- Aggregation --------------------

    def aggregate_field(
        self,
        qs: QuerySet[Universal],
        column: UniversalColumn,
        aggregate_type: AggregationType
    ) -> Any:
        """Aggregate a single column (scalar, JSON, or Array) into one value."""
        qs2, alias = universal_util.adapt_column_for_processing(qs, column)
        func = AGGREGATION_FUNCTIONS[aggregate_type]
        result_kwargs = {"result":func(alias)}
        return qs2.aggregate(**result_kwargs)["result"]
    
    def aggregate_group_fields(
        self,
        qs: QuerySet[Universal],
        group_cols: List[UniversalColumn],
        aggregate_type: AggregationType,
        target_col: str,
        freq: FrequencyType|None
    )  -> Any:
        """
        Group-by aggregation with optional date-frequency and multi-column grouping.
        """
        # Prepend date-frequency if requested
        all_group_cols: List[str] = []
        if freq:
            qs, freq_alias = universal_util.create_frequency_col(qs, freq)
            all_group_cols.append(freq_alias)

        # Adapt group columns
        for col in group_cols:
            qs, alias = universal_util.adapt_column_for_processing(qs, col)
            all_group_cols.append(alias)

        # Adapt target column
        qs, target_alias = universal_util.adapt_column_for_processing(qs, target_col)

        # Perform grouping and aggregation
        func = AGGREGATION_FUNCTIONS[aggregate_type]
        result_kwargs = {"result":func(target_alias)}
        return qs.values(*all_group_cols).annotate(**result_kwargs)

        
    