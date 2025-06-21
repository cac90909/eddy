from typing import List, Any, Dict, str, Sequence, Set
from django.db.models import F, Func, IntegerField, FloatField, DateField
from django.db.models.functions import Cast
from collections import deque
from shared.logger import debug_print
from shared.universal.repository_util import (
    is_json_field,
    is_array_field,
    get_nested_json_column_data_class,
    create_casted_nested_json_column,
    create_unnested_list_column,
    perform_aggregation_on_column,
    transform_target_column_for_aggregation,
    create_grouping_columns,
    group_by_data_with_aggregation,
    build_filter_statement,
    generate_ids_in_traversal,
    get_unique_json_keys,
    get_unique_json_key_values,
    get_unique_json_values,
    get_column_data_type
)
from django.contrib.auth import get_user_model
from django.core.exceptions import FieldDoesNotExist
from django.db.models import QuerySet
from shared.universal.enums import (
    OperatorType,
    UniversalColumn,
    ARRAY_OPERATORS,
    NEGATION_OPERATORS,
    DataType,
    DataStructureType
)
from shared.universal.mappings import (
    OPERATOR_LOOKUPS,
    AGGREGATION_FUNCTIONS
)
import shared.universal.util as UnivRepoUtil
from shared.models import Universal
User = get_user_model()

class UniversalRepository:

    # -------------------- Data Retrieval --------------------

    def get_full_data(self, user_id):
        """Retrieve all data for a specific user."""
        user_instance = User.objects.get(pk=user_id)
        data = Universal.objects.filter(user=user_instance)
        debug_print(f"Finished Query: {data.count()} rows, {type(data)} type")
        return data

    def filter_data(self, qs: QuerySet, col_name: str, filt_val: Any, filt_op: str) -> QuerySet:
        if filt_op in [OperatorType.ARRAY_CONTAINS, OperatorType.ARRAY_NOT_CONTAINS]:
            filt_val = [filt_val] if not isinstance(filt_val, list) else filt_val
        operator = OPERATOR_LOOKUPS.get(filt_op)
        filter_kwargs = { f"{col_name}{operator.lookup_suffix}": filt_val }
        if operator.exclude:
            return qs.exclude(**filter_kwargs)
        return qs.filter(**filter_kwargs)
    
    def get_neighbors(self, qs: QuerySet[Universal], graph_cols: Sequence[str]) -> dict[Any, list[Any]]:
        """
        Returns a mapping from each entry_id to its list of related IDs
        (across the given graph-cols) in one bulk query.
        """
        mapping: dict[Any, list[Any]] = {}
        for entry_id, *related_lists in qs.values_list("entry_id", *graph_cols):
            for idx, lst in enumerate(related_lists):
                if lst:
                    mapping.setdefault(entry_id, []).extend(lst)
        return mapping

    def get_rows_by_ids(self, qs: QuerySet[Universal], entry_ids: Sequence[Any]) -> QuerySet[Universal]:
        return qs.filter(entry_id__in=entry_ids)
    
    def get_distinct_values(self, qs: QuerySet, col_name: str):
        col_struc = UnivRepoUtil.get_column_structure_type(col_name)
        if col_struc == DataStructureType.JSON:
            qs, new_col_name = UnivRepoUtil.create_col_for_json_key(qs, col_name)
            return list(qs.values_list(new_col_name, flat=True).distinct())
        elif col_struc == DataStructureType.LIST:
            qs, new_col_name = UnivRepoUtil.create_col_for_array_field(qs, col_name)
            return list(qs.values_list(new_col_name, flat=True).distinct())
        else:
            return list(qs.values_list(col_name, flat=True).distinct())
    
    # -------------------- Unique Value Extraction --------------------

    #NOTE: rows can currently have the same key names (ex: restaurnts and albums both have a ratings key), handle this later
    def get_unique_json_keys(self, qs: QuerySet):
        """
        Retrieve all unique keys from the JSON column "fields".
        """
        qs, alias = UnivRepoUtil.create_col_for_all_json_keys(qs)
        return set(qs.values_list(alias, flat=True).distinct())
    
    def get_unique_json_key_values(qs: QuerySet, json_key: str) -> Set:
        """
        1) Keep only rows where `fields` actually has that key
        2) Annotate each row with its value for that key, under the alias "value"
        3) Pull out and dedupe those values
        """
        filter_kwargs = {f"{UniversalColumn.FIELDS}{OperatorType.HAS_KEY}":{json_key}}
        qs_with_key = qs.filter(filter_kwargs)

        # 2) Extract & cast that key’s value into a column called "value"
        qs_with_value, alias = UnivRepoUtil.create_col_for_json_key(qs_with_key, json_key)

        # 3) Return the distinct set of those values
        vals = qs_with_value.values_list("value", flat=True).distinct()
        return set(vals)
    
    def get_unique_json_values(self, user_data_queryset):
        """
        Retrieve all unique values from the JSON column "fields".
        """
        unique_values = get_unique_json_values(user_data_queryset)
        debug_print(f"Finished Query: {len(unique_values)} rows, {type(unique_values)} type")
        return unique_values
    
    def get_unique_json_values(qs: QuerySet) -> Set:
        """
        Retrieve all unique JSON *values* across *all* keys in `fields`,
        using only Django ORM calls (no raw SQL).
        """
        all_vals = set()
        # 1) Find every key present in `fields`
        keys = get_unique_json_keys(qs)  # returns Set[str]
        # 2) For each key, grab its values and add to our set
        for key in keys:
            filter_kwargs = {f"{UniversalColumn.FIELDS.value}{OperatorType.HAS_KEY.value}":{key}}
            fields_expr = f"{UniversalColumn.FIELDS.value}__{key}"
            vals_for_key = qs.filter(filter_kwargs).values_list(fields_expr, flat=True).distinct()
            all_vals.update(v for v in vals_for_key if v is not None)
        return all_vals

    # -------------------- Aggregation --------------------

    def aggregate_field(self, qs: QuerySet, col_name: str, agg_type: str):
        """
        For JSON fields: determine output type and annotate with a temporary field "temp"
        For array fields: annotate with a temporary field "temp" using UNNEST.
        For standard fields: directly aggregate.
        """
        col_struc = UnivRepoUtil.get_column_structure_type(col_name)
        agg_func = AGGREGATION_FUNCTIONS.get(agg_type)
        if col_struc == DataStructureType.JSON:
            qs, new_col_name = UnivRepoUtil.create_col_for_json_key(qs, col_name)
            return qs.aggregate(result=agg_func(new_col_name))["result"]
        elif col_struc == DataStructureType.LIST:
            qs, new_col_name = UnivRepoUtil.create_col_for_array_field(qs, col_name)
            return qs.aggregate(result=agg_func(new_col_name))["result"]
        else:
            return qs.aggregate(result=agg_func(col_name))["result"]
    
    # -------------------- Grouping --------------------

    def get_count_group_aggregate(self, data_qs, grp_cols, tgt_col, freq=None):
        return self.get_group_aggregate(data_qs, grp_cols, "count", tgt_col, freq)
    
    def get_min_group_aggregate(self, data_qs, grp_cols, tgt_col, freq=None):
        return self.get_group_aggregate(data_qs, grp_cols, "min", tgt_col, freq)
    
    def get_max_group_aggregate(self, data_qs, grp_cols, tgt_col, freq=None):
        return self.get_group_aggregate(data_qs, grp_cols, "max", tgt_col, freq)
    
    def get_sum_group_aggregate(self, data_qs, grp_cols, tgt_col, freq=None):
        return self.get_group_aggregate(data_qs, grp_cols, "sum", tgt_col, freq)
    
    def get_average_group_aggregate(self, data_qs, grp_cols, tgt_col, freq=None):
        return self.get_group_aggregate(data_qs, grp_cols, "average", tgt_col, freq)

    
    def get_group_aggregate(self, data_qs, grp_cols, agg_op, tgt_col, freq=None):
        """
        Groups the given queryset by one or more group columns and annotates each group with the
        specified aggregate on target_column.
        
        The grouping preparation handles date truncation and JSON key grouping. Then the target column
        is prepared (casting nested JSON or unnesting arrays) and finally, the grouping helper annotates
        each group with the aggregate (e.g. count, average, sum, min, or max) as "result".
        """
        qs_grouped, adj_grp_cols = create_grouping_columns(data_qs, grp_cols, freq)
        qs_prepared, agg_tgt = transform_target_column_for_aggregation(qs_grouped, tgt_col)
        grouped_data = group_by_data_with_aggregation(qs_prepared, adj_grp_cols, agg_op, agg_tgt)
        debug_print("Query finished")
        return grouped_data