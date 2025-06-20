from typing import List, Any, Dict, str, Sequence
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
    NEGATION_OPERATORS
)
from shared.universal.mappings import (
    OPERATOR_TO_LOOKUP_SUFFIX
)
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
        # coerce single value → list for array lookups
        if filt_op in ARRAY_OPERATORS and not isinstance(filt_val, list):
            filt_val = [filt_val]

        # build the lookup key
        suffix = OPERATOR_TO_LOOKUP_SUFFIX.get(filt_op)
        if suffix is None:
            raise ValueError(f"Unsupported filter operator {filt_op!r}")
        lookup = { f"{col_name}{suffix}": filt_val }

        # decide include vs exclude
        if filt_op in NEGATION_OPERATORS:
            return qs.exclude(**lookup)

        return qs.filter(**lookup)
    
    @staticmethod
    def traverse_data(qs: QuerySet, start_id : UniversalColumn.ENTRY_ID, cols : List[UniversalColumn]):
        """
        Traverses hierarchical relationships in the dataset and retrieves all relevant rows.
        """
        visited, to_visit = set(), [start_id]

        while to_visit:
            current_id = to_visit.pop()
            if current_id not in visited:
                visited.add(current_id)
                for col_name in cols.values():
                    related_ids = qs.filter(entry_id=current_id).values_list(col_name, flat=True).first()
                    if related_ids:
                        to_visit.extend(related_ids)

        full_rows = qs.filter(entry_id__in=visited)
        return full_rows
    
    def get_neighbors(
        self,
        qs: QuerySet[Universal],
        graph_cols: Sequence[str],
    ) -> dict[Any, list[Any]]:
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

    def get_rows_by_ids(
        self,
        qs: QuerySet[Universal],
        entry_ids: Sequence[Any],
    ) -> QuerySet[Universal]:
        return qs.filter(entry_id__in=entry_ids)
    
    # -------------------- Unique Value Extraction --------------------
    
    def get_unique_column_values(self, user_data_queryset, column_name):
        """
        Retrieve unique values from a column (non-"fields" column).
        """
        column_type = get_column_data_type(user_data_queryset, column_name)
        if column_type == "arrayfield":  # Unnest array field and retrieve unique values
            queryset = create_unnested_list_column(user_data_queryset, column_name, new_column_name="unnested_value")
            values = queryset.values_list("unnested_value", flat=True).distinct()
        else:  # Retrieve distinct values directly
            values = user_data_queryset.values_list(column_name, flat=True).distinct()
        unique_values = set(values)
        debug_print(f"Finished Query: {len(unique_values)} rows, {type(unique_values)} type")
        return unique_values

    #NOTE: rows can currently have the same key names (ex: restaurnts and albums both have a ratings key), handle this later
    def get_unique_json_keys(self, user_data_queryset):
        """
        Retrieve all unique keys from the JSON column "fields".
        """
        unique_keys = get_unique_json_keys(user_data_queryset)
        debug_print(f"Finished Query: {len(unique_keys)} rows, {type(unique_keys)} type")
        return unique_keys

    def get_unique_json_key_values(self, user_data_queryset, json_key):
        """
        Retrieve unique values for a specific key in the JSON column "fields".
        """
        unique_key_values = get_unique_json_key_values(user_data_queryset, json_key)
        debug_print(f"Finished Query: {len(unique_key_values)} rows, {type(unique_key_values)} type")
        return unique_key_values
    
    def get_unique_json_values(self, user_data_queryset):
        """
        Retrieve all unique values from the JSON column "fields".
        """
        unique_values = get_unique_json_values(user_data_queryset)
        debug_print(f"Finished Query: {len(unique_values)} rows, {type(unique_values)} type")
        return unique_values

    # -------------------- Aggregation --------------------

    def get_count(self, data_qs, col_name):
        return self.get_aggregate(data_qs, col_name, "count")
    
    def get_min(self, data_qs, col_name):
        return self.get_aggregate(data_qs, col_name, "min")
    
    def get_max(self, data_qs, col_name):
        return self.get_aggregate(data_qs, col_name, "max")
    
    def get_average(self, data_qs, col_name):
        return self.get_aggregate(data_qs, col_name, "average")
    
    def get_sum(self, data_qs, col_name):
        return self.get_aggregate(data_qs, col_name, "sum")

    def get_aggregate(self, user_data_queryset, column_name, agg_type):
        """
        For JSON fields: determine output type and annotate with a temporary field "temp"
        For array fields: annotate with a temporary field "temp" using UNNEST.
        For standard fields: directly aggregate.
        #NOTE: this currently assumes datatypes are correct at this point (this method is not restricted and can do things like find sum of dates)
        #NOTE: want to expand on this method, right now can only aggregate on floats and ints (but what about list floats? etc)
        #NOTE: this method annotates on the appropriate datatype, which isnt needed for count, but count is still contained here because it should output the same result
        """
        if is_json_field(column_name):
            output_field_type = get_nested_json_column_data_class(user_data_queryset, column_name)
            queryset = create_casted_nested_json_column(queryset=user_data_queryset, original_column_name=column_name, new_column_name="temp", output_field=output_field_type)
            aggregation_result = perform_aggregation_on_column(queryset=queryset, column_name="temp", aggregation_type=agg_type)
        elif is_array_field(column_name):
            queryset = create_unnested_list_column(queryset=user_data_queryset, original_column_name=column_name, new_column_name="temp")
            aggregation_result = perform_aggregation_on_column(queryset=queryset, column_name="temp", aggregation_type=agg_type)
        else:
            aggregation_result = perform_aggregation_on_column(queryset=user_data_queryset, column_name=column_name, aggregation_type=aggregation_type)
        debug_print("Query finished")
        return aggregation_result
    
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