from shared.logger import debug_print
from shared.repositories.universal_repository_util import (
    is_json_field,
    is_array_field,
    get_nested_json_column_type,
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
from shared.models import Universal
User = get_user_model()

class UniversalRepository:

    # -------------------- Data Retrieval --------------------

    def get_user_data(self, user_id):
        """Retrieve all data for a specific user."""
        user_instance = User.objects.get(pk=user_id)
        data = Universal.objects.filter(user=user_instance)
        debug_print(f"Finished Query: {data.count()} rows, {type(data)} type")
        return data

    def filter_data(self, user_data_queryset, column_name, filter_value, filter_type):
        try:
            Universal._meta.get_field(column_name)
            actual_column = column_name
        except Exception:
            actual_column = f"fields__{column_name}"

        filter_condition = build_filter_statement(actual_column, filter_value, filter_type)

        if filter_type in ['!=', 'array_not_contains']:
            data = user_data_queryset.exclude(**filter_condition) 
        else:
            data = user_data_queryset.filter(**filter_condition)
        debug_print(f"Finished Query: {data.count()} rows, {type(data)} type")
        return data
    
    @staticmethod
    def traverse_data(user_data_queryset, start_id, traversal_directions=None):
        """
        Traverses hierarchical relationships in the dataset and retrieves all relevant rows.
        """
        if traversal_directions is None:
            traversal_directions = ["upwards"]
        traversal_mapping = {
            "upwards": "parents_ids",
            "downwards": "children_ids",
            "horizontal": "siblings_ids"
        }
        traversal_columns = {direction: traversal_mapping[direction] for direction in traversal_directions if direction in traversal_mapping}
        visited_ids = generate_ids_in_traversal(user_data_queryset, start_id, traversal_columns)
        full_rows = user_data_queryset.filter(entry_id__in=visited_ids)
        debug_print(f"Finished Query: {full_rows.count()} rows, {type(full_rows)} type")
        return full_rows
    
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

    def get_aggregate(self, user_data_queryset, column_name, aggregation_type):
        """
        For JSON fields: determine output type and annotate with a temporary field "temp"
        For array fields: annotate with a temporary field "temp" using UNNEST.
        For standard fields: directly aggregate.
        #NOTE: this currently assumes datatypes are correct at this point (this method is not restricted and can do things like find sum of dates)
        #NOTE: want to expand on this method, right now can only aggregate on floats and ints (but what about list floats? etc)
        #NOTE: this method annotates on the appropriate datatype, which isnt needed for count, but count is still contained here because it should output the same result
        """
        if is_json_field(column_name):
            output_field_type = get_nested_json_column_type(user_data_queryset, column_name)
            queryset = create_casted_nested_json_column(queryset=user_data_queryset, original_column_name=column_name, new_column_name="temp", output_field=output_field_type)
            aggregation_result = perform_aggregation_on_column(queryset=queryset, column_name="temp", aggregation_type=aggregation_type)
        elif is_array_field(column_name):
            queryset = create_unnested_list_column(queryset=user_data_queryset, original_column_name=column_name, new_column_name="temp")
            aggregation_result = perform_aggregation_on_column(queryset=queryset, column_name="temp", aggregation_type=aggregation_type)
        else:
            aggregation_result = perform_aggregation_on_column(queryset=user_data_queryset, column_name=column_name, aggregation_type=aggregation_type)
        debug_print("Query finished")
        return aggregation_result
    
    # -------------------- Grouping --------------------
    
    def perform_group_aggregate(self, user_data_queryset, group_columns, aggregate_operation, target_column, frequency=None):
        """
        Groups the given queryset by one or more group columns and annotates each group with the
        specified aggregate on target_column.
        
        The grouping preparation handles date truncation and JSON key grouping. Then the target column
        is prepared (casting nested JSON or unnesting arrays) and finally, the grouping helper annotates
        each group with the aggregate (e.g. count, average, sum, min, or max) as "result".
        """
        qs_grouped, effective_group_columns = create_grouping_columns(user_data_queryset, group_columns, frequency)
        qs_prepared, target_for_agg = transform_target_column_for_aggregation(qs_grouped, target_column)
        grouped_data = group_by_data_with_aggregation(qs_prepared, effective_group_columns, aggregate_operation, target_for_agg)
        debug_print("Query finished")
        return grouped_data