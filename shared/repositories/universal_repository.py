from django.db.models import Q, Func, F, Value
from django.db import connection
from shared.models import Universal
from shared.logger import debug_print
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from django.contrib.auth import get_user_model

User = get_user_model()

class UniversalRepository:
    """
    Repository for interacting with the Universal model.
    """

    @staticmethod
    def get_user_data(user_id):
        """
        Retrieve all data for a specific user.
        """
        user_instance = User.objects.get(pk=user_id)
        data = Universal.objects.filter(user=user_instance)
        debug_print(f'{data.count()} rows retrieved')
        return data

    def get_unique_values_from_list_column(self, user_data_queryset, column_name):
        """
        Retrieve unique values from a list-based column.
        
        Args:
            user_data_queryset: QuerySet containing the data.
            column_name (str): The name of the column containing lists.
        
        Returns:
            set: A set of unique values across all rows for the given column.
        """
        values = (
            user_data_queryset
            .annotate(expanded_value=Func(F(column_name), function="UNNEST"))
            .values_list("expanded_value", flat=True)
            .distinct()
        )
        unique_values = set(values)
        debug_print(f'{len(unique_values)} unique values')
        return unique_values

    @staticmethod
    def get_unique_json_keys(user_data_queryset):
        """
        Retrieve all unique keys from the 'fields' JSON column.
        
        Args:
            user_data_queryset: QuerySet containing the data.
        
        Returns:
            set: A set of unique JSON keys found in the 'fields' column.
        """
        keys = (
            user_data_queryset
            .annotate(json_key=Func(F("fields"), function="jsonb_object_keys"))
            .values_list("json_key", flat=True)
            .distinct()
        )
        unique_keys = set(keys)
        debug_print(f'{len(unique_keys)} unique keys')
        return unique_keys

    @staticmethod
    def get_unique_json_values(user_data_queryset):
        """
        Retrieve unique values for each key in the 'fields' JSON column.
        
        Args:
            user_data_queryset: QuerySet containing the data.
        
        Returns:
            dict: A dictionary where each key is a JSON key and the value is a set of unique values for that key.
        """
        unique_values = {}
        total_vals = 0
        keys = UniversalRepository.get_unique_json_keys(user_data_queryset)
        for key in keys:
            key_values = (
                user_data_queryset
                .filter(**{"fields__has_key": key})
                .annotate(value=F(f"fields__{key}"))
                .values_list("value", flat=True)
                .distinct()
            )
            unique_values[key] = set(key_values)
            total_vals += len(key_values)
        debug_print(f'{total_vals} unique values, across {len(keys)} unique keys')
        return unique_values

    @staticmethod
    def filter_data(user_data_queryset, column_name, filter_value, filter_type):
        """
        Filter data based on column name, filter value, and filter type.
        Args:
            user_data_queryset: QuerySet to filter.
            column_name (str): Column to filter on.
            filter_value: Value to filter by.
            filter_type (str): Type of filter ('=', '<', '>', 'contains', '!=' or 'not_equals').
        Returns:
            QuerySet: Filtered QuerySet.
        """
        filter_map = {
            '=': {f"{column_name}": filter_value},
            '!=': {f"{column_name}__ne": filter_value},
            '<': {f"{column_name}__lt": filter_value},
            '>': {f"{column_name}__gt": filter_value},
            'string_contains': {f"{column_name}__icontains": filter_value},
            'array_contains': {f"{column_name}__contains": filter_value},
            'array_not_contains': {f"{column_name}__contains": filter_value}
        }

        if filter_type not in filter_map:
            raise ValueError(f"Unsupported filter type: {filter_type}")

        if filter_type in ['!=', 'array_not_contains']:
            data = user_data_queryset.exclude(**filter_map[filter_type])
        else:
            data = user_data_queryset.filter(**filter_map[filter_type])
        debug_print(f'{user_data_queryset.count()} rows filtered to {data.count()} rows')
        return data

    @staticmethod
    def sort_data(user_data_queryset, column_name, ascending=True):
        sort_order = column_name if ascending else f"-{column_name}"
        return user_data_queryset.order_by(sort_order)

    @staticmethod
    def group_data(user_data_queryset, column_name):
        from django.db.models import Count
        return user_data_queryset.values(column_name).annotate(count=Count(column_name))

    #NOTE: this currently uses entry_ids instead of the id field generated by django orm
    @staticmethod
    def traverse_data(user_data_queryset, start_id, traversal_types=None):
        if traversal_types is None:
            traversal_types = ["upwards"]

        traversal_mapping = {
            "upwards": "parents_ids",
            "downwards": "children_ids",
            "horizontal": "siblings_ids"
        }

        visited = set()
        to_visit = [start_id]

        while to_visit:
            current_id = to_visit.pop()
            if current_id not in visited:
                visited.add(current_id)
                for t in traversal_types:
                    col_name = traversal_mapping.get(t)
                    if col_name:
                        related_ids = user_data_queryset.filter(entry_id=current_id).values_list(col_name, flat=True).first()
                        if related_ids:
                            to_visit.extend(related_ids)
        full_rows = list(user_data_queryset.filter(entry_id__in=visited))
        debug_print(f'{user_data_queryset.count()} rows traversed to {len(full_rows)} rows')
        return full_rows
