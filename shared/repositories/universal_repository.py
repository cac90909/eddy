from django.db.models import Q, Func, F, Value, Count, Avg, Sum, Min, Max
from django.db import connection
from shared.models import Universal
from shared.logger import debug_print
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from django.contrib.auth import get_user_model
from django.contrib.postgres.fields import ArrayField, JSONField

User = get_user_model()

# ---------------------------------------------------
# Shared ORM Lookup Translation Mapping

OPERATOR_LOOKUP = {
    '=': '',      # equality: no suffix
    '<': '__lt',
    '>': '__gt',
    '<=': '__lte',
    '>=': '__gte'
}

def build_orm_filter(field, operator, value):
    """
    Given a field (or alias) name, an operator (e.g., ">") and a value,
    returns a dictionary suitable for a Django filter.
    For example:
      build_orm_filter("result", ">", 30)
    returns: {"result__gt": 30}
    """
    if operator not in OPERATOR_LOOKUP:
        raise ValueError(f"Unsupported operator: {operator}")
    lookup = OPERATOR_LOOKUP[operator]
    return {f"{field}{lookup}": value}

# ---------------------------------------------------
# Utility Functions to detect field types

def is_array_field(column_name):
    """Return True if the column on Universal is an ArrayField."""
    field = Universal._meta.get_field(column_name)
    return isinstance(field, ArrayField)

def is_json_field(column_name):
    """
    Return True if the column is a JSONField.
    If the column is not explicitly defined on Universal,
    assume it is intended as a key within the JSON container.
    """
    try:
        field = Universal._meta.get_field(column_name)
        return field.get_internal_type() == "JSONField"
    except Exception:
        return True

# ---------------------------------------------------
# Repository Class

class UniversalRepository:
    """
    Repository for interacting with the Universal model.
    All ORM-related queries reside here.
    """

    @staticmethod
    def get_user_data(user_id):
        """Retrieve all data for a specific user."""
        user_instance = User.objects.get(pk=user_id)
        data = Universal.objects.filter(user=user_instance)
        debug_print(f'{data.count()} rows retrieved')
        return data

    def get_unique_column_values(self, user_data_queryset, column_name):
        """
        Retrieve unique values from a column.
        If the column is an array field, unnest the values;
        otherwise, simply use distinct.
        """
        try:
            field = Universal._meta.get_field(column_name)
        except Exception:
            field = None
        if field and isinstance(field, ArrayField):
            values = (
                user_data_queryset
                .annotate(expanded_value=Func(F(column_name), function="UNNEST"))
                .values_list("expanded_value", flat=True)
                .distinct()
            )
        else:
            values = user_data_queryset.values_list(column_name, flat=True).distinct()
        unique_values = set(values)
        debug_print(f'{len(unique_values)} unique values')
        return unique_values

    @staticmethod
    def get_unique_json_keys(user_data_queryset):
        """
        Retrieve all unique keys from the JSON column "fields".
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
        Retrieve unique values for each key in the JSON column "fields".
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

    def filter_data(self, user_data_queryset, column_name, filter_value, filter_type):
        """
        Filter raw data based on a column's value.
        This method performs pre-aggregation filtering.
        If the column is not defined on the model, it is assumed to be nested within the JSON container,
        and "fields__" is prepended.
        """
        try:
            Universal._meta.get_field(column_name)
            actual_column = column_name
        except Exception:
            actual_column = f"fields__{column_name}"
        filter_map = {
            '=': {f"{actual_column}": filter_value},
            '!=': {f"{actual_column}__ne": filter_value},
            '<': {f"{actual_column}__lt": filter_value},
            '>': {f"{actual_column}__gt": filter_value},
            'string_contains': {f"{actual_column}__icontains": filter_value},
            'array_contains': {f"{actual_column}__contains": [filter_value] if not isinstance(filter_value, list) else filter_value},
            'array_not_contains': {f"{actual_column}__contains": [filter_value] if not isinstance(filter_value, list) else filter_value},
        }
        debug_print("filter_data:", column_name, filter_value, filter_type, user_data_queryset)
        if filter_type not in filter_map:
            raise ValueError(f"Unsupported filter type: {filter_type}")
        if filter_type in ['!=', 'array_not_contains']:
            data = user_data_queryset.exclude(**filter_map[filter_type])
        else:
            data = user_data_queryset.filter(**filter_map[filter_type])
        debug_print(f'{user_data_queryset.count()} rows filtered to {data.count()} rows')
        return data

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

    @staticmethod
    def group_by_data(user_data_queryset, group_column, aggregate_dict=None):
        """
        Group data by group_column and annotate with aggregates.
        If aggregate_dict is not provided, defaults to counting rows (alias "result").
        """
        qs = user_data_queryset.values(group_column)
        if aggregate_dict:
            qs = qs.annotate(**aggregate_dict)
        else:
            qs = qs.annotate(result=Count(group_column))
        return qs

    # ---------------------------------------------------
    # Helper Methods for Aggregates

    def apply_grouping(self, user_data_queryset, group_column, frequency=None):
        """
        If grouping on a date field and a frequency is provided,
        apply the appropriate Trunc function to create a grouping field.
        Returns a tuple: (annotated QuerySet, effective_group_column).
        """
        if group_column == "date" and frequency:
            from django.db.models.functions import TruncDay, TruncWeek, TruncMonth, TruncYear
            frequency = frequency.lower()
            if frequency == "daily":
                qs = user_data_queryset.annotate(group_field=TruncDay("date"))
            elif frequency == "weekly":
                qs = user_data_queryset.annotate(group_field=TruncWeek("date"))
            elif frequency == "monthly":
                qs = user_data_queryset.annotate(group_field=TruncMonth("date"))
            elif frequency == "yearly":
                qs = user_data_queryset.annotate(group_field=TruncYear("date"))
            else:
                raise ValueError("Unsupported frequency. Choose from: daily, weekly, monthly, yearly.")
            return qs, "group_field"
        else:
            return user_data_queryset, group_column

    def filter_aggregates(self, grouped_qs, operator, value, alias="result"):
        """
        Filters a grouped and annotated QuerySet (with aggregate alias 'alias')
        using a high-level operator and threshold on the aggregated value.
        Delegates ORM lookup construction to build_orm_filter.
        """
        orm_filter = build_orm_filter(alias, operator, value)
        return grouped_qs.filter(**orm_filter)

    def union_data(self, queryset1, queryset2):
        """Return the union of two QuerySets (they must have the same structure)."""
        return queryset1.union(queryset2)

    def join_data(self, queryset1, queryset2):
        """
        Full join of two QuerySets is not directly supported in Django ORM.
        As a placeholder, convert both to lists and merge them.
        """
        list1 = list(queryset1.values())
        list2 = list(queryset2.values())
        return list1 + list2

    # ---------------------------------------------------
    # Aggregation Methods (non-grouped)

    def count_data(self, user_data_queryset, column_name, alias="result"):
        debug_print("Entering count_data")
        qs = user_data_queryset
        if is_json_field(column_name):
            qs = qs.annotate(extracted=F("fields") if column_name == "fields" else F(column_name))
            agg = qs.aggregate(**{alias: Count("extracted")})
            return agg[alias]
        elif is_array_field(column_name):
            qs = qs.annotate(expanded=Func(F(column_name), function="UNNEST"))
            agg = qs.aggregate(**{alias: Count("expanded")})
            return agg[alias]
        else:
            agg = qs.aggregate(**{alias: Count(column_name)})
            return agg[alias]

    def average_data(self, user_data_queryset, column_name, alias="result"):
        debug_print("Entering average_data")
        qs = user_data_queryset
        if is_json_field(column_name):
            qs = qs.annotate(val=F("fields") if column_name == "fields" else F(column_name))
            agg = qs.aggregate(**{alias: Avg("val")})
            return agg[alias]
        elif is_array_field(column_name):
            qs = qs.annotate(expanded=Func(F(column_name), function="UNNEST"))
            agg = qs.aggregate(**{alias: Avg("expanded")})
            return agg[alias]
        else:
            agg = qs.aggregate(**{alias: Avg(column_name)})
            return agg[alias]

    def sum_data(self, user_data_queryset, column_name, alias="result"):
        debug_print("Entering sum_data")
        qs = user_data_queryset
        if is_json_field(column_name):
            qs = qs.annotate(val=F("fields") if column_name == "fields" else F(column_name))
            agg = qs.aggregate(**{alias: Sum("val")})
            return agg[alias]
        elif is_array_field(column_name):
            qs = qs.annotate(expanded=Func(F(column_name), function="UNNEST"))
            agg = qs.aggregate(**{alias: Sum("expanded")})
            return agg[alias]
        else:
            agg = qs.aggregate(**{alias: Sum(column_name)})
            return agg[alias]

    def min_data(self, user_data_queryset, column_name, alias="result"):
        debug_print("Entering min_data")
        qs = user_data_queryset
        if is_json_field(column_name):
            qs = qs.annotate(val=F("fields") if column_name == "fields" else F(column_name))
            agg = qs.aggregate(**{alias: Min("val")})
            return agg[alias]
        elif is_array_field(column_name):
            qs = qs.annotate(expanded=Func(F(column_name), function="UNNEST"))
            agg = qs.aggregate(**{alias: Min("expanded")})
            return agg[alias]
        else:
            agg = qs.aggregate(**{alias: Min(column_name)})
            return agg[alias]

    def max_data(self, user_data_queryset, column_name, alias="result"):
        debug_print("Entering max_data")
        qs = user_data_queryset
        if is_json_field(column_name):
            qs = qs.annotate(val=F("fields") if column_name == "fields" else F(column_name))
            agg = qs.aggregate(**{alias: Max("val")})
            return agg[alias]
        elif is_array_field(column_name):
            qs = qs.annotate(expanded=Func(F(column_name), function="UNNEST"))
            agg = qs.aggregate(**{alias: Max("expanded")})
            return agg[alias]
        else:
            agg = qs.aggregate(**{alias: Max(column_name)})
            return agg[alias]
