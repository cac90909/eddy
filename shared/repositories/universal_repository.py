# from django.db.models import Q, Func, F, Value, Count, Avg, Sum, Min, Max
# from django.db.models import Q, F
# from django.db.models import Count, Avg, Sum, Min, Max
# from django.db import connection
# from shared.models import Universal
# from shared.logger import debug_print
# from shared.util import log_vars_vals_cls, catch_exceptions_cls
# from django.contrib.auth import get_user_model
# from django.contrib.postgres.fields import ArrayField, JSONField
# from django.db.models import F, Count, Avg, Sum, Min, Max, FloatField, DateField
# from django.db.models.functions import Cast, Func
# from datetime import datetime

from django.db.models import Q, F
from django.db.models import Func, Value
from django.db.models import Count, Avg, Sum, Min, Max
from django.db.models import FloatField, DateField
from django.db.models.functions import Cast
from django.db.models.functions import TruncDay, TruncWeek, TruncMonth, TruncYear
from django.db.models.fields.json import KeyTextTransform, KeyTransform


from django.db import connection

from django.contrib.auth import get_user_model
from django.contrib.postgres.fields import ArrayField, JSONField

from shared.models import Universal
from shared.logger import debug_print
from shared.util import log_vars_vals_cls, catch_exceptions_cls

from datetime import datetime


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
        debug_print('Finished Query')
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
        debug_print('Query finished')
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
        debug_print('Query finished')
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
        debug_print('Query finished')
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
        if filter_type not in filter_map:
            raise ValueError(f"Unsupported filter type: {filter_type}")
        if filter_type in ['!=', 'array_not_contains']:
            data = user_data_queryset.exclude(**filter_map[filter_type])
        else:
            data = user_data_queryset.filter(**filter_map[filter_type])
        debug_print('Finished Query')
        return data

    #NOTE: this currently uses entry_ids instead of the id field generated by django orm
    @staticmethod
    def traverse_data(user_data_queryset, start_id, traversal_directions=None):
        if traversal_directions is None:
            traversal_directions = ["upwards"]

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
                for t in traversal_directions:
                    col_name = traversal_mapping.get(t)
                    if col_name:
                        related_ids = user_data_queryset.filter(entry_id=current_id).values_list(col_name, flat=True).first()
                        if related_ids:
                            to_visit.extend(related_ids)
        full_rows = user_data_queryset.filter(entry_id__in=visited)
        debug_print('Finished Query')
        return full_rows

    @staticmethod
    def group_by_data(user_data_queryset, group_column, aggregate_dict=None):
        """
        Groups the queryset by the specified group_column.
        If aggregate_dict is provided, annotates each group with that aggregate,
        otherwise defaults to counting rows (alias "result").
        """
        qs = user_data_queryset.values(group_column)
        if aggregate_dict:
            qs = qs.annotate(**aggregate_dict)
        else:
            qs = qs.annotate(result=Count(group_column))
        return qs

    def perform_group_aggregate(self, user_data_queryset, group_column, aggregate_operation, target_column, frequency=None):
        """
        Groups the given queryset by group_column (optionally applying date truncation)
        and annotates each group with the specified aggregate on target_column.
        
        Parameters:
          - group_column: the column to group by (e.g., "date" or "Artist").
          - frequency: if grouping by date, one of "daily", "weekly", "monthly", or "yearly".
          - aggregate_operation: one of "count", "average", "sum", "min", "max".
          - target_column: the column on which to perform the aggregation. If not a model field,
                          it is assumed to be a key in the JSON "fields" container.
        
        Returns:
          A QuerySet of dictionaries with the effective group column and an aggregate
          value annotated as "result".
        """
        debug_print("Entering perform_group_aggregate")
        # First, apply grouping to the data source. This handles date truncation and JSON key grouping.
        qs_grouped, effective_group_column = self.prepare_data_for_grouping(user_data_queryset, group_column, frequency)
        
        # Prepare the target column for aggregation.
        if is_json_field(target_column):
            # If target_column is not "fields", use KeyTextTransform.
            if target_column == "fields":
                lookup = F("fields")
            else:
                lookup = KeyTextTransform(target_column, "fields")
            # Here, we cast the value to a FloatField.
            qs = qs_grouped.annotate(target_value=Cast(lookup, output_field=FloatField()))
            target_for_agg = "target_value"
        elif is_array_field(target_column):
            qs = qs_grouped.annotate(target_value=Func(F(target_column), function="UNNEST"))
            target_for_agg = "target_value"
        else:
            qs = qs_grouped
            target_for_agg = target_column

        # Define mapping for aggregates.
        AGGREGATE_MAPPING = {
            "count": lambda target: Count(target) if target else Count("id"),
            "average": lambda target: Avg(target),
            "sum": lambda target: Sum(target),
            "min": lambda target: Min(target),
            "max": lambda target: Max(target)
        }
        if aggregate_operation not in AGGREGATE_MAPPING:
            raise ValueError(f"Unsupported aggregate operation: {aggregate_operation}. Available options: {list(AGGREGATE_MAPPING.keys())}")
        agg_func = AGGREGATE_MAPPING[aggregate_operation](target_for_agg)
        aggregate_dict = {"result": agg_func}
        
        grouped_data = self.group_by_data(qs, effective_group_column, aggregate_dict)
        return grouped_data

    # ---------------------------------------------------
    # Helper Methods for Aggregates

    def prepare_data_for_grouping(self, user_data_queryset, group_column, frequency=None):
        """
        Prepares data for grouping by annotating the data with group relevant labels.
        - If group_column is "date" and frequency is provided, it truncates the date accordingly.
        - Otherwise, if group_column is a model field, it is used directly.
        - If not, it is assumed to be a key in the JSON "fields" and annotated using KeyTextTransform.
        
        Returns a tuple: (annotated queryset, effective_group_column)
        """
        # Handle date frequency truncation.
        if group_column == "date" and frequency:
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
            # Try to see if the group_column exists as a top-level field.
            try:
                Universal._meta.get_field(group_column)
                return user_data_queryset, group_column
            except Exception:
                # Not a top-level field; assume it's a key within the JSON "fields" container.
                effective_group_column = f"json_{group_column}"
                qs = user_data_queryset.annotate(**{effective_group_column: KeyTextTransform(group_column, "fields")})
                return qs, effective_group_column

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

    
    # ---------------Simple Aggregates------------------------

    def _get_output_field_for_column(self, user_data_queryset, field_lookup):
        """
        Inspects one sample value from the queryset for the given field lookup.
        If the sample appears to be a date string (using format "%m/%d/%Y"),
        returns DateField; otherwise returns FloatField.
        
        If no sample is found, defaults to FloatField.
        """
        sample_value = user_data_queryset.values_list(field_lookup, flat=True).first()
        if sample_value is None:
            return FloatField()
        # If sample_value is a string, try to parse it as a date.
        if isinstance(sample_value, str):
            try:
                datetime.strptime(sample_value, "%m/%d/%Y")
                return DateField()
            except ValueError:
                return FloatField()
        # If sample_value is already a date or datetime, use DateField.
        if isinstance(sample_value, (datetime, )):
            return DateField()
        # Otherwise, default to FloatField.
        return FloatField()


    def count_data(self, user_data_queryset, column_name, alias="result"):
        try:
            Universal._meta.get_field(column_name)
            actual_column = column_name
        except Exception:
            actual_column = "fields" if column_name == "fields" else f"fields__{column_name}"
        agg = user_data_queryset.aggregate(**{alias: Count(actual_column)})
        debug_print("Finished Query")
        return agg[alias]


    def average_data(self, user_data_queryset, column_name, alias="result"):
        if is_json_field(column_name):
            # If the column is not the entire JSON field, extract the key from 'fields'
            if column_name == "fields":
                lookup = F("fields")
            else:
                lookup = KeyTextTransform(column_name, "fields")
            # Cast the extracted text to a float so AVG can operate on it.
            qs = user_data_queryset.annotate(val=Cast(lookup, output_field=FloatField()))
            agg = qs.aggregate(**{alias: Avg("val")})
        elif is_array_field(column_name):
            qs = user_data_queryset.annotate(expanded=Func(F(column_name), function="UNNEST"))
            agg = qs.aggregate(**{alias: Avg("expanded")})
        else:
            agg = user_data_queryset.aggregate(**{alias: Avg(column_name)})
        debug_print("Finished Query")
        return agg[alias]


    def sum_data(self, user_data_queryset, column_name, alias="result"):
        if is_json_field(column_name):
            # If the column is not the entire JSON field, extract the key from 'fields'
            if column_name == "fields":
                lookup = F("fields")
            else:
                lookup = KeyTextTransform(column_name, "fields")
            # Cast the extracted text to a float so AVG can operate on it.
            qs = user_data_queryset.annotate(val=Cast(lookup, output_field=FloatField()))
            agg = qs.aggregate(**{alias: Sum("val")})
        elif is_array_field(column_name):
                qs = user_data_queryset.annotate(expanded=Func(F(column_name), function="UNNEST"))
                agg = qs.aggregate(**{alias: Sum("expanded")})
        else:
                agg = user_data_queryset.aggregate(**{alias: Sum(column_name)})
        debug_print("Finished Query")
        return agg[alias]


    def min_data(self, user_data_queryset, column_name, alias="result"):
        if is_json_field(column_name):
            # Use KeyTextTransform if not referencing the entire JSON field
            if column_name == "fields":
                lookup = F("fields")
                field_lookup = "fields"
            else:
                lookup = KeyTextTransform(column_name, "fields")
                field_lookup = f"fields__{column_name}"
            # Grab a sample value from the lookup to decide on casting.
            sample = user_data_queryset.values_list(field_lookup, flat=True).first()
            # Try to parse the sample as a date using an expected format.
            try:
                # Change the format as needed.
                datetime.strptime(sample, "%m/%d/%Y")
                output_field = DateField()
            except Exception:
                output_field = FloatField()
            qs = user_data_queryset.annotate(val=Cast(lookup, output_field=output_field))
            agg = qs.aggregate(**{alias: Min("val")})
        elif is_array_field(column_name):
            qs = user_data_queryset.annotate(expanded=Func(F(column_name), function="UNNEST"))
            agg = qs.aggregate(**{alias: Min("expanded")})
        else:
            agg = user_data_queryset.aggregate(**{alias: Min(column_name)})
        debug_print("Finished Query")
        return agg[alias]


    def max_data(self, user_data_queryset, column_name, alias="result"):
        if is_json_field(column_name):
            # Use KeyTextTransform if not referencing the entire JSON field
            if column_name == "fields":
                lookup = F("fields")
                field_lookup = "fields"
            else:
                lookup = KeyTextTransform(column_name, "fields")
                field_lookup = f"fields__{column_name}"
            # Grab a sample value from the lookup to decide on casting.
            sample = user_data_queryset.values_list(field_lookup, flat=True).first()
            # Try to parse the sample as a date using an expected format.
            try:
                # Change the format as needed.
                datetime.strptime(sample, "%m/%d/%Y")
                output_field = DateField()
            except Exception:
                output_field = FloatField()
            qs = user_data_queryset.annotate(val=Cast(lookup, output_field=output_field))
            agg = qs.aggregate(**{alias: Max("val")})
        elif is_array_field(column_name):
            qs = user_data_queryset.annotate(expanded=Func(F(column_name), function="UNNEST"))
            agg = qs.aggregate(**{alias: Max("expanded")})
        else:
            agg = user_data_queryset.aggregate(**{alias: Max(column_name)})
        debug_print("Finished Query")
        return agg[alias]

