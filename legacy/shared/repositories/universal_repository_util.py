from shared.logger import debug_print
from datetime import datetime
from django.db.models import F, Func, FloatField, DateField, CharField, TextField, BooleanField, IntegerField, JSONField, Count, Avg, Sum, Min, Max, Value
from django.db.models.functions import Cast, TruncDay, TruncWeek, TruncMonth, TruncYear, Lower
from django.db.models.fields.json import KeyTextTransform
from django.contrib.postgres.fields import ArrayField
from shared.models import Universal
from django.db import connection, models
from dateutil.parser import parse as parse_date

# ----------------- Type Mappings -----------------

AGGREGATION_TYPE_MAPPING = {
    "count": Count,
    "avg": Avg,
    "sum": Sum,
    "min": Min,
    "max": Max
}

FREQUENCY_TYPE_MAPPING = {
    "daily": TruncDay,
    "weekly": TruncWeek,
    "monthly": TruncMonth,
    "yearly": TruncYear
}

MODEL_DATA_CLASS_TO_DATA_TYPE_IDENTIFIER_MAPIING = {
    CharField(): "string",
    TextField(): "string",
    BooleanField(): "boolean",
    IntegerField(): "int",
    FloatField(): "float",
    DateField(): "date",
    ArrayField: "list",
    JSONField(): "json"
}

DATA_TYPE_OPERATOR_MAP = {
    "int": ["=", "!=", "<", ">", "<=", ">="],
    "float": ["=", "!=", "<", ">", "<=", ">="],
    "string": ["=", "!=", "string_contains", "string_not_contains"],
    "date": ["=", "!=", "<", ">", "<=", ">="],
    "bool": ["=", "!="],
    "list": ["array_contains", "array_not_contains"]
    }

# ----------------- Type Checking -----------------

def is_array_field(column_name):
    """Return True if the given column is an ArrayField on Universal."""
    field = Universal._meta.get_field(column_name)
    return isinstance(field, ArrayField)

def is_json_field(column_name):
    """Return True if the column is a JSONField. If not defined, assume it is nested JSON."""
    try:
        field = Universal._meta.get_field(column_name)
        return field.get_internal_type() == "JSONField"
    except Exception:
        return True

def get_data_type_from_column_identifier(queryset, column_identifier):
    """
    Determines the data type of a column.
    Returns "json" if it's an undefined field assumed to be nested JSON.
    """
    columns_list = [field.name for field in Universal._meta.fields]
    if column_identifier in columns_list:
        column_data_class = get_column_data_class(queryset=queryset, column_name=column_identifier)
    else:
        column_data_class = get_nested_json_column_data_class(queryset=queryset, column_name=column_identifier)
    data_type_identifier = MODEL_DATA_CLASS_TO_DATA_TYPE_IDENTIFIER_MAPIING.get(column_data_class)
    return data_type_identifier
    
def get_column_data_class(queryset, column_name):
    """
    Determines the data type of a column.
    Returns the Django field class (e.g. models.CharField, models.IntegerField)
    if the field exists on the model; otherwise, it assumes the field is nested JSON
    and returns models.JSONField.
    """
    try:
        field = queryset.model._meta.get_field(column_name)
        # get_internal_type() returns a string such as "CharField" or "IntegerField"
        # We then look up the actual class in django.db.models.
        field_class = getattr(models, field.get_internal_type(), None)
        if field_class is not None:
            return field_class
        else:
            return models.JSONField
    except Exception:
        return models.JSONField    

    
def get_nested_json_column_data_class(queryset, column_name):
    """
    Determines the Django field type for a JSON field based on its first non-null sample value.
    Assumes all values are stored as strings (even if they represent booleans, numbers, or dates)
    and tests for boolean, integer, float, and date. Returns the corresponding Django Field instance.
    """
    lookup = build_lookup_expression(queryset, column_name)
    sample = queryset.values_list(lookup, flat=True).first()
    
    if sample is None:
        return CharField()

    sample = str(sample).strip()
    
    if sample.lower() in ("true", "false"):
        return BooleanField()
    
    try:
        int_val = int(sample)
        if str(int_val) == sample or sample.isdigit():
            return IntegerField()
    except ValueError:
        pass
    
    try:
        float_val = float(sample)
        if '.' in sample:
            return FloatField()
    except ValueError:
        pass
    
    try:
        _ = parse_date(sample)
        return DateField()
    except Exception:
        pass
    
    # Fallback: assume it's a string.
    return CharField()

def get_operators_from_data_type_identifier(data_type_identifier):
    operators = DATA_TYPE_OPERATOR_MAP.get(data_type_identifier)
    if operators is None:
        raise ValueError(f"Unsupported data type: {data_type_identifier}")
    return operators
# ----------------- Connection Cursor Helpers -----------------

# ----------------- Lookup & Expression Building -----------------

def build_lookup_expression(queryset, column_name):
    """
    Builds an ORM lookup expression for a given column.
    Uses `KeyTextTransform` for JSON fields and `F(column_name)` for others.
    """
    try:
        queryset.model._meta.get_field(column_name)
        return F(column_name)
    except Exception:
        return KeyTextTransform(column_name, "fields")

# ----------------- Filtering -----------------

def build_filter_statement(column_name, filter_value, filter_type):
    """
    Constructs a filter condition dictionary for Django ORM filtering.
    """
    filter_map = {
        '=': {f"{column_name}": filter_value},
        '!=': {f"{column_name}": filter_value},  # Handled separately in exclude()
        '<': {f"{column_name}__lt": filter_value},
        '>': {f"{column_name}__gt": filter_value},
        '<=': {f"{column_name}__lte": filter_value},
        '>=': {f"{column_name}__gte": filter_value},
        'string_contains': {f"{column_name}__icontains": filter_value},
        'array_contains': {f"{column_name}__contains": [filter_value] if not isinstance(filter_value, list) else filter_value},
        'array_not_contains': {f"{column_name}__contains": [filter_value] if not isinstance(filter_value, list) else filter_value},
    }

    if filter_type not in filter_map:
        raise ValueError(f"Unsupported filter type: {filter_type}")

    return filter_map[filter_type]

def generate_ids_in_traversal(user_data_queryset, start_id, traversal_columns):
    """
    Recursively traverses relationships in a dataset and collects all relevant entry IDs.
    """
    visited, to_visit = set(), [start_id]

    while to_visit:
        current_id = to_visit.pop()
        if current_id not in visited:
            visited.add(current_id)
            for col_name in traversal_columns.values():
                related_ids = user_data_queryset.filter(entry_id=current_id).values_list(col_name, flat=True).first()
                if related_ids:
                    to_visit.extend(related_ids)

    return visited

def get_unique_id_list(user_data_queryset):
    return list(user_data_queryset.values_list('id', flat=True))

# ----------------- Aggregation -----------------

def perform_aggregation_on_column(queryset, column_name, aggregation_type):
    """
    Performs an aggregation on a column using the specified aggregation function.
    """
    aggregation_func = AGGREGATION_TYPE_MAPPING[aggregation_type]
    return queryset.aggregate(result=aggregation_func(column_name))["result"]

# ----------------- Column Creation (Annotation) -----------------

def create_casted_nested_json_column(queryset, original_column_name, new_column_name, output_field):
    """
    Annotates a queryset with a new field extracted from JSON and casts it.
    """
    lookup = build_lookup_expression(queryset, original_column_name)
    return queryset.annotate(**{new_column_name: Cast(lookup, output_field=output_field)})

def create_date_frequency_label_column(queryset, column_name, frequency_type):
    """
    Annotates the queryset by truncating a date field to a specified frequency.
    Generates a new column name in the format: group_<column_name>_<frequency>.
    """
    frequency_func = FREQUENCY_TYPE_MAPPING.get(frequency_type.lower())
    if not frequency_func:
        raise ValueError("Unsupported frequency. Choose from: daily, weekly, monthly, yearly.")
    
    new_field = f"group_{column_name}_{frequency_type.lower()}"
    return queryset.annotate(**{new_field: frequency_func(column_name)}), new_field

def create_unnested_list_column(queryset, original_column_name, new_column_name):
    """
    Annotates a queryset with an unnested array field.
    """
    return queryset.annotate(**{new_column_name: Func(F(original_column_name), function="UNNEST")})
# def create_unnested_list_column(queryset, original_column_name, new_column_name):
#     """
#     Annotates a queryset with an unnested JSONB array field,
#     converting each element to text.
#     """
#     return queryset.annotate(
#         **{new_column_name: Func(F(original_column_name),
#                                  function="jsonb_array_elements_text",
#                                  output_field=CharField())}
#     )

def create_json_grouping_column(queryset, column_name):
    """
    Creates an annotation for grouping on a JSON field.
    """
    lookup = build_lookup_expression(queryset, column_name)
    new_field = f"group_{column_name}"
    return queryset.annotate(**{new_field: lookup}), new_field

# ----------------- Grouping -----------------

def create_grouping_columns(queryset, group_columns, frequency_type=None):
    """
    Prepares data for grouping by handling date truncation, JSON extraction, and array unnesting.
    """
    if not isinstance(group_columns, list):
        group_columns = [group_columns]

    effective_group_columns = []
    qs = queryset

    for col in group_columns:
        column_type = get_column_data_type(queryset, col)

        if column_type == "datefield" and frequency_type:
            qs, new_field = create_date_frequency_label_column(qs, col, frequency_type)
        elif column_type == "list":
            qs = create_unnested_list_column(qs, col, new_column_name=f"unnested_{col}")
            new_field = f"unnested_{col}"
        elif column_type == "json":
            qs, new_field = create_json_grouping_column(qs, col)
        else:
            new_field = col

        effective_group_columns.append(new_field)

    return qs, effective_group_columns

def transform_target_column_for_aggregation(queryset, target_column):
    """
    Prepares a target column for aggregation by handling JSON and array unnesting.
    """
    if is_json_field(target_column):
        output_field = get_nested_json_column_data_class(queryset, target_column)
        qs = create_casted_nested_json_column(queryset, target_column, "target_temp", output_field)
        return qs, "target_temp"
    elif is_array_field(target_column):
        qs = create_unnested_list_column(queryset, target_column, "target_temp")
        return qs, "target_temp"
    return queryset, target_column

def group_by_data_with_aggregation(queryset, group_columns, aggregation_type, target_column=None):
    """
    Groups the queryset by the provided group_columns and annotates each group with the specified aggregate.
    """
    # Ensure group_columns is a list
    if not isinstance(group_columns, list):
        group_columns = [group_columns]
    # Use the first group column as target if not specified.
    if target_column is None:
        target_column = group_columns[0]
    
    aggregation_func = AGGREGATION_TYPE_MAPPING[aggregation_type]
    if aggregation_func is None:
        raise ValueError(f"Unsupported aggregate operation: {aggregation_func}. Options: {list(AGGREGATION_TYPE_MAPPING.keys())}")
    
    aggregate_dict = {"result": aggregation_func(target_column)}
    grouped_qs = queryset.values(*group_columns).annotate(**aggregate_dict)
    return grouped_qs

# ----------------- Unique Value Extraction -----------------

def get_unique_json_keys(user_data_queryset):
    """
    Retrieve all unique keys from the JSON column "fields".
    """
    return set(user_data_queryset.annotate(json_key=Func(F("fields"), function="jsonb_object_keys")).values_list("json_key", flat=True).distinct())

def get_unique_json_key_values(user_data_queryset, json_key):
    """
    Retrieve unique values for a specific key in the JSON column "fields".
    """
    return set(user_data_queryset.filter(**{"fields__has_key": json_key}).annotate(value=F(f"fields__{json_key}")).values_list("value", flat=True).distinct())

def get_unique_json_values(user_data_queryset):
    """
    Retrieve all unique values from the JSON column "fields",
    regardless of the key they are associated with.
    This implementation uses a raw SQL query with a LATERAL join.
    """
    # Get IDs from the queryset to limit our raw query to the same set of rows.
    ids = get_unique_id_list(user_data_queryset)

    sql = f"""
        SELECT DISTINCT j.value
        FROM universal,
             LATERAL jsonb_each_text(universal.fields::jsonb) AS j(key, value)
        WHERE universal.id IN %s
    """
    #NOTE: we pass represent literals with %s and pass the values in a list in execute (this is for hardcoded values)
    #Passing values manually in the sql statement using {} is not good practice bc of security, and using execute assists with proper datatype formatting (ex: putting quotes around strings)
    with connection.cursor() as cursor:
        cursor.execute(sql, [tuple(ids)])
        rows = cursor.fetchall()
    return set(row[0] for row in rows)