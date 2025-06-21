from django.core.exceptions import FieldDoesNotExist
from django.db.models import F, Func
from django.db.models.fields.json import KeyTextTransform
from shared.models import Universal  # or wherever your model lives

from typing import Type, Any
from django.db import models
from django.db.models import QuerySet, Expression, Field
from django.db.models.functions import Cast
from django.contrib.postgres.fields import ArrayField

from shared.universal.enums import (
    DataType,
    DataStructureType,
    UniversalColumn,
    ARRAY_OPERATORS,
    OperatorType,
    DJANGO_FIELD_TYPES,
    PGFunc
)
from shared.universal.mappings import (
    OPERATOR_LOOKUPS,
    UNIVERSAL_COLUMN_TO_DATATYPE,
    DATA_TYPE_TO_FIELD
)

def get_json_key_type(qs: QuerySet, json_key: str) -> DataType:
    """
    Sample the first non-null value of fields->json_key and parse its type.
    Returns one of STRING, INT, FLOAT, DATE, or falls back to STRING.
    """
    nulls_kw = { f"{UniversalColumn.FIELDS.value}__isnull": False }
    non_null = qs.filter(**nulls_kw)

    json_kw = f"{UniversalColumn.FIELDS.value}__{json_key}"
    val_sample = non_null.values_list(json_kw, flat=True).first()
    
    if val_sample is None:
        return DataType.STRING

    s = str(val_sample).strip().lower()
    if s in ("true", "false"):
        return DataType.BOOLEAN
    try:
        int(s); return DataType.INT
    except: pass
    try:
        float(s); return DataType.FLOAT
    except: pass
    from dateutil.parser import parse as _parse
    try:
        _parse(s); return DataType.DATE
    except: pass

    return DataType.STRING

def get_column_primitive_type(queryset: QuerySet, col_name: str) -> DataType:
    """
    Retrieves data type of the column, handling logic for if column is fields key or not.
    """
    if col_name in {col.value for col in UniversalColumn}:
        col_data_type = UNIVERSAL_COLUMN_TO_DATATYPE.get(UniversalColumn(col_name))
    else:
        col_data_type = get_json_key_type(queryset, col_name)
    return col_data_type

def get_column_structure_type(col_name: str) -> DataStructureType:
    univ_cols = {col.value for col in UniversalColumn}
    if col_name not in univ_cols:
        return DataStructureType.JSON
    if UNIVERSAL_COLUMN_TO_DATATYPE.get(col_name) == DataType.LIST:
        return DataStructureType.LIST
    else:
        return DataStructureType.SCALER

def build_column_ref_expression(col_name: str):
    """
    Return F(column_name) if it's a real field, 
    or KeyTextTransform(column_name, "fields") for JSON.
    """
    if col_name in {col.value for col in UniversalColumn}:
        return F(col_name)
    else:
        return KeyTextTransform(col_name, UniversalColumn.FIELDS.value)
    
def create_col_for_json_key(qs: QuerySet, key_name, alias: str = "json_key"):
    """
    Annotates a queryset with a new field extracted from JSON and casts it.
    """
    col_expr = build_column_ref_expression(key_name)
    col_d_type = get_column_primitive_type(qs, key_name)
    field_cls = DATA_TYPE_TO_FIELD.get(col_d_type)
    cast_expr = Cast(col_expr, output_field=DATA_TYPE_TO_FIELD.get(field_cls()))
    qs_mod = qs.annotate(**{alias: cast_expr})
    return qs_mod, alias

def create_col_for_array_field(qs: QuerySet, col_name: str, alias: str ="arr_field") -> QuerySet:
    """
    Annotate the queryset by unnesting the Postgres ArrayField `col_name` into rows,
    placing each element under `alias`.
    """
    col_expr = build_column_ref_expression(col_name)
    unnest_expr: Expression = Func(col_expr, function=PGFunc.UNNEST)
    qs_mod = qs.annotate(**{alias: unnest_expr})
    return qs_mod, alias

def create_col_for_all_json_keys(qs: QuerySet, alias: str ="json_keys"):
    col_expr = build_column_ref_expression(UniversalColumn.FIELDS)
    extract_expr = Func(col_expr, function=PGFunc.JSONB_OBJECT_KEYS)
    qs_mod = qs.annotate(**{alias: extract_expr})
    return qs_mod , alias
