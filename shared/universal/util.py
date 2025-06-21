from django.core.exceptions import FieldDoesNotExist
from django.db.models import F, Func
from django.db.models.fields.json import KeyTextTransform
from shared.universal.utils.types import get_column_data_type
from shared.universal.enums import DataType
from shared.models import Universal  # or wherever your model lives

from typing import Type, Any
from django.db import models
from django.db.models import QuerySet, Expression
from django.contrib.postgres.fields import ArrayField

from shared.universal.enums import (
    DataType,
    UniversalColumn,
    ARRAY_OPERATORS,
    OperatorType
)
from shared.universal.mappings import (
    OPERATOR_TO_LOOKUP_SUFFIX,
    UNIVERSAL_COLUMN_TO_DATATYPE
)

def _get_json_key_type(queryset, json_key: str) -> DataType:
    """
    Sample the first non-null value of fields->json_key and parse its type.
    Returns one of STRING, INT, FLOAT, DATE, or falls back to STRING.
    """
    sample = queryset.values_list(f"fields__{json_key}", flat=True).filter(~models.Q(fields__isnull=True)).first()
    if sample is None:
        return DataType.STRING

    s = str(sample).strip().lower()
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

def get_column_data_type(queryset, col_name: str) -> DataType:
    """
    Retrieves data type of the column, handling logic for if column is fields key or not.
    """
    if col_name in {col.value for col in UniversalColumn}:
        col_data_type = UNIVERSAL_COLUMN_TO_DATATYPE.get(UniversalColumn(col_name))
    else:
        col_data_type = _get_json_key_type(queryset, col_name)
    return col_data_type

def add_temp_column(qs: QuerySet, expression: Expression, alias: str) -> QuerySet:
    """
    Adds a computed column to the queryset under `alias`, using the given ORM `expression`.
    This is just a thin wrapper around qs.annotate(**{alias: expression}).
    """
    return qs.annotate(**{alias: expression})

def build_filter_kwargs( col_name: str, filt_op: OperatorType, filt_val: Any):
    # coerce single value → list for array lookups
    if filt_op in ARRAY_OPERATORS and not isinstance(filt_val, list):
        filt_val = [filt_val]

    # build the lookup key
    suffix = OPERATOR_TO_LOOKUP_SUFFIX.get(filt_op)
    if suffix is None:
        raise ValueError(f"Unsupported filter operator {filt_op!r}")
    filter_kwargs = { f"{col_name}{suffix}": filt_val }
    return filter_kwargs

def build_column_ref_expression(col_name: str):
    """
    Return F(column_name) if it's a real field, 
    or KeyTextTransform(column_name, "fields") for JSON.
    """
    if col_name in {col.value for col in UniversalColumn}:
        return F(col_name)
    else:
        return KeyTextTransform(col_name, UniversalColumn.FIELDS.value)