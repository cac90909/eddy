from django.core.exceptions import FieldDoesNotExist
from shared.universal.utils.types import get_column_data_type
from shared.universal.enums import DataType
from shared.models import Universal  # or wherever your model lives

from typing import Type, Any
from django.db import models
from django.contrib.postgres.fields import ArrayField

from shared.universal.enums import (
    DataType,
    UniversalColumn
)
from shared.universal.mappings import (
    FILTER_LOOKUP_BUILDERS,
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

def _get_list_values(user_id, qs, col_name):
    # e.g. unnest + distinct
    return qs.values_list(col_name, flat=True).distinct()

def _get_scalar_values(user_id, qs, col_name):
    # e.g. dates, strings, ints, floats
    return qs.values_list(col_name, flat=True).distinct()

def _get_json_values(user_id, qs, json_key):
    # e.g. extract nested JSON values
    return qs.filter(fields__has_key=json_key).values_list(f"fields__{json_key}", flat=True).distinct()

def build_filter_statement(column_name: str, filter_value: Any, filter_type: str) -> dict:
    # filter_type here is guaranteed to be one of OperatorType.value
    try:
        builder = FILTER_LOOKUP_BUILDERS[filter_type]
    except KeyError:
        raise ValueError(f"Unsupported filter type: {filter_type}")
    return builder(column_name, filter_value)