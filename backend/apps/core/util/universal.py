from django.core.exceptions import FieldDoesNotExist
from django.db.models import F, Func, Expression
from django.db.models.fields.json import KeyTextTransform
from core.models import Universal

from collections import deque
from typing import Type, Any, Tuple, List, Dict
from django.db import models
from django.db.models import QuerySet, Expression, Field
from django.db.models.functions import Cast
from django.contrib.postgres.fields import ArrayField

from core.domain.enums.universal import (
    DataType,
    DataStructureType,
    UniversalColumn,
    OperatorType,
    PGFunc,
    FrequencyType
)
from core.domain.mappings.universal import (
    UNIVERSAL_COLUMN_TO_DATATYPE,
    DATA_TYPE_TO_FIELD,
    FREQUENCY_FUNCTIONS
)

def bfs_traverse(
        neighbor_map: Dict[Any, List[Any]],
        start_id: Any
    ) -> List[Any]:
        """
        Perform a breadth-first search over the neighbor_map,
        starting from start_id, and return the list of all visited IDs
        in the order they were first encountered.
        """
        visited = set([start_id])
        queue   = deque([start_id])
        order   = []

        while queue:
            current = queue.popleft()
            order.append(current)
            for nbr in neighbor_map.get(current, []):
                if nbr not in visited:
                    visited.add(nbr)
                    queue.append(nbr)

        return order

def get_json_key_type(qs: QuerySet, json_key: str) -> DataType:
    """
    Determine the primitive DataType of a JSON key by sampling its first non-null value.
    """
    kwargs = { f"{UniversalColumn.FIELDS.value}{OperatorType.IS_NULL}": False }
    non_null = qs.filter(**kwargs)

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

def get_column_primitive_type(queryset: QuerySet, col: str) -> Any:
    """
    Return the DataType for a model field or nested JSON key.
    """
    if col in {col.value for col in UniversalColumn}:
        return UNIVERSAL_COLUMN_TO_DATATYPE.get(UniversalColumn(col))
    else:
        return get_json_key_type(queryset, col)

def get_column_structure_type(col: str) -> DataStructureType:
    """
    Classify a column name as SCALAR, LIST, or JSON.
    """
    univ_cols = {u_col for u_col in UniversalColumn}
    if col not in univ_cols:
        return DataStructureType.JSON
    if UNIVERSAL_COLUMN_TO_DATATYPE.get(UniversalColumn(col)) == DataType.LIST:
        return DataStructureType.LIST
    else:
        return DataStructureType.SCALER

def build_column_ref_expression(col: str) -> Any:
    """
    Return an ORM Expression that references the column or JSON key.
    """
    if col in {col_ for col_ in UniversalColumn}:
        return F(col)
    else:
        return KeyTextTransform(col, UniversalColumn.FIELDS.value)
    
def create_col_for_json_key(
    qs: QuerySet,
    key: str,
    alias: str = "json_value"
) -> Tuple[QuerySet, str]:
    """
    Annotates a queryset with a new field extracted from JSON and casts it.
    """
    col_expr = build_column_ref_expression(key)
    col_d_type = get_column_primitive_type(qs, key)
    field_cls = DATA_TYPE_TO_FIELD.get(col_d_type, models.TextField)
    cast_expr = Cast(col_expr, output_field=field_cls())
    qs_mod = qs.annotate(**{alias: cast_expr})
    return qs_mod, alias

def create_col_for_array_field(
    qs: QuerySet,
    col: str,
    alias: str = "array_element"
) -> Tuple[QuerySet, str]:
    """
    Annotate a queryset by unnesting an ArrayField into rows under `alias`.
    """
    col_expr = build_column_ref_expression(col)
    unnest_expr: Expression = Func(col_expr, function=PGFunc.UNNEST)
    qs_mod = qs.annotate(**{alias: unnest_expr})
    return qs_mod, alias

def create_col_for_all_json_keys(
    qs: QuerySet,
    alias: str = "json_key"
) -> Tuple[QuerySet, str]:
    """
    Annotate each row with its JSONB object's keys under `alias`.
    """
    col_expr = build_column_ref_expression(UniversalColumn.FIELDS)
    extract_expr = Func(col_expr, function=PGFunc.JSONB_OBJECT_KEYS)
    qs_mod = qs.annotate(**{alias: extract_expr})
    return qs_mod, alias

def create_frequency_col(
    qs: QuerySet,
    freq: FrequencyType,
    alias: str = "freq_field"
) -> Tuple[QuerySet, str]:
    """
    Annotate a queryset by truncating the `date` field to a given frequency.
    """
    freq_func = FREQUENCY_FUNCTIONS[freq]
    date_kwargs = {alias: freq_func(UniversalColumn.DATE.value)}
    qs_mod = qs.annotate(**date_kwargs)
    return qs_mod, alias

def adapt_column_for_processing(
    qs: QuerySet,
    col: str,
    alias: str = ''
) -> Tuple[QuerySet, str]:
    """
    Prepare any column for filtering or aggregation:
      - JSON → extract & cast under `alias`
      - LIST → unnest under `alias`
      - SCALAR → no annotation
    Returns (modified_qs, target_column)
    """
    alias = f"agg__{col}" if alias is '' else alias
    col_struc = get_column_structure_type(col)
    if col_struc == DataStructureType.JSON:
        return create_col_for_json_key(qs, col, alias)
    elif col_struc == DataStructureType.LIST:
        return create_col_for_array_field(qs, col, alias)
    return qs, col




