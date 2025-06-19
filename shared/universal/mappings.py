# core/universal/mappings.py
from django.db.models import Count, Avg, Sum, Min, Max
from django.db.models.functions import TruncDay, TruncWeek, TruncMonth, TruncYear
from django.db.models import (
    CharField, TextField, BooleanField, IntegerField, FloatField, DateField, JSONField
)
from django.contrib.postgres.fields import ArrayField

from typing import Any, Dict, Callable, List
from .enums import (
    AggregationType, FrequencyType, DataType, OperatorType
)
from shared.universal.enums import DataType
from shared.operation.enums import UniversalColumn
import shared.universal.util as UniversalUtil 

AGGREGATION_FUNCTIONS = {
    AggregationType.COUNT: Count,
    AggregationType.AVG:   Avg,
    AggregationType.SUM:   Sum,
    AggregationType.MIN:   Min,
    AggregationType.MAX:   Max,
}

FREQUENCY_FUNCTIONS = {
    FrequencyType.DAILY:   TruncDay,
    FrequencyType.WEEKLY:  TruncWeek,
    FrequencyType.MONTHLY: TruncMonth,
    FrequencyType.YEARLY:  TruncYear,
}

FIELD_TO_DATA_TYPE = {
    CharField:    DataType.STRING,
    TextField:    DataType.STRING,
    BooleanField: DataType.BOOLEAN,
    IntegerField: DataType.INT,
    FloatField:   DataType.FLOAT,
    DateField:    DataType.DATE,
    ArrayField:   DataType.LIST,
    JSONField:    DataType.JSON,
}

OPERATORS_BY_TYPE = {
    DataType.INT:     [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.LT.value, OperatorType.GT.value, OperatorType.LTE.value, OperatorType.GTE.value],
    DataType.FLOAT:   [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.LT.value, OperatorType.GT.value, OperatorType.LTE.value, OperatorType.GTE.value],
    DataType.STRING:  [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.STRING_CONTAINS.value, OperatorType.STRING_NOT_CONTAINS.value],
    DataType.DATE:    [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.LT.value, OperatorType.GT.value, OperatorType.LTE.value, OperatorType.GTE.value],
    DataType.BOOLEAN: [OperatorType.EQ.value, OperatorType.NEQ.value],
    DataType.LIST:    [OperatorType.ARRAY_CONTAINS.value, OperatorType.ARRAY_NOT_CONTAINS.value],
}

FILTER_LOOKUP_BUILDERS: Dict[str, Callable[[str, Any], Dict[str, Any]]] = {
    OperatorType.EQ.value:                 lambda col, val: {col: val},
    OperatorType.NEQ.value:                lambda col, val: {col: val},
    OperatorType.LT.value:                 lambda col, val: {f"{col}__lt": val},
    OperatorType.GT.value:                 lambda col, val: {f"{col}__gt": val},
    OperatorType.LTE.value:                lambda col, val: {f"{col}__lte": val},
    OperatorType.GTE.value:                lambda col, val: {f"{col}__gte": val},
    OperatorType.STRING_CONTAINS.value:    lambda col, val: {f"{col}__icontains": val},
    OperatorType.ARRAY_CONTAINS.value:     lambda col, val: {f"{col}__contains": val if isinstance(val, list) else [val]},
    OperatorType.ARRAY_NOT_CONTAINS.value: lambda col, val: {f"{col}__contains": val if isinstance(val, list) else [val]},
}

UNIVERSAL_COLUMN_TO_DATATYPE: dict[UniversalColumn, DataType] = {
    UniversalColumn.DATE:              DataType.DATE,
    UniversalColumn.TEXT:              DataType.STRING,
    UniversalColumn.TITLE:             DataType.STRING,
    UniversalColumn.FUNCTIONALITIES:   DataType.LIST,
    UniversalColumn.SUBJECT_MATTERS:   DataType.LIST,
    UniversalColumn.GENERAL_CATEGORIES:DataType.LIST,
    UniversalColumn.TAGS:              DataType.LIST,
    UniversalColumn.PARENTS_IDS:       DataType.LIST,
    UniversalColumn.CHILDREN_IDS:      DataType.LIST,
    UniversalColumn.SIBLINGS_IDS:      DataType.LIST,
    UniversalColumn.ENTRY_ID:          DataType.STRING,
    UniversalColumn.FIELDS:            DataType.JSON,
    UniversalColumn.USER:              DataType.STRING,
}

ValueProvider = Callable[[int, Any, str], List[Any]]
DATATYPE_TO_VALUE_PROVIDER: dict[DataType, ValueProvider] = {
    DataType.LIST:   UniversalUtil.get_list_values,
    DataType.STRING: UniversalUtil.get_scalar_values,
    DataType.INT:    UniversalUtil.get_scalar_values,
    DataType.FLOAT:  UniversalUtil.get_scalar_values,
    DataType.DATE:   UniversalUtil.get_scalar_values,
    DataType.JSON:   UniversalUtil.get_json_values,  
}
