# core/universal/mappings.py
from typing import Type, NamedTuple

from django.db.models import Count, Avg, Sum, Min, Max
from django.db.models import (
    CharField, 
    TextField, 
    BooleanField, 
    IntegerField, 
    FloatField, 
    DateField, 
    JSONField, 
    Field
)
from django.db.models.functions import TruncDay, TruncWeek, TruncMonth, TruncYear
from django.contrib.postgres.fields import ArrayField

from shared.universal.enums import (
    AggregateType, 
    FrequencyType, 
    DataType, 
    OperatorType, 
    UniversalColumn
)


AGGREGATION_FUNCTIONS = {
    AggregateType.COUNT: Count,
    AggregateType.AVG:   Avg,
    AggregateType.SUM:   Sum,
    AggregateType.MIN:   Min,
    AggregateType.MAX:   Max,
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

DATA_TYPE_TO_FIELD: dict[DataType, Type[Field]] = {
    DataType.STRING:  CharField,    
    DataType.BOOLEAN: BooleanField,
    DataType.INT:     IntegerField,
    DataType.FLOAT:   FloatField,
    DataType.DATE:    DateField,
    DataType.LIST:    ArrayField,
    DataType.JSON:    JSONField,
}


class LookupItem(NamedTuple):
    lookup_suffix: str  
    exclude: bool 
OPERATOR_LOOKUPS: dict[str, LookupItem] = {
OperatorType.EQ.value:                LookupItem("", False),
OperatorType.NEQ.value:               LookupItem("", True),
OperatorType.LT.value:                LookupItem("__lt", False),
OperatorType.GT.value:                LookupItem("__gt", False),
OperatorType.LTE.value:               LookupItem("__lte", False),
OperatorType.GTE.value:               LookupItem("__gte", False),
OperatorType.STRING_CONTAINS.value:   LookupItem("__icontains", False),
OperatorType.ARRAY_CONTAINS.value:    LookupItem("__contains", False),
OperatorType.ARRAY_NOT_CONTAINS.value:LookupItem("__contains", True),
OperatorType.IS_NULL.value:           LookupItem("__isnull", False),
OperatorType.IS_NOT_NULL.value:       LookupItem("__isnull", False),
OperatorType.HAS_KEY.value:           LookupItem("__has_key", False),
OperatorType.DOESNT_HAVE_KEY.value:   LookupItem("__has_key", True),
OperatorType.IN.value:                LookupItem("__in", False),
OperatorType.NOT_IN.value:            LookupItem("__in", True)
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

OPERATORS_BY_TYPE = {
    DataType.INT:     [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.LT.value, OperatorType.GT.value, OperatorType.LTE.value, OperatorType.GTE.value],
    DataType.FLOAT:   [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.LT.value, OperatorType.GT.value, OperatorType.LTE.value, OperatorType.GTE.value],
    DataType.STRING:  [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.STRING_CONTAINS.value, OperatorType.STRING_NOT_CONTAINS.value],
    DataType.DATE:    [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.LT.value, OperatorType.GT.value, OperatorType.LTE.value, OperatorType.GTE.value],
    DataType.BOOLEAN: [OperatorType.EQ.value, OperatorType.NEQ.value],
    DataType.LIST:    [OperatorType.ARRAY_CONTAINS.value, OperatorType.ARRAY_NOT_CONTAINS.value],
}