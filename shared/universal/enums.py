# core/universal/enums.py
from enum import Enum
from django.db.models import (
    CharField, TextField, BooleanField, IntegerField, FloatField, DateField, JSONField
)
from django.db.models import Field

class UniversalColumn(str, Enum):
    DATE                = "date"
    FUNCTIONALITIES     = "functionalities"
    SUBJECT_MATTERS     = "subject_matters"
    GENERAL_CATEGORIES  = "general_categories"
    TITLE               = "title"
    TEXT                = "text"
    TAGS                = "tags"
    PARENTS_IDS         = "parents_ids"
    CHILDREN_IDS        = "children_ids"
    SIBLINGS_IDS        = "siblings_ids"
    FIELDS              = "fields"
    ENTRY_ID            = "entry_id"
    USER                = "user"

class AggregateType(str, Enum):
    COUNT = "count"
    AVG   = "avg"
    SUM   = "sum"
    MIN   = "min"
    MAX   = "max"

class FrequencyType(str, Enum):
    DAILY   = "daily"
    WEEKLY  = "weekly"
    MONTHLY = "monthly"
    YEARLY  = "yearly"

class DataType(str, Enum):
    STRING  = "string"
    INT     = "int"
    FLOAT   = "float"
    DATE    = "date"
    BOOLEAN = "boolean"
    LIST    = "list"
    JSON    = "json"

class OperatorType(str, Enum):
    EQ                 = "="
    NEQ                = "!="
    LT                 = "<"
    GT                 = ">"
    LTE                = "<="
    GTE                = ">="
    STRING_CONTAINS    = "string_contains"
    STRING_NOT_CONTAINS= "string_not_contains"
    ARRAY_CONTAINS     = "array_contains"
    ARRAY_NOT_CONTAINS = "array_not_contains"
    IS_NULL             = "is_null"
    IS_NOT_NULL         = "is_not_null"
    HAS_KEY             = "has_key"
    DOESNT_HAVE_KEY     = "doesnt_have_key"
    IN                  = "in"
    NOT_IN              = "not_in"


TRAVERSABLE_COLUMNS = {
    UniversalColumn.PARENTS_IDS,
    UniversalColumn.CHILDREN_IDS,
    UniversalColumn.SIBLINGS_IDS
}

class DataStructureType(str, Enum):
    SCALER = "scaler"
    JSON = "json"
    LIST = "list"

DJANGO_FIELD_TYPES: Field = {
    CharField,
    TextField,
    BooleanField,
    IntegerField,
    FloatField,
    DateField,
    JSONField
}

class PGFunc(str, Enum):
    UNNEST           = "UNNEST"
    JSONB_OBJECT_KEYS = "jsonb_object_keys"
    JSONB_EACH_TEXT  = "jsonb_each_text"

