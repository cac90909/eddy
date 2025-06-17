# core/universal/enums.py
from enum import Enum

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

class AggregationType(str, Enum):
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



