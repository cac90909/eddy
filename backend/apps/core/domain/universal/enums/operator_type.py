from enum import Enum

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