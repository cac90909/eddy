from enum import Enum

class PGFunc(str, Enum):
    UNNEST           = "UNNEST"
    JSONB_OBJECT_KEYS = "jsonb_object_keys"
    JSONB_EACH_TEXT  = "jsonb_each_text"