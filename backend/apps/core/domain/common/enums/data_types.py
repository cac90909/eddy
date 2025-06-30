from enum import Enum

class DataType(str, Enum):
    STRING  = "string"
    INT     = "int"
    FLOAT   = "float"
    DATE    = "date"
    BOOLEAN = "boolean"
    LIST    = "list"
    JSON    = "json"