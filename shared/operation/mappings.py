from typing import Dict, List
from shared.universal.enums import DataType, OperatorType

DATATYPE_TO_OPERATORS: Dict[DataType, List[str]] = {
    DataType.INT:     ["=", "!=", "<", ">", "<=", ">="],
    DataType.FLOAT:   ["=", "!=", "<", ">", "<=", ">="],
    DataType.DATE:    ["=", "!=", "<", ">", "<=", ">="],
    DataType.STRING:  ["=", "!=", "contains", "not contains"],
    DataType.BOOLEAN: ["=", "!="],
    DataType.LIST:    ["contains", "not contains"],
    DataType.JSON:    ["contains", "not contains"],
}

CONTAINS_OPERATOR_MAP: Dict[str, Dict[DataType, OperatorType]] = {
    "contains": {
        DataType.STRING: OperatorType.STRING_CONTAINS,
        DataType.LIST:   OperatorType.ARRAY_CONTAINS,
        DataType.JSON:   OperatorType.ARRAY_CONTAINS,
    },
    "not contains": {
        DataType.STRING: OperatorType.STRING_NOT_CONTAINS,
        DataType.LIST:   OperatorType.ARRAY_NOT_CONTAINS,
        DataType.JSON:   OperatorType.ARRAY_NOT_CONTAINS,
    },
}