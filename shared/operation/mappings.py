from typing import Dict, List
from shared.universal.enums import (
    DataType, 
    OperatorType,
    UniversalColumn
)
from shared.operation.enums import (
    TraversalDirection
)

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

TRAVERSAL_DIRECTION_TO_UNIVERSAL_COLUMN: Dict[TraversalDirection, UniversalColumn] = {
    TraversalDirection.UPWARDS.value: UniversalColumn.PARENTS_IDS,
    TraversalDirection.DOWNWARDS.value: UniversalColumn.CHILDREN_IDS,
    TraversalDirection.HORIZONTAL.value: UniversalColumn.SIBLINGS_IDS
}