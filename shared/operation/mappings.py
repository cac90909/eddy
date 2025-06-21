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

TRAVERSAL_DIRECTION_TO_UNIVERSAL_COLUMN: Dict[TraversalDirection, UniversalColumn] = {
    TraversalDirection.UPWARDS.value: UniversalColumn.PARENTS_IDS,
    TraversalDirection.DOWNWARDS.value: UniversalColumn.CHILDREN_IDS,
    TraversalDirection.HORIZONTAL.value: UniversalColumn.SIBLINGS_IDS
}