from typing import Dict, List, Type
from shared.universal.enums import (
    DataType, 
    OperatorType,
    UniversalColumn,
    TRAVERSABLE_COLUMNS
)
from shared.operation.enums import (
    TraversalDirection,
    OperationType,
    OperationName
)
from shared.common.serializers import StandardResponseSerializer
from shared.operation.serializers import (
    RawOperationResponseSerializer,
    ListOperationResponseSerializer,
    MetricOperationResponseSerializer,
    EnrichedOperationResponseSerializer
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
    TraversalDirection.UPWARDS: UniversalColumn.PARENTS_IDS,
    TraversalDirection.DOWNWARDS: UniversalColumn.CHILDREN_IDS,
    TraversalDirection.HORIZONTAL: UniversalColumn.SIBLINGS_IDS
}

OPERATION_TYPE_TO_SERIALIZER: Dict[OperationType, Type[StandardResponseSerializer]] = {
    OperationType.RAW: RawOperationResponseSerializer,
    OperationType.LIST: ListOperationResponseSerializer,
    OperationType.METRIC: MetricOperationResponseSerializer,
    OperationType.ENRICHED: EnrichedOperationResponseSerializer
}