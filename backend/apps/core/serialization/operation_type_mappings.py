from core.domain.operation import OperationType
from core.serialization.operation import (
    RawOperationResponseSerializer,
    MetricOperationResponseSerializer,
    EnrichedOperationResponseSerializer,
    ListOperationResponseSerializer
)

OP_TYPE_TO_SERIALIZER = {
    OperationType.RAW: RawOperationResponseSerializer,
    OperationType.METRIC: MetricOperationResponseSerializer,
    OperationType.ENRICHED: EnrichedOperationResponseSerializer,
    OperationType.LIST: ListOperationResponseSerializer
}