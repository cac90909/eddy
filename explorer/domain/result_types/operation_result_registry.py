# explorer/domain/operation_result_registry.py

from typing import Dict, Type
from explorer.domain.result_types.base_result_type import BaseResultType
from explorer.domain.result_types.raw_result_type import RawResultType
from explorer.domain.result_types.enriched_result_type import EnrichedResultType
from explorer.domain.result_types.list_result_type import ListResultType
from explorer.domain.result_types.metric_result_type import MetricResultType
from explorer.domain.result_types.snapshot_result_type import SnapshotResultType
from explorer.domain.result_types.snapshot_list_result_type import SnapshotListResultType
from explorer.domain.result_types.operations_list_result_type import OperationsListResultType
from explorer.domain.result_types.results_list_result_type import ResultsListResultType
from explorer.domain.result_types.operation_chain_result_type import OperationChainResultType
from explorer.domain.result_types.error_result_type import ErrorResultType
from explorer.domain.result_types.status_result_type import StatusResultType
from explorer.domain.result_types.config_result_type import ConfigResultType

_RESULT_TYPE_REGISTRY: Dict[str, Type[BaseResultType]] = {
    "raw": RawResultType,
    "enriched": EnrichedResultType,
    "list": ListResultType,
    "metric": MetricResultType,
    "snapshot": SnapshotResultType,
    "snapshot_list": SnapshotListResultType,
    "operations_list": OperationsListResultType,
    "results_list": ResultsListResultType,
    "operation_chain": OperationChainResultType,
    "error": ErrorResultType,
    "status": StatusResultType,
    "config": ConfigResultType,
}

def get_result_type_class(result_type_name: str) -> Type[BaseResultType] | None:
    return _RESULT_TYPE_REGISTRY.get(result_type_name)
