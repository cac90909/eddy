# explorer/domain/operation_result_types/metric.py

from explorer.cache.service import ExplorerCacheService
from .base_result_type import BaseResultType


class MetricResultType(BaseResultType):
    @staticmethod
    def verify(result_data):
        return isinstance(result_data, (int, float))

    @staticmethod
    def cache_policy(op_instance):
        return True

    @staticmethod
    def data_overview_fields(user_id, result_data):
        full_shape = ExplorerCacheService().get_operation_chain_metadata_key(
            user_id=user_id, metadata_key="full_data_shape"
        )
        current_shape = {
            "num_rows": 1,
            "num_columns": 1,
            "num_values": 1,
        }
        operations_on_chain = ExplorerCacheService().get_operation_chain_metadata_key(
            user_id=user_id, metadata_key="operations_on_chain"
        )
        return {
            "full_data_shape": full_shape,
            "current_data_shape": current_shape,
            "operations_on_chain": operations_on_chain,
        }

    @staticmethod
    def attach_history():
        return True

    @staticmethod
    def attach_snapshots_list():
        return True

    @staticmethod
    def serialize(result_data):
        return result_data
