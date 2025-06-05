# explorer/domain/operation_result_types/list.py
from explorer.services.explorer_cache_service import ExplorerCacheService
from .base_result_type import BaseResultType


class ListResultType(BaseResultType):
    @staticmethod
    def verify(result_data):
        return isinstance(result_data, set)

    @staticmethod
    def cache_policy(op_instance):
        return True

    @staticmethod
    def data_overview_fields(user_id, result_data):
        full_shape = ExplorerCacheService().get_operation_chain_metadata_key(
            user_id=user_id, metadata_key="full_data_shape"
        )
        current_shape = {
            "num_rows": len(result_data),
            "num_columns": 1,
            "num_values": len(result_data),
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
