# explorer/domain/operation_result_types/operation_chain.py

from explorer.util.operation_result_util import is_list_of_operation_instances, serialize_operation_chain
from explorer.services.explorer_cache_service import ExplorerCacheService
from .base_result_type import BaseResultType


class OperationChainResultType(BaseResultType):
    @staticmethod
    def verify(result_data):
        return is_list_of_operation_instances(result_data=result_data)

    @staticmethod
    def cache_policy(op_instance):
        return False

    @staticmethod
    def data_overview_fields(user_id, result_data):
        operations_on_chain = ExplorerCacheService().get_operation_chain_metadata_key(
            user_id=user_id, metadata_key="operations_on_chain"
        )
        return {
            "operations_on_chain": operations_on_chain,
        }

    @staticmethod
    def attach_history():
        return False

    @staticmethod
    def attach_snapshots_list():
        return False

    @staticmethod
    def serialize(result_data):
        return serialize_operation_chain(operation_chain=result_data)
