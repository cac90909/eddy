# explorer/domain/operation_results/enriched_operation_result.py

from explorer.util.operation_result_util import is_queryset_of_dicts
from explorer.cache.service import ExplorerCacheService
from shared.universal.serializers import FlexibleDictSerializer
from .base_result_type import BaseResultType


class EnrichedResultType(BaseResultType):
    @staticmethod
    def verify(result_data):
        return is_queryset_of_dicts(result_data=result_data)

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
            "num_columns": len(result_data[0]) if len(result_data) else 0,
            "num_values": len(result_data) * (len(result_data[0]) if len(result_data) else 0),
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
        return FlexibleDictSerializer(instance=list(result_data), many=True).data
