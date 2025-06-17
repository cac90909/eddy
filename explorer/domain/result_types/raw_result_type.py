# explorer/domain/operation_result_types/raw.py

from explorer.domain.result_types.base_result_type import BaseResultType
from explorer.cache.service import ExplorerCacheService
from shared.universal.serializers import UniversalSerializer
from shared.models import Universal

class RawResultType(BaseResultType):
    @staticmethod
    def verify(result_data):
        from explorer.util.operation_result_util import is_queryset_of_model_instances
        return is_queryset_of_model_instances(result_data=result_data, model_class=Universal)

    @staticmethod
    def cache_policy(op_instance):
        # In your old dict: "cache_policy": lambda op: True
        return True

    @staticmethod
    def data_overview_fields(user_id, result_data):
        full_shape = ExplorerCacheService().get_operation_chain_metadata_key(
            user_id=user_id, metadata_key="full_data_shape"
        )
        current_shape = {
            "num_rows": len(result_data),
            "num_columns": len(result_data[0]._meta.fields) if len(result_data) else 0,
            "num_values": len(result_data) * (len(result_data[0]._meta.fields) if len(result_data) else 0),
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
        # This matches your old lambda: UniversalSerializer(...)
        return UniversalSerializer(instance=list(result_data), many=True).data
