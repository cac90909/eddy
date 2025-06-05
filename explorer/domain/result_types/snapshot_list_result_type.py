# explorer/domain/operation_result_types/snapshot_list.py

from shared.models import Snapshots
from explorer.util.operation_result_util import is_queryset_of_model_instances
from shared.serializers import SnapshotSerializer
from .base_result_type import BaseResultType


class SnapshotListResultType(BaseResultType):
    @staticmethod
    def verify(result_data):
        return is_queryset_of_model_instances(result_data=result_data, model_class=Snapshots)

    @staticmethod
    def cache_policy(op_instance):
        return False

    @staticmethod
    def data_overview_fields(user_id, result_data):
        return {
            "num_snapshots": len(result_data),
        }

    @staticmethod
    def attach_history():
        return False

    @staticmethod
    def attach_snapshots_list():
        return False

    @staticmethod
    def serialize(result_data):
        return [SnapshotSerializer(instance=snapshot).data for snapshot in result_data]
