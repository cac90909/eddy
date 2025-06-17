# explorer/domain/operation_result_types/snapshot.py

from shared.models import Snapshots
from explorer.util.operation_result_util import is_model_instance
from shared.universal.serializers import SnapshotSerializer
from .base_result_type import BaseResultType


class SnapshotResultType(BaseResultType):
    @staticmethod
    def verify(result_data):
        return is_model_instance(result_data=result_data, model_class=Snapshots)

    @staticmethod
    def cache_policy(op_instance):
        return False

    @staticmethod
    def data_overview_fields(user_id, result_data):
        return {}

    @staticmethod
    def attach_history():
        return False

    @staticmethod
    def attach_snapshots_list():
        return False

    @staticmethod
    def serialize(result_data):
        return SnapshotSerializer(instance=result_data).data
