# explorer/domain/operation_result_types/config.py

from .base_result_type import BaseResultType


class ConfigResultType(BaseResultType):
    @staticmethod
    def verify(result_data):
        return result_data is not None

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
        return result_data
