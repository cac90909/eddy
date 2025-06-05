# explorer/domain/operation_result_types/error.py

from .base_result_type import BaseResultType


class ErrorResultType(BaseResultType):
    @staticmethod
    def verify(result_data):
        return (result_data is None) or isinstance(result_data, str)

    @staticmethod
    def cache_policy(op_instance):
        return False

    @staticmethod
    def data_overview_fields(user_id, result_data):
        return {}

    @staticmethod
    def attach_history():
        return True

    @staticmethod
    def attach_snapshots_list():
        return True

    @staticmethod
    def serialize(result_data):
        return result_data
