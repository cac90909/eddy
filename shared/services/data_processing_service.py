from shared.repositories.universal_repository import UniversalRepository
from shared.util import log_vars_vals_cls, catch_exceptions_cls

@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class DataProcessingService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    def get_user_data(self, user_id):
        user_data = self.universal_repository.get_user_data(user_id=user_id)
        return user_data

    # Currently supported filter types: =, !=, <, >, string_contains, array_contains, array_not_contains
    def apply_filter(self, user_id, user_data, column_name, filter_value, filter_type):
        filtered_data = self.universal_repository.apply_filter(
            user_data_queryset=user_data,
            column_name=column_name,
            filter_value=filter_value,
            filter_type=filter_type
        )
        return filtered_data

    def traverse_data(self, user_id, user_data, start_id):
        traversed_data = self.universal_repository.traverse_data(
            user_data_queryset=user_data,
            start_id=start_id
        )
        return traversed_data

    def get_unique_values_from_list_column(self, user_id, user_data, column_name):
        unique_values = self.universal_repository.get_unique_values_from_list_column(
            user_data_queryset=user_data,
            column_name=column_name
        )
        return unique_values

    def get_unique_json_keys(self, user_id, user_data):
        unique_keys = self.universal_repository.get_unique_json_keys(
            user_data_queryset=user_data
        )
        return unique_keys

    def get_unique_json_values(self, user_id, user_data):
        unique_values = self.universal_repository.get_unique_json_values(
            user_data_queryset=user_data
        )
        return unique_values
