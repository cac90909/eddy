from shared.repositories.universal_repository import UniversalRepository
from shared.logger import debug_print
from shared.util import log_vars_vals_cls, catch_exceptions_cls

#@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class UniversalListService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    def get_unique_column_values(self, user_data, column_name):
        unique_values = self.universal_repository.get_unique_column_values(
            user_data_queryset=user_data,
            column_name=column_name
        )
        return unique_values

    def get_unique_json_keys(self, user_data):
        unique_keys = self.universal_repository.get_unique_json_keys(
            user_data_queryset=user_data
        )
        return unique_keys

    def get_unique_json_values(self, user_data):
        unique_values = self.universal_repository.get_unique_json_values(
            user_data_queryset=user_data
        )
        return unique_values
    

