from shared.repositories.universal_repository import UniversalRepository
from shared.logger import debug_print
from shared.util import log_vars_vals_cls, catch_exceptions_cls

#@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class UniversalListService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    def get_unique_column_values(self, data_source, column_name):
        list_res = self.universal_repository.get_unique_column_values(
            user_data_queryset=data_source,
            column_name=column_name
        )
        return list_res

    def get_unique_json_keys(self, data_source):
        list_res = self.universal_repository.get_unique_json_keys(
            user_data_queryset=data_source
        )
        return list_res

    def get_unique_json_key_values(self, data_source, json_key):
        list_res = self.universal_repository.get_unique_json_key_values(
            user_data_queryset=data_source, json_key=json_key
        )
        return list_res
    

