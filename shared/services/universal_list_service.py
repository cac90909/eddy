from shared.repositories.universal_repository import UniversalRepository
from shared.logger import debug_print
from shared.util import log_vars_vals_cls, catch_exceptions_cls

#@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value="Error", exclude=None)
#NOTE: each gets an options call right now, to prevent errors and allow for consistency.
#Retrieving filter options are the only case right now that have an opeeration argument that is not used in their handler.
#Will want to resolve this later.
class UniversalListService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    def get_unique_column_values(self, user_id, data_source, column_name):
        list_res = self.universal_repository.get_unique_column_values(
            user_data_queryset=data_source,
            column_name=column_name
        )
        return list_res

    def get_unique_json_keys(self, user_id, data_source):
        list_res = self.universal_repository.get_unique_json_keys(
            user_data_queryset=data_source
        )
        return list_res

    def get_unique_json_key_values(self, user_id, data_source, json_key):
        list_res = self.universal_repository.get_unique_json_key_values(
            user_data_queryset=data_source, json_key=json_key
        )
        return list_res
    
    def get_unique_json_values(self, user_id, data_source):
        list_res = self.universal_repository.get_unique_json_values(
            user_data_queryset=data_source
        )
        return list_res
    

