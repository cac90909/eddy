from shared.repositories.universal_repository import UniversalRepository
from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from shared.repositories.universal_repository import UniversalRepository

#@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class UniversalRawService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    def get_user_universal(self, user_id):
        queryset_resp = self.universal_repository.get_user_data(user_id=user_id)
        return queryset_resp

    # Currently supported filter types: =, !=, <, >, string_contains, array_contains, array_not_contains
    def filter(self, user_id, data_source, column_name, filter_value, filter_type):
        queryset_resp = self.universal_repository.filter_data(
            user_data_queryset=data_source,
            column_name=column_name,
            filter_value=filter_value,
            filter_type=filter_type
        )
        return queryset_resp

    #Supports a combination of horizontal, upwards, downwards for traversal type(s)
    def traverse(self, user_id, data_source, start_id, traversal_directions):
        queryset_resp = self.universal_repository.traverse_data(
            user_data_queryset=data_source,
            start_id=start_id,
            traversal_directions=traversal_directions
        )
        return queryset_resp

