from shared.repositories.universal_repository import UniversalRepository
from shared.logger import debug_print
from shared.util import log_vars_vals_cls, catch_exceptions_cls

#@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class UniversalRawService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    def init_data(self, user_id, data_source):
        user_data = self.universal_repository.get_user_data(user_id=user_id)
        return user_data

    # Currently supported filter types: =, !=, <, >, string_contains, array_contains, array_not_contains
    def filter_data(self, data_source, column_name, filter_value, filter_type):
        filtered_data = self.universal_repository.filter_data(
            user_data_queryset=data_source,
            column_name=column_name,
            filter_value=filter_value,
            filter_type=filter_type
        )
        return filtered_data

    #Supports a combination of horizontal, upwards, downwards for traversal type(s)
    def traverse_data(self, data_source, start_id, traversal_directions):
        traversed_data = self.universal_repository.traverse_data(
            user_data_queryset=data_source,
            start_id=start_id,
            traversal_directions=traversal_directions
        )
        return traversed_data

