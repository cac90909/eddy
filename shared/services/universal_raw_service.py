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
        data_amount = user_data.count()
        debug_print(f"{data_amount} rows retrieved")
        return user_data, data_amount

    # Currently supported filter types: =, !=, <, >, string_contains, array_contains, array_not_contains
    def filter_data(self, user_id, data_source, column_name, filter_value, filter_type):
        if not data_source:
            raise Exception("No data source passed")
        filtered_data = self.universal_repository.filter_data(
            user_data_queryset=data_source,
            column_name=column_name,
            filter_value=filter_value,
            filter_type=filter_type
        )
        data_amount = filtered_data.count()
        debug_print(f"{data_amount} rows retrieved")
        return filtered_data, data_amount

    #Supports a combination of horizontal, upwards, downwards for traversal type(s)
    def traverse_data(self, user_id, data_source, start_id, traversal_directions):
        if not data_source:
            raise Exception("No data source passed")
        traversed_data = self.universal_repository.traverse_data(
            user_data_queryset=data_source,
            start_id=start_id,
            traversal_directions=traversal_directions
        )
        data_amount = traversed_data.count()
        debug_print(f"{data_amount} rows retrieved")
        return traversed_data, data_amount

