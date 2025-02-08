from shared.repositories.universal_repository import UniversalRepository
from shared.logger import debug_print_vars

class DataProcessingService:

    def __init__(self):
        self.universal_repository = UniversalRepository()

    def get_user_data(self, user_id):
        user_data = self.universal_repository.get_user_data(user_id=user_id)
        debug_print_vars(user_id=user_id, user_data=user_data)
        return user_data
    
    # Currently supported filter types: =, !=, <, >, string_contains, array_contains, array_not_contains
    def apply_filter(self, user_id, user_data, column_name, filter_value, filter_type):
        filtered_data = self.universal_repository.apply_filter(
            user_data_queryset=user_data,
            column_name=column_name,
            filter_value=filter_value,
            filter_type=filter_type
        )
        debug_print_vars(user_id=user_id, filtered_data=filtered_data, column_name=column_name, filter_value=filter_value, filter_type=filter_type)
        return filtered_data
    
    def traverse_data(self, user_id, user_data, start_id):
        traversed_data = self.universal_repository.traverse_data(
            user_data_queryset=user_data,
            start_id=start_id
        )
        debug_print_vars(user_id=user_id, traversed_data=traversed_data, start_id=start_id)
        return traversed_data
    
    def get_unique_values_from_list_column(self, user_id, user_data, column_name):
        unique_values = self.universal_repository.get_unique_values_from_list_column(
            user_data_queryset=user_data,
            column_name=column_name
        )
        debug_print_vars(user_id=user_id, unique_values=unique_values, column_name=column_name)
        return unique_values
    
    def get_unique_json_keys(self, user_id, user_data):
        unique_keys = self.universal_repository.get_unique_json_keys(
            user_data_queryset=user_data
        )
        debug_print_vars(user_id=user_id, unique_keys=unique_keys)
        return unique_keys
    
    def get_unique_json_values(self, user_id, user_data):
        unique_values = self.universal_repository.get_unique_json_values(
            user_data_queryset=user_data
        )
        debug_print_vars(user_id=user_id, unique_values=unique_values)
        return unique_values
