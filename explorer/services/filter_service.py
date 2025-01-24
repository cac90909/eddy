from explorer.repositories.user_data_repository import UserDataRepository
from explorer.services.cache_service import CacheService
from shared.logger import debug_print

class FilterService:
    def __init__(self):
        self.repository = UserDataRepository()
        self.cache_manager = CacheService()

    def filter_data(self, user_id, column_name, filter_value, filter_type):
        """
        Filters data for the given user and parameters.

        Args:
            user_id (int): The ID of the user whose data is being filtered.
            column_name (str): The column to apply the filter on.
            filter_value: The value to filter against.
            filter_type (str): The type of filter ('=', '<', '>').

        Returns:
            List[Dict]: The filtered dataset.
        """
        # Retrieve the user's most recent dataset from the cache
        data = self.cache_manager.get_most_recent_cache_data(user_id)
        if data is None:
            raise ValueError(f"No data found in cache for user ID {user_id}")

        # Apply the filter operation
        filtered_data = self.repository.filter_data(data, column_name, filter_value, filter_type)
        #debug_print(filtered_data)

        # Update the cache with the filtered dataset
        self.cache_manager.cache_data(user_id, filtered_data)

        return list(filtered_data.values())
