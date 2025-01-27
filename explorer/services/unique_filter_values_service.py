from explorer.repositories.user_data_repository import UserDataRepository
from explorer.services.cache_service import CacheService
from shared.logger import debug_print

class UniqueFilterValuesService:
    def __init__(self):
        self.repository = UserDataRepository()
        self.cache_service= CacheService

    def get_unique_filter_values(self, user_id, column_name):
        data = self.cache_service.get_most_recent_cache_data(user_id=user_id)
        return self.repository.get_unique_filter_options(user_data_queryset=data)
