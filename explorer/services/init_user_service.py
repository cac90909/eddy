from explorer.repositories.user_data_repository import UserDataRepository
from explorer.services.cache_service import CacheService

class InitUserService:
    def __init__(self, cache_service=None, repository=None):
        self.cache_service = cache_service or CacheService()
        self.repository = repository or UserDataRepository()

    def initialize_user_data(self, user_id):
        """
        Fetches all data for the given user and caches it as the initial dataset.

        Args:
            user_id (int): The ID of the user.

        Returns:
            Queryset: All data for the user.
        """
        # Fetch user data from the repository
        user_data = self.repository.get_user_data(user_id)

        # Cache the user data as the initial dataset
        self.cache_service.cache_data(user_id, user_data, cache_type="auto")

        return user_data
