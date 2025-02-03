from shared.logger import debug_print
from shared.repositories.universal_repository import UniversalRepository
from explorer.services.cache_service import CacheService



class ExplorerService:


    def __init__(self):
        super().__init__()
        self.universal_repository = UniversalRepository()
        self.cache_service = CacheService()

    #Performs the passed data operation data by invoking the mapped repository method and then saves to automatic cache
    #Currently supports data_operation_types: init_user, filter, traverse
    def perform_data_operation(self, user_id, data_operation_type, data_operation_params):
        debug_print(user_id, data_operation_type, data_operation_params)

        if data_operation_type == "init_user": result_data = self.universal_repository.get_user_data(user_id=user_id)
        else:
            original_data = self.cache_service.get_most_recent_cache_data(user_id=user_id)
            data_operation_params["user_data_queryset"] = original_data
            if data_operation_type == "filter": result_data = self.universal_repository.filter_data(**data_operation_params)
            elif data_operation_type == "traverse": result_data = self.universal_repository.traverse_data(**data_operation_params)
            else: raise ValueError(f"Unsupported data operation type: {data_operation_type}")
        self.cache_service.cache_data(user_id=user_id,data=result_data)

        return result_data






