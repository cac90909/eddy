from shared.logger import debug_print
from shared.repositories.universal_repository import UniversalRepository
from explorer.services.cache_service import CacheService
from explorer.services.snapshots_service import SnapshotsService


class ExplorerService:
    def __init__(self):
        super().__init__()
        self.universal_repository = UniversalRepository()
        self.cache_service = CacheService()
        self.snapshot_service = SnapshotsService()

    def perform_data_operation(self, user_id, data_operation_type, data_operation_params, cache_data=True,
                               original_data=None):
        debug_print(user_id, data_operation_type, data_operation_params)

        if data_operation_type == "init_user":
            result_data = self.universal_repository.get_user_data(user_id=user_id)
        else:
            # Use the provided original_data, or fetch from cache if not provided
            original_data = original_data if original_data is not None else self.cache_service.get_most_recent_cache_data(user_id=user_id)
            # For operations other than init, pass the data into the parameters
            data_operation_params["user_data_queryset"] = original_data
            if data_operation_type == "filter":
                result_data = self.universal_repository.filter_data(**data_operation_params)
            elif data_operation_type == "traverse":
                result_data = self.universal_repository.traverse_data(**data_operation_params)
            else:
                raise ValueError(f"Unsupported data operation type: {data_operation_type}")

        if cache_data:
            self.cache_service.cache_data(user_id=user_id, data=result_data)
        return result_data

    def assemble_dataset_from_operation_chain(self, user_id, operation_chain):
        debug_print(f"Assembling from operation chain: {operation_chain}")
        # Start with the initial data (the 'init_user' operation)
        current_data = self.perform_data_operation(
            user_id=user_id,
            data_operation_type="init_user",
            data_operation_params=None,
            cache_data=False,
            original_data=None
        )
        # Process the rest of the operations in the chain
        for operation in operation_chain[1:]:
            data_operation_type = operation["data_operation_type"]
            data_operation_params = operation["data_operation_params"]
            current_data = self.perform_data_operation(
                user_id=user_id,
                data_operation_type=data_operation_type,
                data_operation_params=data_operation_params,
                cache_data=False,
                original_data=current_data
            )
        # Optionally update the cache with the final dataset
        self.cache_service.cache_data(user_id=user_id, data=current_data)
        return current_data
