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

    def handle_operation(self, user_id, operation_type, operation_params):
        if operation_type == "load_data":
            # Delegate to the assemble (reassembly) method
            snapshot_id = operation_params.get("snapshot_id")
            return self.assemble_dataset_from_operation_chain(user_id=user_id, snapshot_id=snapshot_id)
        else:
            return self.perform_data_operation(user_id=user_id,
                                                    data_operation_type=operation_type,
                                                    data_operation_params=operation_params)

    def perform_data_operation(self, user_id, data_operation_type, data_operation_params):
        debug_print(user_id, data_operation_type, data_operation_params)
        data_operation_params["user_data_queryset"] = None if data_operation_type == "init_user" else self.cache_service.get_most_recent_cache_data(user_id=user_id)
        result_data = self.map_data_operation_to_function(data_operation_type, data_operation_params)
        self.cache_service.cache_data(user_id=user_id, data=result_data)
        return result_data

    def map_data_operation_to_function(self, user_id, data_operation_type, data_operation_params=None):
        return {
            "init_user": self.universal_repository.get_user_data(user_id=user_id, **data_operation_params),
            "filter": self.universal_repository.filter_data(user_id=user_id, **data_operation_params),
            "traverse": self.universal_repository.traverse_data(user_id=user_id, **data_operation_params)
        }[data_operation_type]

    def assemble_dataset_from_operation_chain(self, user_id, snapshot_id):
        # Retrieve the snapshot and its operation chain
        snapshot = self.snapshot_service.get_snapshot(user_id=user_id, snapshot_id=snapshot_id)
        operation_chain = snapshot.operation_chain
        debug_print(f"Assembling from operation chain: {operation_chain}")

        # Start with the initial dataset
        current_data = self.universal_repository.get_user_data(user_id=user_id)

        # Process each subsequent operation in the chain
        for operation in operation_chain[1:]:
            data_operation_type = operation["data_operation_type"]
            data_operation_params = operation["data_operation_params"]
            # Pass the current_data as the data to work on for the operation.
            data_operation_params["user_data_queryset"] = current_data
            # Use the mapping function (or delegate to DataOperationService)
            current_data = self.map_data_operation_to_function(
                user_id=user_id,
                data_operation_type=data_operation_type,
                data_operation_params=data_operation_params
            )
        # Optionally update the cache with the reassembled dataset
        self.cache_service.cache_data(user_id=user_id, data=current_data)
        return current_data


