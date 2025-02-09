from shared.logger import debug_print_vars
from shared.services.data_processing_service import DataProcessingService
from explorer.services.explorer_cache_service import ExplorerCacheService
from shared.services.snapshots_service import SnapshotsService
from shared.repositories.universal_repository import UniversalRepository


class ExplorerService:
    def __init__(self):
        super().__init__()
        self.universal_repository = UniversalRepository()
        self.explorer_cache_service = ExplorerCacheService()
        self.snapshot_service = SnapshotsService()
        self.data_processing_service = DataProcessingService()

    def handle_operation(self, user_id, operation_type, operation_params):
        debug_print_vars(user_id=user_id, operation_type=operation_type, operation_params=operation_params)
        if operation_type == "init_user":
            user_data = self.data_processing_service.get_user_data(user_id=user_id)
            self.explorer_cache_service.create_empty_explorer_cache(user_id=user_id)
            self.explorer_cache_service.cache_dataset_item(user_id=user_id, dataset=user_data)
            self.explorer_cache_service.cache_operation_item(user_id=user_id, operation={"operation_type": "init_user", "operation_params": {}})
            return user_data
        if operation_type == "filter": 
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            filtered_data = self.data_processing_service.apply_filter(user_id=user_id, user_data=user_data, **operation_params)
            self.explorer_cache_service.cache_dataset_item(user_id=user_id, dataset=filtered_data)
            self.explorer_cache_service.cache_operation_item(user_id=user_id, operation={"operation_type": "filter", "operation_params": operation_params})
            return filtered_data
        if operation_type == "traverse":
            user_data = self.get_most_recent_dataset(user_id=user_id)
            traversed_data = self.data_processing_service.traverse_data(user_id=user_id, user_data=user_data, **operation_params)
            self.explorer_cache_service.cache_dataset_item(user_id=user_id, dataset=traversed_data)
            self.explorer_cache_service.cache_operation_item(user_id=user_id, operation={"operation_type": "traverse", "operation_params": operation_params})
            return traversed_data
        if operation_type == "get_filter_values":
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            unique_values = self.data_processing_service.get_unique_values_from_list_column(user_id=user_id, user_data=user_data, **operation_params)
            return unique_values
        if operation_type == "get_json_keys":
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            unique_keys = self.data_processing_service.get_unique_json_keys(user_id=user_id, user_data=user_data)
            return unique_keys
        if operation_type == "get_json_values":
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            unique_values = self.data_processing_service.get_unique_json_values(user_id=user_id, user_data=user_data)
            return unique_values
        if operation_type == "undo_operation":
            self.explorer_cache_service.delete_most_recent_dataset(user_id=user_id)
            self.explorer_cache_service.delete_most_recent_operation(user_id=user_id)
            return self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
        if operation_type == "save_snapshot":
            operation_chain = self.explorer_cache_service.get_most_recent_operation(user_id=user_id)
            snapshot = self.snapshot_service.create_snapshot(user_id=user_id, operation_chain=operation_chain, **operation_params)
            return snapshot
        if operation_type == "load_snapshot":
            snapshot = self.snapshot_service.get_snapshot(user_id=user_id, **operation_params)
            operation_chain = snapshot['operation_chain']
            assembled_dataset_list = self.assemble_dataset_from_operation_chain(user_id=user_id, operation_chain=operation_chain)
            self.explorer_cache_service.clear_dataset_list(user_id=user_id)
            self.explorer_cache_service.clear_operation_chain(user_id=user_id)
            self.explorer_cache_service.cache_dataset_list(user_id=user_id, dataset=assembled_dataset_list)
            self.explorer_cache_service.cache_operation_chain(user_id=user_id, operation_chain=operation_chain)
            return assembled_dataset_list[-1]
        if operation_type == "delete_snapshot":
            snapshot = self.snapshot_service.delete_snapshot(user_id=user_id, **operation_params)
            return snapshot
        if operation_type == "update_snapshot":
            snapshot = self.snapshot_service.update_snapshot(user_id=user_id, **operation_params)
            return snapshot
        if operation_type == "get_all_snapshots":
            snapshots = self.snapshot_service.get_all_snapshots(user_id=user_id)
            return snapshots
        if operation_type == "end_explorer_session":
            self.explorer_cache_service.delete_dataset_list(user_id=user_id)
            return None

    #Currently can return a list > max length allowed by the cache because we cache once, at the end, in handle_operation, not during each iteration of assemble_dataset_from_operation_chain
    def assemble_dataset_from_operation_chain(self, user_id, operation_chain):
        debug_print_vars(user_id=user_id, operation_chain=operation_chain)
        current_data = self.data_processing_service.get_user_data(user_id=user_id)
        dataset_list = [current_data]
        # Process each subsequent operation in the chain
        for operation in operation_chain[1:]:
            op_type = operation["operation_type"]
            op_params = operation["operation_params"]
            if op_type == "filter":
                current_data = self.data_processing_service.apply_filter(user_id=user_id, user_data=current_data, **op_params)
            if op_type == "traverse":
                current_data = self.data_processing_service.traverse_data(user_id=user_id, user_data=current_data, **op_params)
            dataset_list.append(current_data)
        return current_data
