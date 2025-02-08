from shared.logger import debug_print
from shared.services.data_processing_service import DataProcessingService
from shared.services.cache_service import CacheService
from shared.services.snapshots_service import SnapshotsService
from shared.repositories.universal_repository import UniversalRepository


class ExplorerService:
    def __init__(self):
        super().__init__()
        self.universal_repository = UniversalRepository()
        self.cache_service = CacheService()
        self.snapshot_service = SnapshotsService()
        self.data_processing_service = DataProcessingService()


    def handle_operation(self, user_id, operation_type, operation_params):
        debug_print(user_id, operation_type, operation_params)
        if operation_type == "init_user":
            user_data = self.data_processing_service.get_user_data(user_id=user_id)
            self.create_user_cache(user_id=user_id)
            self.cache_dataset_and_operation(user_id=user_id, dataset=user_data, operation=operation_type)
            return user_data
        if operation_type == "filter": 
            user_data = self.get_most_recent_cache_dataset(user_id=user_id)
            filtered_data = self.data_processing_service.apply_filter(user_id=user_id, user_data=user_data, **operation_params)
            self.cache_dataset_and_operation(user_id=user_id, dataset=filtered_data, operation=operation_type)
            return filtered_data
        if operation_type == "traverse":
            user_data = self.get_most_recent_cache_dataset(user_id=user_id)
            traversed_data = self.data_processing_service.traverse_data(user_id=user_id, user_data=user_data, **operation_params)
            self.cache_dataset_and_operation(user_id=user_id, dataset=traversed_data, operation=operation_type)
            return traversed_data
        if operation_type == "get_filter_values":
            user_data = self.get_most_recent_cache_dataset(user_id=user_id)
            unique_values = self.data_processing_service.get_unique_values_from_list_column(user_id=user_id, user_data=user_data, **operation_params)
            return unique_values
        if operation_type == "get_json_keys":
            user_data = self.get_most_recent_cache_dataset(user_id=user_id)
            unique_keys = self.data_processing_service.get_unique_json_keys(user_id=user_id, user_data=user_data, **operation_params)
            return unique_keys
        if operation_type == "get_json_values":
            user_data = self.get_most_recent_cache_dataset(user_id=user_id)
            unique_values = self.data_processing_service.get_unique_json_values(user_id=user_id, user_data=user_data, **operation_params)
            return unique_values
        if operation_type == "undo_operation":
            self.delete_most_recent_cache_dataset_and_operation(user_id=user_id)
            return self.get_most_recent_cache_dataset(user_id=user_id)
        if operation_type == "save_snapshot":
            operation_chain = self.get_most_recent_cache_operation_chain(user_id=user_id)
            snapshot = self.snapshot_service.create_snapshot(user_id=user_id, operation_chain=operation_chain, **operation_params)
            return snapshot
        if operation_type == "load_snapshot":
            snapshot = self.snapshot_service.get_snapshot(user_id=user_id, **operation_params)
            operation_chain = snapshot['operation_chain']
            assembled_data = self.assemble_dataset_from_operation_chain(user_id=user_id, operation_chain=operation_chain)
            self.cache_dataset(user_id=user_id, dataset=assembled_data)
            self.cache_operation_chain(user_id=user_id, operation_chain=operation_chain)
            return assembled_data
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
            self.cache_service.delete_user_cache(user_id=user_id)
            return None

    #Intializes the User Cache
    def create_user_cache(self, user_id):
        debug_print(user_id)
        self.cache_service.cache_user(user_id=user_id, user_data={})
        self.cache_service.cache_user_obj(user_id=user_id, obj_name="datasets", obj_data=[])
        self.cache_service.cache_user_obj(user_id=user_id, obj_name="operation_chain", obj_data=[])

    #Caches Entire New Operation Chain
    def cache_operation_chain(self, user_id, operation_chain):
        debug_print(user_id, operation_chain)
        self.cache_service.cache_user_obj(user_id=user_id, obj_name="operation_chain", item_val=operation_chain)

    #Combines cache_dataset and cache_operation_on_chain (two frequently co-occuring operations)
    def cache_dataset_and_operation(self, user_id, dataset, operation):
        debug_print(user_id, operation)
        self.cache_dataset(user_id=user_id, dataset=dataset)
        self.cache_operation_on_chain(user_id=user_id, data_operation=operation)

    #Adds the Dataset to the Datasets List from User Cache
    def cache_dataset(self, user_id, dataset):
        debug_print(user_id)
        self.cache_service.cache_user_obj_item(user_id=user_id,obj_name="datasets", item_val=dataset)

    #Adds a Single Operation to the Operation Chain
    def cache_operation_on_chain(self, user_id, operation_type, operation_params):
        debug_print(user_id, operation_type, operation_params)
        self.cache_service.cache_user_obj_item(user_id=user_id, obj_name="operation_chain", 
                                               item_val={"operation_type": operation_type, "operation_params": operation_params})

    def get_most_recent_cache_dataset(self, user_id):
        debug_print(user_id)
        return self.cache_service.get_user_cache_obj_item(user_id=user_id, obj_name="datasets", item_index=-1)
    
    def get_most_recent_cache_operation_chain(self, user_id):
        debug_print(user_id)
        return self.cache_service.get_user_cache_obj_item(user_id=user_id, obj_name="operation_chain", item_index=-1)
    
    def delete_most_recent_cache_dataset_and_operation(self, user_id):
        debug_print(user_id)
        self.cache_service.delete_user_cache_obj_item(user_id=user_id, obj_name="datasets", item_index=-1)
        self.cache_service.delete_user_cache_obj_item(user_id=user_id, obj_name="operation_chain", item_index=-1)

    def assemble_dataset_from_operation_chain(self, user_id, operation_chain):
        debug_print(user_id, operation_chain)
        current_data = self.universal_repository.get_user_data(user_id=user_id)
        # Process each subsequent operation in the chain
        for operation in operation_chain[1:]:
            operation_type = operation["data_operation_type"]
            operation_params = operation["data_operation_params"]
            if operation_type == "filter":
                current_data = self.data_processing_service.apply_filter(user_id=user_id, user_data=current_data, **operation_params)
            if operation_type == "traverse":
                current_data = self.data_processing_service.traverse_data(user_id=user_id, user_data=current_data, **operation_params)
        self.cache_service.cache_data(user_id=user_id, data=current_data)
        return current_data


