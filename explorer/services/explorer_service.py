from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from shared.services.data_processing_service import DataProcessingService
from explorer.services.explorer_cache_service import ExplorerCacheService
from shared.services.snapshots_service import SnapshotsService
from shared.serializers import SnapshotSerializer

#@log_vars_vals_cls()
@catch_exceptions_cls(exception_return_value="Error")
class ExplorerService:
    def __init__(self):
        super().__init__()
        self.explorer_cache_service = ExplorerCacheService()
        self.snapshot_service = SnapshotsService()
        self.data_processing_service = DataProcessingService()

    def handle_operation(self, user_id, operation_type, operation_params):
        debug_print()
        if operation_type == "init_user":
            debug_print(f"{operation_type}: get user data --> create empty explorer cache --> cache dataset/operation item")
            user_data_queryset = self.data_processing_service.get_user_data(user_id=user_id)
            self.explorer_cache_service.create_empty_explorer_cache(user_id=user_id)
            self.explorer_cache_service.cache_dataset_item(user_id=user_id, dataset=user_data_queryset)
            self.explorer_cache_service.cache_operation_item(user_id=user_id, operation={"operation_type": "init_user", "operation_params": {}})
            debug_print(self.explorer_cache_service.get_dataset_list(user_id=user_id))
            return list(user_data_queryset)
        #Supports filter types: =, !=, <, >, string_contains, array_contains, array_not_contains
        if operation_type == "filter":
            debug_print(f"{operation_type}: get most recent cache dataset --> filter data --> cache dataset/operation item") 
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            filtered_data_queryset = self.data_processing_service.filter_data(user_id=user_id, user_data=user_data, **operation_params)
            self.explorer_cache_service.cache_dataset_item(user_id=user_id, dataset=filtered_data_queryset)
            self.explorer_cache_service.cache_operation_item(user_id=user_id, operation={"operation_type": "filter", "operation_params": operation_params})
            return list(filtered_data_queryset) 
        #NOTE: this currently uses entry ids, not ids for traversing
        #Supports a combination of traversal type: horizontal, upwards, downwards
        if operation_type == "traverse":
            debug_print(f"{operation_type}: get most recent cache dataset --> traverse data --> cache dataset/operation item") 
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            traversed_data_queryset = self.data_processing_service.traverse_data(user_id=user_id, user_data=user_data, **operation_params)
            self.explorer_cache_service.cache_dataset_item(user_id=user_id, dataset=traversed_data_queryset)
            self.explorer_cache_service.cache_operation_item(user_id=user_id, operation={"operation_type": "traverse", "operation_params": operation_params})
            return list(traversed_data_queryset)
        if operation_type == "get_unique_column_values":
            debug_print(f"{operation_type}: get most recent cache dataset --> get unique values of passed column") 
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            unique_values = self.data_processing_service.get_unique_column_values(user_id=user_id, user_data=user_data, **operation_params)
            return unique_values
        if operation_type == "get_unique_json_keys":
            debug_print(f"{operation_type}: get most recent cache dataset --> get unique keys from fields for all rows") 
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            unique_keys = self.data_processing_service.get_unique_json_keys(user_id=user_id, user_data=user_data)
            return unique_keys
        #Currently returns a concatenated list of all json key value pairings
        #Correction: it returns "A dictionary where each key is a JSON key and the value is a set of unique values for that key."
        if operation_type == "get_unique_json_values":
            debug_print(f"{operation_type}: get most recent cache dataset --> get unique values from fields for all rows") 
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            unique_values = self.data_processing_service.get_unique_json_values(user_id=user_id, user_data=user_data)
            return unique_values
        if operation_type == "undo_operation":
            debug_print(f"{operation_type}: delete most recent dataset/operation --> get most recent dataset") 
            self.explorer_cache_service.delete_most_recent_dataset(user_id=user_id)
            self.explorer_cache_service.delete_most_recent_operation(user_id=user_id)
            return self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
        if operation_type == "save_snapshot":
            debug_print(f"{operation_type}: get operation chain --> save to snapshots table") 
            operation_chain = self.explorer_cache_service.get_operation_chain(user_id=user_id)
            snapshot = self.snapshot_service.create_snapshot(user_id=user_id, operation_chain=operation_chain, **operation_params)
            return snapshot
        if operation_type == "load_snapshot":
            debug_print(f"{operation_type}: get snapshot from table --> assemble dataset from operation chain --> clear current cache --> cache snapshot chain/datasets") 
            snapshot = self.snapshot_service.get_snapshot(user_id=user_id, **operation_params)
            snapshot_serializer = SnapshotSerializer(instance=snapshot)
            snapshot_dict = snapshot_serializer.data #Have to deserialize to operate on object
            operation_chain = snapshot_dict['operation_chain']
            assembled_dataset_querylist_list = self.assemble_dataset_list_from_operation_chain(user_id=user_id, operation_chain=operation_chain)
            self.explorer_cache_service.clear_dataset_list(user_id=user_id)
            self.explorer_cache_service.clear_operation_chain(user_id=user_id)
            self.explorer_cache_service.cache_dataset_list(user_id=user_id, dataset_list=assembled_dataset_querylist_list)
            self.explorer_cache_service.cache_operation_chain(user_id=user_id, operation_chain=operation_chain)
            debug_print(assembled_dataset_querylist_list)
            debug_print(self.explorer_cache_service.get_dataset_list(user_id=user_id))
            most_recent_dataset_queryset = assembled_dataset_querylist_list[-1]
            return list(most_recent_dataset_queryset)
        if operation_type == "delete_snapshot":
            debug_print(f"{operation_type}")
            snapshot = self.snapshot_service.delete_snapshot(user_id=user_id, **operation_params)
            return snapshot
        if operation_type == "update_snapshot":
            debug_print(f"{operation_type}")
            snapshot = self.snapshot_service.update_snapshot(user_id=user_id, **operation_params)
            return snapshot
        if operation_type == "get_all_snapshots":
            debug_print(f"{operation_type} get all snapshots from table")
            snapshots = self.snapshot_service.get_all_snapshots(user_id=user_id)
            return list(snapshots)
        if operation_type == "end_explorer_session":
            debug_print(f"{operation_type}")
            self.explorer_cache_service.delete_dataset_list(user_id=user_id)
            self.explorer_cache_service.delete_operation_chain(user_id=user_id)
            debug_print(f"Ended User's Explorer Session, Cache: {self.explorer_cache_service.get_user_cache(user_id=user_id)}")
            return None

    #Currently can return a list > max length allowed by the cache because we cache once, at the end, in handle_operation, not during each iteration of assemble_dataset_from_operation_chain
    def assemble_dataset_list_from_operation_chain(self, user_id, operation_chain):
        debug_print()
        dataset_list = []
        # Process each subsequent operation in the chain
        for operation in operation_chain:
            op_type = operation["operation_type"]
            op_params = operation["operation_params"]
            if op_type == "init_user":
                current_data = self.data_processing_service.get_user_data(user_id=user_id)
            elif op_type == "filter":
                current_data = self.data_processing_service.filter_data(user_id=user_id, user_data=current_data, **op_params)
            elif op_type == "traverse":
                current_data = self.data_processing_service.traverse_data(user_id=user_id, user_data=current_data, **op_params)
            dataset_list.append(current_data)
        return dataset_list
