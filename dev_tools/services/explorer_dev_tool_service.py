from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from shared.services.data_processing_service import DataProcessingService
from explorer.services.explorer_cache_service import ExplorerCacheService
from explorer.services.explorer_service import ExplorerService
from shared.services.snapshots_service import SnapshotsService

#@log_vars_vals_cls()
@catch_exceptions_cls(exception_return_value="Error")
class ExplorerDevToolService:
    def __init__(self):
        super().__init__()
        self.explorer_cache_service = ExplorerCacheService()
        self.snapshot_service = SnapshotsService()
        self.data_processing_service = DataProcessingService()
        self.explorer_service = ExplorerService()

    def handle_operation(self, user_id, operation_type, operation_params):
        debug_print()
        if operation_type == "get_cache_datasets":
            cache_datasets = self.explorer_cache_service.get_dataset_list(user_id=user_id)
            cache_datasets = [list(querylist) for querylist in cache_datasets]
            return cache_datasets
        if operation_type == "get_most_recent_dataset":
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            return list(user_data)
        if operation_type == "get_cache_operation_chain":
            operation_chain = self.explorer_cache_service.get_operation_chain(user_id=user_id)
            return operation_chain
        if operation_type == "get_most_recent_operation":
            operation = self.explorer_cache_service.get_most_recent_operation(user_id=user_id)
            return operation
        if operation_type == "get_row":
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            queryset = self.data_processing_service.apply_filter(user_id=user_id, user_data=user_data, column_name="id", filter_value=operation_params["id"], filter_type="=")
            row = queryset[0]
            return row
        if operation_type == "get_value":
            user_data = self.explorer_cache_service.get_most_recent_dataset(user_id=user_id)
            queryset = self.data_processing_service.apply_filter(user_id=user_id, user_data=user_data, column_name="id", filter_value=operation_params["id"], filter_type="=")
            row = queryset[0]
            value = getattr(row, operation_params["column_name"])
            return value
        if operation_type == "get_cache_num_datasets":
            cache_datasets = self.explorer_cache_service.get_dataset_list(user_id=user_id)
            return len(cache_datasets)