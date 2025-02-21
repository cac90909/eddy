from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from explorer.services.explorer_cache_service import ExplorerCacheService
from shared.services.snapshots_service import SnapshotsService
from shared.serializers import SnapshotSerializer

from shared.services.universal_raw_service import UniversalRawService
from shared.services.universal_enrichment_service import UniversalEnrichmentService
from shared.services.universal_metric_service import UniversalMetricService
from shared.services.universal_list_service import UniversalListService

#@log_vars_vals_cls()
@catch_exceptions_cls(exception_return_value="Error")
class ExplorerDevToolService:
    def __init__(self):
        super().__init__()
        self.explorer_cache_service = ExplorerCacheService()
        self.snapshot_service = SnapshotsService()
        self.universal_raw_service = UniversalRawService()
        self.universal_enrichment_service = UniversalEnrichmentService()
        self.universal_metric_service = UniversalMetricService()
        self.universal_list_service = UniversalListService()

    def handle_cache_dynamic_operation(self, user_id, operation_name, operation_params):
        debug_print()
        if operation_name == "get_universal_data_list":
            universal_data_list = self.explorer_cache_service.get_universal_data_list(user_id=user_id)
            return universal_data_list
        
        
    def handle_cache_simple_operation(self, user_id, operation_name, operation_params):
        debug_print()
        if operation_name == "get_cache_summary":
            if self.explorer_cache_service.get_user_cache(user_id=user_id):
                universal_data_list = self.explorer_cache_service.get_universal_data_list(user_id=user_id)
                data_amount_list = [universal_data["data_amount"] for universal_data in universal_data_list]
                operation_chain_list = self.explorer_cache_service.get_operation_chain_list(user_id=user_id)
                data = {"universal": data_amount_list, "operation_chain": operation_chain_list}
                data_resp = {"data": data, "data_type":"cache_simple"}
            else:
                data_resp = {"data": "Empty", "data_type":"cache_simple"}
        if operation_name == "get_operation_chain_list":
            operation_chain_list = self.explorer_cache_service.get_operation_chain_list(user_id=user_id)
            data_resp = {"data": operation_chain_list, "data_type":"cache_simple"}
        if operation_name == "empty_cache":
            res = self.explorer_cache_service.empty_explorer_cache(user_id=user_id)
            data_resp = {"data": res, "data_type":"cache_simple"}
        return data_resp
        

    def handle_operation(self, user_id, operation_type, operation_params):
        debug_print()
        if operation_type == "get_cache_datasets":
            cache_datasets = self.explorer_cache_service.get_dataset_list(user_id=user_id)
            cache_datasets = [list(querylist) for querylist in cache_datasets]
            return cache_datasets
        if operation_type == "get_most_recent_dataset":
            user_data = self.explorer_cache_service.get_most_recent_universal_data_obj(user_id=user_id)
            return list(user_data)
        if operation_type == "get_cache_operation_chain":
            operation_chain = self.explorer_cache_service.get_operation_chain(user_id=user_id)
            return operation_chain
        if operation_type == "get_most_recent_operation":
            operation = self.explorer_cache_service.get_most_recent_operation(user_id=user_id)
            return operation
        if operation_type == "get_row":
            user_data = self.explorer_cache_service.get_most_recent_universal_data_obj(user_id=user_id)
            queryset = self.data_processing_service.filter_data(user_id=user_id, user_data=user_data, column_name="id", filter_value=operation_params["id"], filter_type="=")
            row = queryset[0]
            return row
        if operation_type == "get_value":
            user_data = self.explorer_cache_service.get_most_recent_universal_data_obj(user_id=user_id)
            queryset = self.data_processing_service.filter_data(user_id=user_id, user_data=user_data, column_name="id", filter_value=operation_params["id"], filter_type="=")
            row = queryset[0]
            value = getattr(row, operation_params["column_name"])
            return value
        if operation_type == "get_cache_num_datasets":
            cache_datasets = self.explorer_cache_service.get_dataset_list(user_id=user_id)
            num_datasets = len(cache_datasets)
            return num_datasets
        if operation_type == "get_cache_datasets_row_nums":
            cache_datasets = self.explorer_cache_service.get_dataset_list(user_id=user_id)
            datasets_rows = [len(list(querylist)) for querylist in cache_datasets]
            return datasets_rows
        if operation_type == "get_num_snapshots":
            snapshots = self.snapshot_service.get_all_snapshots(user_id=user_id)
            return len(list(snapshots))
        if operation_type == "get_snapshot":
            snapshot = self.snapshot_service.get_snapshot(user_id=user_id, snapshot_id=operation_params["snapshot_id"])
            return snapshot