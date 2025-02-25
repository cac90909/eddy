from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from explorer.services.explorer_cache_service import ExplorerCacheService
from shared.services.snapshots_service import SnapshotsService
from shared.serializers import SnapshotSerializer

from shared.services.universal_raw_service import UniversalRawService
from shared.services.universal_enriched_service import UniversalEnrichmentService
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
        
    def handle_universal_raw_operation(self, user_id, operation_name, operation_params):
        debug_print()
        result_data_type = "universal_raw"
        if operation_name == "get_most_recent_universal_raw":
            result_data = self.explorer_cache_service.get_most_recent_universal_raw_data_obj(user_id=user_id)
            if result_data:
                data_amount = result_data.count()
                debug_print(data_amount)
            else:
                data_amount = "0 rows"
                result_data_type = "empty"
            data_resp = {"data" : result_data, "data_type": result_data_type, "data_amount" : f'{data_amount} rows'}    
        return data_resp
        
        
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
    
    def handle_snapshot_operation(self, user_id, operation_name, operation_params):
        debug_print()
        if operation_name == "get_snapshot":
            snapshot = self.snapshot_service.get_snapshot(user_id=user_id, **operation_params)
            data_resp = {"data": snapshot, "data_type":"snapshot"}
            return data_resp

