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
class ExplorerService:
    def __init__(self):
        super().__init__()
        self.explorer_cache_service = ExplorerCacheService()
        self.snapshot_service = SnapshotsService()
        self.universal_raw_service = UniversalRawService()
        self.universal_enrichment_service = UniversalEnrichmentService()
        self.universal_metric_service = UniversalMetricService()
        self.universal_list_service = UniversalListService()



    def handle_universal_raw_operation(self, user_id, operation_name, operation_params):
        debug_print()
        result_data_type = "universal_raw"
        operation_obj = {"operation_name": operation_name, "operation_type": result_data_type, "operation_params":operation_params}
        operation_map = {
            "init" : self.universal_raw_service.init_data,
            "filter" : self.universal_raw_service.filter_data,
            "traverse" : self.universal_raw_service.traverse_data,
            "undo_operation" : self.explorer_cache_service.delete_most_recent_universal_data_and_operation_obj,   #Change below to cache along the way and only return last dataset
            "load_snapshot" : self.assemble_dataset_list_from_operation_chain
        } 
        if operation_name in ["init", "filter", "traverse"]:
            data_source = self.explorer_cache_service.get_most_recent_universal_raw_data_obj(user_id=user_id)
            result_data, data_amount = operation_map[operation_name](user_id=user_id, data_source=data_source, **operation_params)
        elif operation_name == "undo_operation":
            res = operation_map[operation_name](user_id=user_id, **operation_params)
            result_data = self.explorer_cache_service.get_most_recent_universal_raw_data_obj(user_id=user_id)
            data_amount = result_data.count()
        elif operation_name == "load_snapshot":
            self.explorer_cache_service.empty_explorer_cache(user_id=user_id)
            operation_chain = self.snapshot_service.get_snapshot(user_id=user_id, snapshot_id=operation_params["snapshot_id"]).operation_chain
            data_resp = operation_map[operation_name](user_id=user_id, operation_chain=operation_chain, **operation_params)
            return data_resp
        data_resp = {"data" : result_data, "data_type": result_data_type, "data_amount" : f'{data_amount} rows'}
        if operation_name not in ["load_snapshot", "undo_operation"]:
            self.explorer_cache_service.append_universal_data_and_operation_objs(user_id=user_id, universal_data_obj=data_resp, operation_obj=operation_obj)
        return data_resp
    
    def handle_universal_enrichment_operation(self, user_id, operation_name, operation_params):
        debug_print()
        result_data_type = "universal_enrichment"
        operation_obj = {"operation_name": operation_name, "operation_type": result_data_type, "operation_params":operation_params}
        operation_map = {
            "group_aggregate": self.universal_enrichment_service.group_aggregate_data,
            "filter_group_aggregate" : self.universal_enrichment_service.filter_grouped_aggregate_data
        }
        data_source = self.explorer_cache_service.get_most_recent_universal_raw_data_obj(user_id=user_id)
        result_data, data_amount = operation_map[operation_name](data_source=data_source, **operation_params)
        data_resp = {"data" : result_data, "data_type": result_data_type, "data_amount" : data_amount}
        self.explorer_cache_service.append_universal_data_and_operation_objs(user_id=user_id, universal_data_obj=data_resp, operation_obj=operation_obj)
        return data_resp
    
    def handle_universal_metric_operation(self, user_id, operation_name, operation_params):
        debug_print()
        result_data_type = "universal_metric"
        operation_obj = {"operation_name": operation_name, "operation_type": result_data_type, "operation_params":operation_params}
        operation_map = {
            "get_count": self.universal_metric_service.get_count,
            "get_sum": self.universal_metric_service.get_sum,
            "get_average": self.universal_metric_service.get_average,
            "get_min": self.universal_metric_service.get_min,
            "get_max": self.universal_metric_service.get_max,
        }
        data_source = self.explorer_cache_service.get_most_recent_universal_raw_data_obj(user_id=user_id)
        result_data, data_amount = operation_map[operation_name](data_source=data_source, **operation_params)
        data_resp = {"data" : result_data, "data_type": result_data_type, "data_amount" : data_amount}
        self.explorer_cache_service.append_universal_data_and_operation_objs(user_id=user_id, universal_data_obj=data_resp, operation_obj=operation_obj)
        return data_resp
    
    def handle_universal_list_operation(self, user_id, operation_name, operation_params):
        debug_print()
        result_data_type = "universal_list"
        operation_obj = {"operation_name": operation_name, "operation_type": result_data_type, "operation_params":operation_params}
        operation_map = {
            "unique_column_values": self.universal_list_service.get_unique_column_values,
            "unique_json_keys" : self.universal_list_service.get_unique_json_keys,
            "unique_json_values" : self.universal_list_service.get_unique_json_values
        }
        data_source = self.explorer_cache_service.get_most_recent_universal_raw_data_obj(user_id=user_id)
        result_data, data_amount = operation_map[operation_name](data_source=data_source, **operation_params)
        data_resp = {"data" : result_data, "data_type": result_data_type, "data_amount" : data_amount}
        if not operation_params["filter_values"]:
            self.explorer_cache_service.append_universal_data_and_operation_objs(user_id=user_id, universal_data_obj=data_resp, operation_obj=operation_obj)
        return data_resp
    
    def handle_explorer_state_operation(self, user_id, operation_name, operation_params):
        debug_print()
        result_data_type = "status"
        operation_obj = {"operation_name": operation_name, "operation_type": result_data_type, "operation_params":operation_params}
        operation_map = {
            "end_explorer_session" : self.explorer_cache_service.delete_explorer_cache_objs, 
            "start_explorer_session" : self.explorer_cache_service.create_empty_explorer_cache
        }
        result_data = operation_map[operation_name](user_id=user_id)
        data_resp = {"data" : result_data, "data_type": "status"}
        return data_resp
        
    def handle_snapshot_operation(self, user_id, operation_name, operation_params):
        debug_print()
        result_data_type = "snapshot" if operation_name != "get_all_snapshots" else "snapshot_list"
        operation_obj = {"operation_name": operation_name, "operation_type": result_data_type, "operation_params":operation_params}
        operation_map = {
            "update_snapshot" : self.snapshot_service.update_snapshot,
            "delete_snapshot"  : self.snapshot_service.delete_snapshot,
            "save_snapshot" : self.snapshot_service.create_snapshot,
            "get_all_snapshots" : self.snapshot_service.get_all_snapshots
        }
        result_data = operation_map[operation_name](user_id=user_id, **operation_params)
        data_resp = {"data" : result_data, "data_type" : result_data_type}
        return data_resp
        
        


    #Most Recent Raw Data Instance Is Always the Data Source
    def assemble_dataset_list_from_operation_chain(self, user_id, operation_chain):
        debug_print()
        operation_type_handler_mapping = {
            "universal_raw" : self.handle_universal_raw_operation,
            "universal_enrichment" : self.handle_universal_enrichment_operation,
            "universal_metric" : self.handle_universal_metric_operation,
            "universal_list" : self.handle_universal_list_operation
        }
        # Process each subsequent operation in the chain
        for operation_obj in operation_chain:
            operation_type, operation_name, operation_params = operation_obj["operation_type"], operation_obj["operation_name"], operation_obj["operation_params"]
            operation_type_handler_mapping[operation_type](user_id=user_id, operation_name=operation_name, operation_params=operation_params)
        most_recent_raw_universal_data = self.explorer_cache_service.get_most_recent_universal_raw_data_obj(user_id=user_id)
        return most_recent_raw_universal_data