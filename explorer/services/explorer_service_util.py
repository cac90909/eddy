from shared.logger import debug_print

from shared.services.universal_raw_service import UniversalRawService
from shared.services.universal_enriched_service import UniversalEnrichedService
from shared.services.universal_metric_service import UniversalMetricService
from shared.services.universal_list_service import UniversalListService

from explorer_cache_service import ExplorerCacheService
from shared.services.snapshots_service import SnapshotsService
from explorer.services.explorer_service import ExplorerService

from explorer.domain.operation import (
    #Universal Operations
    InitUserOperation,
    ResetOperation,
    FilterOperation,
    TraverseOperation,
    UndoOperation,
    LoadSnapshotOperation,
    GroupAggregateOperation,
    GetUniqueColumnValuesOperation,
    GetUniqueJsonKeysOperation,
    GetUniqueJsonValuesOperation,
    GetCountOperation,
    GetAverageOperation,
    GetSumOperation,
    GetMinOperation,
    GetMaxOperation,
    #State Operations
    StartExplorerSessionOperation,
    EndExplorerSessionOperation,
    GetOperationChainOperation,
    GetOperationChainOperationsOperation,
    GetOperationChainResultsOperation,
    #Snapshot Operations
    CreateSnapshotOperation,
    DeleteSnapshotOperation,
    UpdateSnapshotOperation,
    GetSnapshotOperation,
    GetAllSnapshotsOperation
)
operation_apps = ["explorer"]
operation_types = ["universal", "state", "snapshot"]

universal_operation_names = ["initalize_user", "reset" "filter", "traverse", "undo", "load_snapshot", 
                          "group_aggregate", 
                          "get_unique_column_values", "get_unique_json_keys", "get_unique_json_values", "get_count", "get_average", "get_sum", "get_min", "get_max"]
state_operation_names = ["start_explorer_session", "end_explorer_session", "reset"]
snapshot_operation_names = ["create_snapshot", "delete_snapshot", "update_snapshot", "get_snapshot", "get_all_snapshots"]

operation_result_data_types = ["raw", "enriched", "list", "metric", "status", "snapshot", "operations_list", "results_list", "operation_chain"]

@staticmethod
def attach_operation_handler(operation):
        operation_name = operation["operation_name"]
        service_operation_name_mapping = {
            #Universal Operations
            "init_user": UniversalRawService.get_user_universal,
            "reset": UniversalRawService.get_user_universal,
            "filter": UniversalRawService.filter,
            "traverse": UniversalRawService.traverse,
            "undo" : ExplorerCacheService.get_most_recent_operation_chain_raw_data_result,
            "load_snapshot" : assemble_dataset_list_from_operation_chain,
            "group_aggregate": UniversalEnrichedService.group_aggregate,
            "get_unique_column_values": UniversalListService.get_unique_column_values,
            "get_unique_json_keys": UniversalListService.get_unique_json_keys,
            "get_unique_json_values": UniversalListService.get_unique_json_values,
            "get_count": UniversalMetricService.get_count,
            "get_average": UniversalMetricService.get_average,
            "get_sum": UniversalMetricService.get_sum,
            "get_min": UniversalMetricService.get_min,
            "get_max": UniversalMetricService.get_max,
            #State Operations
            "start_explorer_session": ExplorerCacheService.create_empty_operation_chain_cache,
            "end_explorer_session": ExplorerCacheService.delete_operation_chain_cache,
            "get_operation_chain": ExplorerCacheService.get_operation_chain,
            "get_operation_chain_operations": ExplorerCacheService.extract_operation_chain_operations,
            "get_operation_chain_results": ExplorerCacheService.extract_operation_chain_result_data,
            #Snapshot Operations
            "create_snapshot": SnapshotsService.create_snapshot,
            "delete_snapshot": SnapshotsService.delete_snapshot,
            "update_snapshot": SnapshotsService.update_snapshot,
            "get_snapshot": SnapshotsService.get_snapshot,
            "get_all_snapshots": SnapshotsService.get_all_snapshots

        }
        operation_func = service_operation_name_mapping.get(operation_name)
        if operation_func is None:
            raise Exception(f"Operation {operation_name} not found")
        operation["handler"] = operation_func
        debug_print("Operation Handler Attached")
        return True

@staticmethod
def validate_operation(operation):
     operation.validate()
     debug_print("Operation Validated")
     return True

@staticmethod
def attach_operation_data_source(user_id, operation):
    operation_type, operation_name = operation["operation_type"], operation["operation_name"]
    if operation_type == "universal":
        if operation_name in ["init_user", "reset"]:
            data_source = None
        if operation_name == "load_snapshot":
            snapshots_service = SnapshotsService()
            snapshot = snapshots_service.get_snapshot(user_id=user_id, snapshot_id=operation["operation_arguments"]["snapshot_id"])
            operation_chain = snapshot.get("operation_chain")
            operation.operation_arguments["data_source"] = operation_chain
            debug_print("Data Source Attached")
        else:
            explorer_cache_service = ExplorerCacheService()
            most_recent_operation_chain_raw_data_result = explorer_cache_service.get_most_recent_operation_chain_raw_data_result(user_id=user_id)
            data_source = most_recent_operation_chain_raw_data_result
            operation.operation_arguments["data_source"] = operation_chain
            debug_print("Data Source Attached")
    else:
         pass
    return True

      
@staticmethod
def handle_operation_setup(user_id, operation):
    operation_name = operation["operation_name"]
    explorer_cache_service = ExplorerCacheService()
    if operation_name in ["init_user", "reset"]:
        explorer_cache_service.create_empty_operation_chain_cache(user_id=user_id)
    if operation_name == "load_snapshot":
        explorer_cache_service.empty_operation_chain(user_id=user_id)
    if operation_name == "undo":
        explorer_cache_service.delete_most_recent_operation_from_chain(user_id=user_id)
    else:
        pass
    debug_print("Operation Setup Complete")
    return True

@staticmethod
def execute_operation(operation):
    try:
        operation.execute()
        debug_print("Operation Executed")
        return True
    except Exception as e:
        debug_print(f"Error executing operation: {e}")
        return False
    



@staticmethod
def assemble_dataset_list_from_operation_chain(user_id, operation_chain):
        debug_print()
        explorer_service = ExplorerService()
        for operation in operation_chain:
            operation_name, operation_arguments = operation["operation_name"], operation["operation_arguments"]
            result = explorer_service.handle_operation(user_id=user_id, operation_name=operation_name, operation_arguments=operation_arguments)
        return result
            

@staticmethod
def create_operation_instance(operation_name, operation_arguments):
    operation_mapping = {
    #Universal Operations
    "init_user": InitUserOperation,
    "reset": ResetOperation,
    "filter": FilterOperation,
    "traverse": TraverseOperation,
    "undo": UndoOperation,
    "load_snapshot": LoadSnapshotOperation,
    "group_aggregate": GroupAggregateOperation,
    "get_unique_column_values": GetUniqueColumnValuesOperation,
    "get_unique_json_keys": GetUniqueJsonKeysOperation,
    "get_unique_json_values": GetUniqueJsonValuesOperation,
    "get_count": GetCountOperation,
    "get_average": GetAverageOperation,
    "get_sum": GetSumOperation,
    "get_min": GetMinOperation,
    "get_max": GetMaxOperation,
    #State Operations
    "start_explorer_session": StartExplorerSessionOperation,
    "end_explorer_session": EndExplorerSessionOperation,
    "get_operation_chain": GetOperationChainOperation,
    "get_operation_chain_operations": GetOperationChainOperationsOperation,
    "get_operation_chain_results": GetOperationChainResultsOperation,
    #Snapshot Operations
    "create_snapshot": CreateSnapshotOperation,
    "delete_snapshot": DeleteSnapshotOperation,
    "update_snapshot": UpdateSnapshotOperation,
    "get_snapshot": GetSnapshotOperation,
    "get_all_snapshots": GetAllSnapshotsOperation
    }
    operation_instance = operation_mapping.get(operation_name)(operation_name=operation_name, operation_arguments=operation_arguments)
    debug_print("Operation Instance Created")
    return operation_instance

@staticmethod
def handle_operation_caching(user_id, operation):
    operation_type, operation_name, operation_result_data_type = operation["operation_type"], operation["operation_name"], operation.result["data_type"]
    explorer_cache_service = ExplorerCacheService()
    if operation_type == "universal": 
        if operation_name in ["undo", "load_snapshot"]: #These are already cached
            pass
        if operation_result_data_type == "list" and operation.operation_arguments["options_call"] == True: #This is the client getting filter options, no need to cache
            pass
        else:
            explorer_cache_service.cache_operation_onto_chain(user_id=user_id, operation=operation)
            debug_print("Operation Cached")
    else:
        pass
    return True

@staticmethod
def attach_data_overview(operation):
    operation_type, operation_result_data_type = operation.operation_type, operation.result["data_type"]
    if operation_type == "universal":
        if operation_result_data_type in ["raw", "enriched"]:
            operation.result.data_overview = {
                "num_rows": operation.result["data"].count(),
                "num_columns" : len(operation.result["data"].count()),
                "num_values" : operation.result["data"].count() * len(operation.result["data"].count())
            }
        if operation_result_data_type == "list":
            operation.result.data_overview = {
                "num_values": len(operation.result["data"])
            }
        if operation_result_data_type == "metric":
            operation.result.data_overview = {
                "num_values": 1
            }
        debug_print("Data Overview Attached")
    else:
        pass
    return True
        

@staticmethod
def prepare_operation_result(operation):
    operation_result = operation.result
    operation_result_data, operation_result_data_type = operation_result["data"], operation_result["data_type"]
    if operation_result_data_type in ["raw", "enriched", "snapshot_list"]:
        operation.result["data"] = list(operation.result["data"])
    if operation_result_data_type in ["list", "metric", "status", "snapshot",  "operations_list"]:
        operation.result["data"] = operation.result["data"]
    if operation_result_data_type == "operation_chain":
        operation_chain = operation_result_data
        for sub_operation in operation_chain:
            sub_operation_dict = operation.to_dict()
            sub_operation_result_data_type = sub_operation_dict["result"]["data_type"]
            if sub_operation_result_data_type in ["raw", "enriched"]:
                sub_operation_dict["data"] = list(sub_operation_dict["data"])
            if sub_operation_result_data_type in ["list", "metric"]:
                sub_operation_dict["data"] = sub_operation_dict["data"]
    if operation_result_data_type == "results_list":
        results_list = operation_result
        for result in results_list:
            result_data_type = result["data_type"]
            if result_data_type in ["raw", "enriched"]:
                result["data"] = list(result["data"])
            if result_data_type in ["list", "metric"]:
                result["data"] = result["data"]
    debug_print("Operation Result Prepared")
    return True






    
