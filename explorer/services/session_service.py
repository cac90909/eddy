from explorer.services.explorer_cache_service import ExplorerCacheService #Move ECS to non-serv folder
from shared.services.universal_raw_service import UniversalRawService
from shared.services.snapshots_service import SnapshotsService
from rest_framework.exceptions import APIException

class ExplorerSessionService:

    def __init__(self):
        self.cache_service = ExplorerCacheService()
        self.universal_raw_service = UniversalRawService()
        self.snapshot_service = SnapshotsService()

    def start_session(self, user_id):
        try:
            self.cache_service.create_empty_operation_chain_cache(user_id)
            return
        except Exception as e:
            raise APIException(e)
        
    def reset_session(self, user_id):
        try:
            self.cache_service.create_empty_operation_chain_cache(user_id)
            full_data = self.universal_raw_service.get_full_data(user_id)
            return full_data
        except Exception as e:
            raise APIException(e)
    
    def end_session(self, user_id):
        try:
            self.cache_service.delete_operation_chain_cache(user_id=user_id)
            return
        except Exception as e:
            raise APIException(e)
    
    def undo_operation(self, user_id):
        try:
            self.cache_service.delete_most_recent_operation_from_chain(user_id)
            most_recent_dataset = self.cache_service.get_most_recent_operation_chain_raw_data_result(user_id)
            return most_recent_dataset
        except Exception as e:
            raise APIException(e)
        
    #TODO - develop assemble  
    def load_snapshot(self, user_id, snapshot_id):
        try:
            self.cache_service.empty_operation_chain(user_id)
            snapshot = self.snapshot_service.get_snapshot(user_id, snapshot_id)
            operation_chain = snapshot.operation_chain
            loaded_data = self._assemble_dataset_list_from_operation_chain(user_id, operation_chain)
            return loaded_data
        except Exception as e:
            raise APIException(e)
        
    def _assemble_dataset_list_from_operation_chain(self, user_id, operation_chain):
        for op in operation_chain:
            pass
            #TODO - implement



    #     def assemble_dataset_list_from_operation_chain(user_id, operation_chain):
    # from explorer.services._______explorer_service import ExplorerService  # local import to avoid circular dependency
    # explorer_service = ExplorerService()
    # final_result = None
    # for op in operation_chain:
    #     op_name = op.get("operation_name")
    #     op_args = op.get("operation_arguments")
    #     try:
    #         op_response = explorer_service.handle_operation(user_id, op_name, op_args)
    #     except Exception as e:
    #         debug_print(f"Error executing operation {op_name}: {e}")
    #         return
    #     # Instead of storing the whole op_response, extract its "data" key if available
    #     if isinstance(op_response, dict) and "data" in op_response:
    #         final_result = op_response["data"]
    #     else:
    #         final_result = op_response
    # return final_result
        

  