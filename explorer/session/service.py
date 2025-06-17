from explorer.cache.service import ExplorerCacheService #Move ECS to non-serv folder
from shared.snapshot.service import SnapshotsService
from explorer.operation.service import ExplorerOperationService
from explorer.metadata.service import ExplorerMetadataService
from explorer.domain.operation_chain import OperationChain
from rest_framework.exceptions import APIException
from typing import Any, Callable, Dict

class ExplorerSessionService:

    def __init__(self):
        self.cache_service = ExplorerCacheService()
        self.snapshot_service = SnapshotsService()
        self.operations_service = ExplorerOperationService()
        self.metadata_service = ExplorerMetadataService()

    def start_session(self, user_id):
        try:
            self.cache_service.init_chain(user_id)
            full_data = self.operations_service.get_full_data(user_id)
            shape = self.metadata_service.compute_data_shape(full_data, result_type="raw")
            self.cache_service.set_full_shape(user_id, shape)
            return full_data
        except Exception as e:
            raise APIException(e)
        
    def reset_session(self, user_id):
        try:
            self.cache_service.clear_chain(user_id)
            self.cache_service.clear_operation_count(user_id)
            full_data = self.operations_service.get_full_data(user_id)
            return full_data
        except Exception as e:
            raise APIException(e)
    
    def end_session(self, user_id):
        try:
            self.cache_service.delete_chain(user_id=user_id)
            self.cache_service.delete_meta(user_id)
            return
        except Exception as e:
            raise APIException(e)
    
    def undo_operation(self, user_id):
        try:
            self.cache_service.remove_last_operation(user_id)
            most_recent_dataset = self.cache_service.last_result(user_id)
            most_recent_dataset_type = self.cache_service.last_operation.type
            shape = self.metadata_service.compute_data_shape(most_recent_dataset, 
                                                             most_recent_dataset_type)
            self.cache_service.set_current_shape(user_id, shape)
            return most_recent_dataset
        except Exception as e:
            raise APIException(e)
        
    def load_snapshot(self, user_id, snapshot_id):
        try:
            self.cache_service.clear_chain(user_id)
            self.cache_service.clear_operation_count(user_id)
            self.cache_service.clear_current_shape(user_id)
            snapshot = self.snapshot_service.get_snapshot(user_id, snapshot_id)
            operation_chain = snapshot.operation_chain
            self._assemble_from_chain(user_id, operation_chain)
            last_result = self.cache_service.last_result
            return last_result
        except Exception as e:
            raise APIException(e)
        
    def _assemble_from_chain(self, user_id: int, chain: OperationChain) -> None:
        """
        Replay each OperationCommand in the given chain in sequence,
        passing the prior result into the next operation.
        """
        result = None
        for op in chain.operations:
            result = self.operations_service.execute(
                user_id=user_id,
                operation_name=op.name,
                operation_args=op.args,
                previous=result
            )











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
        

  