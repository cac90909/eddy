from backend.apps.explorer.services.cache import ExplorerCacheService #Move ECS to non-serv folder
from backend.apps.core.service.snapshot import SnapshotsService
from core.services
from backend.apps.explorer.services.metadata_calculator import ExplorerMetadataCalculator
from explorer.domain.operation_chain import OperationChain
from rest_framework.exceptions import APIException
from typing import Any, Callable, Dict

class ExplorerSessionService:

    def __init__(self):
        self.cache_service = ExplorerCacheService()
        self.snapshot_service = SnapshotsService()
        self.operations_service = ExplorerOperationService()
        self.meta_svc = ExplorerMetadataCalculator()

    def start_session(self, user_id):
        try:
            self.cache_service.init_chain(user_id)
            full_data = self.operations_service.get_full_data(user_id)
            shape = self.meta_svc.generate_result_metadata(full_data, result_type="raw")
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
        








  