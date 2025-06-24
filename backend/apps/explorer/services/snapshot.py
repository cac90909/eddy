from backend.apps.explorer.services.cache import ExplorerCacheService
from backend.apps.core.service.snapshot import SnapshotsService
from rest_framework.exceptions import APIException

class ExplorerSnapshotService():

    def __init__(self):
        self.snapshot_service = SnapshotsService()
        self.cache_service = ExplorerCacheService()

    
    def create_snapshot(self, user_id, title=None, description=None):
        try:
            operation_chain = self.cache_service.extract_operation_chain_operations(user_id=user_id)
            snapshot = self.snapshot_service.create_snapshot(user_id, title, description, operation_chain)
            return snapshot
        except Exception as e:
            raise APIException(e)
        
    def get_snapshot(self, user_id, snapshot_id):
        try:
            snapshot = self.snapshot_service.get_snapshot(user_id, snapshot_id)
            return snapshot
        except Exception as e:
            raise APIException(e)
    
    def get_all_snapshots(self, user_id):
        try:
            queryset_res = self.snapshot_service.get_all_snapshots(user_id)
            return queryset_res
        except Exception as e:
            raise APIException(e)
        
    #TODO - resolve if snapshot_id is retrieved here or sent by client
    def update_snapshot(self, user_id, snapshot_id, title=None, description=None):
        try:
            operation_chain = self.cache_service.extract_operation_chain_operations(user_id=user_id)
            snapshot = self.snapshot_service.update_snapshot(user_id, snapshot_id, title, description, operation_chain)
            return snapshot
        except Exception as e:
            raise APIException(e)
        
    #TODO - resolve if snapshot_id is retrieved here or sent by client
    def delete_snapshot(self, user_id, snapshot_id):
        try:
            self.snapshot_service.delete_snapshot(user_id, snapshot_id)
            return
        except Exception as e:
            raise APIException(e)