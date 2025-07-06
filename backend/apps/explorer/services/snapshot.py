
from core.services.snapshot import SnapshotsService
from rest_framework.exceptions import APIException

class ExplorerSnapshotService():

    def __init__(self):
        self.snap_svc = SnapshotsService()

    
    def create_snapshot(self, user_id, operation_chain, title=None, description=None):
        try:
            snapshot = self.snap_svc.create_snapshot(user_id, title, description, operation_chain)
            return snapshot
        except Exception as e:
            raise APIException(e)
        
    def get_snapshot(self, user_id, snapshot_id):
        try:
            snapshot = self.snap_svc.get_snapshot(user_id, snapshot_id)
            return snapshot
        except Exception as e:
            raise APIException(e)
    
    def get_all_snapshots(self, user_id):
        try:
            queryset_res = self.snap_svc.get_all_snapshots(user_id)
            return queryset_res
        except Exception as e:
            raise APIException(e)
        
    #TODO - resolve if snapshot_id is retrieved here or sent by client
    def update_snapshot(self, user_id, snapshot_id, operation_chain, title=None, description=None):
        try:
            snapshot = self.snap_svc.update_snapshot(user_id, snapshot_id, title, description, operation_chain)
            return snapshot
        except Exception as e:
            raise APIException(e)
        
    #TODO - resolve if snapshot_id is retrieved here or sent by client
    def delete_snapshot(self, user_id, snapshot_id):
        try:
            self.snap_svc.delete_snapshot(user_id, snapshot_id)
            return
        except Exception as e:
            raise APIException(e)