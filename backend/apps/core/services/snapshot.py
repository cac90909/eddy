from core.models import Snapshot
from backend.apps.core.infrastructure.logging.logger import debug_print_vars, debug_print
from core.infrastructure.repositories.snapshot import SnapshotsRepository

class SnapshotsService:
    def __init__(self):
        self.snapshots_repository = SnapshotsRepository()

    def get_all_snapshots(self, user_id):
        snapshot_qs = self.snapshots_repository.get_all_snapshots(user_id=user_id)
        return snapshot_qs

    def get_snapshot(self, user_id, snapshot_id):
        snapshot = self.snapshots_repository.get_snapshot(user_id=user_id, snapshot_id=snapshot_id)
        return snapshot
    
    def get_snapshot_operation_chain(self, user_id, snapshot_id):
        snapshot_ins = self.snapshots_repository.get_snapshot(user_id=user_id, snapshot_id=snapshot_id)
        operation_chain = snapshot_ins.operation_chain
        return operation_chain

    def create_snapshot(self, user_id, title, description, operation_chain):
        debug_print(operation_chain)
        snapshot = self.snapshots_repository.create_snapshot(
            user_id=user_id, title=title, description=description, operation_chain=operation_chain
        )
        return snapshot

    def update_snapshot(self, user_id, snapshot_id, title=None, description=None, operation_chain=None):
        snapshot = self.snapshots_repository.update_snapshot(user_id,snapshot_id, title, description, operation_chain)
        return snapshot

    def delete_snapshot(self, user_id, snapshot_id):
        self.snapshots_repository.delete_snapshot(user_id=user_id, snapshot_id=snapshot_id)
        return "Success"