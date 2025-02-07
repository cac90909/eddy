from shared.logger import debug_print
from shared.repositories.snapshots_repository import SnapshotsRepository

class SnapshotsService:
    def __init__(self):
        self.snapshots_repository = SnapshotsRepository()

    def get_all_snapshots(self, user_id):
        snapshots = self.snapshots_repository.get_all_snapshots(user_id=user_id)
        debug_print(f"Retrieved {snapshots.count()} snapshots for user {user_id}")
        return snapshots

    def get_snapshot(self, user_id, snapshot_id):
        snapshot = self.snapshots_repository.get_snapshot(user_id=user_id, snapshot_id=snapshot_id)
        debug_print(f"Retrieved snapshot {snapshot.snapshot_id} for user {user_id}")
        return snapshot

    def create_snapshot(self, user_id, title, description, operation_chain):
        snapshot = self.snapshots_repository.create_snapshot(
            user_id=user_id, title=title, description=description, operation_chain=operation_chain
        )
        debug_print(f"Created snapshot {snapshot.snapshot_id} for user {user_id} with title '{snapshot.title}'")
        return snapshot

    def update_snapshot(self, user_id, snapshot_id, title=None, description=None, operation_chain=None):
        snapshot = self.snapshots_repository.update_snapshot(
            user_id=user_id, snapshot_id=snapshot_id, title=title, description=description, operation_chain=operation_chain
        )
        debug_print(f"Updated snapshot {snapshot.snapshot_id} for user {user_id}")
        return snapshot

    def delete_snapshot(self, user_id, snapshot_id):
        # Instead of calling snapshot.delete() directly here, we delegate to the repository.
        self.snapshots_repository.delete_snapshot(user_id=user_id, snapshot_id=snapshot_id)
        debug_print(f"Deleted snapshot {snapshot_id} for user {user_id}")
