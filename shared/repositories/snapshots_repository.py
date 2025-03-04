from shared.models import Snapshots
from shared.logger import debug_print
from django.contrib.auth import get_user_model

User = get_user_model()

class SnapshotsRepository:
    def get_all_snapshots(self, user_id):
        """
        Retrieve all snapshots for a given user.
        """
        user_instance = User.objects.get(pk=user_id)
        snapshots = Snapshots.objects.filter(user=user_instance)
        debug_print("Query Finished")
        return snapshots

    def get_snapshot(self, user_id, snapshot_id):
        """
        Retrieve a single snapshot for a given user by its snapshot_id.
        """
        try:
            user_instance = User.objects.get(pk=user_id)
            snapshot = Snapshots.objects.get(user=user_instance, snapshot_id=snapshot_id)
            debug_print("Query Finished")
            return snapshot
        except Exception as e:
            raise e

    def create_snapshot(self, user_id, title, description, operation_chain):
        """
        Create a new snapshot for a given user.
        """
        user_instance = User.objects.get(pk=user_id)
        snapshot = Snapshots.objects.create(
            user=user_instance,
            title=title,
            description=description,
            operation_chain=operation_chain
        )
        debug_print("Query Finished")
        return snapshot

    def update_snapshot(self, user_id, snapshot_id, title=None, description=None, operation_chain=None):
        """
        Update fields of an existing snapshot for a given user.
        Only non-None values will be updated.
        """
        snapshot = self.get_snapshot(user_id, snapshot_id)
        if title is not None:
            snapshot.title = title
        if description is not None:
            snapshot.description = description
        if operation_chain is not None:
            snapshot.operation_chain = operation_chain
        snapshot.save()
        debug_print("Query Finished")
        return snapshot

    def delete_snapshot(self, user_id, snapshot_id):
        """
        Delete a snapshot for a given user.
        """
        snapshot = self.get_snapshot(user_id, snapshot_id)
        deleted_info = snapshot.delete() # Returns a tuple (deleted_count, {model_name: {deleted_count}}) -> eventually use this info for operation validation
        debug_print("Query Finished")
