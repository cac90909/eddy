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
        debug_print(f"{snapshots.count()} snapshot rows retrieved")
        return snapshots

    def get_snapshot(self, user_id, snapshot_id):
        """
        Retrieve a single snapshot for a given user by its snapshot_id.
        """
        user_instance = User.objects.get(pk=user_id)
        snapshot = Snapshots.objects.get(user=user_instance, snapshot_id=snapshot_id)
        debug_print(f"{snapshot} retrieved")
        return snapshot

    def create_snapshot(self, user_id, title, description, operation_chain):
        """
        Create a new snapshot for a given user.
        """
        user_instance = User.objects.get(pk=user_id)
        debug_print(f"{Snapshots.objects.count()} snapshots in table")
        snapshot = Snapshots.objects.create(
            user=user_instance,
            title=title,
            description=description,
            operation_chain=operation_chain
        )
        debug_print(f"{Snapshots.objects.count()} snapshots in table")
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
        debug_print(f"{snapshot} updated")
        return snapshot

    def delete_snapshot(self, user_id, snapshot_id):
        """
        Delete a snapshot for a given user.
        """
        debug_print(f"{Snapshots.objects.count()} snapshots in table")
        snapshot = self.get_snapshot(user_id, snapshot_id)
        snapshot.delete()
        debug_print(f"{Snapshots.objects.count()} snapshots in table")
