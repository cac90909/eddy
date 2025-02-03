from shared.models import Snapshots

class SnapshotsRepository:

    def get_all_snapshots(self, user_id):
        """
        Retrieve all snapshots for a given user.
        """
        return Snapshots.objects.filter(user_id=user_id)

    def get_snapshot(self, user_id, snapshot_id):
        """
        Retrieve a single snapshot for a given user by its snapshot_id.
        """
        return Snapshots.objects.get(user_id=user_id, snapshot_id=snapshot_id)

    def create_snapshot(self, user_id, title, description, operation_chain):
        """
        Create a new snapshot for a given user.
        """
        snapshot = Snapshots.objects.create(
            user_id=user_id,  # You can pass the user instance or the user primary key.
            title=title,
            description=description,
            operation_chain=operation_chain
        )
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
        return snapshot

    def delete_snapshot(self, user_id, snapshot_id):
        """
        Delete a snapshot for a given user.
        """
        snapshot = self.get_snapshot(user_id, snapshot_id)
        snapshot.delete()
