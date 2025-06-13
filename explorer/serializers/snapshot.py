# explorer/serializers/snapshot_request_serializers.py

from rest_framework import serializers
from shared.models import Snapshots

class SaveSnapshotRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    title = serializers.CharField()
    description = serializers.CharField(required=False, allow_blank=True)

class UpdateSnapshotRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    title = serializers.CharField(required=False, allow_blank=True)
    description = serializers.CharField(required=False, allow_blank=True)

class DeleteSnapshotRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class GetSnapshotRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class GetAllSnapshotsRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class SnapshotResponseSerializer(serializers.ModelSerializer):
    class Meta:
        model  = Snapshots
        # you can use "__all__" or list just the fields you want exposed:
        fields = [
            "snapshot_id",
            "user",
            "title",
            "description",
            "operation_chain",
            "created_at",
            "updated_at",
        ]
        read_only_fields = fields  # since it’s a response, nothing is writable
