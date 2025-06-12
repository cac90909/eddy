# explorer/serializers/snapshot_request_serializers.py

from rest_framework import serializers

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
