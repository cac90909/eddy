from rest_framework import serializers
from shared.serializers import UniversalSerializer

class StartSessionRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class ResetSessionRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class UndoOperationRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class LoadSnapshotIntoSessionRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    snapshot_id = serializers.CharField()

class EndSessionRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class ResetSessionResponseSerializer(serializers.Serializer):
    data = UniversalSerializer(many=True)
    meta = serializers.DictField()

class UndoOperationResponseSerializer(serializers.Serializer):
    data = UniversalSerializer(many=True)
    meta = serializers.DictField()

class LoadSnapshotResponseSerializer(serializers.Serializer):
    data = UniversalSerializer(many=True)
    meta = serializers.DictField()

