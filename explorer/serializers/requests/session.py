from rest_framework import serializers

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
