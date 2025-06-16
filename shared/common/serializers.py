from rest_framework import serializers

class FlexibleDictSerializer(serializers.Serializer):
    data = serializers.DictField(child=serializers.JSONField())
    def to_representation(self, instance):
        return instance

class StandardOperationResponseSerializer(serializers.Serializer):
    data = serializers.JSONField()
    meta = serializers.DictField(required=False)
