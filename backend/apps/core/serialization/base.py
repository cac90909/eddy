from typing import ClassVar, Any
from rest_framework import serializers

class FlexibleDictSerializer(serializers.Serializer):
    data: ClassVar[serializers.Field] = serializers.DictField(child=serializers.JSONField())
    def to_representation(self, instance):
        return instance

class StandardResponseSerializer(serializers.Serializer):
    data: ClassVar[Any] = serializers.JSONField()
    meta = serializers.DictField(required=False)
