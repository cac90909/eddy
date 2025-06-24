from typing import ClassVar
from rest_framework import serializers

class FlexibleDictSerializer(serializers.Serializer):
    data: ClassVar[serializers.DictField] = serializers.DictField(child=serializers.JSONField())
    def to_representation(self, instance):
        return instance

class StandardResponseSerializer(serializers.Serializer):
    data: ClassVar[serializers.JSONField] = serializers.JSONField()
    meta = serializers.DictField(required=False)
