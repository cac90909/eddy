from rest_framework import serializers

# === Standardized Response ===
class StandardOperationResponseSerializer(serializers.Serializer):
    data = serializers.JSONField()
    meta = serializers.DictField(required=False)