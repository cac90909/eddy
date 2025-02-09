from rest_framework import serializers
from shared.models import Universal  # adjust the import to your actual model location

class UniversalSerializer(serializers.ModelSerializer):
    class Meta:
        model = Universal
        fields = '__all__'  # or list the fields you want to expose

class OperationChainItemSerializer(serializers.Serializer):
    operation_type = serializers.CharField()
    operation_params = serializers.DictField(child=serializers.JSONField(), required=False)

class UserCacheSerializer(serializers.Serializer):
    datasets = UniversalSerializer(many=True)
    operation_chain = OperationChainItemSerializer(many=True)
