from rest_framework import serializers
from shared.models import Universal, Snapshots  # adjust the import to your actual model location

class UniversalSerializer(serializers.ModelSerializer):
    class Meta:
        model = Universal
        fields = '__all__'  # or list the fields you want to expose

class UniversalDatasetsSerializer(serializers.Serializer):
    dataset = UniversalSerializer(many=True)

class OperationSerializer(serializers.Serializer):
    operation_type = serializers.CharField()
    operation_params = serializers.DictField(child=serializers.JSONField(), required=False)

class OperationChainItemSerializer(serializers.Serializer):
    operation = OperationSerializer(many=True)

class SnapshotSerializer(serializers.ModelSerializer):
    class Meta:
        model = Snapshots
        fields = "__all__"




# class UserCacheSerializer(serializers.Serializer):
#     datasets = UniversalSerializer(many=True)
#     operation_chain = OperationChainItemSerializer(many=True)
