from rest_framework import serializers
from shared.models import Universal, Snapshots  # adjust the import to your actual model location

class UniversalSerializer(serializers.ModelSerializer):
    class Meta:
        model = Universal
        #fields = '__all__'  # or list the fields you want to expose
        exclude = ('user',)

class SnapshotSerializer(serializers.ModelSerializer):
    class Meta:
        model = Snapshots
        fields = "__all__"

class FlexibleDictSerializer(serializers.Serializer):
    data = serializers.DictField(child=serializers.JSONField())

    def to_representation(self, instance):
        return instance

class StandardOperationResponseSerializer(serializers.Serializer):
    data = serializers.JSONField()
    meta = serializers.DictField(required=False)
