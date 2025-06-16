from rest_framework import serializers
from shared.models import Universal, Snapshot

class SnapshotSerializer(serializers.ModelSerializer):
    class Meta:
        model = Snapshot
        fields = "__all__"