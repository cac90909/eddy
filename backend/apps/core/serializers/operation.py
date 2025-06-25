from rest_framework import serializers
from backend.apps.core.serializers.universal import UniversalSerializer
from backend.apps.core.serializers.base import StandardResponseSerializer

class RawOperationResponseSerializer(StandardResponseSerializer):
    """
    Response serializer for raw operations (returns list of model instances).
    data: List[UniversalSerializer]
    meta: Dict
    """
    data = UniversalSerializer(many=True)

# 2) List operations: return a heterogeneous list of values/dicts
class ListOperationResponseSerializer(StandardResponseSerializer):
    """
    Response serializer for list operations (returns a list of JSON-serializable items).
    data: List[JSONField]
    meta: Dict
    """
    data = serializers.ListField(child=serializers.JSONField())

# 3) Enriched operations: return a list of dicts (annotated rows)
class EnrichedOperationResponseSerializer(StandardResponseSerializer):
    """
    Response serializer for enriched operations (returns list of dicts after ORM annotations).
    data: List[DictField]
    meta: Dict
    """
    data = serializers.ListField(child=serializers.DictField())

# 4) Metric operations: return a single numerical value
class MetricOperationResponseSerializer(StandardResponseSerializer):
    """
    Response serializer for metric operations (returns a single numeric metric).
    data: float or int
    meta: Dict
    """
    data = serializers.FloatField()  # or IntegerField if metrics are ints