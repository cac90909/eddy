from rest_framework import serializers
from shared.universal.serializers import UniversalSerializer
from shared.common.serializers import StandardResponseSerializer
from shared.operation.util import build_request_serializer
from shared.operation.registry import OPERATION_SPECS as OP_SPECS

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

# A mapping from OpName enum → concrete Serializer class
REQUEST_SERIALIZERS = {spec.name: build_request_serializer(spec) for spec in OP_SPECS.values()
}