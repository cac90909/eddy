from rest_framework import serializers
from django.db.models import QuerySet
from explorer.domain.operation import Operation
from shared.universal.serializers import UniversalSerializer  # wherever it lives
from typing import List, Dict, int, Any, list, str
from shared.models import Universal

class OperationSerializer(serializers.BaseSerializer):
    """
    Serializes your Operation dataclass, and for the .result field
    dumps either:
      • a list of Universal instances, via UniversalSerializer
      • any other JSON-serializable Python object directly
    """
    def to_representation(self, op: Operation) -> dict:
        # always include these three fields "as is"
        rep = {
            Operation.NAME: op.name,
            Operation.ARGS: op.args,
            Operation.TYPE: op.type,
        }

        # now serialize result based on its runtime type
        result = op.result
        if isinstance(result, QuerySet):
            rep[Operation.RESULT] = UniversalSerializer(result, many=True).data
        else:
            rep[Operation.RESULT] = result

        return rep
    
class OperationResultsSerializer(serializers.BaseSerializer):
    def to_representation(
            self, 
            op_result: QuerySet[Universal]|List[Any]|int|Dict[str, Any]
    ) -> dict:
        if isinstance(op_result, QuerySet):
            return UniversalSerializer(op_result, many=True).data
        else:
           return op_result
        
class OperationNonResultsSerializer(serializers.Serializer):
    name = serializers.CharField()
    args = serializers.DictField(child=serializers.JSONField())
    type = serializers.CharField()
