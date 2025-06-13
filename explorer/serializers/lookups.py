from rest_framework import viewsets, serializers

# Request serializer for lookup options
class ArgumentOptionsRequestSerializer(serializers.Serializer):
    operation_name = serializers.CharField(
        help_text="Name of the operation (e.g., 'filter', 'join')"
    )
    argument = serializers.CharField(
        help_text="Name of the argument to fetch options for (e.g., 'column', 'value')"
    )
    previous_arguments = serializers.DictField(
        child=serializers.CharField(),
        required=False,
        default={},
        help_text="Previous argument values for dependent lookups"
    )

class OperationInfoRequestSerializer(serializers.Serializer):
    operation_name = serializers.CharField(
        help_text="Name of the operation (e.g., 'filter', 'join')"
    )