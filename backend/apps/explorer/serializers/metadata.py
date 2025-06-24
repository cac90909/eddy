from rest_framework import serializers


class OperationChainOperationsRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField(
        help_text="ID of the user whose operation chain operations to retrieve"
    )


class OperationChainResultsRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField(
        help_text="ID of the user whose operation chain results to retrieve"
    )
