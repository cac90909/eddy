from rest_framework import viewsets, serializers
from rest_framework.decorators import action
from rest_framework.response import Response
from drf_spectacular.utils import extend_schema, OpenApiParameter

from explorer.serializers.requests.metadata import (
    OperationChainOperationsRequestSerializer,
    OperationChainResultsRequestSerializer
)
from explorer.serializers.base import StandardOperationResponseSerializer
from explorer.services.explorer_cache_service import ExplorerCacheService


class MetadataViewSet(viewsets.ViewSet):
    """
    ViewSet for exposing cached operation-chain metadata.
    """

    @extend_schema(
        query_serializer=OperationChainOperationsRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(
        detail=False,
        methods=["GET"],
        url_path="operation-chain-operations",
        url_name="operation_chain_operations",
    )
    def operation_chain_operations(self, request):
        """
        Retrieve the list of operations in the current operation chain for a user.
        """
        serializer = OperationChainOperationsRequestSerializer(data=request.query_params)
        serializer.is_valid(raise_exception=True)
        user_id = serializer.validated_data["user_id"]

        ops = ExplorerCacheService().extract_operation_chain_operations(user_id=user_id)
        return Response({"data": ops, "meta": {}})

    @extend_schema(
        request=OperationChainResultsRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(
        detail=False,
        methods=["get"],
        url_path="operation-chain-results",
        url_name="operation_chain_results",
    )
    def operation_chain_results(self, request):
        """
        Retrieve the resulting data for the current operation chain of a user.
        """
        serializer = OperationChainResultsRequestSerializer(data=request.query_params)
        serializer.is_valid(raise_exception=True)
        user_id = serializer.validated_data["user_id"]

        results = ExplorerCacheService().extract_operation_chain_result_data(user_id=user_id)
        return Response({"data": results, "meta": {}})
