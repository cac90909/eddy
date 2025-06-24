from rest_framework import viewsets
from rest_framework.decorators import action
from rest_framework.response import Response
from drf_spectacular.utils import extend_schema

from explorer._dev.serializers import (
    OperationSerializer,
    OperationResultsSerializer,
    OperationNonResultsSerializer,
)
from explorer._dev.service import ExplorerDevService  # wherever your service lives

class DevViewSet(viewsets.ViewSet):
    """
    The normal Explorer endpoints live here (filter, traverse, etc.).
    Below are three dev‐only endpoints to inspect the user's operation chain.
    """

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.dev_svc = ExplorerDevService()

    @action(detail=False, methods=['get'], url_path='chain')
    @extend_schema(
        summary="🔗 Get full operation chain",
        responses=OperationSerializer(many=True),
    )
    def chain(self, request):
        ops = self.dev_svc.get_operation_chain(request.user_id)
        serializer = OperationSerializer(ops, many=True)
        return Response(serializer.data)

    @action(detail=False, methods=['get'], url_path='chain/results')
    @extend_schema(
        summary="🎯 Get only operation results",
        responses=OperationResultsSerializer(many=True),
    )
    def chain_results(self, request):
        results = self.dev_svc.get_operation_chain_results(request.user_id)
        serializer = OperationResultsSerializer(results, many=True)
        return Response(serializer.data)

    @action(detail=False, methods=['get'], url_path='chain/non-results')
    @extend_schema(
        summary="📋 Get only operation instructions (no results)",
        responses=OperationNonResultsSerializer(many=True),
    )
    def chain_non_results(self, request):
        non_results = self.dev_svc.get_operation_chain_non_results(request.user_id)
        serializer = OperationNonResultsSerializer(non_results, many=True)
        return Response(serializer.data)