# explorer/views/operation_viewset.py

from rest_framework.viewsets import ViewSet
from rest_framework.response import Response
from drf_spectacular.utils import extend_schema

from explorer.serializers.operations import *
from explorer.serializers.base import StandardOperationResponseSerializer
from explorer.services.operation_service import ExplorerOperationService


class ExplorerOperationViewSet(ViewSet):

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        # one service instance per request
        self.op_svc = ExplorerOperationService()

    @extend_schema(
        request=GetFullDataRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_full_data(self, request):
        serializer = GetFullDataRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_full_data(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=FilterRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def filter(self, request):
        serializer = FilterRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.filter(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=TraverseRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def traverse(self, request):
        serializer = TraverseRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.traverse(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetUniqueColumnValuesRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_unique_column_values(self, request):
        serializer = GetUniqueColumnValuesRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_unique_column_values(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetUniqueJsonKeysRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_unique_json_keys(self, request):
        serializer = GetUniqueJsonKeysRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_unique_json_keys(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetUniqueJsonValuesRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_unique_json_values(self, request):
        serializer = GetUniqueJsonValuesRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_unique_json_values(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetUniqueJsonKeyValuesRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_unique_json_key_values(self, request):
        serializer = GetUniqueJsonKeyValuesRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_unique_json_key_values(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetCountRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_count(self, request):
        serializer = GetCountRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_count(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetAverageRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_average(self, request):
        serializer = GetAverageRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_average(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetSumRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_sum(self, request):
        serializer = GetSumRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_sum(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetMinRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_min(self, request):
        serializer = GetMinRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_min(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetMaxRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_max(self, request):
        serializer = GetMaxRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_max(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GroupAggregateRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def group_aggregate(self, request):
        serializer = GroupAggregateRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.group_aggregate(**serializer.validated_data)
        return Response({"data": result_payload, "meta": {}})

