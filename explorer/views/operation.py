from rest_framework.viewsets import ViewSet
from rest_framework.response import Response
from drf_spectacular.utils import extend_schema

from explorer.serializers.operations import (
    GetFullDataRequestSerializer, GetFullDataResponseSerializer,
    FilterRequestSerializer, FilterResponseSerializer,
    TraverseRequestSerializer, TraverseResponseSerializer,
    GetUniqueColumnValuesRequestSerializer, UniqueColumnValuesResponseSerializer,
    GetUniqueJsonKeysRequestSerializer, UniqueJsonKeysResponseSerializer,
    GetUniqueJsonValuesRequestSerializer, UniqueJsonValuesResponseSerializer,
    GetUniqueJsonKeyValuesRequestSerializer, UniqueJsonKeyValuesResponseSerializer,
    GetCountRequestSerializer, CountResponseSerializer,
    GetAverageRequestSerializer, AverageResponseSerializer,
    GetSumRequestSerializer, SumResponseSerializer,
    GetMinRequestSerializer, MinResponseSerializer,
    GetMaxRequestSerializer, MaxResponseSerializer,
    GroupAggregateRequestSerializer, GroupAggregateResponseSerializer
)
from explorer.services.operation_service import ExplorerOperationService
from explorer.services.metadata_service import ExplorerMetadataService


class ExplorerOperationViewSet(ViewSet):

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        # one service instance per request
        self.op_svc = ExplorerOperationService()
        self.meta_service = ExplorerMetadataService()

    @extend_schema(
        request=GetFullDataRequestSerializer,
        responses=GetFullDataResponseSerializer
    )
    def get_full_data(self, request):
        serializer = GetFullDataRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_full_data(**serializer.validated_data)
        out_ser = GetFullDataResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_raw(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=FilterRequestSerializer,
        responses=FilterResponseSerializer
    )
    def filter(self, request):
        serializer = FilterRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.filter(**serializer.validated_data)
        out_ser = FilterResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_raw(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=TraverseRequestSerializer,
        responses=TraverseResponseSerializer
    )
    def traverse(self, request):
        serializer = TraverseRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.traverse(**serializer.validated_data)
        out_ser = TraverseResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_raw(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=GetUniqueColumnValuesRequestSerializer,
        responses=UniqueColumnValuesResponseSerializer
    )
    def get_unique_column_values(self, request):
        serializer = GetUniqueColumnValuesRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_unique_column_values(**serializer.validated_data)
        out_ser = UniqueColumnValuesResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_list(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=GetUniqueJsonKeysRequestSerializer,
        responses=UniqueJsonKeysResponseSerializer
    )
    def get_unique_json_keys(self, request):
        serializer = GetUniqueJsonKeysRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_unique_json_keys(**serializer.validated_data)
        out_ser = UniqueJsonKeysResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_list(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=GetUniqueJsonValuesRequestSerializer,
        responses=UniqueJsonValuesResponseSerializer
    )
    def get_unique_json_values(self, request):
        serializer = GetUniqueJsonValuesRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_unique_json_values(**serializer.validated_data)
        out_ser = UniqueJsonValuesResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_list(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=GetUniqueJsonKeyValuesRequestSerializer,
        responses=UniqueJsonKeyValuesResponseSerializer
    )
    def get_unique_json_key_values(self, request):
        serializer = GetUniqueJsonKeyValuesRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_unique_json_key_values(**serializer.validated_data)
        out_ser = UniqueJsonKeyValuesResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_list(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=GetCountRequestSerializer,
        responses=CountResponseSerializer
    )
    def get_count(self, request):
        serializer = GetCountRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_count(**serializer.validated_data)
        out_ser = CountResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_metric(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=GetAverageRequestSerializer,
        responses=AverageResponseSerializer
    )
    def get_average(self, request):
        serializer = GetAverageRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_average(**serializer.validated_data)
        out_ser = AverageResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_metric(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=GetSumRequestSerializer,
        responses=SumResponseSerializer
    )
    def get_sum(self, request):
        serializer = GetSumRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_sum(**serializer.validated_data)
        out_ser = SumResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_metric(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=GetMinRequestSerializer,
        responses=MinResponseSerializer
    )
    def get_min(self, request):
        serializer = GetMinRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_min(**serializer.validated_data)
        out_ser = MinResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_metric(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=GetMaxRequestSerializer,
        responses=MaxResponseSerializer
    )
    def get_max(self, request):
        serializer = GetMaxRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.get_max(**serializer.validated_data)
        out_ser = MaxResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_metric(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})

    @extend_schema(
        request=GroupAggregateRequestSerializer,
        responses=GroupAggregateResponseSerializer
    )
    def group_aggregate(self, request):
        serializer = GroupAggregateRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        result_payload = self.op_svc.group_aggregate(**serializer.validated_data)
        out_ser = GroupAggregateResponseSerializer({"data": result_payload, "meta": {}})

        include_meta = request.query_params.get('include_meta', 'false').lower() == 'true'
        meta = self.meta_service.shape_for_enriched(result_payload) if include_meta else {}

        return Response({"data": out_ser.data['data'], "meta": meta})
