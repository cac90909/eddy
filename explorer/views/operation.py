# explorer/views/operation_viewset.py

"""
ViewSet for executing all registered Explorer operations via POST requests.

Each method on the viewset corresponds to a specific operation and will:
- Validate request payloads via serializers (to be plugged in later)
- Invoke the corresponding UniversalService (e.g., UniversalRawService, etc.)
- Return a standardized response format

Note: Common decorator `@explorer_operation` (not shown here) can be used to:
- Inject metadata (timing, logging, result types)
- Automatically register operation names and serializers
- Ensure uniform validation/caching

# Example Decorator Stub:
#
# def explorer_operation(serializer_class, result_type, cacheable=True):
#     def decorator(func):
#         def wrapper(self, request, *args, **kwargs):
#             ...  # shared logic
#         return wrapper
#     return decorator
"""

"""
This viewset defines all supported data operations within the Explorer context.

Documentation support is added via drf-spectacular:
- Each endpoint is decorated with @extend_schema, explicitly listing the request and response serializers.
- This ensures self-documenting APIs, discoverable through Swagger or Redoc.
- Request validation and field extraction are handled through dedicated request serializers.
- All responses follow a standardized shape via StandardOperationResponseSerializer.
"""

from rest_framework.viewsets import ViewSet
from rest_framework.response import Response
from drf_spectacular.utils import extend_schema

from explorer.serializers.operation_requests import *
from explorer.serializers.responses import StandardOperationResponseSerializer
from shared.services.universal_raw_service import UniversalRawService
from shared.services.universal_list_service import UniversalListService
from shared.services.universal_metric_service import UniversalMetricService
from shared.services.universal_enriched_service import UniversalEnrichedService

class ExplorerOperationViewSet(ViewSet):

    @extend_schema(
        request=GetFullDataRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_full_data(self, request):
        serializer = GetFullDataRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalRawService().get_full_data(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=FilterRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def filter(self, request):
        serializer = FilterRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalRawService().filter(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=TraverseRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def traverse(self, request):
        serializer = TraverseRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalRawService().traverse(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetUniqueColumnValuesRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_unique_column_values(self, request):
        serializer = GetUniqueColumnValuesRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalListService().get_unique_column_values(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetUniqueJsonKeysRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_unique_json_keys(self, request):
        serializer = GetUniqueJsonKeysRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalListService().get_unique_json_keys(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetUniqueJsonValuesRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_unique_json_values(self, request):
        serializer = GetUniqueJsonValuesRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalListService().get_unique_json_values(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetUniqueJsonKeyValuesRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_unique_json_key_values(self, request):
        serializer = GetUniqueJsonKeyValuesRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalListService().get_unique_json_key_values(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetCountRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_count(self, request):
        serializer = GetCountRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalMetricService().get_count(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetAverageRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_average(self, request):
        serializer = GetAverageRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalMetricService().get_average(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetSumRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_sum(self, request):
        serializer = GetSumRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalMetricService().get_sum(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetMinRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_min(self, request):
        serializer = GetMinRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalMetricService().get_min(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetMaxRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def get_max(self, request):
        serializer = GetMaxRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalMetricService().get_max(**validated)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GroupAggregateRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    def group_aggregate(self, request):
        serializer = GroupAggregateRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = UniversalEnrichedService().group_aggregate(**validated)
        return Response({"data": result_payload, "meta": {}})
