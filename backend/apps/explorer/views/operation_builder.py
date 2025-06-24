from rest_framework.viewsets import ViewSet
from rest_framework.response import Response
from rest_framework.decorators import action
from drf_spectacular.utils import extend_schema, OpenApiResponse

from backend.apps.core.views.base import BaseViewSet
from core.operation.serializers import REQUEST_SERIALIZERS
from core.operation.enums import OperationName as OpName
from core.operation.specs import OPERATION_SPECS
from backend.apps.explorer.services.operation_builder import OperationBuilderService
from backend.apps.explorer.serializers.operation_builder import *
from core.common.enums import HTTPMethod as HTTP


class OperationBuilderViewSet(BaseViewSet):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.op_svc = OperationBuilderService()

    @extend_schema(
        request=REQUEST_SERIALIZERS[OpName.FULL_DATA],
        responses={200: FilterResponseSerializer},
        description=OPERATION_SPECS[OpName.FULL_DATA].description
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.FULL_DATA)
    def full_data(self, request):
        validated = self.parse_params(request, REQUEST_SERIALIZERS[OpName.FULL_DATA])
        result, meta = self.op_svc.handle_operation(op_name=OpName.FULL_DATA, **validated)
        return self.envelope_payload(result, FullDataResponseSerializer, meta=meta)


    @extend_schema(
        operation_id=OpName.FILTER.value,
        request=REQUEST_SERIALIZERS[OpName.FILTER],
        responses={200: FilterResponseSerializer},
        description=OPERATION_SPECS[OpName.FILTER].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.FILTER.value)
    def filter(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.FILTER])
        result, meta = self.op_svc.handle_operation(op_name=OpName.FILTER, **validated)
        return self.envelope_payload(result, FilterResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.TRAVERSE.value,
        request=REQUEST_SERIALIZERS[OpName.TRAVERSE],
        responses={200: TraverseResponseSerializer},
        description=OPERATION_SPECS[OpName.TRAVERSE].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.TRAVERSE.value)
    def traverse(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.TRAVERSE])
        result, meta = self.op_svc.handle_operation(op_name=OpName.TRAVERSE, **validated)
        return self.envelope_payload(result, TraverseResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.UNIQUE_COLUMN_VALUES.value,
        request=REQUEST_SERIALIZERS[OpName.UNIQUE_COLUMN_VALUES],
        responses={200: UniqueColumnValuesResponseSerializer},
        description=OPERATION_SPECS[OpName.UNIQUE_COLUMN_VALUES].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.UNIQUE_COLUMN_VALUES.value)
    def unique_column_values(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.UNIQUE_COLUMN_VALUES])
        result, meta = self.op_svc.handle_operation(op_name=OpName.UNIQUE_COLUMN_VALUES, **validated)
        return self.envelope_payload(result, UniqueColumnValuesResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.UNIQUE_JSON_KEYS.value,
        request=REQUEST_SERIALIZERS[OpName.UNIQUE_JSON_KEYS],
        responses={200: UniqueJsonKeysResponseSerializer},
        description=OPERATION_SPECS[OpName.UNIQUE_JSON_KEYS].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.UNIQUE_JSON_KEYS.value)
    def unique_json_keys(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.UNIQUE_JSON_KEYS])
        result, meta = self.op_svc.handle_operation(op_name=OpName.UNIQUE_JSON_KEYS, **validated)
        return self.envelope_payload(result, UniqueJsonKeysResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.UNIQUE_JSON_VALUES.value,
        request=REQUEST_SERIALIZERS[OpName.UNIQUE_JSON_VALUES],
        responses={200: UniqueJsonValuesResponseSerializer},
        description=OPERATION_SPECS[OpName.UNIQUE_JSON_VALUES].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.UNIQUE_JSON_VALUES.value)
    def unique_json_values(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.UNIQUE_JSON_VALUES])
        result, meta = self.op_svc.handle_operation(op_name=OpName.UNIQUE_JSON_VALUES, **validated)
        return self.envelope_payload(result, UniqueJsonValuesResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.UNIQUE_JSON_KEY_VALUES.value,
        request=REQUEST_SERIALIZERS[OpName.UNIQUE_JSON_KEY_VALUES],
        responses={200: UniqueJsonKeyValuesResponseSerializer},
        description=OPERATION_SPECS[OpName.UNIQUE_JSON_KEY_VALUES].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.UNIQUE_JSON_KEY_VALUES.value)
    def unique_json_key_values(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.UNIQUE_JSON_KEY_VALUES])
        result, meta = self.op_svc.handle_operation(op_name=OpName.UNIQUE_JSON_KEY_VALUES, **validated)
        return self.envelope_payload(result, UniqueJsonKeyValuesResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.SIMPLE_COUNT.value,
        request=REQUEST_SERIALIZERS[OpName.SIMPLE_COUNT],
        responses={200: SimpleCountResponseSerializer},
        description=OPERATION_SPECS[OpName.SIMPLE_COUNT].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.SIMPLE_COUNT.value)
    def simple_count(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.SIMPLE_COUNT])
        result, meta = self.op_svc.handle_operation(op_name=OpName.SIMPLE_COUNT, **validated)
        return self.envelope_payload(result, SimpleCountResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.SIMPLE_AVERAGE.value,
        request=REQUEST_SERIALIZERS[OpName.SIMPLE_AVERAGE],
        responses={200: SimpleAverageResponseSerializer},
        description=OPERATION_SPECS[OpName.SIMPLE_AVERAGE].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.SIMPLE_AVERAGE.value)
    def simple_average(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.SIMPLE_AVERAGE])
        result, meta = self.op_svc.handle_operation(op_name=OpName.SIMPLE_AVERAGE, **validated)
        return self.envelope_payload(result, SimpleAverageResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.SIMPLE_SUM.value,
        request=REQUEST_SERIALIZERS[OpName.SIMPLE_SUM],
        responses={200: SimpleSumResponseSerializer},
        description=OPERATION_SPECS[OpName.SIMPLE_SUM].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.SIMPLE_SUM.value)
    def simple_sum(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.SIMPLE_SUM])
        result, meta = self.op_svc.handle_operation(op_name=OpName.SIMPLE_SUM, **validated)
        return self.envelope_payload(result, SimpleSumResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.SIMPLE_MIN.value,
        request=REQUEST_SERIALIZERS[OpName.SIMPLE_MIN],
        responses={200: SimpleMinResponseSerializer},
        description=OPERATION_SPECS[OpName.SIMPLE_MIN].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.SIMPLE_MIN.value)
    def simple_min(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.SIMPLE_MIN])
        result, meta = self.op_svc.handle_operation(op_name=OpName.SIMPLE_MIN, **validated)
        return self.envelope_payload(result, SimpleMinResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.SIMPLE_MAX.value,
        request=REQUEST_SERIALIZERS[OpName.SIMPLE_MAX],
        responses={200: SimpleMaxResponseSerializer},
        description=OPERATION_SPECS[OpName.SIMPLE_MAX].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.SIMPLE_MAX.value)
    def simple_max(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.SIMPLE_MAX])
        result, meta = self.op_svc.handle_operation(op_name=OpName.SIMPLE_MAX, **validated)
        return self.envelope_payload(result, SimpleMaxResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.GROUP_COUNT.value,
        request=REQUEST_SERIALIZERS[OpName.GROUP_COUNT],
        responses={200: GroupCountResponseSerializer},
        description=OPERATION_SPECS[OpName.GROUP_COUNT].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.GROUP_COUNT.value)
    def group_count(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.GROUP_COUNT])
        result, meta = self.op_svc.handle_operation(op_name=OpName.GROUP_COUNT, **validated)
        return self.envelope_payload(result, GroupCountResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.GROUP_SUM.value,
        request=REQUEST_SERIALIZERS[OpName.GROUP_SUM],
        responses={200: GroupSumResponseSerializer},
        description=OPERATION_SPECS[OpName.GROUP_SUM].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.GROUP_SUM.value)
    def group_sum(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.GROUP_SUM])
        result, meta = self.op_svc.handle_operation(op_name=OpName.GROUP_SUM, **validated)
        return self.envelope_payload(result, GroupSumResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.GROUP_AVERAGE.value,
        request=REQUEST_SERIALIZERS[OpName.GROUP_AVERAGE],
        responses={200: GroupAverageResponseSerializer},
        description=OPERATION_SPECS[OpName.GROUP_AVERAGE].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.GROUP_AVERAGE.value)
    def group_average(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.GROUP_AVERAGE])
        result, meta = self.op_svc.handle_operation(op_name=OpName.GROUP_AVERAGE, **validated)
        return self.envelope_payload(result, GroupAverageResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.GROUP_MIN.value,
        request=REQUEST_SERIALIZERS[OpName.GROUP_MIN],
        responses={200: GroupMinResponseSerializer},
        description=OPERATION_SPECS[OpName.GROUP_MIN].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.GROUP_MIN.value)
    def group_min(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.GROUP_MIN])
        result, meta = self.op_svc.handle_operation(op_name=OpName.GROUP_MIN, **validated)
        return self.envelope_payload(result, GroupMinResponseSerializer, meta=meta)

    @extend_schema(
        operation_id=OpName.GROUP_MAX.value,
        request=REQUEST_SERIALIZERS[OpName.GROUP_MAX],
        responses={200: GroupMaxResponseSerializer},
        description=OPERATION_SPECS[OpName.GROUP_MAX].description,
    )
    @action(detail=False, methods=[HTTP.POST], url_path=OpName.GROUP_MAX.value)
    def group_max(self, request):
        validated = self.parse_payload(request, REQUEST_SERIALIZERS[OpName.GROUP_MAX])
        result, meta = self.op_svc.handle_operation(op_name=OpName.GROUP_MAX, **validated)
        return self.envelope_payload(result, GroupMaxResponseSerializer, meta=meta)
    

