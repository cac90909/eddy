from rest_framework import serializers, status
from rest_framework.response import Response
from rest_framework.request import Request
from rest_framework.viewsets import ViewSet
from typing import Type, Any
from core.registry.operation_specs import OPERATION_SPECS as OP_SPECS
from core.domain.enums.base_response import StandardResponse
from backend.apps.core.serializers.base import StandardResponseSerializer
from core.domain.operation_spec import OperationSpec as OpSpec
from core.domain.argument_spec import ArgumentSpec as ArgSpec


def _make_field(arg: ArgSpec) -> serializers.Field:
    #TODO - make a help text or description data member for argspec and implement in spec file
    kwargs = {'required': arg.required, 'help_text': getattr(arg, 'help_text', None)}
    field = serializers.JSONField(**kwargs)
    if arg.multiple is True:
        return serializers.ListField(child=field, required=arg.required)
    else:
        return field

def build_request_serializer(op_spec: OpSpec) -> type:
    attrs = {}
    for arg_spec in op_spec.args:
        field_name = arg_spec.name.value.lower()
        attrs[field_name] = _make_field(arg_spec)

    cls_name = op_spec.name.value.title().replace('_','') + "RequestSerializer"
    new_req_type = type(cls_name, (serializers.Serializer,), attrs)
    return new_req_type

# A mapping from OpName enum → concrete Serializer class
REQUEST_SERIALIZERS = {spec.name: build_request_serializer(spec) for spec in OP_SPECS.values()
}

def parse_request(
        self, 
        request: Request, 
        SerializerClass: Type[serializers.Serializer], 
        *, 
        use_query_params=False):
    data = request.query_params if use_query_params else request.data
    ser  = SerializerClass(data=data)
    ser.is_valid(raise_exception=True)
    return ser.validated_data

def envelope_response(
    self,
    payload,
    ResponseSerializer: Type[StandardResponseSerializer],   
    *,
    meta: dict | None = None,
    status_code=status.HTTP_200_OK,
):
    envelope = {
        StandardResponse.DATA.value: payload,
        StandardResponse.META.value: meta or {},
    }
    ser = ResponseSerializer(envelope)
    return Response(ser.data, status=status_code)