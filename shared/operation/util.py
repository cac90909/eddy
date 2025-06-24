from rest_framework import serializers
from shared.operation.specs import OPERATION_SPECS as OP_SPECS
from shared.operation.domain import (
    OperationSpec as OpSpec, 
    ArgumentSpec as ArgSpec
)

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