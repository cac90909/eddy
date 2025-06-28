from rest_framework.serializers import Serializer
from rest_framework import serializers
from typing import List, Tuple
from drf_spectacular.utils import extend_schema_serializer
from core.registry.operation_specs import OPERATION_SPECS
from core.domain.argument_spec import ArgumentSpec
from core.domain.operation_spec import OperationSpec
from backend.apps.core.serialization.py_drf_type_mappings import PY_TO_DRF_FIELD
from backend.apps.core.serialization.operation_type_mappings import OP_TYPE_TO_SERIALIZER

def build_request_serializer(arg_specs: Tuple[ArgumentSpec, ...]):
    field_defs = {}
    for spec in arg_specs:
        drf_szr_cls = PY_TO_DRF_FIELD.get(spec.dtype)
        if not drf_szr_cls:
            raise RuntimeError(f"No mapping for dtype {spec.dtype}")
        field = drf_szr_cls(required=spec.required, help_text=spec.error_msg or None,)
        if spec.multiple:
            field = serializers.ListField(child=field, required=spec.required)
        field_name = spec.name.value  
        field_defs[field_name] = field
    dynamic_szr = type("DynamicRequestSerializer", (serializers.Serializer,),field_defs)
    return dynamic_szr

REQUEST_SERIALIZERS = {
    op_name: extend_schema_serializer(component_name=f"{op_name.value}Request")(
        # dynamically create + name a subclass of serializers.Serializer
        type(
            f"{op_name.value.title().replace('_','')}RequestSerializer",
            (build_request_serializer(spec.args),),
            {}
        )
    )
    for op_name, spec in OPERATION_SPECS.items()
}

# Build one decorated subclass per OpName
RESPONSE_SERIALIZERS = {
    op_name: extend_schema_serializer(component_name=op_name.value)(
        type(
            f"{op_name.value.title().replace('_','')}ResponseSerializer",
            (OP_TYPE_TO_SERIALIZER[spec.result_type]),
            {}
        )
    )
    for op_name, spec in OPERATION_SPECS.items()
}

