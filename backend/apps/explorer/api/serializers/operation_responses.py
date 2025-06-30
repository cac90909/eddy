from rest_framework.serializers import Serializer
from rest_framework import serializers
from typing import List, Tuple
from drf_spectacular.utils import extend_schema_serializer
from backend.apps.core.domain.operation.maps.op_name_to_spec import OPERATION_SPECS
from backend.apps.core.domain.operation.structures.argument_spec import ArgumentSpec
from backend.apps.core.domain.operation.structures.operation_spec import OperationSpec
from backend.apps.core.serialization.python_type_to_drf_field import PYTHON_TYPE_TO_DRF_FIELD
from backend.apps.core.serialization.operation_type_to_serializer import OP_TYPE_TO_SERIALIZER


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

