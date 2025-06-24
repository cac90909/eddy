from drf_spectacular.utils import extend_schema_serializer
from shared.operation.enums import OperationName
from shared.operation.specs import OPERATION_SPECS

# For each operation, subclass its base response serializer with a unique component name

@extend_schema_serializer(component_name=f"{OperationName.FULL_DATA}")
class FullDataResponseSerializer(
    OPERATION_SPECS[OperationName.FULL_DATA].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.FILTER}")
class FilterResponseSerializer(
    OPERATION_SPECS[OperationName.FILTER].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.TRAVERSE}")
class TraverseResponseSerializer(
    OPERATION_SPECS[OperationName.TRAVERSE].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.SIMPLE_COUNT}")
class SimpleCountResponseSerializer(
    OPERATION_SPECS[OperationName.SIMPLE_COUNT].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.SIMPLE_SUM}")
class SimpleSumResponseSerializer(
    OPERATION_SPECS[OperationName.SIMPLE_SUM].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.SIMPLE_MIN}")
class SimpleMinResponseSerializer(
    OPERATION_SPECS[OperationName.SIMPLE_MIN].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.SIMPLE_MAX}")
class SimpleMaxResponseSerializer(
    OPERATION_SPECS[OperationName.SIMPLE_MAX].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.SIMPLE_AVERAGE}")
class SimpleAverageResponseSerializer(
    OPERATION_SPECS[OperationName.SIMPLE_AVERAGE].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.UNIQUE_COLUMN_VALUES}")
class UniqueColumnValuesResponseSerializer(
    OPERATION_SPECS[OperationName.UNIQUE_COLUMN_VALUES].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.UNIQUE_JSON_KEYS}")
class UniqueJsonKeysResponseSerializer(
    OPERATION_SPECS[OperationName.UNIQUE_JSON_KEYS].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.UNIQUE_JSON_KEY_VALUES}")
class UniqueJsonKeyValuesResponseSerializer(
    OPERATION_SPECS[OperationName.UNIQUE_JSON_KEY_VALUES].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.UNIQUE_JSON_VALUES}")
class UniqueJsonValuesResponseSerializer(
    OPERATION_SPECS[OperationName.UNIQUE_JSON_VALUES].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.GROUP_COUNT}")
class GroupCountResponseSerializer(
    OPERATION_SPECS[OperationName.GROUP_COUNT].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.GROUP_SUM}")
class GroupSumResponseSerializer(
    OPERATION_SPECS[OperationName.GROUP_SUM].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.GROUP_MIN}")
class GroupMinResponseSerializer(
    OPERATION_SPECS[OperationName.GROUP_MIN].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.GROUP_MAX}")
class GroupMaxResponseSerializer(
    OPERATION_SPECS[OperationName.GROUP_MAX].response_serializer
):
    pass

@extend_schema_serializer(component_name=f"{OperationName.GROUP_AVERAGE}")
class GroupAverageResponseSerializer(
    OPERATION_SPECS[OperationName.GROUP_AVERAGE].response_serializer
):
    pass
