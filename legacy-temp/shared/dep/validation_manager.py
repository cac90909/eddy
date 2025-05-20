from pydantic import BaseModel

from validation_schemas import FilterParams, InitUserParams, SortParams, TraverseParams

schema_mapping = {
    "init_user": InitUserParams,
    "filter": FilterParams,
    "sort": SortParams,
    "traverse": TraverseParams
}

def validate_data_operation(operation_type, operation_params):
    validated_params = schema_mapping[operation_type](**operation_params)
    return validated_params