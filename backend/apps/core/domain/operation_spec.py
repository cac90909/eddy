from dataclasses import dataclass

@dataclass(frozen=True)
class OperationSpec:
    name: OperationName
    result_type: OperationType
    args: Tuple[ArgumentSpec, ...]
    description: str
    service_method: Callable[..., Any]
    response_serializer: Type[Any]