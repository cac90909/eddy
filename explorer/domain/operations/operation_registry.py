# explorer/domain/operation_registry.py

from typing import Type
from explorer.domain.operations.base_operation import BaseOperation

# This dict will hold name → class
_OPERATION_REGISTRY: dict[str, Type[BaseOperation]] = {}

def register_operation(cls: Type[BaseOperation]):
    """Class decorator to auto‐register operation classes."""
    name = cls.name
    if name in _OPERATION_REGISTRY:
        raise RuntimeError(f"Duplicate operation name: {name}")
    _OPERATION_REGISTRY[name] = cls
    return cls

def get_operation_class(operation_name: str) -> Type[BaseOperation] | None:
    return _OPERATION_REGISTRY.get(operation_name)
