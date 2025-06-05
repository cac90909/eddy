# explorer/domain/operation_result_types/base_operation_result.py
from abc import ABC, abstractmethod
from typing import Any, Dict, List

class BaseResultType(ABC):
    """
    Encapsulates: 
      - how to verify data_type
      - whether to cache
      - how to build data_overview_fields
      - whether to attach history/snapshots
      - how to serialize
    """

    @staticmethod
    @abstractmethod
    def verify(result_data: Any) -> bool:
        """Return True if the data matches expected Python type/shape."""
        pass

    @staticmethod
    @abstractmethod
    def cache_policy(op_instance) -> bool:
        """Return True if, after running this operation, we should cache it."""
        pass

    @staticmethod
    @abstractmethod
    def data_overview_fields(user_id: int, result_data: Any) -> Dict[str, Any]:
        """
        Return a dict of any “overview” fields (e.g. shapes, chain‐length, etc.).
        Can be empty {} if none are needed.
        """
        pass

    @staticmethod
    @abstractmethod
    def attach_history() -> bool:
        """True if operation history should be included in meta."""
        pass

    @staticmethod
    @abstractmethod
    def attach_snapshots_list() -> bool:
        """True if snapshots list should be included in meta."""
        pass

    @staticmethod
    @abstractmethod
    def serialize(result_data: Any) -> Any:
        """Convert the raw Python object (queryset, dict, int, etc.) into primitives."""
        pass
