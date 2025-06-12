from explorer.domain.enums.operation_category import OperationCategory
from explorer.domain.enums.operation_result_type import OperationResultType
from explorer.domain.enums.display_mode import DisplayMode
from explorer.domain.operations.argument import Argument

import uuid
from datetime import datetime
from abc import ABC, abstractmethod
from typing import Any, Dict, Optional, List

class BaseOperation:
    name: str
    category: OperationCategory
    result_type: OperationResultType
    http_method: str
    display: Optional[DisplayMode] = None

    def __init__(self, user_id: int, **operation_arguments):
        self.id = uuid.uuid4()
        self.user_id = user_id
        self.operation_arguments: Dict[str, Any] = operation_arguments
        self.app = "explorer"
        self.created_at = datetime.now()

        # will be filled in by execute()
        self.result_data: Any = None
        self.result_data_type: Any = None

        # optional hook
        if hasattr(self, "setup") and callable(self.setup):
            self.setup(user_id, self)

    @classmethod
    def arguments(cls) -> List[Argument]:
        return []

    @staticmethod
    def handler(**kwargs):
        raise NotImplementedError("Subclasses must implement a handler.")

    @staticmethod
    def data_source(user_id, op_instance):
        return None

    @staticmethod
    def cache_policy(op_instance) -> bool:
        return False

    def setup(self, user_id, op_instance):
        pass

class SQLBaseOperation(BaseOperation):
    """
    Default for any operation that reads from the most-recent
    operation-chain raw dataset and wants caching.
    """
    category = OperationCategory.DATA_QUERY
    result_type = OperationResultType.RAW
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @staticmethod
    def data_source(user_id: int, op_instance: "SQLBaseOperation"):
        from explorer.services.explorer_cache_service import ExplorerCacheService
        return ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id)

    @staticmethod
    def cache_policy(op_instance: "SQLBaseOperation") -> bool:
        return True