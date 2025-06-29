from typing import Any, Optional
from explorer.infrastructure.metadata_cache import MetaCacheManager


class ExplorerMetadataService:
    """
    Application‑level service that manages domain‑specific metadata fields,
    delegating persistence to the MetaCacheManager (flat key approach).
    """
    FULL_SHAPE_KEY      = "full_shape"
    CURRENT_SHAPE_KEY   = "current_shape"
    OPERATION_COUNT_KEY = "operation_count"

    def __init__(self, cache_mgr: Optional[MetaCacheManager] = None):
        # Inject or create the infra cache manager
        self.cache = cache_mgr or MetaCacheManager()

    def set_full_shape(self, user_id: int, shape: dict) -> None:
        """Store the computed full_shape dict."""
        self.cache.save_field(user_id, self.FULL_SHAPE_KEY, shape)

    def get_full_shape(self, user_id: int) -> Optional[dict]:
        """Retrieve the full_shape dict, or None if not set."""
        return self.cache.load_field(user_id, self.FULL_SHAPE_KEY, default=None)

    def clear_full_shape(self, user_id: int) -> None:
        """Reset full_shape to an empty dict."""
        self.cache.save_field(user_id, self.FULL_SHAPE_KEY, {})

    def set_current_shape(self, user_id: int, shape: dict) -> None:
        """Store the computed current_shape dict."""
        self.cache.save_field(user_id, self.CURRENT_SHAPE_KEY, shape)

    def get_current_shape(self, user_id: int) -> Optional[dict]:
        """Retrieve the current_shape dict, or None if not set."""
        return self.cache.load_field(user_id, self.CURRENT_SHAPE_KEY, default=None)

    def clear_current_shape(self, user_id: int) -> None:
        """Reset current_shape to an empty dict."""
        self.cache.save_field(user_id, self.CURRENT_SHAPE_KEY, {})

    def increment_operation_count(self, user_id: int) -> None:
        """Increment the stored operation_count by 1."""
        current = self.cache.load_field(user_id, self.OPERATION_COUNT_KEY, default=0)
        new_count = (current or 0) + 1
        self.cache.save_field(user_id, self.OPERATION_COUNT_KEY, new_count)

    def get_operation_count(self, user_id: int) -> int:
        """Retrieve the number of operations run so far."""
        return self.cache.load_field(user_id, self.OPERATION_COUNT_KEY, default=0)

    def clear_operation_count(self, user_id: int) -> None:
        """Reset the operation count to 1 (as per legacy behavior)."""
        # If you prefer zero, change this to 0
        self.cache.save_field(user_id, self.OPERATION_COUNT_KEY, 1)