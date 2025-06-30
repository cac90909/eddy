from typing import Any, Optional
from backend.apps.core.infrastructure.cache.base_cache import BaseCache

class MetaCacheManager(BaseCache):
    """
    Infrastructure adapter for persisting individual metadata fields as flat cache entries.
    """
    PREFIX = "explorer"

    def _field_key(self, user_id: int, field: str) -> str:
        return f"{self.PREFIX}:{user_id}:chain_meta:{field}"

    def load_field(self, user_id: int, field: str, default: Any = None) -> Any:
        """Return a single metadata field value (or default if not set)."""
        return self.get(self._field_key(user_id, field), default)

    def save_field(self, user_id: int, field: str, value: Any) -> None:
        """Store a single metadata field under its flat key."""
        self.set(self._field_key(user_id, field), value)

    def delete_field(self, user_id: int, field: str) -> None:
        """Delete a single metadata field from the cache."""
        self.delete(self._field_key(user_id, field))

    def clear_field(self, user_id: int, field: str) -> None:
        """Delete a single metadata field from the cache."""
        self.set(self._field_key(user_id, field), None)