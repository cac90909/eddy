import json
from typing import Any

from backend.apps.core.services.cache import CacheService
from core.domain.operation import Operation, OperationChain
from explorer.services.metadata import ExplorerMetadataService


class ExplorerCacheService(CacheService):
    """
    Caches a user's operation chain and its metadata using flat keys.
    Provides convenience methods to append operations, clear state,
    and fetch the most recent operation or its result.
    """

    PREFIX = "explorer"

    # well-known meta-keys
    FULL_SHAPE_KEY       = "full_shape"
    CURRENT_SHAPE_KEY    = "current_shape"
    OPERATION_COUNT_KEY  = "operation_count"

    def __init__(self, default_timeout: int = 3600):
        super().__init__(default_timeout)
        self.meta_svc = ExplorerMetadataService()


    # ─── Key generation ──────────────────────────────────────────────────────────
    def _chain_key(self, user_id: int) -> str:
        return f"{self.PREFIX}:{user_id}:chain"

    def _meta_key(self, user_id: int) -> str:
        return f"{self.PREFIX}:{user_id}:chain_meta"

    # ─── Operation chain CRUD ───────────────────────────────────────────────────
    def init_chain(self, user_id: int) -> None:
        """Initialize an empty chain and metadata."""
        empty_chain = OperationChain([])
        empty_meta = {}
        self.set(self._chain_key(user_id), empty_chain.to_list())
        self.set(self._meta_key(user_id), empty_meta)

    def get_chain(self, user_id: int) -> OperationChain:
        """Load and return the OperationChain for this user."""
        raw = self.get(self._chain_key(user_id), [])
        return OperationChain.from_list(raw)

    def save_chain(self, user_id: int, chain: OperationChain) -> None:
        """Persist the given OperationChain."""
        self.set(self._chain_key(user_id), chain.to_list())

    def clear_chain(self, user_id: int) -> None:
        """Reset the chain to empty (metadata untouched)."""
        self.set(self._chain_key(user_id), OperationChain([]).to_list())

    def delete_chain(self, user_id: int) -> None:
        """Delete the stored chain."""
        self.delete(self._chain_key(user_id))

    def append_operation(self, user_id: int, op: Operation) -> None:
        """Add one Operation to the end of the chain."""
        chain = self.get_chain(user_id)
        chain.append(op)
        self.save_chain(user_id, chain)

    def remove_last_operation(self, user_id: int) -> None:
        """
        Discard the most recently cached operation.
        """
        chain = self.get_chain(user_id)
        if chain.operations:
            chain.operations.pop()
            self.save_chain(user_id, chain)

    # ─── Recent operation helpers ────────────────────────────────────────────────
    def last_operation(self, user_id: int) -> Operation | None:
        """Return the most recently appended Operation, or None."""
        return self.get_chain(user_id).last_operation

    def last_result(self, user_id: int) -> Any:
        """Return the result_data of the last Operation, or None."""
        return  self.get_chain(user_id).last_result

    # ─── General Metadata CRUD ───────────────────────────────────────────────────
    def get_meta(self, user_id: int) -> dict:
        return self.get(self._meta_key(user_id), {})

    def save_meta(self, user_id: int, meta: dict) -> None:
        self.set(self._meta_key(user_id), meta)

    def clear_meta(self, user_id: int) -> None:
        self.set(self._meta_key(user_id), {})

    def delete_meta(self, user_id: int) -> None:
        self.delete(self._meta_key(user_id))

    def update_meta_key(self, user_id: int, key: str, value: Any) -> None:
        """Set a single metadata field."""
        meta = self.get_meta(user_id)
        meta[key] = value
        self.save_meta(user_id, meta)

    def get_meta_key(self, user_id: int, key: str, default=None) -> Any:
        return self.get_meta(user_id).get(key, default)
    
    # ─── Specific Metadata CRUD ──────────────────────────────────────────────────

    def set_full_shape(self, user_id: int, shape: dict) -> None:
        self.update_meta_key(user_id, self.FULL_SHAPE_KEY, shape)

    def get_full_shape(self, user_id: int) -> dict | None:
        return self.get_meta_key(user_id, self.FULL_SHAPE_KEY, default=None)
    
    def clear_full_shape(self, user_id: int) -> None:
        self.update_meta_key(user_id, self.FULL_SHAPE_KEY, {})

    def set_current_shape(self, user_id: int, shape: dict) -> None:
        self.update_meta_key(user_id, self.CURRENT_SHAPE_KEY, shape)

    def get_current_shape(self, user_id: int) -> dict | None:
        return self.get_meta_key(user_id, self.CURRENT_SHAPE_KEY, default=None)
    
    def clear_current_shape(self, user_id: int) -> None:
        self.update_meta_key(user_id, self.CURRENT_SHAPE_KEY, {})

    def increment_operation_count(self, user_id: int) -> None:
        count = self.get_meta_key(user_id, self.OPERATION_COUNT_KEY, default=0)
        self.update_meta_key(user_id, self.OPERATION_COUNT_KEY, count + 1)

    def get_operation_count(self, user_id: int) -> int:
        return self.get_meta_key(user_id, self.OPERATION_COUNT_KEY, default=0)
    
    def clear_operation_count(self, user_id: int) -> None:
        self.update_meta_key(user_id, self.OPERATION_COUNT_KEY, 1)


    def cache_operation(self, user_id: int, op: Operation):
        op_metadata = self.meta_svc.generate_operation_metadata(op)
        for k,v in op_metadata.items():
            
