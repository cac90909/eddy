import json
from typing import Any

from shared.cache.service import CacheService
from explorer.domain.operation_chain import OperationChain, Operation


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
    @property
    def last_operation(self, user_id: int) -> Operation | None:
        """Return the most recently appended Operation, or None."""
        return self.get_chain(user_id).last_operation

    @property
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


# from django.core.cache import cache
# from shared.services.cache_service import CacheService
# from shared.logger import debug_print_vars, debug_print
# from shared.util import log_vars_vals_cls, catch_exceptions_cls



# @catch_exceptions_cls(exception_return_value={"success": False})
# class ExplorerCacheService(CacheService):
#     MAX_CACHE_LEN = 50  # Max items in a specific cache object for a user

#     # Named variables for subcache keys
#     OPERAION_CHAIN_KEY = "OPERATION_CHAIN"
#     OPERAION_CHAIN_METADATA_KEY = "OPERATION_CHAIN_METADATA"

#     # ---- Initialization & Cache Setup Methods ----
#     def create_empty_operation_chain_cache(self, user_id):
#         if not self.get_user_cache(user_id=user_id):
#             self.create_empty_user_cache(user_id=user_id)
#         self.cache_user_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_KEY, obj_val=[])
#         self.cache_user_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_METADATA_KEY, obj_val={})
#         return "Success"

#     def empty_operation_chain(self, user_id):
#         self.cache_user_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_KEY, obj_val=[])
#         self.cache_user_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_METADATA_KEY, obj_val={})
#         return "Success"

#     # ---- Retrieval Methods ----
#     def get_operation_chain(self, user_id):
#         operation_chain = self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_KEY)
#         return operation_chain 

#     def get_most_recent_operation_from_chain(self, user_id):
#         operation_chain = self.get_operation_chain(user_id=user_id)
#         most_recent_operation = operation_chain[-1] if operation_chain else None
#         return most_recent_operation

#     def get_most_recent_operation_chain_result(self, user_id):
#         operation_chain_results = self.extract_operation_chain_result_data(user_id=user_id)
#         most_recent_data_result = operation_chain_results[-1] if operation_chain_results else None
#         return most_recent_data_result

#     def get_most_recent_operation_chain_result_data(self, user_id):
#         most_recent_result = self.get_most_recent_operation_chain_result(user_id=user_id)
#         return most_recent_result["data"] if most_recent_result else None

#     def get_most_recent_operation_chain_raw_data_result(self, user_id):
#         operation_chain = self.get_operation_chain(user_id=user_id)
#         for operation in operation_chain[::-1]:
#             if operation.result_data_type == "raw":
#                 return operation.result_data
#         return None
    
#     def get_operation_chain_metadata(self, user_id):
#         operation_chain_metadata = self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_METADATA_KEY)
#         return operation_chain_metadata
    
#     def get_operation_chain_metadata_key(self, user_id, metadata_key):
#         operation_chain_metadata = self.get_operation_chain_metadata(user_id=user_id)
#         return operation_chain_metadata.get(metadata_key, None)
    
#     # ---- Modification Methods (Adding/Removing Operations) ----
#     def cache_operation_onto_chain(self, user_id, operation):
#         operation_chain = self.get_operation_chain(user_id=user_id)
#         operation_chain.append(operation)
#         self.cache_operation_chain(user_id=user_id, operation_chain=operation_chain)
#         return "Success"

#     def cache_operation_chain(self, user_id, operation_chain):
#         self.cache_user_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_KEY, obj_val=operation_chain)
#         return "Success"
    
#     def cache_operation_chain_metadata(self, user_id, metadata_key, metadata_value):
#         metadata = self.get_operation_chain_metadata(user_id=user_id)
#         metadata[metadata_key] = metadata_value
#         self.cache_user_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_METADATA_KEY, obj_val=metadata)
#         return "Success"

#     def delete_operation_chain_cache(self, user_id):
#         self.delete_user_cache_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_KEY)
#         return "Success"

#     def delete_most_recent_operation_from_chain(self, user_id):
#         operation_chain = self.get_operation_chain(user_id=user_id)
#         operation_chain.pop(-1)
#         self.cache_operation_chain(user_id=user_id, operation_chain=operation_chain)
#         return "Success"

#     # ---- Helper/Extraction Methods ----

#     def extract_operation_chain_operations(self, user_id):
#         operation_chain = self.get_operation_chain(user_id=user_id)
#         operation_chain_operations = []
#         for operation in operation_chain:
#             operation_chain_operations.append({
#                 "operation_name": operation.operation_name,
#                 "operation_arguments": operation.operation_arguments,
#                 "operation_type": operation.operation_type
#             })
#         return operation_chain_operations

#     def extract_operation_chain_result_data(self, user_id):
#         operation_chain = self.get_operation_chain(user_id=user_id)
#         operation_chain_operations = []
#         for operation in operation_chain:
#             operation_chain_operations.append(operation.result_data)
#         return operation_chain_operations
    
