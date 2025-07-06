from typing import Any
from core.infrastructure.cache.cache_resource import CacheResource
from core.domain.operation.structures.operation import Operation
from core.domain.operation.structures.operation_chain import OperationChain

class ExplorerOperationChainManager:
       
    def __init__(self, chain_cache: CacheResource[OperationChain]):
        self.chain_cache = chain_cache

    def get_latest_operation(self, user_id: int) -> Operation | None:
        """Return the most recently appended Operation, or None."""
        return self.chain_cache.get(user_id).latest_operation

    def get_latest_result(self, user_id: int) -> Any:
        """Return the result_data of the last Operation, or None."""
        return  self.chain_cache.get(user_id).latest_result

    def append_operation(self, user_id: int, op: Operation) -> None:
        """Add one Operation to the end of the chain."""
        chain = self.chain_cache.get(user_id)
        chain.append(op)
        self.chain_cache.set(user_id, chain)

    def pop_operation(self, user_id: int) -> None:
        """Discard the most recently cached operation."""
        chain = self.chain_cache.get(user_id)
        if not chain.is_empty():
            chain.pop()
            self.chain_cache.set(user_id, chain)

    