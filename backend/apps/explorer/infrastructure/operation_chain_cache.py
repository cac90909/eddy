
from typing import Any
from core.infrastructure.base_cache import BaseCache
from core.domain.operation_chain import OperationChain
from core.domain.operation import Operation

class OperationChainCache(BaseCache):
    PREFIX = "explorer"

    def _chain_key(self, user_id: int) -> str:
        return f"{self.PREFIX}:{user_id}:chain"
    
    def init_chain(self, user_id: int) -> None:
        """Initialize an empty chain and metadata."""
        empty_chain = OperationChain([])
        self.set(self._chain_key(user_id), empty_chain.to_list())

    def get_chain(self, user_id: int) -> OperationChain:
        return self.get(self._chain_key(user_id), [])

    def save_chain(self, user_id: int, op_chain: OperationChain) -> None:
        self.set(self._chain_key(user_id), op_chain)

    def clear_chain(self, user_id: int) -> None:
        """Reset the chain to empty (metadata untouched)."""
        self.set(self._chain_key(user_id), OperationChain([]).to_list())

    def delete_chain(self, user_id: int) -> None:
        self.delete(self._chain_key(user_id))

    def append_operation(self, user_id: int, op: Operation) -> None:
        """Add one Operation to the end of the chain."""
        chain = self.get_chain(user_id)
        chain.append(op)
        self.save_chain(user_id, chain)

    def pop_operation(self, user_id: int) -> None:
        """
        Discard the most recently cached operation.
        """
        chain = self.get_chain(user_id)
        if chain.operations:
            chain.operations.pop()
            self.save_chain(user_id, chain)

    def get_latest_operation(self, user_id: int) -> Operation | None:
        """Return the most recently appended Operation, or None."""
        return self.get_chain(user_id).last_operation

    def get_latest_result(self, user_id: int) -> Any:
        """Return the result_data of the last Operation, or None."""
        return  self.get_chain(user_id).last_result