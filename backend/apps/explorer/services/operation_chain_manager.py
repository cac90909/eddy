from typing import Any
from core.domain.operation.structures.operation import Operation
from core.domain.operation.structures.operation_chain import OperationChain
from explorer.infrastructure.explorer_cache import ExplorerCache

class ExplorerOperationChainManager:
       
    def __init__(self):
        self.chain_cache = ExplorerCache().chain

    def _get_chain(self, user_id: int) -> OperationChain:
        return self.chain_cache.get(user_id)
    def _set_chain(self, user_id: int, new_chain: OperationChain):
        self.chain_cache.set(user_id, new_chain)

    def get_latest_operation(self, user_id: int) -> Operation:
        """Return the most recently appended Operation, or None."""
        return self._get_chain(user_id).latest_operation

    def get_latest_result(self, user_id: int) -> Any:
        """Return the result_data of the last Operation, or None."""
        return self._get_chain(user_id).latest_result

    def append_operation(self, user_id: int, op: Operation) -> None:
        """Add one Operation to the end of the chain."""
        chain = self._get_chain(user_id)
        chain.append(op)
        self._set_chain(user_id, chain)

    def pop_operation(self, user_id: int) -> None:
        """Discard the most recently cached operation."""
        chain = self._get_chain(user_id)
        if not chain.is_empty():
            chain.pop()
            self._set_chain(user_id, chain)

    def reset_chain(self, user_id) -> None:
        self._set_chain(user_id, OperationChain([]))
    def delete_chain(self, user_id: int) -> None:
        self.chain_cache.delete(user_id)

    