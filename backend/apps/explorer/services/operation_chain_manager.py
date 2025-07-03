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