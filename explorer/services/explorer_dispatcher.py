# explorer/services/explorer_dispatcher.py

import json
from typing import Any, Dict
from explorer.domain.operation_registry import get_operation_class
from explorer.domain.operation_result_registry import get_result_type_class
from explorer.domain.operation import Operation
from explorer.domain.operation_result import OperationResult
from explorer.services.explorer_cache_service import ExplorerCacheService
from explorer.util.operation_validation import validate_args_against_metadata
from explorer.util.operation_result_util import get_operation_history, get_all_snapshots

class ExplorerDispatcher:
    def __init__(self):
        self.cache_service = ExplorerCacheService()

    def handle_operation(
        self,
        user_id: int,
        operation_name: str,
        operation_arguments: Dict[str, Any],
    ) -> Dict[str, Any]:
        # 1. Lookup the operation class
        OperationClass = get_operation_class(operation_name)
        if OperationClass is None:
            return OperationResult.error(f"Operation not found: {operation_name}")

        # 2. Validate required args
        missing = validate_args_against_metadata(OperationClass, operation_arguments)
        if missing:
            return OperationResult.error(f"Missing required arguments: {missing}")

        # 3. (Optional) Precondition logic:
        # if hasattr(OperationClass, "precondition"):
        #     cond, msg = OperationClass.precondition(user_id, operation_arguments)
        #     if not cond:
        #         return OperationResult.error(f"Precondition failed: {msg}")

        # 4. Create an Operation domain object for metadata tracking
        op_instance = Operation(
            operation_name=operation_name,
            operation_arguments=operation_arguments,
            # You can also store category/result_type here if you want
        )

        # 5. Run any setup()
        if hasattr(OperationClass, "setup") and callable(OperationClass.setup):
            OperationClass.setup(user_id, op_instance)

        # 6. Fetch prior data (if needed)
        data_source = None
        if hasattr(OperationClass, "data_source") and callable(OperationClass.data_source):
            data_source = OperationClass.data_source(user_id, op_instance)

        # 7. Execute the handler
        try:
            # Note: we pass user_id and data_source plus all declared args
            result_data = OperationClass.handler(
                user_id=user_id,
                data_source=data_source,
                **operation_arguments
            )
        except Exception as e:
            return OperationResult.error(f"Execution error: {str(e)}")

        # 8. Determine which ResultType class to use
        result_type_name = OperationClass.result_type.value  # e.g. "raw", "enriched", etc.
        ResultTypeClass = get_result_type_class(result_type_name)
        if ResultTypeClass is None:
            return OperationResult.error(f"No result‐type for: {result_type_name}")

        # 9. Verify the returned data matches the expected Python type/shape
        if not ResultTypeClass.verify(result_data):
            return OperationResult.error(
                f"Result data failed type check for result_type={result_type_name}"
            )

        # 10. Fill op_instance metadata so cache knows about it
        op_instance.result_data = result_data
        op_instance.result_data_type = OperationClass.result_type

        # 11. Caching
        if ResultTypeClass.cache_policy(op_instance):
            self.cache_service.cache_operation_onto_chain(user_id, op_instance)

        # 12. Build “data_overview_fields” (if any)
        overview_fields = ResultTypeClass.data_overview_fields(user_id, result_data)

        # 13. Should we attach operation history? snapshots?
        attach_history_flag = ResultTypeClass.attach_history()
        attach_snapshots_flag = ResultTypeClass.attach_snapshots_list()

        # 14. Serialize the result for HTTP
        serialized_data = ResultTypeClass.serialize(result_data)

        # 15. Assemble final payload
        meta: Dict[str, Any] = {
            "data_type": result_type_name,
            "operation_name": operation_name,
            "operation_bid": op_instance.id,
        }
        if overview_fields:
            meta["data_overview"] = overview_fields
        if attach_history_flag:
            meta["operations_history"] = get_operation_history(user_id)
        if attach_snapshots_flag:
            meta["snapshots_list"] = get_all_snapshots(user_id)

        return OperationResult(data=serialized_data, meta=meta).to_dict()
