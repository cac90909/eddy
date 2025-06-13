# explorer/services/explorer_dispatcher.py

import json
from typing import Any, Dict
from explorer.domain.operations.operation_registry import get_operation_class
from explorer.domain.result_types.operation_result_registry import get_result_type_class
from explorer.domain.operation_result import OperationResult
from explorer.util.operation_util import validate_args_against_metadata
from explorer.util.operation_result_util import get_operation_history, get_all_snapshots
from explorer.services.explorer_cache_service import ExplorerCacheService

class ExplorerDispatcher:
    def __init__(self):
        self.cache_service = ExplorerCacheService()

    def handle_operation(
        self,
        user_id: int,
        operation_name: str,
        operation_arguments: Dict[str, Any]
    ) -> Dict[str, Any]:
        # 1. Lookup
        OperationClass = get_operation_class(operation_name)
        if OperationClass is None:
            return OperationResult.error(f"Operation not found: {operation_name}")

        # 2. Validate args
        missing = validate_args_against_metadata(OperationClass, operation_arguments)
        if missing:
            return OperationResult.error(f"Missing required arguments: {missing}")

        # 3. Instantiate & execute
        op_instance = OperationClass(user_id, **operation_arguments)
        try:
            op_instance.execute()
        except Exception as e:
            return OperationResult.error(f"Execution error: {str(e)}")

        # 4. Pick correct result‐type helper
        result_type_name = op_instance.result_data_type.value
        ResultTypeClass = get_result_type_class(result_type_name)
        if not ResultTypeClass:
            return OperationResult.error(f"No result‐type for: {result_type_name}")

        # 5. Verify & maybe cache
        if not ResultTypeClass.verify(op_instance.result_data):
            return OperationResult.error(f"Result data failed type check for {result_type_name}")

        if ResultTypeClass.cache_policy(op_instance):
            self.cache_service.cache_operation_onto_chain(user_id, op_instance)

        # 6. Build response payload
        overview = ResultTypeClass.data_overview_fields(user_id, op_instance.result_data)
        attach_history = ResultTypeClass.attach_history()
        attach_snapshots = ResultTypeClass.attach_snapshots_list()
        serialized = ResultTypeClass.serialize(op_instance.result_data)

        meta: Dict[str, Any] = {
            "data_type": result_type_name,
            "operation_name": operation_name,
            "operation_bid": op_instance.id,
        }
        if overview:
            meta["data_overview"] = overview
        if attach_history:
            meta["operations_history"] = get_operation_history(user_id)
        if attach_snapshots:
            meta["snapshots_list"] = get_all_snapshots(user_id)

        return OperationResult(data=serialized, meta=meta).to_dict()
