import uuid
from datetime import datetime
from abc import ABC, abstractmethod
from shared.logger import debug_print

class Operation(ABC):
    def __init__(self, operation_name, operation_type, operation_arguments):
        self.id = uuid.uuid4()
        self.operation_name = operation_name
        self.operation_type = operation_type
        self.operation_arguments = operation_arguments #Renamed from params
        self.data_type = "operation"
        self.app = "explorer"
        self.created_at = datetime.now()
        self.result = {}
        self.handler = None

    @property
    @abstractmethod
    def required_arguments(self):
        """Return a list of required argument keys for this operation."""
        pass
    
    @property
    @abstractmethod
    def result_data_type(self):
        """Return a list of required argument keys for this operation."""
        pass

    def validate(self):
        for arg in self.required_arguments:
            if arg not in self.operation_arguments:
                raise ValueError(f"Argument {arg} is required for operation {self.operation_name}")
        debug_print("Valid")

    def execute(self, user_id):
        result_data = self.handler(user_id=user_id, **self.operation_arguments)
        self.result["data"] = result_data
        self.result["data_type"] = self.result_data_type

    def to_dict(self):
        return {
            "id": self.id,
            "operation_name": self.operation_name,
            "operation_type": self.operation_type,
            "operation_arguments": self.operation_arguments,
            "data_type": self.data_type,
            "created_at": self.created_at,
            "result": self.result,
            "app": self.app
        }
    def get_operation_result_dict(self):
        return {
            "id": self.id,
            "result": self.result,
        }

    def log_info(self):
        debug_print(f"operation_name: {self.operation_name}, operation_type: {self.operation_type}, operation_arguments: {self.operation_arguments}")

# --- Universal Operations ---

class InitUserOperation(Operation):
    required_arguments = ["user_id"]
    result_data_type = "raw"
    operation_name = "init_user"
    operation_type = "universal"

class ResetOperation(Operation):
    required_arguments = ["user_id"]
    result_data_type = "raw"
    operation_name = "reset" 
    operation_type = "universal"

class FilterOperation(Operation):
    required_arguments = ["user_id", "column_name", "filter_value", "filter_type"]
    result_data_type = "raw"
    operation_name = "filter"
    operation_type = "universal"

class TraverseOperation(Operation):
    required_arguments = ["user_id", "start_id", "traversal_directions"]
    result_data_type = "raw"
    operation_name = "traverse"
    operation_type = "universal"

class UndoOperation(Operation):
    required_arguments = ["user_id"]
    result_data_type = "raw"
    operation_name = "undo"
    operation_type = "universal"

class LoadSnapshotOperation(Operation):
    required_arguments = ["user_id", "snapshot_id"]
    result_data_type = "raw"
    operation_name = "load_snapshot"
    operation_type = "universal"

class GroupAggregateOperation(Operation):
    required_arguments = ["user_id", "group_column", "aggregate_operation", "target_column"]
    result_data_type = "enriched"
    operation_name = "group_aggregate"
    operation_type = "universal"

class GetUniqueColumnValuesOperation(Operation):
    required_arguments = ["user_id", "column_name", "options_call"]
    result_data_type = "list"
    operation_name = "get_unique_column_values"
    operation_type = "universal"

class GetUniqueJsonKeysOperation(Operation):
    required_arguments = ["user_id", "options_call"]
    result_data_type = "list"
    operation_name = "get_unique_json_keys"
    operation_type = "universal"

class GetUniqueJsonKeyValuesOperation(Operation):
    required_arguments = ["user_id", "json_key", "options_call"]
    result_data_type = "list"
    operation_name = "get_unique_json_key_values"
    operation_type = "universal"

class GetCountOperation(Operation):
    required_arguments = ["user_id", "column_name"]
    result_data_type = "metric"
    operation_name = "get_count"
    operation_type = "universal"

class GetAverageOperation(Operation):
    required_arguments = ["user_id", "column_name"]
    result_data_type = "metric"
    operation_name = "get_average"
    operation_type = "universal"

class GetSumOperation(Operation):
    required_arguments = ["user_id", "column_name"]
    result_data_type = "metric"
    operation_name = "get_sum"
    operation_type = "universal"

class GetMinOperation(Operation):
    required_arguments = ["user_id", "column_name"]
    result_data_type = "metric"
    operation_name = "get_min"
    operation_type = "universal"

class GetMaxOperation(Operation):
    required_arguments = ["user_id", "column_name"]
    result_data_type = "metric"
    operation_name = "get_max"
    operation_type = "universal"


# --- State Operations ---


class StartExplorerSessionOperation(Operation):
    required_arguments = ["user_id"]
    result_data_type = "status"
    operation_name = "start_explorer_session"
    operation_type = "state"

class EndExplorerSessionOperation(Operation):
    required_arguments = ["user_id"]
    result_data_type = "status"
    operation_name = "end_explorer_session"
    operation_type = "state"

class GetOperationChainOperation(Operation):
    required_arguments = ["user_id"]
    result_data_type = "operation_chain"
    operation_name = "get_operation_chain"
    operation_type = "state"

class GetOperationChainOperationsOperation(Operation):
    required_arguments = ["user_id"]
    result_data_type = "operations_list"
    operation_name = "get_operation_chain_operations"
    operation_type = "state"

class GetOperationChainResultsOperation(Operation):
    required_arguments = ["user_id"]
    result_data_type = "results_list"
    operation_name = "get_operation_chain_results"
    operation_type = "state"


# --- Snapshot Operations ---


class CreateSnapshotOperation(Operation):
    required_arguments = ["user_id", "snapshot_name"]
    result_data_type = "snapshot"
    operation_name = "create_snapshot"
    operation_type = "snapshot"

class DeleteSnapshotOperation(Operation):
    required_arguments = ["user_id", "snapshot_id"]
    result_data_type = "status"
    operation_name = "delete_snapshot"
    operation_type = "snapshot"

class UpdateSnapshotOperation(Operation):
    required_arguments = ["user_id", "snapshot_id"]
    result_data_type = "snapshot"
    operation_name = "update_snapshot"
    operation_type = "snapshot"

class GetSnapshotOperation(Operation):
    required_arguments = ["user_id", "snapshot_id"]
    result_data_type = "snapshot"
    operation_name = "get_snapshot"
    operation_type = "snapshot"

class GetAllSnapshotsOperation(Operation):
    required_arguments = ["user_id"]
    result_data_type = "snapshot_list"
    operation_name = "get_all_snapshots"
    operation_type = "snapshot"


