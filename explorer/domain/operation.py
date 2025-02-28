import uuid
from datetime import datetime
from abc import ABC, abstractmethod
from shared.logger import debug_print
from explorer.services.explorer_service_config import OPERATION_DEFINITIONS

class Operation(ABC):
    def __init__(self, operation_name, operation_type, operation_arguments):
        self.id = uuid.uuid4()
        self.data_type = "operation"
        self.operation_name = operation_name
        self.operation_type = operation_type
        self.operation_arguments = operation_arguments #Renamed from params
        self.app = "explorer"
        self.created_at = datetime.now()
        self.result = {}

    def validate_args(self, expected_arguments):
        for arg in expected_arguments:
            if arg not in self.operation_arguments and arg != "user_id": #Suspending check on user_id for right now
                return False, f"Missing argument {arg}"
        debug_print("Valid")
        return True, None

    #TODO - Incorporate this (right now we assign the expected type, assuming it is the right type)
    def validate_result(self, expected_result_data_type):
        # if self.result["data_type"] != expected_result_data_type:
        #     raise ValueError(f"Result data type {self.result['data_type']} does not match expected type {expected_result_data_type}")
        try:
            self.result["data_type"] = expected_result_data_type
        except Exception as e:
            debug_print(f"Error validating result: {e}")

    def execute(self, user_id, handler, data_source=None):
        if data_source is not None:
            self.operation_arguments["data_source"] = data_source
        try:
            result_data = handler(user_id=user_id, **self.operation_arguments) #NOTE: handlers in config lambda are preconfigured with user id argument
            self.result["data"] = result_data
        except Exception as e:
            debug_print(f"Error executing operation {self.operation_name}: {e}")

    def to_dict(self):
        return {
            "id": self.id,
            "data_type": self.data_type,
            "operation_name": self.operation_name,
            "operation_type": self.operation_type,
            "operation_arguments": self.operation_arguments,
            "app": self.app,
            "created_at": self.created_at,
            "result": self.result
        }
    def get_operation_result_dict(self):
        return {
            "id": self.id,
            "result": self.result,
        }

    def log_info(self):
        debug_print(f"operation_name: {self.operation_name}, operation_type: {self.operation_type}, operation_arguments: {self.operation_arguments}")

    def __repr__(self):
        return (
            f"Operation(id={self.id}, operation_name={self.operation_name}, "
            f"operation_type={self.operation_type}, operation_arguments={self.operation_arguments}, "
            f"app={self.app}, created_at={self.created_at}, result={self.result})"
        )

    def __str__(self):
        return self.__repr__()
    
    def create_error_result(self, error_message):
        self.result = {
            "data_type": "error",
            "data": error_message
        }
        return self.result


