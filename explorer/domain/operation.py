import uuid
from datetime import datetime
from abc import ABC, abstractmethod
from shared.logger import debug_print
from explorer.config.operation_config import OPERATION_DEFINITIONS

class Operation(ABC):
    def __init__(self, operation_name, operation_type, operation_arguments):
        self.id = uuid.uuid4()
        self.operation_name = operation_name
        self.operation_type = operation_type
        self.operation_arguments = operation_arguments #Renamed from params
        self.app = "explorer"
        self.created_at = datetime.now()
        self.result_data = {}
        self.result_data_type = None

    def execute(self, user_id, handler, data_source=None):
        kwargs = self.operation_arguments.copy()
        if data_source is not None:
            kwargs["data_source"] = data_source
        result_data = handler(user_id=user_id, **kwargs)
        return result_data

    def to_dict(self):
        return {
            "id": self.id,
            "operation_name": self.operation_name,
            "operation_type": self.operation_type,
            "operation_arguments": self.operation_arguments,
            "app": self.app,
            "created_at": self.created_at,
            "result_data": self.result_data,
            "result_data_type" : self.result_data_type
        }

    def log_info(self):
        debug_print(f"operation_name: {self.operation_name}, operation_type: {self.operation_type}, operation_arguments: {self.operation_arguments}")

    def __repr__(self):
        return (
            f"Operation(id={self.id}, operation_name={self.operation_name}, "
            f"operation_type={self.operation_type}, operation_arguments={self.operation_arguments}, "
            f"app={self.app}, created_at={self.created_at}, result={self.result_data})"
        )

    def __str__(self):
        return self.__repr__()


