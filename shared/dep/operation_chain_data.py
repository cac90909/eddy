import uuid
from datetime import datetime
from abc import ABC, abstractmethod
from shared.logger import debug_print

class OperationChainData(ABC):
    def __init__(self, data):
        self.id = uuid.uuid4()
        self.data = data
        self.data_type = "None"
        self.app = "explorer"
        self.created_at = datetime.now()
    
    @abstractmethod
    def to_dict(self):
        return {
            'id': self.id,
            'data': self.data,
            'data_type': self.data_type,
            'app': self.app,
            'created_at': self.created_at
        }



class OperationChain(OperationChainData):
    data_type = "operation_chain"

    def to_dict(self):
        obj_dict = super().to_dict()
        operation_dict_list = []
        for operation in self.data:
            operation_dict = operation.to_dict()
            operation_dict_list.append(operation_dict)
        obj_dict['data'] = operation_dict_list
        return obj_dict
        

class OperationChainResultData(OperationChainData):
    data_type = "results_list"

class OperationChainOperations(OperationChainData):
    data_type = "operations_list"
        
