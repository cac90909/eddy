#Probably fold this into just static methods, not much purpose for class-ing this i think
from rest_framework.exceptions import APIException, NotFound
from typing import Any

import core.operation.arguments.util as OperationArgumentsUtil
from backend.apps.core.domain.operation.maps.op_name_to_spec import OPERATION_SPECS
from backend.apps.core.domain.operation.structures.operation_spec import OperationSpec, ArgumentSpec
from core.domain.enums.operation import OperationName, OperationArgumentName
from backend.apps.explorer.services.cache import ExplorerCacheService

class ExplorerLookupService:

    def __init__(self):
        self.cache_svc = ExplorerCacheService()

    def get_operation_argument_options(
        self,
        user_id: int,
        op_name: str,
        arg_name: str,
        prev_args: dict[str, any],
    ) -> list[Any]:
        #NOTE: assumes all operations need prev data src right now
        try:
            op_enum = OperationName(op_name)
            op_spec: OperationSpec = OPERATION_SPECS[op_enum]
        except (ValueError, KeyError):
            raise NotFound(f"{op_name!r} is not a valid operation")
        try:
            arg_enum = OperationArgumentName(arg_name)
            arg_spec: ArgumentSpec = op_spec.args[arg_enum]
        except ValueError:
            raise NotFound(f"{arg_name!r} is not a valid argument of {op_name}")
        
        choices_fn = arg_spec.choices_fn
        data_src = self.cache_svc.last_result
        try:
            options = OperationArgumentsUtil.invoke_choices_fn(choices_fn, user_id, data_src, prev_args)
            return options
        except Exception as e:
            raise APIException(e)
    
    def get_operation_info(self, operation_name):
        #info on arguments, description of arguments, description of operatoin
        pass