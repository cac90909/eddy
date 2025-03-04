from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status
import json
from shared.util import catch_exceptions_cls
from shared.logger import debug_print_vars, debug_print
from explorer.services.explorer_service import ExplorerService
from explorer.config.operation_result_config import OPERATION_RESULT_DEFINITIONS

@catch_exceptions_cls(exception_return_value="Error")
class ExplorerView(APIView):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.explorer_service = ExplorerService()

    def get(self, request):
        try:
            print()
            debug_print(request.build_absolute_uri())
            debug_print(request.query_params.dict())
            user_id = request.query_params.get("user_id")
            operation_name = request.query_params.get("operation_name")
            operation_arguments = json.loads(request.query_params.get("operation_arguments", "{}"))
            result = self.explorer_service.handle_operation(user_id=user_id, operation_name=operation_name, operation_arguments=operation_arguments)
            serialized_result_data = OPERATION_RESULT_DEFINITIONS[result["meta"]["data_type"]]["serialization"](result["data"])
            result["data"] = serialized_result_data
            return Response(data=result, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)
        
    def post(self, request):
        try:
            print()
            debug_print(request.build_absolute_uri())
            debug_print(request.query_params.dict())
            user_id = request.data.get("user_id")
            operation_name = request.data.get("operation_name")
            operation_arguments = json.loads(request.data.get("operation_arguments", "{}"))
            result = self.explorer_service.handle_operation(user_id=user_id, operation_name=operation_name, operation_arguments=operation_arguments)
            serialized_result_data = OPERATION_RESULT_DEFINITIONS[result["meta"]["data_type"]]["serialization"](result["data"])
            result["data"] = serialized_result_data
            return Response(data=result, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def put(self, request):
        try:
            print()
            debug_print(request.build_absolute_uri())
            debug_print(request.query_params.dict())
            user_id = request.data.get("user_id")
            operation_name = request.data.get("operation_name")
            operation_arguments = json.loads(request.query_params.get("operation_arguments", "{}"))
            result = self.explorer_service.handle_operation(user_id=user_id, operation_name=operation_name, operation_arguments=operation_arguments)
            serialized_result_data = OPERATION_RESULT_DEFINITIONS[result["meta"]["data_type"]]["serialization"](result["data"])
            result["data"] = serialized_result_data
            return Response(data=result, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def delete(self, request):
        try:
            print()
            debug_print(request.build_absolute_uri())
            debug_print(request.query_params.dict())
            user_id = request.query_params.get("user_id")
            operation_name = request.query_params.get("operation_name")
            operation_arguments = json.loads(request.query_params.get("operation_arguments", "{}"))
            result = self.explorer_service.handle_operation(user_id=user_id, operation_name=operation_name, operation_arguments=operation_arguments)
            serialized_result_data = OPERATION_RESULT_DEFINITIONS[result["meta"]["data_type"]]["serialization"](result["data"])
            result["data"] = serialized_result_data
            return Response(data=result, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)