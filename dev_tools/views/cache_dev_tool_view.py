from django.forms.models import model_to_dict
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status
import json
from shared.logger import debug_print_vars
#from shared.util import log_vars_vals_cls, catch_exceptions_cls
from dev_tools.services.cache_dev_tool_service import CacheDevToolService

class CacheDevToolView(APIView):


    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.cache_dev_tool_service = CacheDevToolService()

    def get(self, request):
        try:
            debug_print_vars(url=request.build_absolute_uri(), query_params=request.query_params.dict())
            user_id = request.query_params.get("user_id")
            operation_type = request.query_params.get("operation_type")
            operation_params = request.query_params.get("operation_params")
            cache_type = request.query_params.get("cache_type")
            if operation_params:
                try:
                    operation_params = json.loads(operation_params)
                except json.JSONDecodeError:
                    return Response({"error": "Invalid JSON for operation_params"}, status=status.HTTP_400_BAD_REQUEST)
            else:
                operation_params = {}
            result_data = self.cache_dev_tool_service.handle_cache_operation(
                user_id=user_id,
                cache_type=cache_type,
                operation_type=operation_type,
                operation_params=operation_params
            )
            data_serializable = [model_to_dict(obj) for obj in result_data]
            return Response({"data": data_serializable, "row_count": len(data_serializable)}, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)