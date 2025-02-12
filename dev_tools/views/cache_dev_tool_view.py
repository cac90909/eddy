import json
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status
from shared.logger import debug_print_vars, debug_print
from dev_tools.services.cache_dev_tool_service import CacheDevToolService
from shared.serializers import UniversalSerializer, UniversalDatasetsSerializer, OperationSerializer, OperationChainItemSerializer
from django.db.models.query import QuerySet
from shared.util import catch_exceptions_cls

@catch_exceptions_cls(exception_return_value={"success": False, "error": "Unhandled exception"})
class CacheDevToolView(APIView):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.cache_dev_tool_service = CacheDevToolService()

    def get(self, request):
        try:
            print()
            debug_print(request.build_absolute_uri())
            debug_print(request.query_params.dict())
            user_id = request.query_params.get("user_id")
            operation_type = request.query_params.get("operation_type")
            cache_type = request.query_params.get("cache_type")
            operation_params = request.query_params.get("operation_params", {})
            
            operation_params = json.loads(operation_params)

            cache_data = self.cache_dev_tool_service.handle_cache_operation(
                user_id=user_id,
                cache_type=cache_type,
                operation_type=operation_type,
                operation_params=operation_params
            )
            
            if operation_type == "get_cache_datasets":
                debug_print(cache_data)
                #serializer = UniversalSerializer(instance=cache_data, many=True)
                pass 
                for dataset in cache_data:
                    pass
                serializer = UniversalDatasetsSerializer(instance=cache_data, many=True)
                return Response({"data": serializer.data}, status=status.HTTP_200_OK)
            elif operation_type == "get_user_cache_stats":
                return Response({"data": cache_data}, status=status.HTTP_200_OK)
            else:
                return Response({"error": f"Operation type '{operation_type}' not supported for cache type '{cache_type}'."}, status=status.HTTP_400_BAD_REQUEST)
        
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)
