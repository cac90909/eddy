import json
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status
from shared.logger import debug_print_vars
from dev_tools.services.cache_dev_tool_service import CacheDevToolService
from shared.serializers import UserCacheSerializer
from django.db.models.query import QuerySet

class CacheDevToolView(APIView):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.cache_dev_tool_service = CacheDevToolService()

    def get(self, request):
        try:
            print()
            debug_print_vars(url=request.build_absolute_uri(), query_params=request.query_params.dict())
            user_id = request.query_params.get("user_id")
            operation_type = request.query_params.get("operation_type")
            cache_type = request.query_params.get("cache_type")
            operation_params = request.query_params.get("operation_params")
            
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
            
            if isinstance(result_data, dict) and not result_data.get("success", True):
                return Response(result_data, status=status.HTTP_400_BAD_REQUEST)
            
            if operation_type == "get_user_cache":
            # Extract the "data" portion from the service result.
                data_part = result_data.get("data", {})
                # Flatten the "datasets" field if needed.
                datasets = data_part.get("datasets", [])
                if isinstance(datasets, list) and datasets:
                    # If the first element is a QuerySet, assume it's the only element and flatten it.
                    if isinstance(datasets[0], QuerySet):
                        data_part["datasets"] = list(datasets[0])
                
                # Now pass the data to the serializer as an instance.
                serializer = UserCacheSerializer(instance=data_part)
                return Response({"data": serializer.data}, status=status.HTTP_200_OK)
            elif operation_type == "get_user_cache_stats":
                return Response({"data": result_data}, status=status.HTTP_200_OK)
            else:
                return Response({"error": f"Operation type '{operation_type}' not supported for cache type '{cache_type}'."}, status=status.HTTP_400_BAD_REQUEST)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)
