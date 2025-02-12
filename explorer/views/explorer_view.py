from django.http import JsonResponse
from django.forms.models import model_to_dict
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status
import json
from shared.logger import debug_print_vars, debug_print
from explorer.services.explorer_service import ExplorerService
from shared.serializers import UniversalSerializer

class ExplorerView(APIView):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        # Instantiate the service
        self.explorer_service = ExplorerService()

    def get(self, request):
        try:
            # Log the full accessed URL
            print()
            debug_print(request.build_absolute_uri())
            debug_print(request.query_params.dict())
            user_id = request.query_params.get("user_id")
            operation_type = request.query_params.get("operation_type")
            operation_params = request.query_params.get("operation_params")
            operation_params = json.loads(operation_params)
            
            result_data = self.explorer_service.handle_operation(
                user_id=user_id,
                operation_type=operation_type,
                operation_params=operation_params
            )

            if operation_type in ["init_user", "filter"]:
                serializer = UniversalSerializer(instance=result_data, many=True)
                debug_print(f"{len(serializer.data)} serialized response rows")
                return Response(data=serializer.data, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def post(self, request):
        try:
            debug_print_vars(data=request.data)
            user_id = request.data.get("user_id")
            operation_type = request.data.get("operation_type")
            operation_params = request.data.get("operation_params", {})
            result_data = self.explorer_service.handle_operation(
                user_id=user_id,
                operation_type=operation_type,
                operation_params=operation_params
            )
            return Response({"data": result_data}, status=status.HTTP_201_CREATED)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def put(self, request):
        try:
            debug_print_vars(data=request.data)
            user_id = request.data.get("user_id")
            operation_type = request.data.get("operation_type")
            operation_params = request.data.get("operation_params", {})
            result_data = self.explorer_service.handle_operation(
                user_id=user_id,
                operation_type=operation_type,
                operation_params=operation_params
            )
            return Response({"data": result_data}, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def delete(self, request):
        try:
            debug_print_vars(data=request.data)
            user_id = request.data.get("user_id")
            operation_type = request.data.get("operation_type")
            operation_params = request.data.get("operation_params", {})
            result_data = self.explorer_service.handle_operation(
                user_id=user_id,
                operation_type=operation_type,
                operation_params=operation_params
            )
            return Response({"data": result_data}, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)
