# src/views/explorer_view.py
import json
from django.http import JsonResponse
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status
from shared.logger import debug_print
from explorer.services.explorer_service import ExplorerService

class ExplorerView(APIView):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        # Instantiate the service
        self.explorer_service = ExplorerService()

    def get(self, request):
        try:
            debug_print("ExplorerView GET called", {"query_params": request.query_params.dict()})
            user_id = request.query_params.get("user_id")
            operation_type = request.query_params.get("operation_type")
            operation_params = request.query_params.get("operation_params")
            if operation_params:
                try:
                    operation_params = json.loads(operation_params)
                except json.JSONDecodeError:
                    return Response({"error": "Invalid JSON for operation_params"}, status=status.HTTP_400_BAD_REQUEST)
            else:
                operation_params = {}
            result_data = self.explorer_service.handle_operation(
                user_id=user_id,
                operation_type=operation_type,
                operation_params=operation_params
            )
            # Determine row_count if result_data is iterable
            row_count = len(result_data) if hasattr(result_data, "__len__") else 1
            return Response({"data": result_data, "row_count": row_count}, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def post(self, request):
        try:
            debug_print("ExplorerView POST called", {"data": request.data})
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
            debug_print("ExplorerView PUT called", {"data": request.data})
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
            debug_print("ExplorerView DELETE called", {"data": request.data})
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
