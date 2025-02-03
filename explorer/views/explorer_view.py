from django.http import JsonResponse
from django.views import View

from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status

from shared.logger import debug_print
from explorer.services.explorer_service import ExplorerService


class ExplorerView(APIView):

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.explorer_service = ExplorerService

    def get(self, request):
        try:
            debug_print(request)
            user_id = request.query_params.get("user_id")
            operation_type = request.query_params.get("operation_type")
            operation_params = request.query_params.get("operation_params", None)
            result_data = self.explorer_service.handle_operation(user_id=user_id, operation_type=operation_type, operation_params=operation_params)
            return JsonResponse({"data":result_data, "row_count":len(result_data)}, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)

