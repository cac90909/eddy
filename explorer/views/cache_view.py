from django.http import JsonResponse
from django.views import View

from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status

from shared.logger import debug_print
from explorer.services.cache_service import CacheService


class CacheView(APIView):

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.cache_service = CacheService

    def get(self, request):
        try:
            debug_print(request)
            user_id = request.query_params.get("user_id")
            cache_type = request.query_params.get("cache_type")
            if cache_type == "manual":
                result_data = CacheService
            elif cache_type == "deep_save":
                result_data = CacheService
            else:
                raise ValueError(f"Unsupported cache type: {cache_type}")
            return JsonResponse({"data":result_data, "row_count":len(result_data)}, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)

