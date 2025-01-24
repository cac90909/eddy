from django.http import JsonResponse
from django.views import View

from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status

from shared.factories import create_explorer_service


class ExplorerView(APIView):

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

    def get(self, request):
        try:
            user_id = request.query_params.get("user_id")
            data_operation_type = request.query_params.get("data_operation_type")
            data_operation_params = request.query_params.get("data_operation_params", None)
            cache_operation_params = request.query_params.get("cache_operation_params", None)

            service = create_explorer_service()
            result_data = service.perform_data_operation(user_id=user_id, data_operation_type=data_operation_type,
                                                         data_operation_params=data_operation_params, cache_operation_params=cache_operation_params)
            #serialized_data = result_data.to_dict(orient="records")
            #return JsonResponse(serialized_data, safe=False)

            return Response(result_data, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": "Something went wrong."}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)
