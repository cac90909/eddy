# explorer/views.py

import json
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status
from explorer.services.explorer_dispatcher import ExplorerDispatcher

class ExploreView(APIView):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.dispatcher = ExplorerDispatcher()

    def get(self, request):
        try:
            user_id = int(request.query_params.get("user_id"))
            operation_name = request.query_params.get("operation_name")
            operation_arguments = json.loads(request.query_params.get("operation_arguments", "{}"))

            result_payload = self.dispatcher.handle_operation(
                user_id=user_id,
                operation_name=operation_name,
                operation_arguments=operation_arguments,
            )
            return Response(data=result_payload, status=200)

        except ValueError as e:
            return Response({"error": str(e)}, status=400)
        except Exception as e:
            return Response({"error": str(e)}, status=500)
        
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