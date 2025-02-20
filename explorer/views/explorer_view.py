from django.http import JsonResponse
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status
import json
from shared.util import catch_exceptions_cls
from shared.logger import debug_print_vars, debug_print
from explorer.services.explorer_service import ExplorerService
from shared.serializers import UniversalSerializer, SnapshotSerializer, FlexibleDictSerializer

@catch_exceptions_cls(exception_return_value="Error")
class ExplorerView(APIView):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.explorer_service = ExplorerService()
        self.universal_serializer = UniversalSerializer()
        self.snapshot_serializer = SnapshotSerializer()
        print()
        debug_print(self.request.build_absolute_uri())
        debug_print(self.request.query_params.dict())

    request_operation_type_handler_mapping = {
        "explorer_raw" : ExplorerService.handle_universal_raw_operation,
        "explorer_enrichment" : ExplorerService.handle_universal_enrichment_operation,
        "explorer_metric" : ExplorerService.handle_universal_metric_operation,
        "explorer_list" : ExplorerService.handle_universal_list_operation,
        "explorer_state" : ExplorerService.handle_explorer_state_operation,
        "snapshot" : ExplorerService.handle_snapshot_operation
    }

    response_data_type_serializer_mapping = {
        "universal_raw" : lambda data : UniversalSerializer(instance=data, many=True),
        "universal_enrichment" : lambda data : FlexibleDictSerializer(instance=data, many=True),
        "universal_metric" : lambda data : data,
        "universal_list" : lambda data : data,
        "status" : lambda data : data,
        "snapshot" : lambda snapshot : SnapshotSerializer(instance=snapshot),
        "snapshot_list" : lambda snapshot_list : [SnapshotSerializer(instance=snapshot) for snapshot in snapshot_list]
    }

    def get(self, request):
        try:
            user_id = request.query.get("user_id")
            operation_type = request.query_params.get("operation_type")
            operation_name = request.query_params.get("operation_name")
            operation_params = json.loads(request.query_params.get("operation_params", "{}"))

            result_data = self.request_operation_type_handler_mapping[operation_type](user_id=user_id, operation_name=operation_name, operation_params=operation_params)

            serialized_data = self.response_data_type_serializer_mapping[result_data["data_type"]](result_data["data"])
            return Response(data=serialized_data, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)
        
    def post(self, request):
        try:
            user_id = request.data.get("user_id")
            operation_type = request.data.get("operation_type")
            operation_name = request.data.get("operation_type")
            operation_params = json.loads(request.query_params.get("operation_params", "{}"))

            result_data = self.request_operation_type_handler_mapping[operation_type](user_id=user_id, operation_name=operation_name, operation_params=operation_params)

            serialized_data = self.response_data_type_serializer_mapping[result_data["data_type"]](result_data["data"])
            return Response(data=serialized_data, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def put(self, request):
        try:
            user_id = request.data.get("user_id")
            operation_type = request.data.get("operation_type")
            operation_name = request.data.get("operation_type")
            operation_params = json.loads(request.query_params.get("operation_params", "{}"))

            result_data = self.request_operation_type_handler_mapping[operation_type](user_id=user_id, operation_name=operation_name, operation_params=operation_params)

            serialized_data = self.response_data_type_serializer_mapping[result_data["data_type"]](result_data["data"])
            return Response(data=serialized_data, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def delete(self, request):
        try:
            user_id = request.data.get("user_id")
            operation_type = request.data.get("operation_type")
            operation_name = request.data.get("operation_type")
            operation_params = json.loads(request.query_params.get("operation_params", "{}"))

            result_data = self.request_operation_type_handler_mapping[operation_type](user_id=user_id, operation_name=operation_name, operation_params=operation_params)

            serialized_data = self.response_data_type_serializer_mapping[result_data["data_type"]](result_data["data"])
            return Response(data=serialized_data, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)