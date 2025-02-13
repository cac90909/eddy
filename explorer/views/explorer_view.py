from django.http import JsonResponse
from django.forms.models import model_to_dict
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status
import json
from shared.util import catch_exceptions_cls
from shared.logger import debug_print_vars, debug_print
from explorer.services.explorer_service import ExplorerService
from shared.serializers import UniversalSerializer, SnapshotSerializer

@catch_exceptions_cls(exception_return_value="Error")
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

            if operation_type in ["init_user", "filter", "traverse"]:
                serializer = UniversalSerializer(instance=result_data, many=True)
                debug_print(f"{len(serializer.data)} serialized response rows")
                return Response(data=serializer.data, status=status.HTTP_200_OK)
            if operation_type in ["get_filter_values", "get_json_keys", "get_json_values"]:
                debug_print(f"{len(result_data)} serialized response rows")
                return Response(data=result_data, status=status.HTTP_200_OK)
            if operation_type in ["get_all_snapshots"]:
                serialized_snapshots = []
                for snapshot in result_data:
                    serializer = SnapshotSerializer(instance=snapshot)
                    serialized_snapshots.append(serializer.data)
                return Response(data=serialized_snapshots, status=status.HTTP_200_OK)
            if operation_type in ["load_snapshot"]:
                serializer = UniversalSerializer(instance=result_data, many=True)
                debug_print(f"{len(result_data)} serialized response rows")
                return Response(data=serializer.data, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def post(self, request):
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

            if operation_type in ["update_snapshot"]:
                serializer = SnapshotSerializer(instance=result_data)
                debug_print(f"{(serializer.data)} snapshot serialized")
                return Response(data=serializer.data, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def put(self, request):
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

            if operation_type in ["update_snapshot"]:
                serializer = SnapshotSerializer(instance=result_data)
                debug_print(f"{(serializer.data)} snapshot serialized")
                return Response(data=serializer.data, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def delete(self, request):
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
            if operation_type in ["undo_operation"]:
                serializer = UniversalSerializer(instance=result_data, many=True)
                debug_print(f"{len(serializer.data)} serialized response rows")
                return Response(data=serializer.data, status=status.HTTP_200_OK)
            if operation_type in ["delete_snapshot"]:
                debug_print(f'Deleted Snapshot {(operation_params["snapshot_id"])}')
                return Response(status=status.HTTP_200_OK)
            if operation_type in ["end_explorer_session"]:
                debug_print(f'Deleted Snapshot {(operation_params["snapshot_id"])}')
                return Response(status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)
