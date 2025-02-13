import json
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status
from shared.logger import debug_print_vars, debug_print
from dev_tools.services.explorer_dev_tool_service import ExplorerDevToolService
from shared.serializers import UniversalSerializer, UniversalDatasetsSerializer, OperationSerializer, OperationChainItemSerializer, SnapshotSerializer
from django.db.models.query import QuerySet
from shared.util import catch_exceptions_cls

@catch_exceptions_cls(exception_return_value={"success": False, "error": "Unhandled exception"})
class ExplorerDevToolView(APIView):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.explorer_dev_tool_service = ExplorerDevToolService()

    def get(self, request):
        try:
            print()
            debug_print(request.build_absolute_uri())
            debug_print(request.query_params.dict())
            user_id = request.query_params.get("user_id")
            operation_type = request.query_params.get("operation_type")
            operation_params = request.query_params.get("operation_params", {})
     
            operation_params = json.loads(operation_params)

            explorer_data = self.explorer_dev_tool_service.handle_operation(
                user_id=user_id,
                operation_type=operation_type,
                operation_params=operation_params
            )
            
            

            if operation_type == "get_cache_datasets":
                serialized_datasets = []
                total_rows = 0
                for dataset in explorer_data:
                    serializer = UniversalSerializer(instance=dataset, many=True)
                    serialized_datasets.append(serializer.data)
                    total_rows += len(serializer.data)
                debug_print(f"{len(serialized_datasets)} serialized datasets in response, with total rows {total_rows}")
                return Response(serialized_datasets, status=status.HTTP_200_OK)
            if operation_type == "get_most_recent_dataset":
                serializer = UniversalSerializer(instance=explorer_data, many=True)
                debug_print(f"{len(serializer.data)} rows in response")
                return Response(serializer.data, status=status.HTTP_200_OK)
            if operation_type == "get_cache_operation_chain":
                serialized_operation_chain = []
                for operation in explorer_data:
                    serializer = OperationSerializer(instance=operation)
                    serialized_operation_chain.append(serializer.data)
                debug_print(f"{len(serialized_operation_chain)} operations in chain")
                return Response(serialized_operation_chain, status=status.HTTP_200_OK)
            if operation_type == "get_most_recent_operation":
                debug_print(explorer_data)
                serializer = OperationSerializer(instance=explorer_data)
                return Response(serializer.data, status=status.HTTP_200_OK)
            if operation_type == "get_row":
                debug_print(explorer_data)
                serializer = UniversalSerializer(instance=explorer_data)
                return Response(serializer.data, status=status.HTTP_200_OK)
            if operation_type == "get_value":
                debug_print(explorer_data)
                return Response(explorer_data, status=status.HTTP_200_OK)
            if operation_type == "get_cache_num_datasets":
                debug_print(explorer_data)
                return Response(explorer_data, status=status.HTTP_200_OK)
            if operation_type == "get_cache_datasets_row_nums":
                return Response(explorer_data, status=status.HTTP_200_OK)
            if operation_type == "get_num_snapshots":
                return Response(explorer_data, status=status.HTTP_200_OK)
            if operation_type == "get_snapshot":
                serializer = SnapshotSerializer(instance=explorer_data)
                debug_print(f"{(serializer.data)} snapshot serialized")
                return Response(data=serializer.data, status=status.HTTP_200_OK)

            else:
                return Response({"error": f"Operation type '{operation_type}' not supported"}, status=status.HTTP_400_BAD_REQUEST)
        
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)
