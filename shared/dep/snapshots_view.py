from django.forms.models import model_to_dict
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import status

from shared.logger import debug_print
from shared.services.snapshots_service import SnapshotsService


class SnapshotsView(APIView):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        # Instantiate the service rather than assigning the class
        self.snapshots_service = SnapshotsService()

    #Supports getting all snapshots ("all") or a single snapshot ("one"), passed through "get_type" parameter
    def get(self, request):
        """
        GET can return either a list of snapshots ("get_type=all")
        or a single snapshot ("get_type=one" with snapshot_id provided).
        """
        try:
            debug_print(request)
            user_id = request.query_params.get("user_id")
            get_type = request.query_params.get("get_type")
            if get_type == "all":
                snapshots = self.snapshots_service.get_all_snapshots(user_id=user_id)
                # Convert the queryset into a list of dictionaries.
                data = [model_to_dict(snapshot) for snapshot in snapshots]
                row_count = len(data)
            elif get_type == "one":
                snapshot_id = request.query_params.get("snapshot_id")
                snapshot = self.snapshots_service.get_snapshot(user_id=user_id, snapshot_id=snapshot_id)
                data = model_to_dict(snapshot)
                row_count = 1
            else:
                raise ValueError(f"Unsupported get_type: {get_type}")
            return Response({"data": data, "row_count": row_count}, status=status.HTTP_200_OK)
        except ValueError as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_500_INTERNAL_SERVER_ERROR)

    def post(self, request):
        """
        Create a new snapshot. Expects user_id, title, description, and operation_chain in request.data.
        """
        try:
            user_id = request.data.get("user_id")
            title = request.data.get("title")
            description = request.data.get("description")
            operation_chain = request.data.get("operation_chain")

            snapshot = self.snapshots_service.create_snapshot(
                user_id=user_id,
                title=title,
                description=description,
                operation_chain=operation_chain
            )
            data = model_to_dict(snapshot)
            return Response({"data": data}, status=status.HTTP_201_CREATED)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)

    def put(self, request):
        """
        Update an existing snapshot. Expects user_id, snapshot_id and any fields to update in request.data.
        """
        try:
            user_id = request.data.get("user_id")
            snapshot_id = request.data.get("snapshot_id")
            title = request.data.get("title")
            description = request.data.get("description")
            operation_chain = request.data.get("operation_chain")

            snapshot = self.snapshots_service.update_snapshot(
                user_id=user_id,
                snapshot_id=snapshot_id,
                title=title,
                description=description,
                operation_chain=operation_chain
            )
            data = model_to_dict(snapshot)
            return Response({"data": data}, status=status.HTTP_200_OK)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)

    def delete(self, request):
        """
        Delete a snapshot. Expects user_id and snapshot_id in request.data.
        """
        try:
            user_id = request.data.get("user_id")
            snapshot_id = request.data.get("snapshot_id")

            self.snapshots_service.delete_snapshot(user_id=user_id, snapshot_id=snapshot_id)
            return Response({"message": f"Snapshot {snapshot_id} deleted successfully."}, status=status.HTTP_200_OK)
        except Exception as e:
            return Response({"error": str(e)}, status=status.HTTP_400_BAD_REQUEST)
