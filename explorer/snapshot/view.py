"""
This viewset defines all supported snapshot-related actions within the Explorer context.

Endpoints follow RESTful conventions:
- `GET /explorer/snapshot/` → Get all snapshots
- `GET /explorer/snapshot/<id>/` → Get a specific snapshot
- `POST /explorer/snapshot/` → Save a new snapshot
- `PUT /explorer/snapshot/<id>/` → Update a snapshot
- `DELETE /explorer/snapshot/<id>/` → Delete a snapshot

Documentation support is added via drf-spectacular:
- Each endpoint is decorated with @extend_schema, explicitly listing the request and response serializers.
- This ensures self-documenting APIs, discoverable through Swagger or Redoc.
- Request validation and field extraction are handled through dedicated request serializers.
- All responses follow a standardized shape via StandardOperationResponseSerializer.
"""

from rest_framework.viewsets import ViewSet
from rest_framework.response import Response
from rest_framework.decorators import action
from drf_spectacular.utils import extend_schema, OpenApiResponse
from rest_framework import status

from explorer.snapshot.serializers import *
from explorer.serializers.base import StandardOperationResponseSerializer
from explorer.snapshot.service import ExplorerSnapshotService
from shared.services.snapshots_service import SnapshotsService

#TODO -> this class makes use of SnapshotResponseSerializer (snapshots are being returned)
#        , really the snapshot serializer should reside in shared (core) and then we create 
#        serializers in explorer that reference the shared snapshot serializer.
class SnapshotViewSet(ViewSet):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.snapshot_service = ExplorerSnapshotService()

    @extend_schema(
        request=SaveSnapshotRequestSerializer,
        responses=SnapshotResponseSerializer
    )
    @action(detail=False, methods=["post"], url_path="", url_name="save_snapshot")
    def save_snapshot(self, request):
        in_ser = SaveSnapshotRequestSerializer(data=request.data)
        in_ser.is_valid(raise_exception=True)
        validated = in_ser.validated_data
        snapshot =  self.snapshot_service.create_snapshot(
            user_id=validated["user_id"],
            operation_chain=None,  # Injected in real context
            title=validated["title"],
            description=validated.get("description")
        )
        out_ser = SnapshotResponseSerializer(snapshot)
        return Response({"data": out_ser.data, "meta": {}})

    @extend_schema(
        request=GetSnapshotRequestSerializer,
        responses=SnapshotResponseSerializer
    )
    @action(detail=True, methods=["get"], url_path="", url_name="get_snapshot")
    def get_snapshot(self, request, pk=None):
        in_ser = GetSnapshotRequestSerializer(data=request.query_params)
        in_ser.is_valid(raise_exception=True)
        validated = in_ser.validated_data
        snapshot = self.snapshot_service.get_snapshot(user_id=validated["user_id"], snapshot_id=pk)
        out_ser = SnapshotResponseSerializer(snapshot)
        return Response({"data": out_ser.data, "meta": {}})

    @extend_schema(
        request=GetAllSnapshotsRequestSerializer,
        responses=SnapshotResponseSerializer(many=True)
    )
    @action(detail=False, methods=["get"], url_path="", url_name="get_all_snapshots")
    def get_all_snapshots(self, request):
        in_ser = GetAllSnapshotsRequestSerializer(data=request.query_params)
        in_ser.is_valid(raise_exception=True)
        validated = in_ser.validated_data
        snapshot = self.snapshot_service.get_all_snapshots(user_id=validated["user_id"])
        out_ser = SnapshotResponseSerializer(snapshot, many=True)
        return Response({"data": out_ser.data, "meta": {}})

    @extend_schema(
        request=UpdateSnapshotRequestSerializer,
        responses=SnapshotResponseSerializer
    )
    @action(detail=True, methods=["put"], url_path="", url_name="update_snapshot")
    def update_snapshot(self, request, pk=None):
        in_ser = UpdateSnapshotRequestSerializer(data=request.data)
        in_ser.is_valid(raise_exception=True)
        validated = in_ser.validated_data
        snapshot = self.snapshot_service.update_snapshot(
            user_id=validated["user_id"],
            snapshot_id=pk,
            title=validated.get("title"),
            description=validated.get("description")
        )
        out_ser = SnapshotResponseSerializer(snapshot)
        return Response({"data": out_ser.data, "meta": {}})


    #NOTE: can drop the serializer usage here so only user id is used (but still keep extend schema for drf-spec. doc purposes)
    @extend_schema(
        request=DeleteSnapshotRequestSerializer,
        responses={204: OpenApiResponse(description="Snapshot deleted successfully")}
    )
    @action(detail=True, methods=["delete"], url_path="", url_name="delete_snapshot")
    def delete_snapshot(self, request, pk=None):
        in_ser = DeleteSnapshotRequestSerializer(data=request.query_params)
        in_ser.is_valid(raise_exception=True)
        validated = in_ser.validated_data
        self.snapshot_service.delete_snapshot(user_id=validated["user_id"], snapshot_id=pk)
        return Response(status=status.HTTP_204_NO_CONTENT)
