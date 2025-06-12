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
from drf_spectacular.utils import extend_schema
from rest_framework import status

from explorer.serializers.requests.snapshot import *
from explorer.serializers.responses.base import StandardOperationResponseSerializer
from shared.services.snapshots_service import SnapshotsService

class SnapshotViewSet(ViewSet):

    @extend_schema(
        request=SaveSnapshotRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(detail=False, methods=["post"], url_path="", url_name="save_snapshot")
    def save_snapshot(self, request):
        serializer = SaveSnapshotRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = SnapshotsService().create_snapshot(
            user_id=validated["user_id"],
            operation_chain=None,  # Injected in real context
            title=validated["title"],
            description=validated.get("description")
        )
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetSnapshotRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(detail=True, methods=["get"], url_path="", url_name="get_snapshot")
    def get_snapshot(self, request, pk=None):
        serializer = GetSnapshotRequestSerializer(data=request.query_params)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = SnapshotsService().get_snapshot(user_id=validated["user_id"], snapshot_id=pk)
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=GetAllSnapshotsRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(detail=False, methods=["get"], url_path="", url_name="get_all_snapshots")
    def get_all_snapshots(self, request):
        serializer = GetAllSnapshotsRequestSerializer(data=request.query_params)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = SnapshotsService().get_all_snapshots(user_id=validated["user_id"])
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=UpdateSnapshotRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(detail=True, methods=["put"], url_path="", url_name="update_snapshot")
    def update_snapshot(self, request, pk=None):
        serializer = UpdateSnapshotRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = SnapshotsService().update_snapshot(
            user_id=validated["user_id"],
            snapshot_id=pk,
            title=validated.get("title"),
            description=validated.get("description")
        )
        return Response({"data": result_payload, "meta": {}})

    @extend_schema(
        request=DeleteSnapshotRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(detail=True, methods=["delete"], url_path="", url_name="delete_snapshot")
    def delete_snapshot(self, request, pk=None):
        serializer = DeleteSnapshotRequestSerializer(data=request.query_params)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data
        result_payload = SnapshotsService().delete_snapshot(user_id=validated["user_id"], snapshot_id=pk)
        return Response({"data": result_payload, "meta": {}})
