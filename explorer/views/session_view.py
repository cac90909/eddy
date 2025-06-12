"""
This viewset defines all supported session-related actions within the Explorer context.

Endpoints follow command-style naming under a RESTful structure:
- `POST /explorer/session/start/`    → Start a new explorer session
- `POST /explorer/session/reset/`    → Reset explorer session
- `POST /explorer/session/undo/`     → Undo the most recent operation
- `POST /explorer/session/load/`     → Load a snapshot into the session
- `POST /explorer/session/end/`      → End the explorer session

Each method uses a dedicated request serializer and a standard response format. All documentation
is powered by drf-spectacular.
"""

from rest_framework.viewsets import ViewSet
from rest_framework.decorators import action
from rest_framework.response import Response
from drf_spectacular.utils import extend_schema

from explorer.serializers.session_requests import *
from explorer.serializers.responses import StandardOperationResponseSerializer
from explorer.services.explorer_cache_service import ExplorerCacheService
from shared.services.snapshots_service import SnapshotsService
from shared.services.universal_raw_service import UniversalRawService
from explorer.util import operation_util

class ExplorerSessionViewSet(ViewSet):

    @extend_schema(
        request=StartSessionRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(detail=False, methods=["post"], url_path="start", url_name="start_session")
    def start_session(self, request):
        serializer = StartSessionRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        user_id = serializer.validated_data["user_id"]

        ExplorerCacheService().create_empty_operation_chain_cache(user_id=user_id)
        config = operation_util.assemble_operations_config(user_id=user_id)
        return Response({"data": config, "meta": {}})

    @extend_schema(
        request=ResetSessionRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(detail=False, methods=["post"], url_path="reset", url_name="reset_session")
    def reset_session(self, request):
        serializer = ResetSessionRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        user_id = serializer.validated_data["user_id"]

        ExplorerCacheService().create_empty_operation_chain_cache(user_id=user_id)
        raw_data = UniversalRawService().get_full_data(user_id=user_id)
        return Response({"data": raw_data, "meta": {}})

    @extend_schema(
        request=UndoOperationRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(detail=False, methods=["post"], url_path="undo", url_name="undo_operation")
    def undo_operation(self, request):
        serializer = UndoOperationRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        user_id = serializer.validated_data["user_id"]

        chain = ExplorerCacheService().get_operation_chain(user_id=user_id)
        if len(chain) <= 1:
            return Response({"data": None, "meta": {"error": "Cannot undo operation: no operations to undo"}}, status=400)

        ExplorerCacheService().delete_most_recent_operation_from_chain(user_id=user_id)
        result = ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id)
        return Response({"data": result, "meta": {}})

    @extend_schema(
        request=LoadSnapshotIntoSessionRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(detail=False, methods=["post"], url_path="load", url_name="load_snapshot")
    def load_snapshot(self, request):
        serializer = LoadSnapshotIntoSessionRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        validated = serializer.validated_data

        ExplorerCacheService().empty_operation_chain(user_id=validated["user_id"])
        operation_chain = SnapshotsService().get_snapshot_operation_chain(
            user_id=validated["user_id"], snapshot_id=validated["snapshot_id"]
        )
        data = operation_util.assemble_dataset_list_from_operation_chain(
            user_id=validated["user_id"], operation_chain=operation_chain
        )
        return Response({"data": data, "meta": {}})

    @extend_schema(
        request=EndSessionRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(detail=False, methods=["post"], url_path="end", url_name="end_session")
    def end_session(self, request):
        serializer = EndSessionRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        user_id = serializer.validated_data["user_id"]

        ExplorerCacheService().delete_operation_chain_cache(user_id=user_id)
        return Response({"data": "Session ended", "meta": {}})
