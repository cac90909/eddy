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
from drf_spectacular.utils import extend_schema, OpenApiResponse
from rest_framework import status


from explorer.session.serializers import *
from explorer.serializers.base import StandardOperationResponseSerializer
from backend.apps.explorer.services.cache import ExplorerCacheService
from backend.apps.core.service.snapshot import SnapshotsService
from shared.services.universal_raw_service import UniversalRawService
from explorer.util import operation_util
from explorer.session.service import ExplorerSessionService

class SessionViewSet(ViewSet):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.session_serivce = ExplorerSessionService()

    @extend_schema(
        request=StartSessionRequestSerializer,
        responses={201: OpenApiResponse(description="Session started successfully")}
    )
    @action(detail=False, methods=["post"], url_path="start", url_name="start_session")
    def start_session(self, request):
        in_ser = StartSessionRequestSerializer(data=request.data)
        validated = in_ser.is_valid(raise_exception=True)
        self.session_serivce.start_session(user_id=validated['user_id'])
        return Response(status=status.HTTP_201_CREATED)

    @extend_schema(
        request=ResetSessionRequestSerializer,
        responses={200: ResetSessionResponseSerializer}
    )
    @action(detail=False, methods=["post"], url_path="reset", url_name="reset_session")
    def reset_session(self, request):
        in_ser = ResetSessionRequestSerializer(data=request.data)
        validated = in_ser.is_valid(raise_exception=True)
        full_data = self.session_serivce.reset_session(user_id=validated['user_id'])
        out_ser = ResetSessionResponseSerializer(full_data, many=True)
        return Response(
            {"data": out_ser.data, "meta": {}},
            status=status.HTTP_200_OK
        )
    
    @extend_schema(
        request=EndSessionRequestSerializer,
        responses={204: OpenApiResponse(description="Session ended successfully")}
    )
    @action(detail=False, methods=["post"], url_path="end", url_name="end_session")
    def end_session(self, request):
        in_ser = EndSessionRequestSerializer(data=request.data)
        validated = in_ser.is_valid(raise_exception=True)
        self.session_serivce.end_session(user_id=validated['user_id'])
        return Response(status=status.HTTP_204_NO_CONTENT)

    @extend_schema(
        request=UndoOperationRequestSerializer,
        responses=UndoOperationResponseSerializer
    )
    @action(detail=False, methods=["post"], url_path="undo", url_name="undo_operation")
    def undo_operation(self, request):
        in_ser = UndoOperationRequestSerializer(data=request.data)
        validated = in_ser.is_valid(raise_exception=True)
        previous_data = self.session_serivce.undo_operation(user_id=validated['user_id'])
        out_ser = UndoOperationResponseSerializer(previous_data, many=True)
        return Response(
            {"data": out_ser.data, "meta": {}},
            status=status.HTTP_200_OK
        )

    @extend_schema(
        request=LoadSnapshotIntoSessionRequestSerializer,
        responses=LoadSnapshotResponseSerializer
    )
    @action(detail=False, methods=["post"], url_path="load", url_name="load_snapshot")
    def load_snapshot(self, request):
        in_ser = LoadSnapshotIntoSessionRequestSerializer(data=request.data)
        validated = in_ser.is_valid(raise_exception=True)
        loaded_data = self.session_serivce.load_snapshot(user_id=validated['user_id'])
        out_ser = UndoOperationResponseSerializer(loaded_data, many=True)
        return Response(
            {"data": out_ser.data, "meta": {}},
            status=status.HTTP_200_OK
        )