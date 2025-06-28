from rest_framework.viewsets import ViewSet
from rest_framework import status, serializers
from rest_framework.response import Response
from rest_framework.request import Request
from typing import Type, Any, Dict, Mapping, cast
from core.domain.enums.base_response import StandardResponse

class BaseViewSet(ViewSet):
    """
    A shared base for all ExplorerOperationViewSets.
    Provides:
      • parse_request()
      • envelope_response()
    """

    def validate_request_body(self, 
                   request: Request, 
                   SerializerClass: Type[serializers.Serializer]
        ) -> Mapping[str, Any]:
        """Validate JSON payloads (POST/PUT)."""
        ser = SerializerClass(data=request.data)
        ser.is_valid(raise_exception=True)
        return cast(Mapping[str, Any], ser.validated_data)

    def parse_params(
            self, 
            request: Request, 
            SerializerClass: Type[serializers.Serializer]
        ) -> Mapping[str, Any]:
        """Validate query-param GET requests (handles lists via ListField)."""
        raw = request.query_params
        data: dict[str, Any] = {}
        temp_ser = SerializerClass()  # to inspect which fields are lists
        for name, field in temp_ser.fields.items():
            if isinstance(field, serializers.ListField):
                data[name] = raw.getlist(name)
            else:
                data[name] = raw.get(name)
        ser = SerializerClass(data=data)
        ser.is_valid(raise_exception=True)
        return cast(Mapping[str, Any], ser.validated_data)

    def envelope_response(
        self,
        payload: Any,
        ResponseSerializer: Type[serializers.Serializer],
        *,
        meta: Dict[str, Any] | None = None,
        status_code: int = status.HTTP_200_OK,
    ) -> Response:
        env = {StandardResponse.DATA: payload, 
               StandardResponse.META: meta or {}}
        ser = ResponseSerializer(env)
        return Response(ser.data, status=status_code)