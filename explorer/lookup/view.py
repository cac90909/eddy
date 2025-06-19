from rest_framework.viewsets import ViewSet
from rest_framework.decorators import action
from rest_framework.response import Response
from drf_spectacular.utils import extend_schema
from explorer.lookup.serializers import (ArgumentOptionsRequestSerializer, 
                                        ArgumentOptionsResponseSerializer,
                                        OperationInfoRequestSerializer)
from explorer.lookup.service import LookupService

class LookupViewSet(ViewSet):

    def __init__(self):
        self.lookup_svc = LookupService()
    
    @extend_schema(
        request=ArgumentOptionsRequestSerializer,
        responses=ArgumentOptionsResponseSerializer
    )
    @action(
        detail=False,
        methods=['GET'],
        url_path='options',
        url_name='lookup_options'
    )
    def operation_arg_options(self, request):
        serializer = ArgumentOptionsRequestSerializer(data=request.query_params)
        serializer.is_valid(raise_exception=True)
        op         = serializer.validated_data['operation_name']
        arg        = serializer.validated_data['argument']
        prev_args  = serializer.validated_data.get('previous_arguments', {})

        # 2) fetch choices
        choices = self.lookup_svc.get_operation_argument_options(
            user_id   = request.user.id,
            op_name   = op,
            arg_name  = arg,
            prev_args = prev_args,
        )

        # 3) wrap in standard envelope
        out_ser = ArgumentOptionsResponseSerializer({"data": choices, "meta": {}})
        return Response(out_ser.data)
    
    @extend_schema(
        request=OperationInfoRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(
        detail=False,
        methods=['GET'],
        url_path='operation_info',
        url_name='operation_info'
    )
    def operation_info(self, request):
        # Validate input
        serializer = OperationInfoRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        op = serializer.validated_data['operation_name']

        # Business logic: fetch options for the given operation and argument
       
        op_info = LookupService.get_operation_info(op)

        # Wrap in the standard response format
        return Response({
            "data": op_info,
            "meta": {}
        })
