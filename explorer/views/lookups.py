from rest_framework.viewsets import ViewSet
from rest_framework.decorators import action
from rest_framework.response import Response
from drf_spectacular.utils import extend_schema
from explorer.serializers.requests.lookups import (ArgumentOptionsRequestSerializer, 
                                                   OperationInfoRequestSerializer)
from explorer.serializers.responses.base import StandardOperationResponseSerializer
from explorer.services.lookup_service import LookupService

class LookupViewSet(ViewSet):
    
    @extend_schema(
        request=ArgumentOptionsRequestSerializer,
        responses=StandardOperationResponseSerializer
    )
    @action(
        detail=False,
        methods=['GET'],
        url_path='options',
        url_name='lookup_options'
    )
    def operation_arg_options(self, request):
        # Validate input
        serializer = ArgumentOptionsRequestSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        op = serializer.validated_data['operation_name']
        arg = serializer.validated_data['argument']
        prev_args = serializer.validated_data.get('previous_arguments', {})

        # Business logic: fetch options for the given operation and argument
       
        choices = LookupService.get_arg_options_for_operation(op, arg, prev_args)

        # Wrap in the standard response format
        return Response({
            "data": choices,
            "meta": {}
        })
    
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
    


# In your urls.py, wire it up:
# from rest_framework.routers import DefaultRouter
# from explorer.views.lookup_viewset import LookupViewSet
#
# router = DefaultRouter()
# router.register(r'explorer/lookups', LookupViewSet, basename='lookup')
# urlpatterns = router.urls
