from rest_framework.viewsets import ViewSet
from rest_framework.decorators import action
from drf_spectacular.utils import extend_schema

from core.views.base import BaseViewSet
from explorer.serializers.operation_executor import REQUEST_SERIALIZERS
from explorer.serializers.operation_executor import RESPONSE_SERIALIZERS
from core.registry.operation_specs import OPERATION_SPECS
from core.domain.enums.operation import OperationName
from core.domain.operation_spec import OperationSpec
from explorer.services.operation_executor import ExplorerOperationExecutorService

class BuilderViewSet(BaseViewSet):
    """Dynamically generated endpoints for every OperationName."""
    lookup_field  = "op_name"


def make_action(op_spec: OperationSpec):
    # capture the two serializers and the doc pieces at factory time
    req_ser = REQUEST_SERIALIZERS[op_spec.name]
    resp_ser = RESPONSE_SERIALIZERS[op_spec.name]
    desc     = op_spec.description
    op_id    = op_spec.name.value
    exec_svc = ExplorerOperationExecutorService()  # or however you wire this up

    @extend_schema(
        request   = req_ser,
        responses = {200: resp_ser},
        description = desc,
        operation_id = op_id,
    )
    @action(detail=False, methods=['post'], url_path=op_spec.name.value)
    def _fn(self, request):
        # call the BaseViewSet helpers off the class directly
        validated = BaseViewSet.validate_request_body(self, request, req_ser)
        result, meta = exec_svc.handle_operation(
            request.user.id, op_spec.name, **validated
        )
        return BaseViewSet.envelope_response(self, result, resp_ser, meta=meta)

    # give it a unique Python name so DRF can wire it up
    _fn.__name__ = f"op_{op_spec.name.value}"
    return _fn


# at import time, attach one method per OpSpec
for spec in OPERATION_SPECS.values():
    setattr(
        BuilderViewSet,
        f"op_{spec.name.value}",
        make_action(spec)
    )
    

