from shared.models import Snapshots, Universal
from explorer.util import operation_result_util
from explorer.domain.operation_result import OperationResult
from explorer.services.explorer_cache_service import ExplorerCacheService
from shared.serializers import UniversalSerializer, SnapshotSerializer, FlexibleDictSerializer

OPERATION_RESULT_DEFINITIONS = {
    "raw": {                        
        "data_type_verification": lambda result_data : operation_result_util.is_queryset_of_model_instances(result_data=result_data, model_class=Universal),
        "cache_policy": lambda op: True,
        "data_overview_fields" : 
            {
                "full_data_shape" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="full_data_shape"),
                "current_data_shape" : lambda user_id, result_data : {
                    "num_rows": len(result_data),
                    "num_columns": len(result_data[0]._meta.fields),
                    "num_values": len(result_data) * len(result_data[0]._meta.fields)
                },
                "operations_on_chain" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="operations_on_chain"),     
        },
        "operations_history": lambda user_id : ExplorerCacheService().extract_operation_chain_operations(user_id=user_id),
        "serialization" : lambda result_data : UniversalSerializer(instance=list(result_data), many=True).data
    },
    "enriched": {
        "data_type_verification": lambda result_data : operation_result_util.is_queryset_of_dicts(result_data=result_data),
        "cache_policy": lambda op: True,
        "data_overview_fields" : 
            {
                "full_data_shape" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="full_data_shape"),
                "current_data_shape" : lambda user_id, result_data : {
                    "num_rows": len(result_data),
                    "num_columns": len(result_data[0]),
                    "num_values": len(result_data) * len(result_data[0])
                },
                "operations_on_chain" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="operations_on_chain"),      
        },
        "operations_history_attachment": True,
        "serialization" : lambda result_data : FlexibleDictSerializer(instance=list(result_data), many=True).data,
    },
    "list": {
        "data_type_verification": lambda result_data : isinstance(result_data, set),
        "cache_policy": lambda op: True,
        "data_overview_fields" : 
            {
                "full_data_shape" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="full_data_shape"),
                "current_data_shape" : lambda user_id, result_data : {
                    "num_rows": len(result_data),
                    "num_columns": 1,
                    "num_values": len(result_data)
                },
                "operations_on_chain" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="operations_on_chain"),   
        },
        "operations_history": lambda user_id : ExplorerCacheService().extract_operation_chain_operations(user_id=user_id),
        "serialization" : lambda result_data : result_data
    },
    "metric": {
        "data_type_verification": lambda result_data : isinstance(result_data, int) or isinstance(result_data, float),
        "cache_policy": lambda op: True,
        "data_overview_fields" : 
            {
                "full_data_shape" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="full_data_shape"),
                "current_data_shape" : lambda user_id, result_data : {
                    "num_rows": 1,
                    "num_columns": 1,
                    "num_values":1
                },
                "operations_on_chain" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="operations_on_chain"),   
        },
        "operations_history": lambda user_id : ExplorerCacheService().extract_operation_chain_operations(user_id=user_id),
        "serialization" : lambda result_data : result_data
    },
    "snapshot": {
        "data_type_verification": lambda result_data : operation_result_util.is_model_instance(result_data=result_data, model_class=Snapshots),
        "cache_policy": lambda op: False,
        "data_overview_fields" : {},
        "operations_history": None,
        "serialization" : lambda result_data : SnapshotSerializer(instance=result_data).data
    },
    "snapshot_list": {
        "data_type_verification": lambda result_data : operation_result_util.is_queryset_of_model_instances(result_data=result_data, model_class=Snapshots),
        "cache_policy": lambda op: False,
        "data_overview_fields" : {
            "num_snapshots" : lambda user_id, result_data : len(result_data),
        },
        "operations_history": None,
        "serialization" : lambda result_data : [SnapshotSerializer(instance=snapshot).data for snapshot in result_data]

    },
    "operations_list": {
        "data_type_verification": lambda result_data : operation_result_util.is_list_of_dicts(result_data=result_data),
        "cache_policy": lambda op: False,
               "data_overview_fields" : 
            {
                "operations_on_chain" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="operations_on_chain"),
            },
        "operations_history": None,
        "serialization" : lambda result_data : result_data
    },
    "results_list": {
        "data_type_verification": lambda result_data : operation_result_util.is_list_of_querysets_of_model_instances(result_data=result_data, model_class=Universal),
        "cache_policy": lambda op: False,
                "data_overview_fields" : 
            {
                "operations_on_chain" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="operations_on_chain"),
            },
        "operations_history": None,
        "serialization" : lambda result_data : operation_result_util.serialize_results_list(results_list=result_data)
    },
    "operation_chain": {
        "data_type_verification": lambda result_data : operation_result_util.is_list_of_operation_instances(result_data=result_data),
        "cache_policy": lambda op: False,
        "data_overview_fields" : 
            {
                "operations_on_chain" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="operations_on_chain"),
            },
        "operations_history": None,
        "serialization" : lambda result_data : operation_result_util.serialize_operation_chain(operation_chain=result_data)
    },
    "error": {
        "data_type_verification": lambda result_data : result_data is None or isinstance(result_data, str),
        "cache_policy": lambda op: False,
        "data_overview_fields" : {},
        "operations_history": None,
        "serialization" : lambda result_data : result_data
    },
    "status": {
        "data_type_verification": lambda result_data : result_data is None or isinstance(result_data, str), #Eventually this wont work cause (in the example of delete, our rep returns # of deleted records which needs interpreation)
        "cache_policy": lambda op: False,
        "data_overview_fields" : {},
        "operations_history": None,
        "serialization" : lambda result_data : result_data
    },
}



