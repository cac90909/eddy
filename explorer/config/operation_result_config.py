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
        "attach_operations_history": True,
        "attach_snapshots_list": True,
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
        "attach_operations_history": True,
        "attach_snapshots_list": True,
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
        "attach_operations_history": True,
        "attach_snapshots_list": True,
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
        "attach_operations_history": True,
        "attach_snapshots_list": True,
        "serialization" : lambda result_data : result_data
    },
    "snapshot": {
        "data_type_verification": lambda result_data : operation_result_util.is_model_instance(result_data=result_data, model_class=Snapshots),
        "cache_policy": lambda op: False,
        "data_overview_fields" : {},
        "operations_history": None,
        "attach_operations_history": False,
        "attach_snapshots_list": False,
        "serialization" : lambda result_data : SnapshotSerializer(instance=result_data).data
    },
    "snapshot_list": {
        "data_type_verification": lambda result_data : operation_result_util.is_queryset_of_model_instances(result_data=result_data, model_class=Snapshots),
        "cache_policy": lambda op: False,
        "data_overview_fields" : {
            "num_snapshots" : lambda user_id, result_data : len(result_data),
        },
        "attach_operations_history": False,
        "attach_snapshots_list": False,
        "serialization" : lambda result_data : [SnapshotSerializer(instance=snapshot).data for snapshot in result_data]

    },
    "operations_list": {
        "data_type_verification": lambda result_data : operation_result_util.is_list_of_dicts(result_data=result_data),
        "cache_policy": lambda op: False,
               "data_overview_fields" : 
            {
                "operations_on_chain" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="operations_on_chain"),
            },
        "attach_operations_history": False,
        "attach_snapshots_list": False,
        "serialization" : lambda result_data : result_data
    },
    "results_list": {
        "data_type_verification": lambda result_data : operation_result_util.is_list_of_querysets_of_model_instances(result_data=result_data, model_class=Universal),
        "cache_policy": lambda op: False,
                "data_overview_fields" : 
            {
                "operations_on_chain" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="operations_on_chain"),
            },
        "attach_operations_history": False,
        "attach_snapshots_list": False,
        "serialization" : lambda result_data : operation_result_util.serialize_results_list(results_list=result_data)
    },
    "operation_chain": {
        "data_type_verification": lambda result_data : operation_result_util.is_list_of_operation_instances(result_data=result_data),
        "cache_policy": lambda op: False,
        "data_overview_fields" : 
            {
                "operations_on_chain" : lambda user_id, result_data : ExplorerCacheService().get_operation_chain_metadata_key(user_id=user_id, metadata_key="operations_on_chain"),
            },
        "attach_operations_history": False,
        "attach_snapshots_list": False,
        "serialization" : lambda result_data : operation_result_util.serialize_operation_chain(operation_chain=result_data)
    },
    "error": {
        "data_type_verification": lambda result_data : result_data is None or isinstance(result_data, str),
        "cache_policy": lambda op: False,
        "data_overview_fields" : {},
        "attach_operations_history": True,
        "attach_snapshots_list": True,
        "serialization" : lambda result_data : result_data
    },
    "status": {
        "data_type_verification": lambda result_data : result_data is None or isinstance(result_data, str), #Eventually this wont work cause (in the example of delete, our rep returns # of deleted records which needs interpreation)
        "cache_policy": lambda op: False,
        "data_overview_fields" : {},
       "attach_operations_history": True,
        "attach_snapshots_list": True,
        "serialization" : lambda result_data : result_data
    },
    "config": {
        "data_type_verification": lambda result_data : result_data is not None,
        "cache_policy": lambda op: False,
        "data_overview_fields" : {},
        "attach_operations_history": False,
        "attach_snapshots_list": False,
        "serialization" : lambda result_data : result_data
    }
}


#Convert config into gql queries
#Each operation is in the same query class and each gets its own resolver
#So i dont have to explicitly replicate patteerns (like invoking the same validation check or cache invocation) I think each resolver should invoke handle operation and then just
#code in parameters that would be specifiy to the current operation (resolver)
#Then once we get our result, we can either continue with the handle operatio nflow of data, or we can return the result to the resolver, validate there, and then return either a graphql object
#if we error, or if it passses validation, then put it in a response builder (which would just be the second half of handle operation) and then implement flags that are customized on a per operation basis

#In terms of informing the frontend for how to use the graphql, we would 1) send the config still. It contains similar information, and we could define it as its own graphql object or just a query
#(whicheever makes the process easier) and then it would also contain the graphql calls for explorer navigation
#For typical operation result responses (of type raw, enriched, list, metric), I think we should probably still return a data, meta structured response, but now we can explicitly define the subfields so that
#React will know to expect them. Wecould handle these calls on a more use case basis (seperating dcalls that would updat the display and calls for operation history etc) but then this means we need to make seequentials calls
#each time which comes with the issue of complexity as well as either sending the frontend more instructions for how to do that, or hardcoding, and timing responses (which arent hard hurdles tbh, but just 
#trying to keep things simple as they can)
#ON THE OTHER HAND, it really only is like a couple more calls: apply operation call, operation history call, data overview call, ( i think thats actually itr lol)

#Okay, slightly tweaking the approach. I still think we need a centralized config file for a per operation basis, because we could contain all infor pertaining to a single operation in the resolver and it would execute as intended
#But, if we wanted to send the user the config info containing the navigation instrustrctions and those graphql calls, that wouldnt be able to go in the resolve. We would have to hardcode it in a seperate config graphql
#object, so to prevent that, everything goes inside our config and then our graphql just reads from it. Which also probably means we wont have a resolver for every single operation

#HONESTLY< i dont even know which i prefer or which would be better

#I think i am leaning towards a single config file, but ima sit on it for a sec