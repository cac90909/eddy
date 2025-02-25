from shared.serializers import UniversalSerializer, SnapshotSerializer, FlexibleDictSerializer

def serialize_result_data(result_data, result_data_type):
    if result_data_type not in result_data_type_serializer_mapping:
        raise ValueError("Invalid response data type: {}".format(result_data_type))
    return result_data_type_serializer_mapping[result_data_type](result_data)

result_data_type_serializer_mapping = {
        "raw" : lambda universal_data : UniversalSerializer(instance=list(universal_data), many=True).data,
        "enriched" : lambda universal_data : FlexibleDictSerializer(instance=universal_data, many=True).data,
        "metric" : lambda universal_data : universal_data,
        "list" : lambda universal_data : universal_data,
        "status" : lambda status_data : status_data,
        "snapshot" : lambda snapshot_data : SnapshotSerializer(instance=snapshot_data).data,
        "snapshot_list" : lambda snapshot_list : [SnapshotSerializer(instance=snapshot).data for snapshot in snapshot_list],
        "operation_chain" : lambda operation_chain : serialize_operation_chain(operation_chain),
        "operations_list" : lambda operations_list : operations_list,
        "results_list" : lambda results_list : serialize_results_list(results_list),
    }

def serialize_operation_chain(operation_chain):
    for operation in operation_chain:
        operation_result = operation["result"]
        operation_result_data, operation_result_data_type = operation_result["data"], operation_result["data_type"]
        serialized_result_data = result_data_type_serializer_mapping[operation_result_data_type](operation_result_data)
        operation["result"]["data"] = serialized_result_data
    return operation_chain

def serialize_results_list(results_list):
    for result in results_list:
        result_data, result_data_type = result["data"], result["data_type"]
        serialized_result_data = result_data_type_serializer_mapping[result_data_type](result_data)
        result["data"] = serialized_result_data
    return results_list