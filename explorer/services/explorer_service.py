class ExplorerService:

    def __init__(self, data_operations_service, cache_service):
        super().__init__()
        self.data_operations_service = data_operations_service
        self.cache_service = cache_service

    def perform_data_operation(self, user_id, data_operation_type, data_operation_params, cache_operation_params):
        operation_mapping = {
            "init_user": self.data_operations_service.get_user_data,
            "filter": self.data_operations_service.filter_data,
            "traverse_siblings": self.data_operations_service.traverse_siblings,
            "traverse_children": self.data_operations_service.traverse_children,
            "sort": self.data_operations_service.sort_data,
            "union": self.data_operations_service.union_data
        }

        operation = operation_mapping[data_operation_type]
        #validated_params = validate_data_operation(data_operation_type, data_operation_parameters)

        if data_operation_type == "init_user":
            result_data = self.data_operations_service.get_user_data(user_id=user_id)
            self.save_data_to_cache(user_id=user_id, data=result_data, cache_type="Auto")
        elif data_operation_type in ["filter", "traverse_siblings", "traverse_children", "sort"]:
            original_data = self.get_data_from_cache(user_id=user_id, **cache_operation_params)
            result_data = operation(data=original_data, **data_operation_params)
            self.cache_service.cache_data(user_id=user_id, data=result_data, **cache_operation_params)
        elif data_operation_type in ["union"]:
            original_data1 = self.get_most_recent_cache_data(user_id=user_id) #Most recent item in cache (current dataset they are looking at)
            original_data2 = self.get_data_from_cache(user_id=user_id, **cache_operation_params)
            result_data = operation(data1=original_data1, data2=original_data2)
        else:
            raise ValueError(f"Unsupported data operation type: {data_operation_type}")

        return result_data


    def get_most_recent_cache_data(self, user_id):
        return self.cache_service.get_most_recent_cache_data(user_id=user_id)

    def get_data_from_cache(self, user_id, cache_type, cache_num):
        cache_data = self.cache_service.get_cache_data(user_id=user_id, cache_type=cache_type, cache_num=cache_num)
        return cache_data

    def save_data_to_cache(self, user_id, data, cache_type):
        self.cache_service.cache_data(user_id=user_id, data=data, cache_type=cache_type)






