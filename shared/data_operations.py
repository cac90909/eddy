
class DataOperationsService:

    def __init__(self, repository, adapter):
        super().__init__()
        self.repository = repository
        self.adapter = adapter

    def get_user_data(self, user_id):
        user_data = self.repository.get_data_subset(column="User ID", value=user_id)
        return user_data

    def filter_data(self, data, column_name, filter_value, filter_type):
        filtered_data = self.adapter.filter_by_value(df=data, column_name=column_name, filter_type=filter_type, filter_value=filter_value)
        return filtered_data

    def sort_data(self, data, column_names, ascending=True):
        sorted_data = self.adapter.sort_on_columns(df=data, column_names=column_names, ascending=ascending)
        return sorted_data

    #NOTE - this only traverses things below the current item, would need a separate function or additional functionality
    #if you wanted to return items both above and below the current item
    def traverse_children(self, data, start_value):
        children_data = self.adapter.traverse_data(df=data, start_value=start_value, column_name="Parents")
        return children_data

    def traverse_siblings(self, data, start_value):
        sibling_data = self.adapter.traverse_siblings(df=data, start_value=start_value, column_name="Siblings")
        return sibling_data


    def union_data(self, data1, data2):
        union_data = self.adapter.union_data(df1=data1, df2=data2)
        return union_data


