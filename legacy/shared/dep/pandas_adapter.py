import pandas as pd


class PandasAdapter:
    @staticmethod
    def generate_data(raw_data):
        df = pd.DataFrame(data=raw_data)
        return df

    @staticmethod
    def operator_lambda_helper(data_object, operator, value):
        """
        Helper function to map an operator to a lambda and apply it to the data.

        Args:
            data_object: A Pandas Series or value to apply the operator to.
            operator (str): The operator ('>', '=', '<').
            value: The value to compare against.

        Returns:
            Boolean Series: Result of applying the operator to the data_object.
        """
        operator_mapping = {
            '>': lambda x: x > value,
            '=': lambda x: x == value,
            '<': lambda x: x < value,
            'substring': lambda x: x.str.contains(value, case=False, na=False)
        }
        if operator not in operator_mapping:
            raise ValueError(f"Invalid operator type: {operator}. Use one of {list(operator_mapping.keys())}.")
        return operator_mapping[operator](data_object)

    @staticmethod
    def filter_by_value(df, column_name, filter_type, filter_value):
        """
        Filter rows in the DataFrame based on a column, an operator, and a value.

        Args:
            column_name (str): The column to filter on.
            filter_type (str): The operator ('>', '=', '<').
            filter_value: The value to compare against.

        Returns:
            pd.DataFrame: Filtered DataFrame.
        """
        # Apply the helper to filter the DataFrame
        condition = PandasAdapter.operator_lambda_helper(data_object=df[column_name], operator=filter_type, value=filter_value)
        return df[condition]

    @staticmethod
    def filter_by_value_list(df, column_name, filter_value_list):
        filtered_data = df[df[column_name].isin(filter_value_list)]
        return filtered_data

    @staticmethod
    def filter_json_column_value(df, column_name, json_field_name, filter_type, filter_value):
        """
        Filter rows in the DataFrame where a JSON field in a column matches a condition.

        Args:
            column_name (str): The column containing JSON data.
            json_field_name (str): The field within the JSON to filter on.
            filter_type (str): The operator ('>', '=', '<').
            filter_value: The value to compare against.

        Returns:
            pd.DataFrame: Filtered DataFrame.
        """
        condition = df[column_name].apply(
            lambda x: PandasAdapter.operator_lambda_helper(x[json_field_name], filter_type, filter_value)
        )
        return df[condition]

    @staticmethod
    def sort_on_columns(df, column_names, ascending=True):
        """
        Sort the DataFrame by one or more columns.

        Args:
            column_name_list (str or list): The column(s) to sort by.
            ascending (bool or list): Sort order(s) corresponding to column(s). Defaults to True.

        Returns:
            pd.DataFrame: Sorted DataFrame.
        """
        # Ensure column_name_list is a list for consistency
        if isinstance(column_names, str):
            column_name_list = [column_names]
        # Ensure ascending is a list if sorting by multiple columns
        if isinstance(ascending, bool):
            ascending = [ascending] * len(column_names)
        # Validate length compatibility
        if len(column_names) != len(ascending):
            raise ValueError("Length of ascending must match length of column_name_list.")
        # Perform sorting
        return df.sort_values(by=column_names, ascending=ascending)

    @staticmethod
    def join_dfs(df1, df2, column_name):
        return pd.merge(df1, df2, on=column_name)

    @staticmethod
    def union_data(df1, df2):
        return pd.concat([df1, df2]).drop_duplicates().reset_index(drop=True)


    @staticmethod
    def traverse_data(df, start_value, column_name):
        """
        Perform the traversal logic on the DataFrame with cycle detection.

        Args:
            df (pd.DataFrame): The DataFrame containing the project hierarchy.
            starting_value (int): The starting Project ID.

        Returns:
            pd.DataFrame: DataFrame containing the full hierarchy.

        Raises:
            ValueError: If a cycle is detected in the hierarchy.
        """
        results = {start_value}
        to_check = [start_value]
        visited = set()  # To track visited nodes for cycle detection

        while to_check:
            current_id = to_check.pop()

            # Check for cycles (would be an error in parent-child relationships but to be expected in sibling relationships)
            if current_id in visited:
                continue

            visited.add(current_id)  # Mark the current node as visited

            # Find direct children of the current project
            is_related = df[column_name].apply(lambda x: str(current_id) in str(x).split(";"))
            related_nodes = df[is_related]
            #nodes = df[df[column_name] == current_id]["ID"].tolist()

            # Add these children to the results and mark them for further checking
            new_nodes = set(related_nodes) - results
            results.update(new_nodes)
            to_check.extend(new_nodes)

        return df[df["ID"].isin(results)]








