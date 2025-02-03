from django.db.models import Q
from shared.models import Universal
from shared.logger import debug_print
from django.db.models import Func, F, Value
from django.db import connection

class UniversalRepository:
    """
    Repository for interacting with the UserData model.
    """

    @staticmethod
    def get_user_data(user_id):
        """
        Retrieve all data for a specific user.
        """
        return Universal.objects.filter(user_id=user_id)

    """Right now, this handles getting unique values for list columns as well as json column (keys + values).
    #Eager loading on json values (could wait until user specifies a key to filter on and then get its possible values)
    #I think down the line, frontend may be delegated to handle this since they already have the data on hand
    #And making a query to get the unique values for a dataset already possessed may be excessive
    """
    @staticmethod
    def get_unique_filter_options(user_data_queryset):
        """
        Retrieve unique values for specific columns containing arrays and unique keys and values from a JSON column.
        Columns: functionalities, subject_matters, general_categories, tags, and fields (JSON).

        Args:
            user_data_queryset: QuerySet to process.

        Returns:
            Dict: A dictionary containing unique values for each column and unique keys and values for the JSON column.
        """
        columns_to_extract = [
            "functionalities",
            "subject_matters",
            "general_categories",
            "tags",
        ]

        unique_values = {}

        # Process array-based columns
        for column in columns_to_extract:
            column_values = (
                user_data_queryset.annotate(expanded_values=Func(F(column), function="UNNEST"))
                .values_list("expanded_values", flat=True)
                .distinct()
            )
            unique_values[column] = set(column_values)

        # Process JSON column (fields)
        json_column = "fields"

        # Step 1: Extract all unique keys from the JSON column
        json_keys = (
            user_data_queryset.annotate(keys=Func(Value(json_column), function="jsonb_object_keys"))
            .values_list("keys", flat=True)
            .distinct()
        )
        unique_values[json_column] = {"keys": set(json_keys)}

        # Step 2: Extract unique values for each JSON key
        for key in json_keys:
            key_values = (
                user_data_queryset.filter(**{f"{json_column}__has_key": key})
                .annotate(value=F(f"{json_column}__{key}"))
                .values_list("value", flat=True)
                .distinct()
            )
            unique_values[json_column][key] = set(key_values)

        return unique_values

    """Raw SQL version of get_unique_filter_options (makes a single query using UNION instead of 4 queries)"""
    # @staticmethod
    # def get_unique_filter_options(user_id):
    #     """
    #     Retrieve unique values for functionalities, subject_matters, general_categories, and tags
    #     in a single query using PostgreSQL's UNNEST and UNION.
    #
    #     Args:
    #         user_id (int): The ID of the user whose data is being processed.
    #
    #     Returns:
    #         Dict: A dictionary containing unique values for each column.
    #     """
    #     query = """
    #            SELECT DISTINCT value, 'functionalities' AS source
    #            FROM UNNEST(ARRAY(
    #                SELECT functionalities
    #                FROM user_data
    #                WHERE user_id = %s
    #            )) AS value
    #            UNION
    #            SELECT DISTINCT value, 'subject_matters' AS source
    #            FROM UNNEST(ARRAY(
    #                SELECT subject_matters
    #                FROM user_data
    #                WHERE user_id = %s
    #            )) AS value
    #            UNION
    #            SELECT DISTINCT value, 'general_categories' AS source
    #            FROM UNNEST(ARRAY(
    #                SELECT general_categories
    #                FROM user_data
    #                WHERE user_id = %s
    #            )) AS value
    #            UNION
    #            SELECT DISTINCT value, 'tags' AS source
    #            FROM UNNEST(ARRAY(
    #                SELECT tags
    #                FROM user_data
    #                WHERE user_id = %s
    #            )) AS value
    #        """
    #
    #     with connection.cursor() as cursor:
    #         cursor.execute(query, [user_id, user_id, user_id, user_id])
    #         rows = cursor.fetchall()
    #
    #     # Parse the rows into a dictionary
    #     result = {"functionalities": set(), "subject_matters": set(), "general_categories": set(), "tags": set()}
    #     for value, source in rows:
    #         result[source].add(value)
    #
    #     return result

    #Equipped to work on JSON/non-JSON Data

    @staticmethod
    def filter_data(user_data_queryset, column_name, filter_value, filter_type):
        """
        Filter data based on column name, filter value, and filter type.
        Args:
            user_data_queryset: QuerySet to filter.
            column_name (str): Column to filter on.
            filter_value: Value to filter by.
            filter_type (str): Type of filter ('=', '<', '>', 'contains', '!=' or 'not_equals').

        Returns:
            QuerySet: Filtered QuerySet.
        """
        filter_map = {
            '=': {f"{column_name}": filter_value},
            '!=': {f"{column_name}__ne": filter_value},
            '<': {f"{column_name}__lt": filter_value},
            '>': {f"{column_name}__gt": filter_value},
            'string_contains': {f"{column_name}__icontains": filter_value},
            'array_contains': {f"{column_name}__contains": filter_value},
            'array_not_contains': {f"{column_name}__contains": filter_value}
        }

        #If wanting option for JSON filtering where you return valid rows + rows not containing passed key
        """# Build the filter query
        query = Q(**filter_map[filter_type])

        if not strict_filter:
            # Include rows where the key does not exist
            query |= ~Q(**{f"{column_name}__isnull": False})"""

        if filter_type not in filter_map:
            raise ValueError(f"Unsupported filter type: {filter_type}")

        # Handle inverse filtering
        if filter_type in ['!=', 'array_not_contains']:
            return user_data_queryset.exclude(**filter_map[filter_type])

        return user_data_queryset.filter(**filter_map[filter_type])

    @staticmethod
    def sort_data(user_data_queryset, column_name, ascending=True):
        """
        Sort data based on a column.
        Args:
            user_data_queryset: QuerySet to sort.
            column_name (str): Column to sort by.
            ascending (bool): Sort order.

        Returns:
            QuerySet: Sorted QuerySet.
        """
        sort_order = column_name if ascending else f"-{column_name}"
        return user_data_queryset.order_by(sort_order)

    @staticmethod
    def group_data(user_data_queryset, column_name):
        """
        Group data by a specific column.
        Args:
            user_data_queryset: QuerySet to group.
            column_name (str): Column to group by.

        Returns:
            QuerySet: Grouped QuerySet.
        """
        # Example implementation for grouping with annotations
        from django.db.models import Count
        return user_data_queryset.values(column_name).annotate(count=Count(column_name))

    @staticmethod
    def traverse_data(user_data_queryset, start_id, column_name="parent_ids"):
        """
        Traverse data starting at a specific node.
        Args:
            user_data_queryset: QuerySet containing the data.
            start_id: Starting node ID.
            column_name (str): Column containing relationships (e.g., parent IDs).

        Returns:
            List: IDs of all related nodes.
        """
        visited = set()
        to_visit = [start_id]

        while to_visit:
            current_id = to_visit.pop()
            if current_id not in visited:
                visited.add(current_id)
                # Fetch related IDs (assuming they are stored as lists in the DB)
                related_ids = user_data_queryset.filter(id=current_id).values_list(column_name, flat=True).first()
                if related_ids:
                    to_visit.extend(related_ids)

        return list(visited)


