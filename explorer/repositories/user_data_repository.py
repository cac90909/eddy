from django.db.models import Q
from explorer.models import UserData
from shared.logger import debug_print

class UserDataRepository:
    """
    Repository for interacting with the UserData model.
    """

    @staticmethod
    def get_user_data(user_id):
        """
        Retrieve all data for a specific user.
        """
        return UserData.objects.filter(user_id=user_id)

    @staticmethod
    def filter_data(user_data_queryset, column_name, filter_value, filter_type):
        """
        Filter data based on column name, filter value, and filter type.
        Args:
            user_data_queryset: QuerySet to filter.
            column_name (str): Column to filter on.
            filter_value: Value to filter by.
            filter_type (str): Type of filter ('=', '<', '>', 'contains').

        Returns:
            QuerySet: Filtered QuerySet.
        """
        filter_map = {
            '=': {f"{column_name}": filter_value},
            '<': {f"{column_name}__lt": filter_value},
            '>': {f"{column_name}__gt": filter_value},
            'contains': {f"{column_name}__icontains": filter_value},
        }

        if filter_type not in filter_map:
            raise ValueError(f"Unsupported filter type: {filter_type}")

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
