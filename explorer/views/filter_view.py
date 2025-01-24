from django.http import JsonResponse
from django.views import View
from explorer.services.filter_service import FilterService

class FilterView(View):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.service = FilterService()

    def get(self, request):
        """
        Handles GET requests to filter data.

        Query Parameters:
            user_id (int): The ID of the user.
            column_name (str): The column to filter on.
            filter_value: The value to filter by.
            filter_type (str): The filter type ('=', '<', '>').

        Returns:
            JsonResponse: Filtered data or an error message.
        """
        try:
            user_id = int(request.GET.get("user_id"))
            column_name = request.GET.get("column_name")
            filter_value = request.GET.get("filter_value")
            filter_type = request.GET.get("filter_type")

            if not all([user_id, column_name, filter_value, filter_type]):
                return JsonResponse({"error": "Missing required parameters"}, status=400)

            filtered_data = self.service.filter_data(user_id, column_name, filter_value, filter_type)
            return JsonResponse({"row_count": len(filtered_data), "data": filtered_data}, status=200)
        except Exception as e:
            return JsonResponse({"error": str(e)}, status=500)
