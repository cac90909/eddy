from django.http import JsonResponse
from django.views import View
from explorer.services.unique_filter_values_service import UniqueFilterValuesService
from shared.logger import debug_request

class UniqueFilterValuesView(View):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.service = UniqueFilterValuesService()

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
            debug_request(request)
            user_id = int(request.GET.get("user_id"))

            if not all([user_id]):
                return JsonResponse({"error": "Missing required parameters"}, status=400)

            unique_filter_values = self.service.get_unique_filter_values(user_id)
            return JsonResponse({ "data": unique_filter_values}, status=200)
        except Exception as e:
            return JsonResponse({"error": str(e)}, status=500)
