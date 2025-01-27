from django.http import JsonResponse
from django.views import View
from explorer.services.init_user_service import InitUserService
from explorer.views.data_operation_view import DataOperationView
from shared.logger import debug_request


class InitUserView(View):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.service = InitUserService()

    def get(self, request):
        """
        Handles GET requests to initialize the user's session.

        Args:
            request: The HTTP request object containing the user_id.

        Returns:
            JsonResponse: A JSON response containing all the user's data.
        """
        debug_request(request)
        user_id = request.GET.get("user_id")

        if not user_id:
            return JsonResponse({"error": "User ID is required."}, status=400)

        try:
            # Initialize user data and fetch it
            user_data = self.service.initialize_user_data(user_id)
            # Serialize data to JSON format
            serialized_data = list(user_data.values())
            return JsonResponse({ "message": "User data initialized.", "row_count": len(user_data), "data": serialized_data}, status=200)
        except Exception as e:
            return JsonResponse({"error": str(e)}, status=500)
