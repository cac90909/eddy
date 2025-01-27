from django.http import JsonResponse
from django.views import View
from shared.logger import debug_print


class DataOperationView(View):
    operation_name = None  # Set this in subclasses
    operation_request = None

    #NOTE: dispatch is almost like middleware - it is called before any get, post, etc method with a request. So if a view
    #inherits from data operation view, dispatch is always called first. If you wanted something more across the board,
    #instead of having their behavior for only objects inherinting from data operation view, you would define your own
    #custom middleware
    def dispatch(self, request, *args, **kwargs):
        # Announce the operation
        if self.operation_name:
            debug_print("Operation on: ", self.operation_request)  # Replace with logging if needed
        else:
            debug_print("A data operation view was called.")

        # Proceed with the normal lifecycle
        return super().dispatch(request, *args, **kwargs)
