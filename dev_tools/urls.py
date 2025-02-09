from django.urls import path
from dev_tools.views.cache_dev_tool_view import CacheDevToolView

urlpatterns = [
    path('cache', CacheDevToolView.as_view(), name='cache-dev-tool'),
]
