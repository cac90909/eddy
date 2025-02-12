from django.urls import path
from dev_tools.views.cache_dev_tool_view import CacheDevToolView
from dev_tools.views.explorer_dev_tool_view import ExplorerDevToolView

urlpatterns = [
    path('cache', CacheDevToolView.as_view(), name='cache-dev-tool'),
    path('explorer', ExplorerDevToolView.as_view(), name='explorer-dev-tool')
]
