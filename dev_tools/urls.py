from django.urls import path
from dev_tools.views.explorer_dev_tool_view import ExplorerDevToolView

urlpatterns = [
    path('explorer', ExplorerDevToolView.as_view(), name='explorer-dev-tool')
]
