from django.urls import path
from explorer.views.explorer_view import ExplorerView
from explorer.views.operation_view import ExplorerOperationView

urlpatterns = [
    path('', ExplorerView.as_view(), name='explorer-base'),
    path('operation', ExplorerOperationView.as_view(), name='operation')
]
