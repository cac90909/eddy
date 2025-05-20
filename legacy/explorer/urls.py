from django.urls import path
from explorer.views.explorer_view import ExplorerView

urlpatterns = [
    path('', ExplorerView.as_view(), name='explorer-base'),
]
