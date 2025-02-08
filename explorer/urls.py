from django.urls import path
from explorer.views.explorer_view import ExplorerView
from explorer.views.cache_view import CacheView
from shared.dep.snapshots_view import SnapshotsView

urlpatterns = [
    path('', ExplorerView.as_view(), name='explorer-base'),
]
