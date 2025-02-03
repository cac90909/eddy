from django.urls import path
from explorer.views.explorer_view import ExplorerView
from explorer.views.cache_view import CacheView
from explorer.views.snapshots_view import SnapshotsView

urlpatterns = [
    path('', ExplorerView.as_view(), name='explorer-base'),
    path('cache', CacheView.as_view(), name='explorer-cache'),
    path('snapshots', SnapshotsView.as_view(), name='explorer-snapshots')
]
