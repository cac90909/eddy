from django.urls import path
from rest_framework.routers import DefaultRouter
from backend.apps.explorer.views.lookup import LookupViewSet
from backend.apps.explorer.views.metadata import MetadataViewSet
from backend.apps.explorer.views.operation_builder import OperationBuilderViewSet
from backend.apps.explorer.views.snapshot import SnapshotViewSet
from backend.apps.explorer.views.session import SessionViewSet
from backend.apps.explorer.views._dev import DevViewSet

router = DefaultRouter()
router.register(r'operation', OperationBuilderViewSet, basename='operation')

urlpatterns = [
    #path('', ExplorerView.as_view(), name='explorer-base'),
    path('operation-builder', OperationBuilderViewSet.as_view(), name='operation-builder'),
    path('snapshot', SnapshotViewSet.as_view(), name='snapshot'),
    path('session', SessionViewSet.as_view(), name='session'),
    path('lookup', LookupViewSet.as_view(), name='lookup'),
    path('metadata', MetadataViewSet.as_view(), name='metadata'),
    path('dev', DevViewSet.as_view(), name='dev'),
]
