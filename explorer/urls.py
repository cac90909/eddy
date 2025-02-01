from django.urls import path
from explorer.views.cache_view import CacheView

urlpatterns = [
    # path('init/', InitUserView.as_view(), name='explorer-init'),
    # path('filter/', FilterView.as_view(), name='explorer-filter'),
    # path('filter/values', UniqueFilterValuesView.as_view(), name='explorer-filter-values')
    path('cache', CacheView.as_view(), name='explorer-cache')
]
