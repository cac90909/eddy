from django.urls import path
from explorer.views.filter_view import FilterView
from explorer.views.init_user_view import InitUserView
from explorer.views.unique_filter_values_view import UniqueFilterValuesView

urlpatterns = [
    path('init/', InitUserView.as_view(), name='explorer-init'),
    path('filter/', FilterView.as_view(), name='explorer-filter'),
    path('filter/values', UniqueFilterValuesView.as_view(), name='explorer-filter-values')
]
