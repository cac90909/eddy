from django.urls import path
from explorer.views.filter_view import FilterView
from explorer.views.init_user_view import InitUserView

urlpatterns = [
    path('init/', InitUserView.as_view(), name='explorer-init'),
    path('filter/', FilterView.as_view(), name='explorer-filter'),
]
