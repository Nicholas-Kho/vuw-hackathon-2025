from django.urls import path
from .views import hello
from api.views import search_view, random_view, test_view

urlpatterns = [
    path("hello/", hello),
    path('search/', search_view),
    path('random/', random_view),
    path('test/', test_view)
]
