from django.urls import path
from .views import hello
from api.views import search_view

urlpatterns = [
    path("hello/", hello),
    path('search/', search_view),

]
