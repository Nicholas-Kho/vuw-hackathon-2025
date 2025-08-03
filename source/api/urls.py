from django.urls import path
from .views import hello
from api.views import search_view, random_view, drunkards_walk_view, get_hops_view

urlpatterns = [
    path("hello/", hello),
    path('search/', search_view),
    path('random/', random_view),
    path('drunkards_walk/', drunkards_walk_view),
    path('get_hops/', get_hops_view)
]
