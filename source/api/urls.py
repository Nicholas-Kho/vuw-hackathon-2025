from django.urls import path
from .views import hello, search_view, submit_text_view, submissions_page
from api.views import search_view, random_view, drunkards_walk_view, get_hops_view

urlpatterns = [
    path("hello/", hello),
    path('search/', search_view),
    path('submit-text/', submit_text_view),
    path('submissions/', submissions_page, name='submissions'),
    path('random/', random_view),
    path('drunkards_walk/', drunkards_walk_view),
    path('get_hops/', get_hops_view)
]
