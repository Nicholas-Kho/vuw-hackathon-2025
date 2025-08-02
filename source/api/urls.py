from django.urls import path
from .views import hello, search_view, submit_text_view, submissions_page
from api.views import search_view

urlpatterns = [
    path("hello/", hello),
    path('search/', search_view),
    path('submit-text/', submit_text_view),
    path('submissions/', submissions_page, name='submissions'),

]
