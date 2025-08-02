from rest_framework.decorators import api_view
from rest_framework.response import Response
from rest_framework import status
from api.tepapa import search_tepapa

@api_view(['GET'])
def hello(request):
    return Response({"message": "Hello from Django!"})

@api_view(['POST'])
def search_view(request):
    query = request.data.get('query')
    if not query:
        return Response({'error': 'Query is required'}, status=status.HTTP_400_BAD_REQUEST)

    try:
        results = search_tepapa(query)
        return Response(results)
    except Exception as e:
        print(f"Te Papa API error: {e}")
        return Response({'error': 'Failed to fetch from Te Papa API'}, status=500)


@api_view(['POST'])
def submit_text_view(request):
    user_text = request.data.get('text')
    if not user_text:
        return Response({'error': 'No text provided'}, status=status.HTTP_400_BAD_REQUEST)

    # Save to database
    submission = UserSubmission.objects.create(text=user_text)

    return Response({'message': 'Text received and saved', 'id': submission.id})

from django.shortcuts import render
from .models import UserSubmission

def submissions_page(request):
    all_submissions = UserSubmission.objects.order_by('-submitted_at')
    return render(request, 'submissions.html', {'submissions': all_submissions})