from rest_framework.decorators import api_view
from rest_framework.response import Response
from rest_framework import status
from api.tepapa import search_tepapa
from django.http import JsonResponse

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


from rest_framework.decorators import api_view
from rest_framework.response import Response
from rest_framework import status
from .models import UserSubmission  # Make sure to import your model

@api_view(['POST'])
def submit_text_view(request):
    user_text = request.data.get('text')
    counter = request.data.get('counter', 0)
    search_query = request.data.get('searchQuery', '')
    won = request.data.get('won', False)

    if not user_text:
        return Response({'error': 'No text provided'}, status=status.HTTP_400_BAD_REQUEST)

    # Update existing submission or create a new one if not exists
    submission, created = UserSubmission.objects.update_or_create(
        text=user_text,
        defaults={
            'counter': counter,
            'word': search_query,
            'won': won
        }
    )

    if created:
        message = "Submission created"
    else:
        message = "Submission updated"

    return Response({'message': message, 'id': submission.id})


from django.shortcuts import render
from .models import UserSubmission
import json
from django.http import JsonResponse

def submissions_page(request):
    if request.method == "POST":
        # Parse JSON body
        data = json.loads(request.body)

        text = data.get('text')
        counter = data.get('counter', 0)
        word = data.get('searchQuery')  # match frontend key
        won = data.get('won', False)

        if not text:
            return JsonResponse({"error": "Missing text (player name)"}, status=400)

        # Update existing submission by text or create new one
        obj, created = UserSubmission.objects.update_or_create(
            text=text,
            defaults={
                'counter': counter,
                'word': word,
                'won': won
            }
        )
        return JsonResponse({"message": "Success", "created": created})

    else:
        # GET request: render the leaderboard page
        submissions = UserSubmission.objects.all().order_by('-submitted_at')
        return render(request, 'submissions.html', {'submissions': submissions})