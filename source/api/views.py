from rest_framework.decorators import api_view
from rest_framework.response import Response
from api.tepapa import search_tepapa, random_obj, test

@api_view(['GET'])
def hello(request):
    return Response({"message": "Hello from Django!"})

@api_view(['GET'])
def random_view(request):
    try:
        results = random_obj()
        return Response(results)
    except Exception as e:
        print(f"Te Papa API error: {e}")
        return Response({'error': 'Failed to fetch from Te Papa API'}, status=500)    

@api_view(['GET'])
def test_view(request):
    try:
        results = test()
        return Response(results)
    except Exception as e:
        print(f"Te Papa API error: {e}")
        return Response({'error': 'Failed to fetch from Te Papa API'}, status=500)    

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
