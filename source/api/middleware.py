from django.utils.deprecation import MiddlewareMixin

class TrackIPMiddleware(MiddlewareMixin):
    def process_request(self, request):
        ip = get_client_ip(request)
        print(f"Request from IP: {ip}")
        # You could also store IPs in cache/db here

def get_client_ip(request):
    x_forwarded_for = request.META.get('HTTP_X_FORWARDED_FOR')
    if x_forwarded_for:
        ip = x_forwarded_for.split(',')[0]
    else:
        ip = request.META.get('REMOTE_ADDR')
    return ip
