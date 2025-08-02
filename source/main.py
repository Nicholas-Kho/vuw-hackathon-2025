import requests
from dotenv import load_dotenv
import os

# Load our API key from the .env
load_dotenv()
api_key = os.getenv("API_KEY")

url = 'https://data.tepapa.govt.nz/collection/search'
headers = {
    'Content-Type': 'application/json',
    'x-api-key': f'{api_key}'  # thanks chatgpt
}
data = {
    "from": 0,
    "size": 10,
    "query": "beige plates",
    "sort": [
        {"field": "title", "order": "DESC"}
    ]
}

response = requests.post(url, headers=headers, json=data)

print("Status code:", response.status_code)
print("Response JSON:", response.json())