import requests
from dotenv import load_dotenv
import os
import random

# Load our API key from the .env
load_dotenv()
api_key = os.getenv("API_KEY")


def search_tepapa(text):
    url = 'https://data.tepapa.govt.nz/collection/search'
    headers = {
        'Content-Type': 'application/json',
        'x-api-key': f'{api_key}'  # thanks chatgpt
    }
    data = {
        "from": 0,
        "size": 10,
        "query": f"{text}",
        "sort": [
            {"field": "title", "order": "DESC"}
        ]
    }

    response = requests.post(url, headers=headers, json=data)

    print("Status code:", response.status_code)
    print("Response JSON:", response.json())
    return response.json()

# Getting start & end points:
# Land on some initial page
# Parse JSON and find outgoing links
# Pick one at random, add it to a list, repeat previous step

def random_obj():
    # random obj ids
    one_of = [405835, 51952]
    selected = random.choice(one_of)
    url = f'https://data.tepapa.govt.nz/collection/object/{selected}'
    headers = {
        'Content-Type': 'application/json',
        'x-api-key': f'{api_key}'
    }

    response = requests.get(url, headers=headers)

    print("Status code (randomObj):", response.status_code)
    print("Response JSON (randomObj):", response.json())
    return response.json()
