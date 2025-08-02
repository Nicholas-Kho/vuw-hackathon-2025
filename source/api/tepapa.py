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
    return response.json()

# type hop : (id, content_type) where content_type is one of: object, agent, taxon, collection

# Hop -> Hop
# getNextHop
# 
# Hop -> Maybe Object
# tryHop2Obj

def objToHop(objId):
    url = f'https://data.tepapa.govt.nz/collection/object/{objId}'
    headers = {
        'Content-Type': 'application/json',
        'x-api-key': f'{api_key}'
    }
    response = requests.get(url, headers=headers)
    obj = response.json()
    fieldsToSearch = ["isTypeOf", "isMadeOf", "depicts", "production"]
    random.shuffle(fieldsToSearch)
    while (fieldsToSearch):
        head = fieldsToSearch.pop()
        try:
            res = random.choice(obj[head])
            return (res["id"], res["type"])
        except KeyError as e:
            continue
    return []

def catToHop(cat):
    url = f'https://data.tepapa.govt.nz/collection/category/{cat["id"]}/related'
    headers = {
        'Content-Type': 'application/json',
        'x-api-key': f'{api_key}'
    }
    response = requests.get(url, headers=headers)
    f = random.choice(response.json()["result"])
    return (f["id"], "object")

def hopToHop(hop):
    (id, hopType) = hop
    match hopType:
        case "object":
            return objToHop(id)
        case "Category":
            return catToHop(id)
        case _:
            print("It's fucked: ", hopType)
            return None


def test():
    print(hopToHop(hopToHop(hopToHop(objToHop(51952)))))
    return {}

