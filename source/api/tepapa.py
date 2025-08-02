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
        "size": 20  ,
        "query": f"{text}"
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
    max_id = 10280035
    while True:
        try:
            selected = random.randint(0, max_id)
            url = f'https://data.tepapa.govt.nz/collection/object/{selected}'
            headers = {
                'Content-Type': 'application/json',
                'x-api-key': f'{api_key}'
            }
            response = requests.get(url, headers=headers).json()
            if response['type'] == "Object":
                return response
        except:
            continue

def get_starting_hop():
    choices = [51952, 61379, 925000, 2090487, 1246459]
    return (random.choice(choices), "Object")

# Hop: Either Object or Category
# We store (Id, type)
# Depending on type, we decide how to get to the next hop
# Object -> Check fields "Made of, depicts, intended for, ... etc." for cats.
# None means no hop found
# Category -> Get broader/narrower category, or a child object.

def hopOnce(hop):
    (id, content_type) = hop
    match content_type:
        case "Object":
            return hopFromObj(id)
        case "Category":
            return hopFromCollection(id)

def hopFromObj(oid):
    url = f'https://data.tepapa.govt.nz/collection/object/{oid}'
    headers = {
        'Content-Type': 'application/json',
        'x-api-key': f'{api_key}'
    }
    response = requests.get(url, headers=headers)
    obj = response.json()
    fields_to_try = ["isTypeOf", "productionUsedTechnique", "unknownAssociation", "isAbout", "isMadeOf", "depicts", "influencedBy", "intendedFor", "refersTo"]
    random.shuffle(fields_to_try)
    for field in fields_to_try:
        try:
            filt = list(filter(lambda inner: inner["type"] == "Category", obj[field]))
            inner = random.choice(filt)
            print("Hopping to ", inner["title"], "   Id: ", inner['id'])
            return (inner["id"], "Category")
        except (KeyError, IndexError) as e:
            continue
    print("uh oh obj->something")
    return None

def hopFromCollection(cid):
    return random.choice([hopFromCollectionToObj,hopFromCollectionToCollection])(cid)

def hopFromCollectionToObj(cid):
    url = f'https://data.tepapa.govt.nz/collection/category/{cid}/related'
    headers = {
        'Content-Type': 'application/json',
        'x-api-key': f'{api_key}'
    }
    response = requests.get(url, headers=headers)
    resp = response.json()
    obj = random.choice(resp["results"][:12])
    print("Hopping to ", obj["title"])
    return (obj["id"], "Object")

def hopFromCollectionToCollection(cid):
    url = f'https://data.tepapa.govt.nz/collection/category/{cid}'
    headers = {
        'Content-Type': 'application/json',
        'x-api-key': f'{api_key}'
    }
    response = requests.get(url, headers=headers)
    resp = response.json()
    fields_to_try = ["relatedTerms", "broaderTerms", "narrowerTerms"]
    random.shuffle(fields_to_try)
    for field in fields_to_try:
        try:
            # print("Looking for: ", field)
            # print("Result: ", resp.get(field))
            filt = list(filter(lambda inner: inner["type"] == "Category", resp[field]))
            inner = random.choice(filt)
            print("Hopping to ", inner["title"])
            return (inner["id"], "Category")
        except (KeyError, IndexError) as e:
            continue
    print("Uh oh col->col")
    return None

def manyHops(pathTracker,hop):
    hopsLeft = random.randint(1,3)
    while True:
        try:
            accumulator = hop
            while not(hopsLeft <= 0 and accumulator[1] == "Object"):
                hopsLeft -= 1
                pathTracker.append(accumulator)
                accumulator = hopOnce(accumulator) 
            pathTracker.append(accumulator)
            break
        except TypeError:
            print("Aborted path")
            continue

# Return a list of hops. The first hop is the starting point.
# Guaranteed to be non-empty and the first and last hops are objects.
def drunkards_walk():
    accumulator = get_starting_hop()
    path = []
    manyHops(path,accumulator)
    return path
