# vuw-hackathon-2025

## Backend**
located in `backend/backend` (cooked)

## Frontend**
located in `backend/frontend/src`



## Dependencies:
### virtual environment:
`python3 -m venv venv`
`source venv/bin/activate`

### backend:
`pip3 install requests`
`pip3 install dotenv` or `python-dotenv`
### django:
`pip3 install djangorestframework` 
`pip install django-cors-headers`

### .env:
in `backend/api`, create a `.env` file containing the following contents:
```
API_KEY = GYxOLdjYl7z6deFeoKFQYevF8L7eVSVCEkr1h5LA
API_TOKEN = GYxOLdjYl7z6deFeoKFQYevF8L7eVSVCEkr1h5LA```

frontend: nodejs
`install npm` (ask chatgpt)

**Deployment**
- Two terminals are needed.
- in `backend/`, run `python manage.py runserver`. This loads the backend
- in `backend/frontend`, run `npm run start`, loading the frontend
