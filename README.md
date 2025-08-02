## Backend
located in `source/backend` (cooked)

## Frontend
located in `source/frontend/src`
`app.js` is the file that runs when you `npm run start`



## Dependencies:
virtual environment:
`python3 -m venv venv`
`source venv/bin/activate`

### backend:
`pip3 install requests`
`pip3 install dotenv` or `python-dotenv`

### django:
`pip3 install djangorestframework` 
`pip install django-cors-headers`

### .env:
in `source/api`, create a `.env` file containing the following contents:
```
API_KEY = GYxOLdjYl7z6deFeoKFQYevF8L7eVSVCEkr1h5LA
API_TOKEN = GYxOLdjYl7z6deFeoKFQYevF8L7eVSVCEkr1h5LA```

### frontend: nodejs
`install npm` (ask chatgpt)

### homepage - ignore for now
`npm install gsap`
`npm i three @react-three/fiber`
`npm install framer-motion`

## Deployment
- Two terminals are needed.
- in `source/`, run `python manage.py runserver`. This loads the backend
- in `source/frontend`, run `npm run start`, loading the frontend