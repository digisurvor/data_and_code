import time
import random
from geopy.exc import GeocoderTimedOut, GeocoderServiceError

def get_geocode(app, query, retries=3):
    for attempt in range(retries):
        try:
            return app.geocode(query, addressdetails=True, timeout=10)
        except (GeocoderTimedOut, GeocoderServiceError) as e:
            wait = 2 ** attempt + random.random()  # exponential backoff with jitter
            print(f"Geocoding error: {e}. Retrying in {wait:.2f} seconds...")
            time.sleep(wait)
    return None  # after max retries, return None