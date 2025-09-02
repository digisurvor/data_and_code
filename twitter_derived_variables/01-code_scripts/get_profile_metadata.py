import re
import textstat as ts
import emoji
import pandas as pd
import time
from langdetect import detect

from get_geocode import get_geocode
from geopy.geocoders import Nominatim
app = Nominatim(user_agent="location_lookup") # Instantiate a new Nominatim client

from text_analysis_functions import strip_text, count_capitals
from real_name_checker import similarity_score, get_salutation, check_names
from get_tweet_text_features import get_tweet_text_features

#### ---- FUNCTION: GET PROFILE METADATA ---- ###

def get_profile_metadata(profile_data, tnlp_models):

    # Create an empty dictionary to be populated
    meta_dict = {}
    
    # Psuedoanonymised Linkage ID 
    meta_dict["ID"] = profile_data["ID"]

    ## - 01: METADATA - ##
    
    # Created at date
    meta_dict["created_date"] = pd.to_datetime(profile_data["created_at"]).date().strftime("%Y-%m-%d")

    # Public metrics
    meta_dict["followers_count"] = profile_data["followers_count"] # followers count
    meta_dict["following_count"] = profile_data["following_count"] # following count
    meta_dict["tweet_count"] = profile_data["tweet_count"] # tweet count
    meta_dict["listed_count"] = profile_data["listed_count"] # listed count

    ## - 02: DISPLAY NAME - ##
    display_name = profile_data["display_name"]

    # Strip out unwanted characters (e.g., emojis, URLs, etc.)
    name_stripped = strip_text(display_name)

    # Text width
    meta_dict["DN_length"] = len(display_name)  # Length of name (unstripped)

    # Count special characters
    meta_dict["DN_exclamation_count"] = name_stripped.count("!")  # Exclamation marks
    meta_dict["DN_question_mark_count"] = name_stripped.count("?")  # Question marks
    meta_dict["DN_hashtag_count"] = len(re.findall(r"#\w+", display_name)) # Hashtags
    meta_dict["DN_emoji_count"] = emoji.emoji_count(display_name)  # Emojis

    if name_stripped: # Only calculate if processed text is not empty
            # Count text features
            meta_dict["DN_character_count"] = ts.char_count(name_stripped)  # Characters
            meta_dict["DN_capt_character_count"] = count_capitals(name_stripped)  # Capitalized characters
            meta_dict["DN_capt_character_prop"] = round(meta_dict["DN_capt_character_count"] /
                                                        meta_dict["DN_character_count"], 3) # Capitalized characters (%)
            meta_dict["DN_letter_count"] = ts.letter_count(name_stripped)  # Letters
            meta_dict["DN_word_count"] = ts.lexicon_count(name_stripped)  # Words
            meta_dict["DN_unique_word_count"] = len(set(name_stripped.lower().split()))  # Unique words

            # Real name checker
            meta_dict["DN_handle_similarity"] = similarity_score(display_name, profile_data["screen_name"]) # Similarity to user handle
            meta_dict["DN_listed_salutation"] = bool(get_salutation(display_name)) # Listed salutation (T/F)
            name_checks = check_names(display_name) # Check if listed names are real
            meta_dict["DN_first_name"] = name_checks["first_name"] # First name
            meta_dict["DN_last_name"] = name_checks["last_name"] # Last name

    ## - 03: LOCATION - ##
    location = profile_data["location"]

    # Is it populated?
    meta_dict["LC_field_populated"] = bool(location)

    if meta_dict["LC_field_populated"]:

            # Split the location field (seperated by commas or slashes)
            tokens = re.split(r'[,/]', location)
            location_split = [token.strip() for token in tokens if token.strip()]

            # Number of levels listed 
            meta_dict["LC_level_count"] = len(location_split)

            # Identify location levels 
            location_levels = {
                "level_identified": [],
                "level_type": []
            }

            try:
                for level in location_split:
                    geo_location = get_geocode(app, level)

                    if geo_location:
                        # Append 1 if level is identified
                        location_levels["level_identified"].append(1)

                        # Append the address type
                        location_levels["level_type"].append(geo_location.raw.get("addresstype"))
                    
                    else: 
                        # Append 0 if level is not identified
                        location_levels["level_identified"].append(0)

                    time.sleep(1)  # prevents exceeding rate limit
            
                # Append to metadata dictionary
                meta_dict["LC_levels_identified"] = sum(location_levels["level_identified"])
                meta_dict["LC_level_types"] = location_levels["level_type"]

            except Exception as e:
                meta_dict["LC_levels_identified"] = str(e)

    ## - 04: DESCRIPTION - ##
    description = profile_data["description"]

    # Is it populated?
    meta_dict["DS_field_populated"] = bool(description)

    if meta_dict["DS_field_populated"]:

        try:
            # Predicted language
            meta_dict["DS_language"] = detect(description)
        except Exception as e:
            meta_dict["DS_language"] = str(e)

        # Derive textual features
        description_text_features = get_tweet_text_features(description,tnlp_models,expanded_urls=None)

        # Store derived features in metadata dictionary
        meta_dict.update({f'DS_{key}': value for key, value in description_text_features.items()})

    # Return metadata dictionary
    return meta_dict