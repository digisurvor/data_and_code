import pandas as pd
import re
from get_tweet_text_features import get_tweet_text_features

#### ---- FUNCTION: GET POST METADATA ---- ###

def get_tweet_metadata(tweet, tnlp_models):

    # Break the function early if row data is missing
    if tweet["created_at_x"] == None:
        return

    # Create an empty dictionary to be populated
    meta_dict = {}

    # Psuedoanonymised Linkage ID 
    meta_dict["ID"] = tweet["ID"]

    try:
        ## - 01: Post Type - ##
        if tweet["referenced_tweet_type"] != None:
            meta_dict["post_type"] = tweet["referenced_tweet_type"]
        else:
            meta_dict["post_type"] = "original"

        ## - 02: Post Created Date - ##

        # Created at date 
        meta_dict["created_date"] = pd.to_datetime(tweet["created_at_x"]).date().strftime("%Y-%m-%d")

        ## - 03: Post Source - ##

        # Append as is to dictionary
        meta_dict["post_source"] = tweet["source_x"]

        ## - 04: Post Language - ##

        # Append as is to dictionary
        meta_dict["post_language"] = tweet["language_x"]

        ## - 05: Engagement Metrics - ## 

        # Retweet Count (as is)
        meta_dict["retweet_count"] = tweet["retweet_count_x"]
        
        # Reply Count (as is)
        meta_dict["reply_count"] = tweet["reply_count_x"]

        # Like Count (as is)
        meta_dict["like_count"] = tweet["like_count_x"]

        # Quote Count (as is)
        meta_dict["quote_count"] = tweet["quote_count_x"]

        # Total engagement
        total_engagement = (tweet["retweet_count_x"] + 
                            tweet["reply_count_x"] +
                            tweet["like_count_x"] +
                            tweet["quote_count_x"])

        meta_dict["total_engagement"] = int(total_engagement)

        # Reply ratio (Replies / Retweets + Likes)
        try:
            reply_ratio = (tweet["reply_count_x"] / (
                tweet["retweet_count_x"] + tweet["like_count_x"]))
        except ZeroDivisionError as e:
            reply_ratio = 0

        meta_dict["reply_ratio"] = int(reply_ratio)

        ## - 06: Media Attachment - ##

        # Contains media attachment (Image/Video/GIF)?
        meta_dict["contains_media"] = bool(tweet["media_keys"])
        
        ## - 07: Geo-Location Tag - ##

        # Contains geo-tag?
        meta_dict["contains_geotag"] = bool(tweet["geo_coordinates"])
        
        ## - 08: Reply Settings - ##

        # Tweet reply settings (as is)
        meta_dict["reply_settings"] = tweet["reply_settings_x"]

        ## - 09: Referenced Tweet Metadata - ##

        if tweet["referenced_tweet_type"] != None:
            try:
                meta_dict["RT_created_date"] = pd.to_datetime(tweet["created_at_y"]).date().strftime("%Y-%m-%d") # created date
                meta_dict["RT_post_source"] = tweet["source_y"] # source
                meta_dict["RT_post_language"] = tweet["language_y"] # language
                meta_dict["RT_retweet_count"] = tweet["retweet_count_y"] # retweet count
                meta_dict["RT_reply_count"] = tweet["reply_count_y"] # reply count
                meta_dict["RT_like_count"] = tweet["like_count_y"] # like count
                meta_dict["RT_quote_count"] = tweet["quote_count_y"] # quote count

                # Total engagement
                RT_total_engagement = (tweet["retweet_count_y"] + 
                                    tweet["reply_count_y"] +
                                    tweet["like_count_y"] +
                                    tweet["quote_count_y"])

                meta_dict["RT_total_engagement"] = int(RT_total_engagement)

                # Reply ratio
                try:
                    RT_reply_ratio = (tweet["reply_count_y"] / (
                        tweet["retweet_count_y"] + tweet["like_count_y"]))
                except ZeroDivisionError:
                    RT_reply_ratio = 0

                meta_dict["RT_reply_ratio"] = int(RT_reply_ratio)

            except AttributeError:
                pass

        ## - 10: Derived Tweet Text Features - ##
        
        # Extract any expanded URLs
        if bool(tweet["entities_urls"]):
            expanded_urls = re.findall(r"'expanded_url': '([^']+)'", tweet["entities_urls"])
        else:
            expanded_urls = None

        # Derive textual features
        tweet_text_features = get_tweet_text_features(tweet["tweet_text"],tnlp_models,expanded_urls=expanded_urls)

        # Store derived features in metadata dictionary
        meta_dict.update({f'TEXT_{key}': value for key, value in tweet_text_features.items()})

    except AttributeError:
        return

    # Return metadata data frame
    return meta_dict