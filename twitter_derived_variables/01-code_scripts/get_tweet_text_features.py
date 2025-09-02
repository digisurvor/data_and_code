import re
import textstat as ts
import emoji

from text_analysis_functions import strip_text, count_capitals, grammar_score
from url_bias_checker import url_bias_check

def get_tweet_text_features(text,tnlp_models,expanded_urls=None):
    
    # Ensure the text input is a string
    if not isinstance(text, str):
        raise ValueError("Input must be a string.")
        
    # Initialize variable dictionary
    vars_dict = {}

    # Strip out unwanted characters (e.g., emojis, URLs, etc.)
    text_stripped = strip_text(text)

    # Text width
    vars_dict["post_length"] = len(text)  # Length of post (unstripped)

    # Count special characters
    vars_dict["exclamation_count"] = text_stripped.count("!")  # Exclamation marks
    vars_dict["question_mark_count"] = text_stripped.count("?")  # Question marks
    vars_dict["hashtag_count"] = len(re.findall(r"#\w+", text)) # Hashtags
    vars_dict["emoji_count"] = emoji.emoji_count(text)  # Emojis
    vars_dict["mentions_count"] = len(re.findall(r"@\w+", text)) # @Mentions
    urls = re.findall(r"http[s]?://\S+", text) # URLs
    vars_dict["url_count"] = len(urls)  # URL count

    # Match URLs against Media Bias/Fact Check database (additional parameter)
    if expanded_urls != None:
        url_mbfc_score = url_bias_check(expanded_urls)
        vars_dict["url_mbfc_matches"] = len(url_mbfc_score["bias"])  # URL MBFC matches
        vars_dict["url_mbfc_bias"] = url_mbfc_score["bias"]  # URL bias score(s)
        vars_dict["url_mbfc_credibility"] = url_mbfc_score["credibility"]  # URL credibility score(s)
    else:
        vars_dict["url_mbfc_matches"] = 0
        vars_dict["url_mbfc_bias"] = []
        vars_dict["url_mbfc_credibility"] = []

    if text_stripped: # Only calculate if processed text is not empty
        
        # Count language features
        vars_dict["character_count"] = ts.char_count(text_stripped)  # Characters
        vars_dict["capt_character_count"] = count_capitals(text_stripped)  # Capitalized characters
        vars_dict["capt_character_prop"] = round(vars_dict["capt_character_count"] /
                                            vars_dict["character_count"], 3) # Capitalized characters (%)
        vars_dict["letter_count"] = ts.letter_count(text_stripped)  # Letters
        vars_dict["word_count"] = ts.lexicon_count(text_stripped)  # Words
        vars_dict["unique_word_count"] = len(set(text_stripped.lower().split()))  # Unique words
        vars_dict["sentence_count"] = ts.sentence_count(text_stripped)  # Sentences
        vars_dict["syllable_count"] = ts.syllable_count(text_stripped)  # Syllables
        vars_dict["monosyllable_count"] = ts.monosyllabcount(text_stripped)  # Monosyllables
        vars_dict["polysyllable_count"] = ts.polysyllabcount(text_stripped)  # Polysyllables

        # Calculate language grammaticality, reading ease, and reading grade
        vars_dict["grammar_score"] = grammar_score(text_stripped)  # Grammaticality score
        vars_dict["reading_ease"] = ts.flesch_reading_ease(text_stripped)  # Flesch reading ease
        vars_dict["non_eng_reading_ease"] = ts.mcalpine_eflaw(text_stripped)  # McAlpine reading ease (Non-English Readers)
        vars_dict["reading_grade"] = ts.text_standard(text_stripped, float_output=True)  # Language reading grade (Consensus)

        # Estimate additional features using TweetNLP
        topic_model = tnlp_models[0]
        sentiment_model = tnlp_models[1]
        irony_model = tnlp_models[2]
        offensive_model = tnlp_models[3]
        emotion_model = tnlp_models[4]
        hate_model = tnlp_models[5]
        ner_model = tnlp_models[6]

        topics = topic_model.topic(text_stripped, return_probability=True)  # Topic classification
        vars_dict["topics"] = topics['label']  # Topic labels
        vars_dict["topic_prob"] = {label: topics["probability"][label] for label in topics["label"]}  # Probabilities for detected topics

        sentiment = sentiment_model.sentiment(text_stripped, return_probability=True)  # Sentiment analysis
        vars_dict["sentiment"] = sentiment['label']  # Sentiment label
        vars_dict["sentiment_prob"] = sentiment["probability"][sentiment["label"]]  # Probability for the detected sentiment

        irony = irony_model.irony(text_stripped, return_probability=True)  # Irony detection
        vars_dict["irony"] = irony['label']  # Irony label
        vars_dict["irony_prob"] = irony["probability"][irony["label"]]  # Probability for the detected irony

        offensive = offensive_model.offensive(text_stripped, return_probability=True)  # Offensive language detection
        vars_dict["offensive"] = offensive['label']  # Offensive label
        vars_dict["offensive_prob"] = offensive["probability"][offensive["label"]]  # Probability for the detected offensive label

        emotion = emotion_model.emotion(text_stripped, return_probability=True)  # Emotion detection
        vars_dict["emotion"] = emotion['label']  # Emotion label
        vars_dict["emotion_prob"] = emotion["probability"][emotion["label"]]  # Probability for the detected emotion

        hate = hate_model.hate(text_stripped, return_probability=True)  # Hate detection
        vars_dict["hate"] = hate['label']  # Hate label
        vars_dict["hate_prob"] = hate["probability"][hate["label"]]  # Probability for the detected hate

        named_entities = ner_model.ner(text_stripped, return_probability=True)  # Named entity recognition
        vars_dict["entity_count"] = len(named_entities)  # Count of named entities
        vars_dict["entity_types"] = [entity['type'] for entity in named_entities]  # Entity types
        vars_dict["entity_prob"] = [entity['probability'] for entity in named_entities]  # Probabilities for detected entity types

    # Return the dictionary of features
    return vars_dict