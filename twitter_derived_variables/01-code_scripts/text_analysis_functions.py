
# Modules 
import emoji
import re

# Global singleton variable for LanguageTool
_tool_instance = None

def get_tool():
    global _tool_instance
    if _tool_instance is None:
        import language_tool_python  # Import is inside the get_tool function to prevent Java memory overload
        _tool_instance = language_tool_python.LanguageTool('en-US')
    return _tool_instance

# Calculate grammaticality score using LanguageTool
def grammar_score(post_text_stripped):

    # Handle empty text
    if not post_text_stripped.strip():
        return "N/A"
    
    # Initialise language tool
    tool = get_tool()

    # Check grammar errors
    matches = tool.check(post_text_stripped)
    num_errors = len(matches)

    # Count words
    total_words = len(post_text_stripped.split())

    # Calculate grammar score
    score = max(0, 100 * (1 - num_errors / total_words))  # Ensure score is not negative
    return round(score, 2)

# Strip out URLS, emojis, hashtags, @mentions, extra whitespace
def strip_text(text):
    # Remove URLs
    text_without_urls = re.sub(r"http[s]?://\S+", "", text)
    # Remove emojis
    text_without_emojis = emoji.replace_emoji(text_without_urls, replace="")
    # Remove hashtags
    text_without_hashtags = re.sub(r"#\S+", "", text_without_emojis)
    # Remove @mentions
    text_without_mentions = re.sub(r"@\S+", "", text_without_hashtags)
    # Remove extra whitespace
    text_without_whitespace = re.sub(r"\s+", " ", text_without_mentions).strip()
    # Return stripped text
    return text_without_whitespace

# Count the number of capitalised characters
def count_capitals(text):
    capital_letters = [char for char in text if char.isupper()]
    capital_count = len(capital_letters)
    return capital_count