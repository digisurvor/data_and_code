import tldextract as tlde
import pandas as pd

# Load the MBFC dataset
mbfc = pd.read_csv('media-bias-scrubbed-results.csv')

# Convert credibility ratings to numerical values
mbfc['factual_reporting_rating'] = mbfc['factual_reporting_rating'].map({'MIXED': -1, 'HIGH': 0, "VERY HIGH": 1})

# Extract domain level from URL
mbfc['domain'] = mbfc['url'].apply(lambda x: tlde.extract(x).domain)

# Caculate mean values where domains have been duplicated 
mbfc['bias_mean'] = mbfc.groupby('domain')['bias_rating'].transform('mean')
mbfc['factual_reporting_mean'] = mbfc.groupby('domain')['factual_reporting_rating'].transform('mean')

# Drop duplicates
mbfc = mbfc.drop_duplicates(subset='domain', keep='first')

# Function
def url_bias_check(urls):

    # Extract domain level from URL
    domains = [tlde.extract(url).domain for url in urls]

    # Find matches in the MBFC dataframe
    matches = mbfc[mbfc['domain'].isin(domains)]  # Filter for matches

    # Extract bias rating and factual credibility
    bias = matches['bias_mean'].tolist()
    credibility = matches['factual_reporting_mean'].tolist()

    # Create a dictionary to store the results
    results = {
        'bias': bias,
        'credibility': credibility
    }

    return results  # Return matched rows