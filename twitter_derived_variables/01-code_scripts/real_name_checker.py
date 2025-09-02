from rapidfuzz import fuzz
from nameparser import HumanName

# Load names dataset
from names_dataset import NameDataset
nd = NameDataset()

# Check similarity between name and handle
def similarity_score(name, handle):
    name = name.lower().replace(" ", "")
    handle = handle.lower().replace("@", "")
    score = fuzz.ratio(name, handle)
    return round(score,2) # returns 0 to 100

# Extract any salutation
def get_salutation(name):
    name = HumanName(name)
    title = name.title
    return title

# Check if first and last names exist in the names dataset
def check_names(name):
    name = HumanName(name)
    first_name_check = nd.search(name.first)
    last_name_check = nd.search(name.last)
    if bool(first_name_check["first_name"]):
        first_name_ranks = first_name_check["first_name"]["rank"]
        first_name_commmonality = sum(1 for v in first_name_ranks.values() if v is not None)
    else: 
        first_name_commmonality = 0
    if bool(last_name_check["last_name"]):
        last_name_ranks = last_name_check["last_name"]["rank"]
        last_name_commmonality = sum(1 for v in last_name_ranks.values() if v is not None)
    else: 
        last_name_commmonality = 0
    first_name_real = True if first_name_commmonality >=3 else False
    last_name_real = True if last_name_commmonality >=3 else False
    name_check_dict = {"first_name":  first_name_real, 
                       "last_name": last_name_real}
    return name_check_dict