import argparse
import multiprocessing
import pandas as pd
from tqdm import tqdm
import tweetnlp as tnlp

# Load metadata extraction functions
from get_profile_metadata import get_profile_metadata
from get_tweet_metadata import get_tweet_metadata

# GLOBAL VARIABLE shared in each process (but not across them)
tnlp_models = None

# INITIALIZER to load models ONCE per process
def init_models():
    global tnlp_models
    model_names = ['topic_classification', 'sentiment', 'irony',
                   'offensive', 'emotion', 'hate', 'ner']
    tnlp_models = [tnlp.load_model(name) for name in model_names]

# MAIN BATCH FUNCTION
def process_batch(batch_args):
    batch_df, function_type, file_path, verbose = batch_args

    row_start = batch_df.index[0]
    row_end = batch_df.index[-1]

    try:
        if verbose:
            print(f"[INFO] Processing rows {row_start} to {row_end}...")

        function_map = {
            "profile": get_profile_metadata,
            "tweet": get_tweet_metadata
        }

        function = function_map[function_type]

        var_dict = batch_df.apply(function, axis=1, args=(tnlp_models,))
        processed_batch = pd.DataFrame(var_dict.tolist())
        processed_batch.to_csv(file_path, index=False)

    except Exception as e:
        print(f"[ERROR] Failed on rows {row_start} to {row_end}: {e}")
        with open("processing_errors.log", "a") as f:
            f.write(f"{row_start}-{row_end}, {type(e).__name__}: {str(e)}\n")

# MAIN ENTRY POINT
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Batch processor for TweetNLP-based feature extraction.")
    parser.add_argument('--input', type=str, required=True, help='Path to input .pkl file')
    parser.add_argument('--type', type=str, required=True, choices=["profile", "tweet"], help='Data type')
    parser.add_argument('--file_path', type=str, required=True, help='Output file path prefix (no extension)')
    parser.add_argument('--batch_size', type=int, default=10, help='Rows per batch')
    parser.add_argument('--start_row', type=int, default=0, help='Row to start from')
    parser.add_argument('--verbose', action='store_true', help='Enable verbose output')
    parser.add_argument('--nproc', type=int, default=None, help='Number of processes to use')

    args = parser.parse_args()

    df = pd.read_pickle(args.input)
    start = args.start_row
    end = len(df)
    batch_size = args.batch_size

    batches = []
    for batch_start in range(start, end, batch_size):
        batch_df = df.iloc[batch_start:batch_start + batch_size].copy()
        file_path = f"{args.file_path}_rows_{batch_start}_to_{batch_start + len(batch_df) - 1}.csv"
        batches.append((batch_df, args.type, file_path, args.verbose))

    num_processes = args.nproc or max(1, multiprocessing.cpu_count() - 1)
    if args.verbose:
        print(f"[INFO] Using {num_processes} processes to handle {len(batches)} batches...")

    with multiprocessing.Pool(processes=num_processes, initializer=init_models) as pool:
        list(tqdm(pool.imap_unordered(process_batch, batches), total=len(batches), desc="Processing Batches"))