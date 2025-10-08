import pandas as pd
import glob
import os

# === CONFIGURATION ===
# Folder containing your CSV files
input_folder = "C:/Users/Gebruiker/Documents/Masters CCS/Year1/Block12/Methods in CCS/algebra/methodsccs-group3/methodsccs-group3/"

# Columns to remove
columns_to_drop = ["Retrieval", "Transformation", "block", "correct_response", "cur_problem", "datetime", "experiment_file", "response", "fbstate", "noprogress", "num_correct", "trial_index_inlevel", "trials_since_levelup"]

# Output file name
output_file = "fulldata_combined.csv"

# === MAIN SCRIPT ===
def combine_csv_files(input_folder, columns_to_drop, output_file):
    # Match all CSV files that start with "subject-" and end with .csv
    csv_files = glob.glob(os.path.join(input_folder, "subject-*.csv"))

    all_dataframes = []

    for file in csv_files:
        print(f"Processing: {file}")
        df = pd.read_csv(file)

        # Drop columns that exist in this file
        df = df.drop(columns=[col for col in columns_to_drop if col in df.columns], errors='ignore')

        #Drop practice rows
        
        if "practice" in df.columns:
            df = df[df["practice"].str.lower() != "yes"]

        all_dataframes.append(df)

    # Combine all data into one DataFrame
    combined_df = pd.concat(all_dataframes, ignore_index=True)

    # Save combined data to CSV
    combined_df.to_csv(output_file, index=False)
    print(f"\n✅ Combined file saved as: {output_file}")

# Run the script
if __name__ == "__main__":
    combine_csv_files(input_folder, columns_to_drop, output_file)
