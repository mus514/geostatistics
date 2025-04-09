# Activate environment
#cd 1.BirdNET
#conda activate birdNET
#python

# Libraries
import multiprocessing as mp
from pathlib import Path
import pandas as pd
import BirdNET_Parallel_functions_Blob
import shutil

# Set paths
BirdNETscript = Path("P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/2-Analyses/BirdNET-Analyzer/analyze.py")
output_path = Path("C:/Users/Jurie/Desktop/New_data/Audiomoths_2023")
meta_data_path = Path("P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/1-Data/AudioMoth_recordings/Audiomoths_2023/metaData_Blob_TEST.csv")
blob_lookup = Path("C:/Users/Jurie/Downloads/lookup_table_Audio_multi.csv")
batchsize = 1 #1
steps = 3 # Number of sites to run in parallel. Increasing this to much might cause memory issues

# Define main function
def main():
    # Create output folder
    output = output_path / f"{output_path.name}_Processed"
    output.mkdir(exist_ok=True)
    # Read in meta data
    metaData = pd.read_csv(meta_data_path)
    # Read in blob lookup
    blob_data = pd.read_csv(blob_lookup)
    # Convert time column data type and calculate number of week in the year
    metaData['time'] = pd.to_datetime(metaData['time'])
    metaData['date'] = metaData['time'].dt.date
    metaData['week'] = metaData['time'].dt.isocalendar().week
    # Loop over sets within the metaData
    for i in range(0,len(metaData),steps):
        # Convert rows to dictionaries
        rows = metaData.iloc[i:i+steps]
        rows = rows.to_dict('records')
        # Create a pool of worker processes
        with mp.Pool() as pool:
            pool.starmap(BirdNET_Parallel_functions_Blob.parallel_process, [(row, blob_data, output, batchsize, BirdNETscript) for row in rows])
    # Delete downloaded data
    temp_dir = output.parent / "Data"
    shutil.rmtree(temp_dir)

# Run in parallel
if __name__ == "__main__":
    main()