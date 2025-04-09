# Activate environment
#cd 3.BirdNET
#conda activate birdNET
#python

# Libraries
import multiprocessing as mp
from pathlib import Path
import pandas as pd
import BirdNET_Parallel_functions_P_drive

# Set paths
BirdNETscript = Path("P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/2-Analyses/BirdNET-Analyzer/analyze.py")
data_path = Path("P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/1-Data/AudioMoth_recordings/Audiomoths_2023_test")
meta_data_path = Path("P:/Projets/Actif/2023_ECCC4_Biodiv/3-Analyses/1-Data/AudioMoth_recordings/Audiomoths_2023_test/metaData_TEST.csv")
batchsize = 1 #1
steps = 3 #3

# Define main function
def main():
    # Create output folder
    output = data_path.parent / f"{data_path.name}_Processed"
    output.mkdir(exist_ok=True)
    # Read in meta data
    metaData = pd.read_csv(meta_data_path)
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
            pool.starmap(BirdNET_Parallel_functions_P_drive.parallel_process, [(row, data_path, output, batchsize, BirdNETscript) for row in rows])

# Run in parallel
if __name__ == "__main__":
    main()