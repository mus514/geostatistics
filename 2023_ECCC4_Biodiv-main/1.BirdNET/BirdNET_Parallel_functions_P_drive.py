# functions.py
import subprocess

# Function to prepare data for script
def parallel_process(row, data_path, output, batchsize, BirdNETscript):
     # Grab meta data
     ID = row['ID']
     lat = row['lat']
     long = row['long']
     week = row['week']
     # Create inputs for bird net
     audio = data_path / ID
     sub_output = output / ID
     # Create output folder
     sub_output.mkdir(exist_ok=True)
     # Run command
     subprocess.run(["python", BirdNETscript,
                    "--i", audio,
                    "--o", sub_output,
                    "--lat", str(lat),
                    "--lon", str(long),
                    "--week", str(week),
                    "--batchsize", str(batchsize)])
