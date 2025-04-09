import subprocess
import os
import requests
import re

# Function to prepare data for script
def parallel_process(row, data_path, output, batchsize, BirdNETscript):
     # Grab meta data
     ID = row['ID']
     lat = row['lat']
     long = row['long']
     week = row['week']
     # Prepare Blob lookup
     data_path.loc[:, 'combined'] = data_path['field'].astype(str) + '-' + data_path['year'].astype(str) + '-' + data_path['sequence'].astype(str)
     # Filter Blob to data of interest
     data_path = data_path[data_path['combined'].str.contains(ID)]
     # Grab download links
     links = data_path['file_path'].tolist()
     # Create temp download folder
     temp_dir = output.parent / "Data"
     temp_dir.mkdir(exist_ok=True)
     # Loop over links and download
     for link in links:
          # Create name for saving
          name = re.search(r'/([^/]+\.WAV)', link).group(1)
          file_name = os.path.join(temp_dir,name)
          # Download
          response = requests.get(link)
          if response.status_code == 200:
               with open(file_name, 'wb') as f:
                    f.write(response.content)
               print(f"Downloaded and saved as {file_name}")
          else:
               print(f"Failed to download {link}")   
     # Create inputs for bird net
     audio = temp_dir
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
