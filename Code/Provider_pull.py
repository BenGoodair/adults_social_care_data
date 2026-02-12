#Playing with CQC API

#load modules


import http.client
import pandas as pd
import requests
import json
import ast
import re
import time
from pandas import json_normalize


import ssl






#####PROVIDER API#####


# Primary key
api_key = 'f3092d60de7e466ea0f7baad721e7b23'

context = ssl._create_unverified_context()


# Task 1 - Get all location IDs
conn = http.client.HTTPSConnection('api.service.cqc.org.uk', context=context)
headers = {
    'Ocp-Apim-Subscription-Key': api_key  # Use the correct header field for the subscription key
}

try:
    conn.request('GET', '/public/v1/providers?page=1&perPage=10000', headers=headers)
    res = conn.getresponse()
    data = res.read()  # Read the response to avoid issues with the connection being left open
    print(data)
finally:
    conn.close() 

json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df1 = pd.DataFrame(json_data)


try:
    conn.request('GET', '/public/v1/providers?page=2&perPage=10000', headers=headers)
    res = conn.getresponse()
    data = res.read()  # Read the response to avoid issues with the connection being left open
    print(data)
finally:
    conn.close() 

json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df2 = pd.DataFrame(json_data)


try:
    conn.request('GET', '/public/v1/providers?page=3&perPage=10000', headers=headers)
    res = conn.getresponse()
    data = res.read()  # Read the response to avoid issues with the connection being left open
    print(data)
finally:
    conn.close() 

json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df3 = pd.DataFrame(json_data)


try:
    conn.request('GET', '/public/v1/providers?page=4&perPage=10000', headers=headers)
    res = conn.getresponse()
    data = res.read()  # Read the response to avoid issues with the connection being left open
    print(data)
finally:
    conn.close() 

json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df4 = pd.DataFrame(json_data)


try:
    conn.request('GET', '/public/v1/providers?page=5&perPage=10000', headers=headers)
    res = conn.getresponse()
    data = res.read()  # Read the response to avoid issues with the connection being left open
    print(data)
finally:
    conn.close() 

json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df5 = pd.DataFrame(json_data)


try:
    conn.request('GET', '/public/v1/providers?page=6&perPage=10000', headers=headers)
    res = conn.getresponse()
    data = res.read()  # Read the response to avoid issues with the connection being left open
    print(data)
finally:
    conn.close() 

json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df6 = pd.DataFrame(json_data)

try:
    conn.request('GET', '/public/v1/providers?page=7&perPage=10000', headers=headers)
    res = conn.getresponse()
    data = res.read()  # Read the response to avoid issues with the connection being left open
    print(data)
finally:
    conn.close() 

json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df7 = pd.DataFrame(json_data)







df = pd.concat([df1, df2, df3,
                df4, df5, df6,
                df7])


#extract all the IDs

# Function to extract locationId
def extract_location_id(location_dict):
    return location_dict.get('providerId')

# Apply the function to the 'locations' column
df['providerId'] = df['providers'].apply(extract_location_id)

#save all carehomes
csv_file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_2025.csv"

# Use the to_csv method to write the DataFrame to a CSV file
df.to_csv(csv_file_path, index=False)  # Set index=False to avoid writing row numbers as a column





import pandas as pd
import json
import time
import http.client

import subprocess

# Start caffeinate
caffeinate_process = subprocess.Popen(["caffeinate", "-i"])

try:
    # Your long-running Python code here
    while True:

        # Define the size of each batch
        batch_size = 10000  # Adjust the batch size based on your needs

        # Initialize variables to track batch number
        batch_number = 5

        # Loop through the DataFrame in chunks
        for i in range((batch_number - 1) * batch_size, len(df), batch_size):
             # Get a chunk of the DataFrame
            df_chunk = df.iloc[i:i + batch_size]

            # Create an empty list to store the results of the current batch
            result_data = []

            # Loop through each 'providerId' in the chunk
            for location_id in df_chunk['providerId']:
                while True:
                    try:
                        conn = http.client.HTTPSConnection('api.service.cqc.org.uk', context=context)
                        conn.request('GET', f'/public/v1/providers/{location_id}', headers=headers)
                        res = conn.getresponse()

                        if res.status == 200:
                            data = res.read().decode()  # Decode the data from bytes to a string
                            json_data = json.loads(data)  # Parse the JSON data

                            # Append the data for the current locationId to the result list
                            result_data.append(json_data)
                            print(f"API successful for locationId {location_id}. Jon's cute")

                            break  # Successfully received data, exit the retry loop

                        elif res.status == 429:
                            # If API rate limit exceeded, wait and then retry
                            print(f"API rate limit exceeded for locationId {location_id}. Waiting and retrying...")
                            time.sleep(60)  # Wait for a minute before retrying

                        else:
                            # Handle other HTTP status codes as needed
                            print(f"Failed to retrieve data for locationId {location_id} with status code {res.status}")
                            break  # Exit the retry loop if it's not a rate limit issue or other recoverable error

                    except Exception as e:
                        print(f"An error occurred for locationId {location_id}: {str(e)}")
                        break  # Exit the retry loop on any exception

            # Convert the result_data list to a DataFrame
            result_df = pd.DataFrame(result_data)

            # Define a CSV file path for the current batch
            csv_file_path = f"Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_info_2025_batch_{batch_number}.csv"

            # Save the result DataFrame to a CSV file
            result_df.to_csv(csv_file_path, index=False)

            print(f"Batch {batch_number} saved successfully.")

            # Increment the batch number for the next loop
            batch_number += 1
except KeyboardInterrupt:
    pass
finally:
    # Stop caffeinate when script ends
    caffeinate_process.terminate()



providers = pd.concat([pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_info_2025_batch_1.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_info_2025_batch_2.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_info_2025_batch_3.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_info_2025_batch_4.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_info_2025_batch_5.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_info_2025_batch_6.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_info_2025_batch_7.csv")
                       
                       
])


# Define a CSV file path for the current batch
csv_file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/providers_info_batch_all.csv"
# Save the result DataFrame to a CSV file
providers.to_csv(csv_file_path, index=False)














































