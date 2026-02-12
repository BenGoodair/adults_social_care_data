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

#caffeinate -i

#####LOCATION API#####

api_key = 'f3092d60de7e466ea0f7baad721e7b23'

context = ssl._create_unverified_context()


# Task 1 - Get all location IDs
conn = http.client.HTTPSConnection('api.service.cqc.org.uk', context=context)
headers = {
    'Ocp-Apim-Subscription-Key': api_key  # Use the correct header field for the subscription key
}

#Task 1 = get all location IDs
conn.request('GET','/public/v1/locations?careHome=N&perPage=10000&page=1', headers=headers)
res = conn.getresponse()

data = res.read().decode('utf-8')  # Decode the data from bytes to a string
json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df1 = pd.DataFrame(json_data)

conn.request('GET','/public/v1/locations?careHome=N&perPage=10000&page=2', headers=headers)
res = conn.getresponse()

data = res.read().decode('utf-8')  # Decode the data from bytes to a string
json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df2 = pd.DataFrame(json_data)

conn.request('GET','/public/v1/locations?careHome=N&perPage=10000&page=3', headers=headers)
res = conn.getresponse()

data = res.read().decode('utf-8')  # Decode the data from bytes to a string
json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df3 = pd.DataFrame(json_data)

conn.request('GET','/public/v1/locations?careHome=N&perPage=10000&page=4', headers=headers)
res = conn.getresponse()

data = res.read().decode('utf-8')  # Decode the data from bytes to a string
json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df4 = pd.DataFrame(json_data)

conn.request('GET','/public/v1/locations?careHome=N&perPage=10000&page=5', headers=headers)
res = conn.getresponse()

data = res.read().decode('utf-8')  # Decode the data from bytes to a string
json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df5 = pd.DataFrame(json_data)

conn.request('GET','/public/v1/locations?careHome=N&perPage=10000&page=6', headers=headers)
res = conn.getresponse()

data = res.read().decode('utf-8')  # Decode the data from bytes to a string
json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df6 = pd.DataFrame(json_data)

conn.request('GET','/public/v1/locations?careHome=N&perPage=10000&page=7', headers=headers)
res = conn.getresponse()

data = res.read().decode('utf-8')  # Decode the data from bytes to a string
json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df7 = pd.DataFrame(json_data)

conn.request('GET','/public/v1/locations?careHome=N&perPage=10000&page=8', headers=headers)
res = conn.getresponse()

data = res.read().decode('utf-8')  # Decode the data from bytes to a string
json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df8 = pd.DataFrame(json_data)

conn.request('GET','/public/v1/locations?careHome=N&perPage=10000&page=9', headers=headers)
res = conn.getresponse()

data = res.read().decode('utf-8')  # Decode the data from bytes to a string
json_data = json.loads(data)       # Parse the JSON data

# Create a pandas DataFrame from the JSON data
df9 = pd.DataFrame(json_data)




df = pd.concat([df1, df2, df3, df4, df5,
                df6, df7, df8, df9])

#save all carehomes
csv_file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_cqc_api_2025.csv"

# Use the to_csv method to write the DataFrame to a CSV file
df.to_csv(csv_file_path, index=False)  # Set index=False to avoid writing row numbers as a column

#extract all the IDs

# Function to extract locationId
def extract_location_id(location_dict):
    return location_dict.get('locationId')

# Apply the function to the 'locations' column
df['locationId'] = df['locations'].apply(extract_location_id)




#####SHORTCUT WITH SMALL SAMPLE OF LOCATIONIDS########
#THIS OVERWRITES THE PREVIOUS DF WITH ALL LOCATION IDS, HASHTAG OUT TO RUN FULL SAMPLE#

#df = pd.DataFramedata = {'locationId': ['1-10000302982', '1-10000812939', '1-10000813008', '1-1000210669', '1-1000401911', '1-10005690253', '1-10005776421', '1-1000587219', '1-10007027366', '1-1000711804', '1-10007274905', '1-10016894058', '1-10017508735', '1-1001921065', '1-1002113960', '1-1002185812', '1-1002282014', '1-10023785774', '1-1002551837', '1-10040312985', '1-10040429539', '1-10040429567', '1-10040879614', '1-10042343393', '1-10042435531', '1-10042502639', '1-1004508435', '1-1004589685']}











#15:57 start, 17:02 finish = 65 minutes to run this code - until line 139 - where it says end_time
#new pb = 54 mins

start_time = time.time()


# Create an empty list to store the results
result_data = []


import subprocess

# Start caffeinate
caffeinate_process = subprocess.Popen(["caffeinate", "-i"])

try:
    # Your long-running Python code here
    while True:

        # Define the size of each batch
        batch_size = 10000  # Adjust the batch size based on your needs

        # Initialize variables to track batch number
        batch_number = 2

        # Loop through the DataFrame in chunks
        for i in range((batch_number - 1) * batch_size, len(df), batch_size):
             # Get a chunk of the DataFrame
            df_chunk = df.iloc[i:i + batch_size]

            # Create an empty list to store the results of the current batch
            result_data = []

            # Loop through each 'providerId' in the chunk
            for location_id in df_chunk['locationId']:
                while True:
                    try:
                        conn = http.client.HTTPSConnection('api.service.cqc.org.uk', context=context)
                        conn.request('GET', f'/public/v1/locations/{location_id}', headers=headers)
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
            csv_file_path = f"Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_info_2025_batch_{batch_number}.csv"

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



result_df = pd.concat([pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_info_2025_batch_1.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_info_2025_batch_2.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_info_2025_batch_3.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_info_2025_batch_4.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_info_2025_batch_5.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_info_2025_batch_6.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_info_2025_batch_7.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_info_2025_batch_8.csv"),
                       pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_homes_info_2025_batch_9.csv")
                       
                       
])





# Now result_df contains data for all 'locationId' values, and it has handled rate limiting and errors

#save all carehomes
csv_file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/all_non_carehomes_all_info_2025.csv"

# Use the to_csv method to write the DataFrame to a CSV file
result_df.to_csv(csv_file_path, index=False)  # Set index=False to avoid writing row numbers as a column

print("--- %s seconds ---" % (time.time() - start_time))
end_time = time.time()



#current ratings!#

# Now, we need to handle the issue where single quotes might be used in the JSON strings
def fix_json_format(x):
    if isinstance(x, str):
        # Replace single quotes with double quotes
        x = x.replace("'", '"')
        # Try to load the string as JSON
        try:
            return json.loads(x)
        except json.JSONDecodeError as e:
            print(f"Error decoding JSON: {e}")
            return None
    return x

# Apply the function to the 'currentRatings' column
result_df['currentRatings'] = result_df['currentRatings'].apply(fix_json_format)


currentRatings_df = json_normalize(result_df['currentRatings'])

# Reset index for both result_df['locationId'] and currentRatings_df to ensure they have consistent indices
locationId_df = result_df['locationId'].reset_index(drop=True)
currentRatings_df_reset = currentRatings_df.reset_index(drop=True)

currentRatings_df = pd.concat([locationId_df, currentRatings_df_reset], axis=1)


#duplicate check
duplicates = currentRatings_df[currentRatings_df.duplicated(subset=['locationId'], keep=False)]



# 

# Convert the data to a DataFrame
KeyQs = json_normalize(currentRatings_df['overall.keyQuestionRatings'])


# Create a new DataFrame with the desired columns
new_df = pd.DataFrame({
    'locationId': currentRatings_df['locationId'],
    'name': [item.get('name') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 0]],
    'rating': [item.get('rating') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 0]],
    'reportDate': [item.get('reportDate') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 0]],
    'reportLinkId': [item.get('reportLinkId') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 0]]
})

new_df2 = pd.DataFrame({
    'locationId': currentRatings_df['locationId'],
    'name': [item.get('name') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 1]],
    'rating': [item.get('rating') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 1]],
    'reportDate': [item.get('reportDate') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 1]],
    'reportLinkId': [item.get('reportLinkId') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 1]]
})

new_df3 = pd.DataFrame({
    'locationId': currentRatings_df['locationId'],
    'name': [item.get('name') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 2]],
    'rating': [item.get('rating') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 2]],
    'reportDate': [item.get('reportDate') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 2]],
    'reportLinkId': [item.get('reportLinkId') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 2]]
})



new_df4 = pd.DataFrame({
    'locationId': currentRatings_df['locationId'],
    'name': [item.get('name') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 3]],
    'rating': [item.get('rating') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 3]],
    'reportDate': [item.get('reportDate') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 3]],
    'reportLinkId': [item.get('reportLinkId') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 3]]
})

new_df5 = pd.DataFrame({
    'locationId': currentRatings_df['locationId'],
    'name': [item.get('name') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 4]],
    'rating': [item.get('rating') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 4]],
    'reportDate': [item.get('reportDate') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 4]],
    'reportLinkId': [item.get('reportLinkId') if isinstance(item, dict) else None for item in KeyQs.iloc[:, 4]]
})





## Create a new DataFrame with the desired columns
#new_df2 = pd.DataFrame({'locationId': currentRatings_df['locationId'],
#                       'name': [item.get['name'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 1]],
#                       'rating': [item.get['rating'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 1]],
#                       'reportDate': [item.get['reportDate'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 1]],
#                       'reportLinkId': [item.get['reportLinkId'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 1]]})
#
## Create a new DataFrame with the desired columns
#new_df3 = pd.DataFrame({'locationId': currentRatings_df['locationId'],
#                       'name': [item['name'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 2]],
#                       'rating': [item['rating'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 2]],
#                       'reportDate': [item['reportDate'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 2]],
#                       'reportLinkId': [item['reportLinkId'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 2]]})
#
## Create a new DataFrame with the desired columns
#new_df4 = pd.DataFrame({'locationId': currentRatings_df['locationId'],
#                       'name': [item['name'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 3]],
#                       'rating': [item['rating'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 3]],
#                       'reportDate': [item['reportDate'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 3]],
#                       'reportLinkId': [item['reportLinkId'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 3]]})
#
## Create a new DataFrame with the desired columns
#new_df5 = pd.DataFrame({'locationId': currentRatings_df['locationId'],
#                       'name': [item['name'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 4]],
#                       'rating': [item['rating'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 4]],
#                       'reportDate': [item['reportDate'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 4]],
#                       'reportLinkId': [item['reportLinkId'] if isinstance(item, dict) else None for item in KeyQs.iloc[:, 4]]})
#
#

new_df = pd.concat([new_df, new_df2, new_df3, new_df4, new_df5])

#save all carehomes
#csv_file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/current_ratings_only_2025.csv"

# Use the to_csv method to write the DataFrame to a CSV file
#
#new_df.to_csv(csv_file_path, index=False)  # Set index=False to avoid writing row numbers as a column



#some duplicates exist for locations with current rating before 2014 so the 'effective' 'safe' etc all get 'none' - these get removed
duplicates = new_df[new_df.duplicated(subset=['locationId', 'name'], keep=False)]

new_df = new_df[['locationId', 'name', 'rating']]

pivoted_df = pd.pivot_table(new_df, values='rating', index='locationId', columns='name', aggfunc='first')
# Reset the index
pivoted_df.reset_index(inplace=True)

# Rename the index name to None
pivoted_df.index.name = None

currentRatings_df = currentRatings_df[['locationId', 'reportDate', 'overall.reportLinkId', 'overall.keyQuestionRatings', 'serviceRatings', 'overall.rating' ]]

currentRatings_df = currentRatings_df.merge(pivoted_df, left_on='locationId', right_on='locationId')
new_df = new_df.merge(currentRatings_df, left_on='locationId', right_on='locationId')

currentRatings_df =  currentRatings_df.rename(columns={"overall.reportLinkId":"reportLinkId","overall.keyQuestionRatings": "keyQuestionRatings", "overall.rating": "Overall"})


#save all carehomes
csv_file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_home_current_ratings_only_2025.csv"

# Use the to_csv method to write the DataFrame to a CSV file
currentRatings_df.to_csv(csv_file_path, index=False)  # Set index=False to avoid writing row numbers as a column






#Historic ratings!#


# Now, we need to handle the issue where single quotes might be used in the JSON strings
def fix_json_format(x):
    if isinstance(x, str):
        # Replace single quotes with double quotes
        x = x.replace("'", '"')
        # Try to load the string as JSON
        try:
            return json.loads(x)
        except json.JSONDecodeError as e:
            print(f"Error decoding JSON: {e}")
            return None
    return x

# Apply the function to the 'currentRatings' column
result_df['historicRatings'] = result_df['historicRatings'].apply(fix_json_format)


# Initialize an empty list to store DataFrames
dfs = []

# Iterate through each row in the 'historicRatings' column
for index, row in result_df.iterrows():
    ratings_data = row['historicRatings']
    location_id = row['locationId']  # Extract the 'locationId' from the original DataFrame
    if isinstance(ratings_data, list):
        for rating_data in ratings_data:
            data_dict = {
                'organisationId': rating_data.get('organisationId', ''),
                'locationId': location_id,  # Include 'locationId'
                'reportLinkId': rating_data.get('reportLinkId', ''),
                'reportDate': rating_data.get('reportDate', '')
            }

            overall_data = rating_data.get('overall', {})
            data_dict['overall_rating'] = overall_data.get('rating', '')

            key_question_ratings = overall_data.get('keyQuestionRatings', [])
            for key_question in key_question_ratings:
                key_name = key_question.get('name', '')
                key_rating = key_question.get('rating', '')
                data_dict[f'{key_name}_rating'] = key_rating

            # Append the data_dict as a DataFrame to the list
            dfs.append(pd.DataFrame([data_dict]))

# Concatenate the list of DataFrames into a single DataFrame
historicRatings_df = pd.concat(dfs, ignore_index=True)



historicRatings_df = historicRatings_df.rename(columns={"overall_rating":"Overall","Well-led_rating": "Well-led", "Caring_rating": "Caring", "Responsive_rating": "Responsive", "Effective_rating": "Effective", "Safe_rating":"Safe"})
historicRatings_df['serviceRatings']='NaN'

#save all carehomes
csv_file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_home_historic_ratings_only_2025.csv"

# Use the to_csv method to write the DataFrame to a CSV file
historicRatings_df.to_csv(csv_file_path, index=False)  # Set index=False to avoid writing row numbers as a column





allRatings_df = pd.concat([historicRatings_df, currentRatings_df])

allRatings_df  = allRatings_df[allRatings_df['reportLinkId'].notnull()]


#save all carehomes
csv_file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_home_all_ratings_2025.csv"

# Use the to_csv method to write the DataFrame to a CSV file
allRatings_df.to_csv(csv_file_path, index=False)  # Set index=False to avoid writing row numbers as a column




#now flatten the JSON columns
data = pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/all_non_carehomes_all_info_2025.csv")

df = pd.DataFrame(data)

# Convert the string representation of lists to actual lists of dictionaries
df['relationships'] = df['relationships'].apply(ast.literal_eval)

# Function to normalize the nested list into separate columns for each relationship
def normalize_relationships(row):
    max_len = max(len(row), 1)  # to handle empty list case
    ids = [item.get('relatedLocationId') for item in row] + [None] * (max_len - len(row))
    names = [item.get('relatedLocationName') for item in row] + [None] * (max_len - len(row))
    types = [item.get('type') for item in row] + [None] * (max_len - len(row))
    reasons = [item.get('reason') for item in row] + [None] * (max_len - len(row))
    
    # Create columns with indices like relatedLocationIds_1, relatedLocationIds_2
    expanded_columns = {
        **{f'relatedLocationIds_{i+1}': ids[i] for i in range(max_len)},
        **{f'relatedLocationNames_{i+1}': names[i] for i in range(max_len)},
        **{f'types_{i+1}': types[i] for i in range(max_len)},
        **{f'reasons_{i+1}': reasons[i] for i in range(max_len)}
    }
    return pd.Series(expanded_columns)

# Apply the function to expand the 'relationships' column into separate columns
expanded = df['relationships'].apply(normalize_relationships)

# Join the expanded columns back to the original DataFrame
df = df.join(expanded)


#save all carehomes
csv_file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_carehomes_relationship_expanded.csv"

# Use the to_csv method to write the DataFrame to a CSV file
df.to_csv(csv_file_path, index=False)  # Set index=False to avoid writing row numbers as a column






import pandas as pd
import json
from pandas import json_normalize
import numpy as np
from datetime import datetime

# Function to safely extract assessment data
def extract_assessment_data(row):
    try:
        # If the assessment column is already a string representation of JSON
        if isinstance(row, str):
            # Remove any leading/trailing whitespace and check if it's valid JSON
            row = row.strip()
            if row:
                return json.loads(row)
            return []
        # If it's already parsed as a list or dictionary
        elif isinstance(row, (list, dict)):
            return row
        # Handle NaN or None values
        else:
            return []
    except (json.JSONDecodeError, TypeError) as e:
        print(f"Error parsing JSON: {e}")
        # Try to see if this is a valid Python literal (sometimes JSON gets read as Python literals)
        try:
            import ast
            if isinstance(row, str):
                return ast.literal_eval(row)
            return []
        except:
            return []


# Function to extract key question ratings from an assessment
def extract_key_question_ratings(assessment):
    if not assessment:
        return {}
    
    result = {}
    
    # Try to get the assessment plan
    try:
        if isinstance(assessment, list):
            # If assessment is a list, process the first item
            if len(assessment) > 0:
                assessment_item = assessment[0]
                result['assessment_count'] = len(assessment)
            else:
                return {}
        else:
            assessment_item = assessment
            result['assessment_count'] = 1
            
        # Extract assessment plan published date
        result['assessment_published_date'] = assessment_item.get('assessmentPlanPublishedDateTime', '')
        
        # Check if ratings exist
        if 'ratings' in assessment_item and 'asgRatings' in assessment_item['ratings']:
            asg_ratings = assessment_item['ratings']['asgRatings'][0]
            
            # Extract assessment plan metadata
            result['assessment_plan_id'] = asg_ratings.get('assessmentPlanId', '')
            result['assessment_title'] = asg_ratings.get('title', '')
            result['assessment_date'] = asg_ratings.get('assessmentDate', '')
            result['assessment_status'] = asg_ratings.get('assessmentPlanStatus', '')
            result['service_name'] = asg_ratings.get('name', '')
            result['overall_rating'] = asg_ratings.get('rating', '')
            result['rating_status'] = asg_ratings.get('status', '')
            result['overall_narrative'] = asg_ratings.get('narrative', '')
            result['people_experience'] = asg_ratings.get('overallPeopleExperience', '')
            
            # Extract ratings for each key question
            if 'keyQuestionRatings' in asg_ratings:
                for kq in asg_ratings['keyQuestionRatings']:
                    question_name = kq.get('name', '').lower()
                    if question_name:
                        # Store the rating for this question
                        result[f'{question_name}_rating'] = kq.get('rating', '')
                        result[f'{question_name}_status'] = kq.get('status', '')
                        result[f'{question_name}_narrative'] = kq.get('narrative', '')
                        result[f'{question_name}_commentary'] = kq.get('commentary', '')
                        result[f'{question_name}_score'] = kq.get('percentageScore', np.nan)
                        
                        # Extract topic areas for this question if available
                        if 'topicareas' in kq:
                            topic_areas = {}
                            for i, topic in enumerate(kq['topicareas']):
                                topic_name = topic.get('name', '').lower().replace(' ', '_')
                                if topic_name:
                                    topic_areas[f'{question_name}_topic_{i+1}_name'] = topic_name
                                    topic_areas[f'{question_name}_topic_{i+1}_id'] = topic.get('qualityStatementId', '')
                                    topic_areas[f'{question_name}_topic_{i+1}_score'] = topic.get('qualityStatementscore', np.nan)
                                    topic_areas[f'{question_name}_topic_{i+1}_status'] = topic.get('status', '')
                            
                            # Add all topic areas to the result
                            result.update(topic_areas)
    except (KeyError, IndexError, TypeError) as e:
        print(f"Error extracting key question ratings: {e}")
    
    return result

def main():
    # Load the data
    print("Loading data...")
    file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/all_non_carehomes_all_info_2025.csv"
    
    # Try different approaches to load the file correctly
    try:
        # First try: standard read with inferred JSON
        df = pd.read_csv(file_path)
        print(f"Successfully loaded CSV with shape: {df.shape}")
        
        # Check if 'assessment' column exists
        if 'assessment' not in df.columns:
            print("Warning: 'assessment' column not found in the dataset")
            print(f"Available columns: {df.columns.tolist()}")
            
            # Try to find a similarly named column
            possible_assessment_cols = [col for col in df.columns if 'assess' in col.lower()]
            if possible_assessment_cols:
                print(f"Found possible assessment columns: {possible_assessment_cols}")
                assessment_col = possible_assessment_cols[0]
                print(f"Using '{assessment_col}' as assessment column")
            else:
                print("No assessment-related columns found. Exiting.")
                return
        else:
            assessment_col = 'assessment'
        
        # Check sample of assessment data
        print("\nSample assessment data:")
        sample = df[assessment_col].head(1).iloc[0]
        print(f"Type: {type(sample)}")
        print(f"Sample: {str(sample)[:500]}...")
        
        # Process each row's assessment data
        print("\nProcessing assessment data...")
        results = []
        
        for i, row in df.iterrows():
            if i % 1000 == 0:
                print(f"Processing row {i}/{len(df)}")
            
            # Extract location ID and other key identifier columns
            location_data = {
                'locationId': row.get('locationId', ''),
                'name': row.get('name', ''),
                'postalCode': row.get('postalCode', '')
            }
            
            # Extract assessment data
            assessment_data = extract_key_question_ratings(extract_assessment_data(row[assessment_col]))
            
            # Combine location and assessment data
            combined_data = {**location_data, **assessment_data}
            results.append(combined_data)
        
        # Create a new dataframe with the flattened data
        print("Creating flattened dataframe...")
        flattened_df = pd.DataFrame(results)
        
        # Check if we got more than just the location columns
        assessment_cols = [col for col in flattened_df.columns 
                          if col not in ['locationId', 'name', 'postalCode']]
        
        if not assessment_cols:
            print("\nWARNING: No assessment columns were extracted. Let's try another approach.")
            
            # Try to parse the assessment column directly
            print("Attempting to directly normalize the JSON data...")
            
            # Check if we can parse at least one row correctly
            for i, row in df.iterrows():
                assessment_val = row[assessment_col]
                if pd.notna(assessment_val) and assessment_val:
                    try:
                        # Try parsing as JSON
                        parsed = json.loads(assessment_val) if isinstance(assessment_val, str) else assessment_val
                        
                        # If successful, break out of the loop
                        print(f"Successfully parsed row {i}")
                        
                        # Create a new dataframe with just location ID and the parsed assessment
                        location_cols = ['locationId', 'name', 'postalCode']
                        location_data = df[location_cols].copy()
                        
                        # Try json_normalize on the successfully parsed data
                        parsed_data = []
                        for j, row in df.iterrows():
                            try:
                                assessment_val = row[assessment_col]
                                if pd.notna(assessment_val) and assessment_val:
                                    assessment_dict = json.loads(assessment_val) if isinstance(assessment_val, str) else assessment_val
                                    parsed_data.append(assessment_dict)
                                else:
                                    parsed_data.append({})
                            except:
                                parsed_data.append({})
                        
                        # Attempt to normalize the assessment data
                        try:
                            assessment_df = json_normalize(parsed_data, errors='ignore')
                            
                            # Create the final flattened dataframe
                            flattened_df = pd.concat([location_data.reset_index(drop=True), 
                                                      assessment_df.reset_index(drop=True)], axis=1)
                            
                            print(f"Successfully created flattened dataframe with shape: {flattened_df.shape}")
                            break
                        except Exception as e:
                            print(f"Error normalizing JSON data: {e}")
                            break
                    except Exception as e:
                        continue
        
        # Fill NaN values for key fields
        rating_columns = [col for col in flattened_df.columns if '_rating' in col]
        for col in rating_columns:
            flattened_df[col] = flattened_df[col].fillna('Not Rated')
        
        print(f"\nFinal shape: {flattened_df.shape}")
        print(f"Columns in flattened data ({len(flattened_df.columns)}):")
        print(flattened_df.columns.tolist())
        
        # Save the flattened data
        output_file = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/flattened_non_carehomes_assessments.csv"
        print(f"Saving to {output_file}")
        flattened_df.to_csv(output_file, index=False)
        
        print("Done!")
    
    except Exception as e:
        print(f"Error in main processing: {e}")
        
        # Try alternative loading approach with explicit JSON handling
        print("\nTrying alternative loading approach...")
        
        try:
            # Load with dtype specification to keep assessment as string
            df = pd.read_csv(file_path, dtype={assessment_col: str})
            print(f"Successfully loaded CSV with dtype specification: {df.shape}")
            
            # See if there are any assessments we can parse
            sample_rows = []
            for i, row in df.iterrows():
                if i >= 10:  # Look at first 10 rows
                    break
                try:
                    assessment_val = row[assessment_col]
                    if isinstance(assessment_val, str) and assessment_val.strip():
                        parsed = json.loads(assessment_val)
                        sample_rows.append((i, parsed))
                except:
                    continue
            
            if sample_rows:
                print(f"Found {len(sample_rows)} parseable rows out of 10 examined")
                example_idx, example_parsed = sample_rows[0]
                print(f"Example from row {example_idx}:")
                print(json.dumps(example_parsed, indent=2)[:500] + "...")
            else:
                print("Could not find any parseable assessment data in the first 10 rows")
        except Exception as e:
            print(f"Error in alternative loading approach: {e}")

if __name__ == "__main__":
    main()




all_ratings = pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/non_care_home_all_ratings_2025.csv")

all_assessments = pd.read_csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/flattened_non_carehomes_assessments.csv")
all_assessments = all_assessments[all_assessments['overall_rating'] != "Not Rated"]



# List all column names
print(all_assessments.columns.tolist())
print(all_ratings.columns.tolist())

all_assessments["reportDate"] = pd.to_datetime(all_assessments["assessment_published_date"], dayfirst=False).dt.strftime("%d/%m/%Y")


assesements_simple = all_assessments[['locationId', 'reportDate', 'overall_rating', 'caring_rating', 'effective_rating', 'safe_rating', 'well-led_rating','responsive_rating']]

assesements_simple.rename(columns={"overall_rating": "Overall"}, inplace=True)
assesements_simple.rename(columns={"caring_rating": "Caring"}, inplace=True)
assesements_simple.rename(columns={"effective_rating": "Effective"}, inplace=True)
assesements_simple.rename(columns={"safe_rating": "Safe"}, inplace=True)
assesements_simple.rename(columns={"well-led_rating": "Well-led"}, inplace=True)
assesements_simple.rename(columns={"responsive_rating": "Responsive"}, inplace=True)

import pandas as pd

# Assuming assessments_simple is a pandas DataFrame
assesements_simple["organisationId"] = pd.NA
assesements_simple["reportLinkId"] = pd.NA
assesements_simple["serviceRatings"] = pd.NA
assesements_simple["keyQuestionRatings"] = pd.NA


allRatings_df_with_assessments = pd.concat([all_ratings, assesements_simple])



#save all carehomes
csv_file_path = "Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_materials/Data/all_ratings_non_care_homes_2025_with_assessments.csv"

# Use the to_csv method to write the DataFrame to a CSV file
allRatings_df_with_assessments.to_csv(csv_file_path, index=False)  # Set index=False to avoid writing row numbers as a column








