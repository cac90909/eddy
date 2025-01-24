import csv
from shared.models import UserData

# Define the path to your CSV file
csv_file_path = "test_data.csv"

def parse_semicolon_separated(value):
    """
    Helper function to split semicolon-separated strings into lists.
    Returns None for empty or missing values.
    """
    if not value or value.strip() == "":
        return []
    return value.split(";")

def load_test_data(file_path):
    """
    Reads data from a CSV file, processes it, and inserts it into the user_data_test table.
    """
    try:
        # Clear existing data from the table
        UserData.objects.all().delete()
        print("Existing data cleared.")

        # Open the CSV file
        with open(file_path, mode="r") as file:
            reader = csv.DictReader(file)

            # Iterate over each row in the CSV
            for row in reader:
                # Process semicolon-separated fields into lists
                functionalities = parse_semicolon_separated(row["Functionalities"])
                subject_matters = parse_semicolon_separated(row["Subject Matters"])
                general_categories = parse_semicolon_separated(row["General Categories"])
                parents = parse_semicolon_separated(row["Parents"])
                children = parse_semicolon_separated(row["Children"])
                siblings = parse_semicolon_separated(row["Siblings"])
                tags = parse_semicolon_separated(row['Tags'])

                # Insert data into the database
                UserData.objects.create(
                    date=row["Date"],  # Assuming the date is in the format recognized by Django
                    functionalities=functionalities,
                    subject_matters=subject_matters,
                    general_categories=general_categories,
                    title=row["Title"] or None,
                    text=row["Text"] or None,
                    tags=tags,
                    parent_ids=parents,
                    children_ids=children,
                    siblings=siblings,
                    fields=row["Fields"] or None,
                    entry_id=row["ID"],
                    user_id=row["User ID"],
                )
        print("Data successfully loaded.")
    except Exception as e:
        print(f"Error loading data: {e}")

# Call the function
load_test_data(csv_file_path)
