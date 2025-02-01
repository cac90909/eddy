import csv
from datetime import datetime
from shared.models import UserData

def import_csv(file_path):
    def process_semicolon_separated(value):
        """Converts a semicolon-separated string into a list."""
        return value.split(';') if value else []

    with open(file_path, mode='r') as file:
        reader = csv.DictReader(file)
        for row in reader:
            UserData.objects.create(
                date=datetime.strptime(row['Date'], '%m/%d/%Y'),
                functionality=process_semicolon_separated(row['Functionalities']),
                subject_matter=process_semicolon_separated(row['Subject Matters']),
                general_categories=process_semicolon_separated(row['General Categories']),
                title=row['Title'] or None,
                text=row['Text'] or None,
                tags=process_semicolon_separated(row['Tags']),
                parent_id=process_semicolon_separated(row['Parents']),
                child_id=process_semicolon_separated(row['Children']),
                siblings=process_semicolon_separated(row['Siblings']),
                fields=row['Fields'] or None,  # Assuming `fields` is raw JSON
                entry_id=row['ID'],
                user_id=row['User ID']
            )
