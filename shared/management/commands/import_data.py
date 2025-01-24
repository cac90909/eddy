import os
import csv
from django.core.management.base import BaseCommand
from shared.models import UserData
from datetime import datetime
import json

class Command(BaseCommand):
    help = "Import user data from a CSV file"

    def add_arguments(self, parser):
        parser.add_argument('--file_path', type=str, default="shared/postgres/test_data.csv", help="Path to the CSV file")

    # def parse_fields_to_json(self, fields_str):
    #     """
    #     Convert a fields string like "Rating: 8/10, Location: New Delhi, India"
    #     into a JSON object like {"Rating": "8/10", "Location": "New Delhi, India"}.
    #     """
    #     if not fields_str or not isinstance(fields_str, str):
    #         return {}
    #
    #     try:
    #         # Split by comma and then by colon
    #         fields_dict = {}
    #         for pair in fields_str.split(','):
    #             key, value = pair.split(':', 1)
    #             fields_dict[key.strip()] = value.strip()
    #         return fields_dict
    #     except ValueError:
    #         # Handle improperly formatted fields
    #         print(f"Error parsing fields: {fields_str}")
    #         return {}
    import json

    def parse_fields_to_json(self,fields_string, delimiter="--"):
        """
        Parse a fields string with a custom delimiter into a JSON object.

        Args:
            fields_string (str): The string to parse.
            delimiter (str): The custom delimiter for separating key-value pairs.

        Returns:
            str: A JSON string representation of the fields, or None if parsing fails.
        """
        if not fields_string:
            return None

        try:
            fields_dict = {
                item.split(":")[0].strip(): item.split(":")[1].strip()
                for item in fields_string.split(delimiter)
            }
            return fields_dict
        except Exception as e:
            return None  # Return None if parsing fails

    def handle(self, *args, **kwargs):

        file_path = kwargs['file_path']

        if not os.path.exists(file_path):
            self.stdout.write(self.style.ERROR(f"File not found: {file_path}"))
            return

        try:
            with open(file_path, mode="r") as file:
                reader = csv.DictReader(file)
                UserData.objects.all().delete()
                for row in reader:
                    UserData.objects.create(
                        date=datetime.strptime(row['Date'], '%m/%d/%Y'),
                        functionalities=[fn.strip() for fn in row['Functionalities'].split(";") if fn],
                        subject_matters=[sm.strip() for sm in row['Subject Matters'].split(";") if sm] if row['Subject Matters'] else [],
                        general_categories=[cat.strip() for cat in row['General Categories'].split(";") if cat] if row['General Categories'] else [],
                        title=row['Title'] if row['Title'].strip() else None,
                        text=row['Text'] if row['Text'].strip() else None,
                        tags=[tag.strip() for tag in row['Tags'].split(";") if tag] if row['Tags'] else [],
                        parents_ids=[pid.strip() for pid in row['Parents IDs'].split(";") if pid] if row['Parents IDs'] else [],
                        children_ids=[cid.strip() for cid in row['Children IDs'].split(";") if cid] if row['Children IDs'] else [],
                        siblings_ids=[sid.strip() for sid in row['Siblings IDs'].split(";") if sid] if row['Siblings IDs'] else [],
                        fields=self.parse_fields_to_json(row['Fields']) if row['Fields'] else [],
                        entry_id=row['Entry ID'],
                        user_id=row['User ID']
                    )
            self.stdout.write(self.style.SUCCESS("Data imported successfully!"))
        except Exception as e:
            self.stdout.write(self.style.ERROR(f"Error: {e}"))


