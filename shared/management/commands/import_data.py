import os
import csv
from django.core.management.base import BaseCommand
from shared.models import Universal
from datetime import datetime
from django.contrib.auth import get_user_model

User = get_user_model()

class Command(BaseCommand):
    help = "Import user data from a CSV file"

    def add_arguments(self, parser):
        parser.add_argument(
            '--file_path',
            type=str,
            default="shared/postgres/test_data.csv",
            help="Path to the CSV file"
        )

    def parse_fields_to_json(self, fields_string, delimiter="--"):
        """
        Parse a fields string with a custom delimiter into a JSON object.
        """
        if not fields_string:
            return None
        try:
            fields_dict = {
                item.split(":")[0].strip(): item.split(":")[1].strip()
                for item in fields_string.split(delimiter)
            }
            return fields_dict
        except Exception:
            return None  # Return None if parsing fails

    def handle(self, *args, **kwargs):
        file_path = kwargs['file_path']

        if not os.path.exists(file_path):
            self.stdout.write(self.style.ERROR(f"File not found: {file_path}"))
            return

        try:
            with open(file_path, mode="r") as file:
                reader = csv.DictReader(file)
                # Delete existing data
                Universal.objects.all().delete()
                # Get the default user only once before the loop
                default_user = User.objects.get(pk=1)
                for row in reader:
                    Universal.objects.create(
                        date=datetime.strptime(row['Date'], '%m/%d/%Y'),
                        functionalities=[fn.strip() for fn in row['Functionalities'].split(";") if fn],
                        subject_matters=[sm.strip() for sm in row['Subject Matters'].split(";") if sm] if row['Subject Matters'] else [],
                        general_categories=[cat.strip() for cat in row['General Categories'].split(";") if cat] if row['General Categories'] else [],
                        title=row['Title'].strip() if row['Title'].strip() else None,
                        text=row['Text'].strip() if row['Text'].strip() else None,
                        tags=[tag.strip() for tag in row['Tags'].split(";") if tag] if row['Tags'] else [],
                        parents_ids=[pid.strip() for pid in row['Parents IDs'].split(";") if pid] if row['Parents IDs'] else [],
                        children_ids=[cid.strip() for cid in row['Children IDs'].split(";") if cid] if row['Children IDs'] else [],
                        siblings_ids=[sid.strip() for sid in row['Siblings IDs'].split(";") if sid] if row['Siblings IDs'] else [],
                        fields=self.parse_fields_to_json(row['Fields']) if row['Fields'] else None,
                        entry_id=row['Entry ID'],
                        user=default_user
                    )
            self.stdout.write(self.style.SUCCESS("Data imported successfully!"))
        except Exception as e:
            self.stdout.write(self.style.ERROR(f"Error: {e}"))
