import csv
from core.models import UserData
from datetime import datetime

from django.contrib.auth import get_user_model

User = get_user_model()

def export_data_native_format(output_file_path):
    """
    Exports data from the user_data_test table in native format, retaining JSON structure.
    """
    try:
        # Fetch all rows from the table
        data = UserData.objects.all().values()

        # Open the output file
        with open(output_file_path, mode='w', newline='', encoding='utf-8') as file:
            writer = csv.DictWriter(file, fieldnames=list(data[0].keys()))
            writer.writeheader()

            for row in data:
                writer.writerow(row)
        print(f"Data successfully exported in native format to {output_file_path}.")
    except Exception as e:
        print(f"Error exporting data: {e}")


def export_data_csv_format(output_file_path):
    """
    Exports data from the user_data_test table, converting lists to semicolon-separated strings.
    """
    def convert_list_to_string(value):
        """
        Converts a list into a semicolon-separated string.
        Returns an empty string if the value is None.
        """
        if isinstance(value, list):
            return ";".join(map(str, value))
        return value or ""

    try:
        # Fetch all rows from the table
        data = UserData.objects.all().values()

        # Open the output file
        with open(output_file_path, mode='w', newline='', encoding='utf-8') as file:
            writer = csv.DictWriter(file, fieldnames=list(data[0].keys()))
            writer.writeheader()

            for row in data:
                # Convert list fields to semicolon-separated strings
                row["functionality"] = convert_list_to_string(row["functionality"])
                row["subject_matter"] = convert_list_to_string(row["subject_matter"])
                row["general_categories"] = convert_list_to_string(row["general_categories"])
                row["parent_id"] = convert_list_to_string(row["parent_id"])
                row["child_id"] = convert_list_to_string(row["child_id"])
                row["siblings"] = convert_list_to_string(row["siblings"])
                row["tags"] = convert_list_to_string(row["tags"])

                writer.writerow(row)
        print(f"Data successfully exported in CSV format to {output_file_path}.")
    except Exception as e:
        print(f"Error exporting data: {e}")


# Usage Example
# Generate the filename with timestamp
current_time = datetime.now().strftime("%Y%m%d_%H%M%S")
native_output_path = f"./output_native_{current_time}.csv"
csv_output_path = f"./output_conv_{current_time}.csv"

export_data_native_format(native_output_path)
export_data_csv_format(csv_output_path)
