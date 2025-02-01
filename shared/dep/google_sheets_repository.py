from googleapiclient.discovery import build
from google.oauth2.service_account import Credentials
from decouple import config
from .base_repository import BaseRepository

from shared.logger import debug_print

import pandas as pd


class GoogleSheetsRepository(BaseRepository):
    def __init__(self):
        credentials_path = config('GOOGLE_CREDENTIALS_PATH')
        self.spreadsheet_id = config('SPREADSHEET_ID')
        self.credentials = Credentials.from_service_account_file(credentials_path)
        self.sheets_api_service = build('sheets', 'v4', credentials=self.credentials)


    def get_data_subset(self, column, value):
        try:
            result = self.sheets_api_service.spreadsheets().values().get(
                spreadsheetId=self.spreadsheet_id,
                range="Testing!A1:Z1000"
            ).execute()
            values = result.get('values', [])
            df = pd.DataFrame(values[1:], columns=values[0])
            #df[column] = df[column].astype(int)
            filtered_df = df[df[column] == value]
            debug_print(filtered_df.shape, column, value)
            return filtered_df
        except Exception as e:
            print(f"Error fetching data: {e}")
            return []



    # def save_data(self, range_name, data):
    #     try:
    #         body = {'values': data}
    #         response = self.service.spreadsheets().values().update(
    #             spreadsheetId=self.spreadsheet_id,
    #             range=range_name,
    #             valueInputOption="RAW",
    #             body=body
    #         ).execute()
    #         return response
    #     except Exception as e:
    #         print(f"Error saving data: {e}")
    #         return None
