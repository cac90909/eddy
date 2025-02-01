#To be worked on when I switch over from google sheets to a different DB
from .base_repository import BaseRepository

class DatabaseRepository(BaseRepository):
    def __init__(self, connection):
        self.connection = connection

    def get_data(self, query):
        # Implement database-specific logic here
        pass

    def save_data(self, data):
        # Implement database-specific logic here
        pass
