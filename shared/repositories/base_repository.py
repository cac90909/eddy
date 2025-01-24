from abc import ABC, abstractmethod

class BaseRepository(ABC):
    @abstractmethod
    def get_data_subset(self, column, val):
        pass

    # @abstractmethod
    # def save_data(self, data):
    #     """Save passed data (entire user dataset)"""
    #     pass
    #
    # @abstractmethod
    # def create_rows(self, data):
    #     """Creates rows based on the passed data (subset of a user's data)"""
    #     pass
    #
    # @abstractmethod
    # def delete_rows(self, row_ids):
    #     """Deletes row containing row_ids"""
    #     pass
    #
    # @abstractmethod
    # def update_rows(self, data, row_ids):
    #     """Updates rows with row_ids to be passed data"""
    #     pass
