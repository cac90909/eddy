from shared.repositories.google_sheets_repository import GoogleSheetsRepository
from shared.pandas_adapter import PandasAdapter

from shared.data_operations import DataOperationsService
from explorer.services.cache_service import CacheManager
from explorer.services.explorer_service import ExplorerService





def create_explorer_service():
    repository = GoogleSheetsRepository()
    adapter = PandasAdapter()
    data_operations_service = DataOperationsService(repository=repository, adapter=adapter)
    explorer_service = ExplorerService(data_operations_service=data_operations_service, cache_service=CacheManager)
    return explorer_service
