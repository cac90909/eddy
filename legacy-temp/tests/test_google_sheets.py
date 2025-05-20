import pytest
from shared.repositories import GoogleSheetsRepository
from shared.logger import get_logger

# Create a logger for this module
logger = get_logger(__name__, log_file="test_logs.log")
logger.info(f"Logger Name: {logger.name}")
logger.info(f"Logger Level: {logger.level}")
logger.info(f"Logger Handlers: {[type(h).__name__ for h in logger.handlers]}")

@pytest.fixture
def google_sheets_repo():
    """Fixture to initialize the GoogleSheetsRepository."""
    return GoogleSheetsRepository()

def test_get_data(google_sheets_repo):
    """Test fetching data from Google Sheets."""
    data = google_sheets_repo.get_data("Sheet1!A1:D10")
    assert data is not None, "Data fetch returned None; expected a list of rows."
    assert len(data) > 0, "Data fetch returned an empty list; expected at least one row."
    logger.info(f"Fetched Data: {data}")

def test_save_data(google_sheets_repo):
    """Test saving data to Google Sheets."""
    new_data = [["Pytest", "Testing"], ["Django", "Integration"]]
    response = google_sheets_repo.save_data("Sheet1!A1:B2", new_data)
    assert response is not None  # Check that response is valid
    assert "updatedCells" in response  # Check API responded with expected field
    logger.info(f"Save Response: {response}")

