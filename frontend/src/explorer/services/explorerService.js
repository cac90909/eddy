import axios from "axios";

const API_BASE_URL = "http://localhost:8000/explorer";

const explorerService = {
  initUser: async (userId) => {
    const response = await axios.get(`${API_BASE_URL}/init/`, {
      params: { user_id: userId },
    });
    return response.data;
  },

  filterData: async (columnName, filterCondition, filterValue) => {
    const response = await axios.get(`${API_BASE_URL}/filter`, {
      params: { column_name: columnName, filter_type: filterCondition, filter_value: filterValue },
    });
    return response.data;
  },
};

export default explorerService;
