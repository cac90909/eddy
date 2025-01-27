import axios from "axios";

const API_BASE_URL = "http://localhost:8000/explorer";

//Object that maps keys to functions (backend api calls) with the relevant parameters
const explorerService = {
  initUser: async (userId) => {
  console.log("explorer service: calling init user")
    const response = await axios.get(`${API_BASE_URL}/init/`, {
      params: { user_id: userId },
    });
    return response.data;
  },

  filterData: async (columnName, filterCondition, filterValue) => {
  console.log("explorer service: calling filter")
    const response = await axios.get(`${API_BASE_URL}/filter/`, {
      params: { column_name: columnName, filter_type: filterCondition, filter_value: filterValue },
    });
    return response.data;
  },
};

export default explorerService;
