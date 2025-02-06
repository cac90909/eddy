// src/services/explorerService.js
import axios from "axios";

const EXPLORER_BASE_URL = "http://localhost:8000/explorer";

const explorerService = {
  initUser: async (userId) => {
    console.log("explorerService.initUser called with userId:", userId);
    const response = await axios.get(`${EXPLORER_BASE_URL}/`, {
      params: {
        user_id: userId,
        operation_type: "init_user",
        operation_params: "{}"
      }
    });
    console.log("explorerService.initUser response:", response.data);
    return response.data;
  },

  getUniqueFilterOptions: async (userId) => {
    console.log("explorerService.getUniqueFilterOptions called with userId:", userId);
    const response = await axios.get(`${EXPLORER_BASE_URL}/`, {
      params: {
        user_id: userId,
        operation_type: "filter_options",
        operation_params: "{}"
      }
    });
    console.log("explorerService.getUniqueFilterOptions response:", response.data);
    return response.data;
  },

  filterData: async (userId, columnName, filterValue, filterType) => {
    console.log("explorerService.filterData called with:", { userId, columnName, filterValue, filterType });
    const operationParams = { column_name: columnName, filter_value: filterValue, filter_type: filterType };
    const response = await axios.get(`${EXPLORER_BASE_URL}/`, {
      params: {
        user_id: userId,
        operation_type: "filter",
        operation_params: JSON.stringify(operationParams)
      }
    });
    console.log("explorerService.filterData response:", response.data);
    return response.data;
  },

  traverseData: async (userId, startId, columnName) => {
    console.log("explorerService.traverseData called with:", { userId, startId, columnName });
    const operationParams = { start_id: startId, column_name: columnName };
    const response = await axios.get(`${EXPLORER_BASE_URL}/`, {
      params: {
        user_id: userId,
        operation_type: "traverse",
        operation_params: JSON.stringify(operationParams)
      }
    });
    console.log("explorerService.traverseData response:", response.data);
    return response.data;
  }
};

export default explorerService;


// import axios from "axios";

// const API_BASE_URL = "http://localhost:8000/explorer";

// //Object that maps keys to functions (backend api calls) with the relevant parameters
// const explorerService = {
//   initUser: async (userId) => {
//   console.log("explorer service: calling init user")
//     const response = await axios.get(`${API_BASE_URL}/`, {
//       params: { user_id: userId },
//     });
//     return response.data;
//   },

//   filterData: async (columnName, filterCondition, filterValue) => {
//   console.log("explorer service: calling filter")
//     const response = await axios.get(`${API_BASE_URL}/`, {
//       params: { column_name: columnName, filter_type: filterCondition, filter_value: filterValue },
//     });
//     return response.data;
//   },

//   loadSnapshot: async (userId, snapshotId) => {
//     console.log("explorer service: calling load snapshot")
//       const response = await axios.get(`${API_BASE_URL}/filter/`, {
//         params: { column_name: columnName, filter_type: filterCondition, filter_value: filterValue },
//       });
//       return response.data;
//     },
// };

// export default explorerService;
