// src/services/snapshotService.js
import axios from "axios";

const SNAPSHOT_BASE_URL = "http://localhost:8000/explorer/snapshots";

// Currently logic for obtaining the Operation Chain lives on backend
const snapshotService = {
  getAllSnapshots: async (userId) => {
    console.log("snapshotService.getAllSnapshots called with userId:", userId);
    const response = await axios.get(`${SNAPSHOT_BASE_URL}/`, {
      params: { user_id: userId, get_type: "all" }
    });
    console.log("snapshotService.getAllSnapshots response:", response.data);
    return response.data;
  },

  getSnapshot: async (userId, snapshotId) => {
    console.log("snapshotService.getSnapshot called with:", { userId, snapshotId });
    const response = await axios.get(`${SNAPSHOT_BASE_URL}/`, {
      params: { user_id: userId, get_type: "one", snapshot_id: snapshotId }
    });
    console.log("snapshotService.getSnapshot response:", response.data);
    return response.data;
  },

  createSnapshot: async (userId, title, description) => {
    console.log("snapshotService.createSnapshot called with:", { userId, title, description });
    const response = await axios.post(`${SNAPSHOT_BASE_URL}/`, {
      user_id: userId,
      title,
      description,
      operation_chain: operationChain
    });
    console.log("snapshotService.createSnapshot response:", response.data);
    return response.data;
  },

  updateSnapshot: async (userId, snapshotId, title, description) => {
    console.log("snapshotService.updateSnapshot called with:", { userId, snapshotId, title, description });
    const response = await axios.put(`${SNAPSHOT_BASE_URL}/`, {
      user_id: userId,
      snapshot_id: snapshotId,
      title,
      description,
      operation_chain: operationChain
    });
    console.log("snapshotService.updateSnapshot response:", response.data);
    return response.data;
  },

  deleteSnapshot: async (userId, snapshotId) => {
    console.log("snapshotService.deleteSnapshot called with:", { userId, snapshotId });
    // For DELETE, axios expects data to be in the config object.
    const response = await axios.delete(`${SNAPSHOT_BASE_URL}/`, {
      data: { user_id: userId, snapshot_id: snapshotId }
    });
    console.log("snapshotService.deleteSnapshot response:", response.data);
    return response.data;
  }
};

export default snapshotService;
