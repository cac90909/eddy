// src/services/cacheService.js
import axios from "axios";

const CACHE_BASE_URL = "http://localhost:8000/explorer/cache";

const cacheService = {
  getMostRecentCache: async (userId) => {
    console.log("cacheService.getMostRecentCache called with userId:", userId);
    const response = await axios.get(`${CACHE_BASE_URL}/`, {
      params: { user_id: userId }
    });
    console.log("cacheService.getMostRecentCache response:", response.data);
    return response.data;
  }
};

export default cacheService;
