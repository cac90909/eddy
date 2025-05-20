import axios from "axios";

const EXPLORER_BASE_URL = "http://localhost:8000/explorer";

const config = {
  headers: {
    "Content-Type": "application/json",
  },
};

export async function callOperation(operationName, userId, operationArguments, httpMethod) {
  const opArgsStr = JSON.stringify(operationArguments || {});
  let response;
  if (httpMethod === "GET") {
    response = await axios.get(`${EXPLORER_BASE_URL}/`, {
      params: {
        user_id: userId,
        operation_name: operationName,
        operation_arguments: opArgsStr,
      },
    });
  } else if (httpMethod === "POST") {
    response = await axios.post(
      `${EXPLORER_BASE_URL}/`,
      {
        user_id: userId,
        operation_name: operationName,
        operation_arguments: opArgsStr,
      },
      config
    );
  } else if (httpMethod === "PUT") {
    response = await axios.put(
      `${EXPLORER_BASE_URL}/`,
      {
        user_id: userId,
        operation_name: operationName,
        operation_arguments: opArgsStr,
      },
      config
    );
  } else if (httpMethod === "DELETE") {
    response = await axios.delete(`${EXPLORER_BASE_URL}/`, {
      params: {
        user_id: userId,
        operation_name: operationName,
        operation_arguments: opArgsStr,
      },
      ...config,
    });
  }
  return response && response.data ? response.data : {};
}