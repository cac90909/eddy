import axios from "axios";
import { useExplorerConfig } from "../../contexts/ExplorerConfigContext";
import { useUser } from "../../contexts/UserSessionContext";

const EXPLORER_BASE_URL = "http://localhost:8000/explorer";

const handleOperation = async (operationName, operationArguments) => {
  // Moved hooks inside the function to ensure they're called in a React component context.
  const { explorerConfig } = useExplorerConfig();
  const { userId } = useUser();

  // Use the provided operationName to find the http_method.
  if (operationName === "start_explorer_session") {
    const httpMethod = "GET"
  } else {
  const httpMethod = explorerConfig.operations_config.find(
    (op) => op.operation_name === operationName
  )?.http_method;
}
  
  const operationArgumentsString = JSON.stringify(operationArguments || {});
  const config = {
    headers: {
      "Content-Type": "application/json",
    },
  };

  console.log(userId, operationName, operationArgumentsString);

  let response;

  if (httpMethod === "GET") {
    response = await axios.get(`${EXPLORER_BASE_URL}/`, {
      params: {
        user_id: userId,
        operation_name: operationName,
        operation_arguments: operationArgumentsString,
      },
    });
  } else if (httpMethod === "POST") {
    response = await axios.post(
      `${EXPLORER_BASE_URL}/`,
      {
        user_id: userId,
        operation_name: operationName,
        operation_arguments: operationArgumentsString,
      },
      config
    );
  } else if (httpMethod === "PUT") {
    response = await axios.put(
      `${EXPLORER_BASE_URL}/`,
      {
        user_id: userId,
        operation_name: operationName,
        operation_arguments: operationArgumentsString,
      },
      config
    );
  } else if (httpMethod === "DELETE") {
    // DELETE passes parameters via query parameters.
    response = await axios.delete(`${EXPLORER_BASE_URL}/`, {
      params: {
        user_id: userId,
        operation_name: operationName,
        operation_arguments: operationArgumentsString,
      },
      ...config,
    });
  }
  const responseData = response && response.data ? response.data : {};
  console.log("ExplorerService.handleOperation response:", responseData);
  return responseData;
};

export default handleOperation;
