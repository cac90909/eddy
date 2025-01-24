import React, { useEffect, useState } from "react";
import { useLocation } from "react-router-dom";
import explorerService from "../services/explorerService";
import FilterForm from "../components/FilterForm";
import DataTable from "../components/DataTable";

function ExplorerPage() {
  const { state } = useLocation();
  const userId = state?.userId || null;

  const [data, setData] = useState([]);
  const [columns, setColumns] = useState([]);

  useEffect(() => {
    if (!userId) {
      console.log("No userId provided");
      return;
    }

    const fetchData = async () => {
      try {
        console.log("Fetching data for userId:", userId);
        const response = await explorerService.initUser(userId);
        console.log("API Response:", response);

        if (response.data && Array.isArray(response.data)) {
          setData(response.data); // Set the array from the `data` property
          setColumns(Object.keys(response.data[0] || {})); // Extract column headers from the first object
        } else {
          console.error("Unexpected response format:", response);
        }
      } catch (error) {
        console.error("Error fetching data:", error);
      }
    };

    fetchData();
  }, [userId]);

  if (!userId) return <div>No User ID Provided</div>;

  console.log("Data to render:", data);
  console.log("Columns to render:", columns);

  return (
    <div>
      <h1>Explorer</h1>
      <FilterForm onApplyFilter={() => {}} columns={columns} />
      <DataTable data={data} columns={columns} />
    </div>
  );
}

export default ExplorerPage;
