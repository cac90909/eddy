import React from "react";
import RawDisplay from "./data_displays/RawDisplay";
import EnrichedDisplay from "./data_displays/EnrichedDisplay";
import MetricDisplay from "./data_displays/MetricDisplay";
import ListDisplay from "./data_displays/ListDisplay";

const DataDisplay = ({ data, dataType }) => {
  switch (dataType) {
    case "metric":
      return <MetricDisplay data={data} />;
    case "list":
      return <ListDisplay data={data} />;
    case "enriched":
      return <EnrichedDisplay data={data} />;
    case "raw":
    default:
      return <RawDisplay data={data} />;
  }
};

export default DataDisplay;
