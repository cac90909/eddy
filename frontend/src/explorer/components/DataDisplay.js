import React from "react";
import RawDisplay from "./RawDisplay";
import EnrichedDisplay from "./EnrichedDisplay";
import MetricDisplay from "./MetricDisplay";
import ListDisplay from "./ListDisplay";

const DataDisplay = ({ data, dataType }) => {
  switch (dataType) {
    case "universal_metric":
      return <MetricDisplay data={data} />;
    case "universal_list":
      return <ListDisplay data={data} />;
    case "universal_enrichment":
      return <EnrichedDisplay data={data} />;
    case "universal_raw":
    default:
      return <RawDisplay data={data} />;
  }
};

export default DataDisplay;
