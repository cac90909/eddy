import React from "react";
import DateFilter from "./filters/DateFilter";
import DropdownFilter from "./filters/DropdownFilter";
import TextFilter from "./filters/TextFilter";

function FilterPanel({ onApplyFilter, columns }) {
  return (
    <div>
      <h3>Filters</h3>
      <DateFilter column="date" onApplyFilter={onApplyFilter} />
      <DropdownFilter column="functionalities" options={["Log", "Task", "Project", "Idea", "Journal", "Note"]} onApplyFilter={onApplyFilter} />
      <DropdownFilter column="subject_matters" dynamicOptions columns={columns} onApplyFilter={onApplyFilter} />
      <DropdownFilter column="general_categories" dynamicOptions columns={columns} onApplyFilter={onApplyFilter} />
      <DropdownFilter column="tags" dynamicOptions columns={columns} onApplyFilter={onApplyFilter} />
      <TextFilter column="title" onApplyFilter={onApplyFilter} />
      <TextFilter column="text" onApplyFilter={onApplyFilter} />
      {/* Placeholder for Fields */}
      <p>Fields filter coming soon!</p>
    </div>
  );
}

export default FilterPanel;
