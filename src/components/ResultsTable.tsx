// src/components/ResultsTable.tsx
"use client";
import React from "react";
import type { Universal } from "@prisma/client";

interface ResultsTableProps {
  items: Universal[];
}

export default function ResultsTable({ items }: ResultsTableProps) {
  if (items.length === 0) {
    return <p className="p-4 text-center">No results found.</p>;
  }

  // `keyof Universal` creates a union of ALL valid keys on Universal
  // `(keyof Universal)[]` means "an array of those keys"
  const columns: (keyof Universal)[] = [
    "id", "date", "functionalities", "subject_matters", "general_categories",
    "title", "text", "tags", "parents_ids", "children_ids", "siblings_ids",
    "fields", "entry_id",
  ];

  return (
    <div className="min-h-full">
      <table className="min-w-max table-auto border-collapse whitespace-nowrap">
        <thead>
          <tr className="bg-gray-100">
            {columns.map((col) => (
              <th key={col} className="px-4 py-2 text-left font-medium">
                {col.replace(/_/g, " ")}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {items.map((item) => (
            <tr key={item.id} className="even:bg-gray-50">
              {columns.map((col) => (
                <td key={col} className="px-4 py-2 align-top">
                  {formatCell(item[col])}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function formatCell(value: any) {
  if (Array.isArray(value)) return value.join(", ");
  if (typeof value === "object" && value !== null) return JSON.stringify(value);
  if (value === null || value === undefined) return "—";
  return String(value);
}
