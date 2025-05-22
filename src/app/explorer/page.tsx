// src/app/explorer/page.tsx
"use client";
import { useState, useEffect } from "react";
import type { Filter } from "@/lib/pipeline";
import ResultsTable from "@/components/ResultsTable";

export default function ExplorerPage() {
  const [ops, setOps] = useState<Filter[]>([]);
  const [rows, setRows] = useState<any[]>([]);

  useEffect(() => {
    fetch("/api/explorer", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ operations: ops }),
    })
      .then((r) => r.json())
      .then((data) => setRows(data.rows));
  }, [ops]);

  return (
    <div className="flex flex-col h-screen">
      {/* Top Section */}
      <div className="p-4 bg-gray-50 flex space-x-2">
        <button className="px-3 py-1 bg-blue-500 text-white rounded">Op 1</button>
        <button className="px-3 py-1 bg-blue-500 text-white rounded">Op 2</button>
        <button className="px-3 py-1 bg-blue-500 text-white rounded">Op 3</button>
      </div>

      {/* Middle Section */}
      <div className="flex flex-1 overflow-hidden">
        {/* Left 30%: Operations Pane */}
        <div className="w-1/3 p-4 border-r overflow-auto">
          <h2 className="text-xl font-semibold mb-2">Operations</h2>
          {/* TODO: Add forms and buttons to push new filters via setOps */}
        </div>

        {/* Right 70%: Results Table */}
        <div className="flex-1 p-4 overflow-auto">
          <ResultsTable items={rows} />
        </div>
      </div>

      {/* Bottom Section */}
      <div className="p-4 bg-gray-100">
        <p className="text-sm">
          Showing <strong>{rows.length}</strong> rows
        </p>
      </div>
    </div>
  );
}


