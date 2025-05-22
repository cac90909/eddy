// src/components/UniversalList.tsx
"use client";

import type { Universal } from "@prisma/client";

export default function UniversalList({ items }: { items: Universal[] }) {
  return (
    <ul>
      {items.map(u => (
        <li key={u.id}>
          <h3>{u.title}</h3>
          <p>{u.text}</p>
          {/* … */}
        </li>
      ))}
    </ul>
  );
}
