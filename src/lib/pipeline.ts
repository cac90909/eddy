// src/lib/pipeline.ts
import { Prisma, Universal } from "@prisma/client";
import { db } from "@/db/client";

// 1️⃣ The operators we support
export type FilterOperator =
  | "equals"
  | "contains"
  | "in"
  | "gt"
  | "gte"
  | "lt"
  | "lte";

// 2️⃣ A Filter is just these three pieces
export interface Filter {
  field: keyof Universal;    // e.g. "title" or "date"
  operator: FilterOperator;  // e.g. "contains"
  value: any;                // e.g. "foo" or [1,2,3]
}

// 3️⃣ Takes a base Prisma query + one filter, merges it into `where`
export function applyFilter(
  base: Prisma.UniversalFindManyArgs,
  filter: Filter
): Prisma.UniversalFindManyArgs {
  const { field, operator, value } = filter;
  return {
    ...base,
    where: {
      ...base.where,
      [field]: { [operator]: value },
    },
  };
}

// 4️⃣ Runs all of your operations in sequence, then fetches
export async function runExplorerPipeline(
  operations: Filter[]
): Promise<Universal[]> {
  let query: Prisma.UniversalFindManyArgs = {
    where: { userId: 1 },     // default to your single user
    orderBy: { date: "desc" },
  };

  for (const op of operations) {
    query = applyFilter(query, op);
  }

  return db.universal.findMany(query);
}
