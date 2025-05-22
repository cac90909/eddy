// src/app/api/universal/route.ts
import { NextResponse } from "next/server";
import { getFullUniversal } from "@/lib/universal";
import { runExplorerPipeline, Filter } from "@/lib/pipeline";

export async function GET() {
  const universals = await getFullUniversal();  // defaults to ID=1
  return NextResponse.json(universals);
}


export async function POST(req: Request) {
  // 1) parse the operations array from the request body
  const { operations } = (await req.json()) as { operations: Filter[] };

  // 2) execute the pipeline
  const rows = await runExplorerPipeline(operations);

  // 3) return both the results and the ops (so the UI can echo them)
  return NextResponse.json({ rows, operations });
}

