// src/app/page.tsx
import Link from "next/link";

export default function HomePage() {
  const items = [
    { href: "/explorer",   title: "Explorer",   description: "Browse and query your Universal data." },
    { href: "/metrics",    title: "Metrics",    description: "Define and track your KPIs." },
    { href: "/dashboards", title: "Dashboards", description: "Visualize your metrics & datasets." },
  ];

  return (
    <main className="min-h-screen flex flex-col items-center justify-center p-8 space-y-6">
      <h1 className="text-4xl font-bold">Welcome to Eddy</h1>
      <div className="grid grid-cols-1 sm:grid-cols-3 gap-6 w-full max-w-4xl">
        {items.map(({ href, title, description }) => (
          <Link
            key={href}
            href={href}
            className="block p-6 border rounded-2xl shadow hover:shadow-lg transition"
          >
            <h2 className="text-2xl font-semibold mb-2">{title} →</h2>
            <p className="text-sm opacity-80">{description}</p>
          </Link>
        ))}
      </div>
    </main>
  );
}
