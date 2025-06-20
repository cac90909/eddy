from collections import deque
from typing import Any

def bfs(neighbor_map: dict[Any, list[Any]], start: Any) -> set[Any]:
    visited, queue = set(), deque([start])
    while queue:
        node = queue.popleft()
        if node in visited: continue
        visited.add(node)
        queue.extend(n for n in neighbor_map.get(node, []) if n not in visited)
    return visited