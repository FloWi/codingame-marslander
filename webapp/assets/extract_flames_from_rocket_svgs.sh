flames=$(xq --xml-output  '.svg | { g: .g.g[] | select(."@id"|startswith("flame"))  } | . += { id: .g."@id"} | { svg: . } ' rocket_*.svg); echo "<svg>$flames</svg>" > flames.svg
