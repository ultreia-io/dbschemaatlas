# dbschemaatlas 0.2.1

- Display mandatory, primary-key and foreign-key icons beside report column names, with an icon legend.
- Deploy release websites through develop using the released tag as the site source.

# dbschemaatlas 0.2.0

- Restore interactive reports with all rows visible and their natural order preserved.
- Show missing comments in red, language badges, and quoted schema and table descriptions.
- Restore dependency graphs with schema colours, directed relationships and linked table details.
- Add a legend for column icons, node colours and relationships, with hover preview and click-to-pin.
- Add matching zoom, fit and help icons with descriptive tooltips.
- Improve graph fitting and provide a draggable divider with keyboard resizing between graph and details.
- Refine detail tables with unwrapped column names, compact type columns and full-height scrolling.
- Embed report and graph presentation assets for offline viewing.

# dbschemaatlas 0.1.0

- Extract PostgreSQL schema metadata through caller-supplied DBI connections.
- Save and load portable metadata snapshots with composite-key support.
- Build dependency and reverse-usage models, trees, graphs, and offline HTML reports.
- Delegate connection lifecycle, query execution, and SQL loading to `dbiutils`.
- License the package under LGPL-2.1, matching `dbiutils`.
