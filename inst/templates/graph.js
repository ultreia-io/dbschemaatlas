(function(el, x, data) {
window.dbTables_columns = data.columns;
window.dbTables_dependencies = data.dependencies;
window.dbTables_usages = data.usages;
window.dbTables_descriptions = data.descriptions;
var layout = document.createElement("div");
layout.className = "atlas-graph-layout";
var graphPanel = document.createElement("div");
graphPanel.className = "atlas-graph-panel";
var detailsPanel = el.parentNode.querySelector(".details-panel");
var legend = el.parentNode.querySelector(".graph-legend");
el.parentNode.insertBefore(layout, el);
layout.appendChild(graphPanel);
graphPanel.appendChild(el);
if (legend) el.querySelector(".vis-network").appendChild(legend);
if (detailsPanel) {
  // A small draggable divider; keep at least 25% of the width for either pane.
  const divider = document.createElement("div");
  divider.className = "graph-divider";
  divider.title = "Drag to resize, or click the divider and use Left/Right arrows.";
  divider.tabIndex = 0;
  divider.setAttribute("role", "separator");
  divider.setAttribute("aria-orientation", "vertical");
  divider.setAttribute("aria-label", "Resize graph and table details");
  divider.setAttribute("aria-valuemin", "25");
  divider.setAttribute("aria-valuemax", "75");
  divider.setAttribute("aria-valuenow", "58");
  layout.appendChild(divider);
  layout.appendChild(detailsPanel);
  let graphShare = 58;
  function resizePanels(percent) {
    graphShare = Math.max(25, Math.min(75, percent));
    graphPanel.style.flexGrow = graphShare;
    detailsPanel.style.flexGrow = 100 - graphShare;
    divider.setAttribute("aria-valuenow", String(Math.round(graphShare)));
    if (window.myNetwork) {
      window.myNetwork.setSize("100%", "100%");
      window.myNetwork.redraw();
    }
  }
  divider.addEventListener("pointerdown", event => {
    event.preventDefault();
    divider.focus();
    divider.setPointerCapture(event.pointerId);
  });
  divider.addEventListener("pointermove", event => {
    if (!divider.hasPointerCapture(event.pointerId)) return;
    const width = layout.clientWidth - divider.offsetWidth;
    resizePanels(100 * (event.clientX - layout.getBoundingClientRect().left) / width);
  });
  divider.addEventListener("pointerup", event => {
    if (divider.hasPointerCapture(event.pointerId)) divider.releasePointerCapture(event.pointerId);
  });
  divider.addEventListener("keydown", event => {
    if (event.key !== "ArrowLeft" && event.key !== "ArrowRight") return;
    event.preventDefault();
    resizePanels(graphShare + (event.key === "ArrowLeft" ? -2 : 2));
  });
}

// Hover previews the legend; clicking pins it until clicked again.
if (legend) {
  let legendPinned = false;
  const helpButton = legend.querySelector("summary");
  legend.addEventListener("mouseenter", () => { legend.open = true; });
  legend.addEventListener("mouseleave", () => { legend.open = legendPinned; });
  helpButton.addEventListener("click", event => {
    event.preventDefault();
    legendPinned = !legendPinned;
    legend.open = legendPinned || legend.matches(":hover");
  });
  legend.addEventListener("toggle", () => {
    helpButton.setAttribute("aria-expanded", String(legend.open));
  });
  document.addEventListener("keydown", event => {
    if (event.key === "Escape") { legendPinned = false; legend.open = false; }
  });
}

function selectNode(nodeId) {
  if (!window.myNetwork || !Object.prototype.hasOwnProperty.call(window.dbTables_columns, nodeId)) return;
  window.myNetwork.setSelection( { nodes: [nodeId] });
  navigateToNode(nodeId);
}

function navigateToNode(nodeId) {
  showNodeDetails(nodeId);
  highlightNeighborhood(nodeId)
}

function handleHashChange() {
  const nodeId = decodeURIComponent( window.location.hash.substring(1) );
  if (!nodeId) clearNodeDetails(); else selectNode(nodeId);
}
function highlightNeighborhood(nodeId) {
    const connectedEdges = window.myNetwork.getConnectedEdges(nodeId);
    const edges = window.myNetwork.body.data.edges;
    edges.get().forEach(edge => { edges.update({ id: edge.id, width: 1 }); });
    connectedEdges.forEach(edgeId => { edges.update({ id: edgeId, width: 2 });});
}
function fitGraph() {
  // The built-in fit adds 10% padding; this leaves about 4% around the graph.
  window.myNetwork.fit({ animation: false, maxZoomLevel: 10 });
  window.myNetwork.moveTo({ scale: window.myNetwork.getScale() * 1.06, animation: false });
}

function initialiseGraph() {
  if (!window.myNetwork) {
    window.requestAnimationFrame(initialiseGraph);
    return;
  }
  window.myNetwork.setSize("100%", "100%");
  fitGraph();
  // Keep the standard fit icon, using the same compact fit as the initial view.
  const fitButton = el.querySelector(".vis-zoomExtends");
  const compactFitButton = fitButton.cloneNode(true);
  fitButton.replaceWith(compactFitButton);
  compactFitButton.addEventListener("click", fitGraph);
  for (const [selector, label] of [
    [".vis-zoomIn", "Zoom in"],
    [".vis-zoomOut", "Zoom out"],
    [".vis-zoomExtends", "Fit graph"]
  ]) {
    const button = el.querySelector(selector);
    button.title = label;
    button.setAttribute("aria-label", label);
  }
  window.addEventListener("hashchange", handleHashChange);
  handleHashChange();
}
if (document.readyState === "complete") initialiseGraph();
else window.addEventListener("load", initialiseGraph);

function clearNodeDetails() {
  document.getElementById('table-details').innerHTML = '<p>Select a node in the graph.</p>';
  const edges = window.myNetwork.body.data.edges;
  edges.get().forEach(edge => { edges.update({ id: edge.id, width: 1 }); });
}

/**
 * Parse language-tagged text values.
 *
 * Extracts text blocks prefixed by language tags such as `[EN]` or `[FR]`.
 * If no language tag is detected, the full input is returned as English.
 *
 * @param {string} text - Text containing one or more language-tagged blocks.
 * @returns {Object.<string, string>} Object keyed by language code.
 */
function parseLangValues(text) {
  const regex = /(?:^|\n)[ \t]*(?:\[([A-Z]{2})\][ \t]*:?|([A-Z]{2}):)[ \t]*([\s\S]*?)(?=\n[ \t]*(?:\[[A-Z]{2}\]|[A-Z]{2}:)|$)/g;
  const result = {};

  let match;

  while ((match = regex.exec(text)) !== null) {
    const lang = match[1] || match[2];
    const value = match[3].trim();

    result[lang] = value;
  }

  if (Object.keys(result).length === 0) {
    result.EN = text.trim();
  }

  return result;
}

function renderDescription(description) {
  if (!description || description === '') {
    return '<p class="error"><b><i>Not filled</i></b></p>';
  }

  const descriptionPerLanguage = parseLangValues(String(description));

  const languages = Object.keys(descriptionPerLanguage);
  if (languages.includes("EN")) {
    languages.splice(languages.indexOf("EN"), 1);
    languages.unshift("EN");
  }
  const result = languages.map(lang => {
    const badgeClass = lang === "EN" ? "bg-primary" : "bg-secondary";
    return `<div class="description-language"><span class="badge ${badgeClass}">${escapeHtml(lang)}</span>&nbsp;<b><i>${escapeHtml(descriptionPerLanguage[lang])}</i></b></div>`;
  }).join('');

  return result;
}

function showNodeDetails(nodeId) {
  var columns = window.dbTables_columns[nodeId] || [];
  var dependencies = window.dbTables_dependencies[nodeId] || [];
  var usages = window.dbTables_usages[nodeId] || [];
  var description = renderDescription(window.dbTables_descriptions[nodeId] || '');
  var html = `
  <h3>Table <b><i>${nodeId}</i></b></h3>
  <h4>Description</h4>
  <blockquote class="table-description">${description}</blockquote>
  ${generate_table_columns(nodeId, columns)}
  ${generate_table_dependencies(nodeId, dependencies)}
  ${generate_table_usages(nodeId, usages)}
  `;
  document.getElementById( 'table-details' ).innerHTML = html;
  document.querySelector('.details-panel') ?.scrollTo({ top: 0, behavior: 'smooth' });
}

function generate_table_columns(nodeId, data) {
  const rows = data.map(datum => `
    <tr${isTrue(datum.primary_key) ? ' class="primary_key"' : ''}>
      <td>${renderColumnName(datum.column, datum.mandatory, datum.primary_key, datum.foreign_key)}</td>
      <td>${escapeHtml(datum.type)}</td>
      <td>${renderDescription(datum.description)}</td>
    </tr>
  `).join('');

  return `
    <h4>Columns</h4>
    <table class="columns-table">
      <thead>
        <tr>
          <th>Column</th>
          <th>Type</th>
          <th>Description</th>
        </tr>
      </thead>
      <tbody>
        ${rows}
      </tbody>
    </table>
  `;
}
function patchReportAnchors(html) {
  if (!html || html.length === 0) { return html; }
  return html.replace( /href=(['"])#table_([^'"]+)\1/g, 'href=$1#$2$1' );
}

function generate_table_dependencies(nodeId, data) {
  if (data.length == 0) {
  return `
    <h4>Dependencies</h4>
    <p>No dependency</p>
  `;
  }
  const rows = data.map(datum => `
    <tr${isTrue(datum.primary_key) ? ' class="mandatory"' : ''}>
      <td>${renderColumnNames(datum.columns, datum.mandatory, datum.primary_key, false)}</td>
      <td>${datum.dependency_type}</td>
      <td>${patchReportAnchors(datum.dependency_table)}</td>
      <td>${renderColumnNames(datum.dependency_columns, datum.dependency_mandatory, datum.dependency_primary_key, false)}</td>
    </tr>
  `).join('');
  return `
    <h4>Dependencies</h4>
    <table>
      <thead>
        <tr>
          <th>Column</th>
          <th>Relation type</th>
          <th>Dependency table</th>
          <th>Dependency column</th>
        </tr>
      </thead>
      <tbody>
        ${rows}
      </tbody>
    </table>
  `;
}

function generate_table_usages(nodeId, data) {
  if (data.length == 0) {
  return `
    <h4>Usages</h4>
    <p>No usage</p>
    <br/>
  `;
  }
  const rows = data.map(datum => `
    <tr>
      <td>${renderColumnNames(datum.columns, datum.mandatory, datum.primary_key, false)}</td>
      <td>${datum.usage_type}</td>
      <td>${patchReportAnchors(datum.usage_table)}</td>
      <td>${renderColumnNames(datum.usage_columns, datum.usage_mandatory, datum.usage_primary_key, false)}</td>
    </tr>
  `).join('');

  return `
    <h4>Usages</h4>
    <table>
      <thead>
        <tr>
          <th>Column</th>
          <th>Relation type</th>
          <th>Usage table</th>
          <th>Usage column</th>
        </tr>
      </thead>
      <tbody>
        ${rows}
      </tbody>
    </table>
  `;
}

function hasValue(value) {
  return value !== null && value !== undefined && String(value).trim() !== "";
}

function isTrue(value) {
  return value === true || value === "TRUE" || value === "true" || value === "YES";
}

function iconSpan(className, title) {
  return `<span class="${className}" title="${title}" aria-hidden="true"></span>`;
}

function escapeHtml(value) {
  if (value === null || value === undefined) return "";
  return String(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
}

function renderColumnNames(columns, mandatory, primary_key, foreign_key) {
  const cols = Array.isArray(columns) ? columns : [columns];
  const mandatoryValues = Array.isArray(mandatory) ? mandatory : cols.map(() => mandatory);
  const pkValues = Array.isArray(primary_key) ? primary_key : cols.map(() => primary_key);
  const fkValues = Array.isArray(foreign_key) ? foreign_key : cols.map(() => foreign_key);

  return cols.map((column, index) =>
    renderColumnName(
      column,
      mandatoryValues[index],
      pkValues[index],
      fkValues[index]
    )
  ).join("<br/>");
}

function renderColumnName(column, mandatory, primary_key, foreign_key) {
  const is_mandatory = isTrue(mandatory);
  const is_pk = isTrue(primary_key);
  const is_fk = isTrue(foreign_key);
  const not_null = is_mandatory ? iconSpan("mandatory-icon", "Mandatory") : "";
  const pk = is_pk ? iconSpan("pk-icon", "Primary Key") : "";
  const fk = is_fk ? iconSpan("fk-icon", "Foreign Key") : "";
  const name = escapeHtml(column);
  const label = is_pk  ? `<span class="pk-column">${name}</span>` : name;
  return `${pk}${fk}${not_null}${label}`;
}
})
