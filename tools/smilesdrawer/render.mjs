#!/usr/bin/env node
/*
 * Development-only SmilesDrawer reference generator.
 *
 * This script is intentionally not part of the runtime or of `eldev test`.
 * It requires the local ignored `smilesDrawer/` checkout and the jsdom
 * dependency installed in this directory.
 */

import { execFileSync } from "node:child_process";
import { JSDOM } from "jsdom";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import SmilesDrawer from "../../smilesDrawer/dist/smiles-drawer.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const root = resolve(here, "../..");
const checkout = resolve(root, "smilesDrawer");
const fixtureDir = resolve(root, "test/fixtures/smilesdrawer");
const manifestPath = resolve(fixtureDir, "manifest.json");

const dom = new JSDOM("<!doctype html><html><body></body></html>");
Object.assign(globalThis, {
  window: dom.window,
  document: dom.window.document,
  SVGElement: dom.window.SVGElement,
  SVGSVGElement: dom.window.SVGSVGElement,
  HTMLCanvasElement: dom.window.HTMLCanvasElement,
});
// jsdom has no canvas implementation. SmilesDrawer then uses its deterministic
// estimateTextSize() fallback instead of a platform-dependent canvas metric.
dom.window.HTMLCanvasElement.prototype.getContext = () => null;

let randomState = 0x12345678;
function resetRandom(seed) {
  randomState = seed >>> 0;
}
Math.random = () => {
  randomState = (randomState + 0x6d2b79f5) >>> 0;
  let t = randomState;
  t = Math.imul(t ^ (t >>> 15), t | 1);
  t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};

const proto = SmilesDrawer.SvgDrawer.prototype;
const originalDrawEdge = proto.drawEdge;
const originalDrawAromaticityRing = proto.drawAromaticityRing;

proto.drawEdge = function (edgeId, debug) {
  const before = this.svgWrapper.paths.length;
  originalDrawEdge.call(this, edgeId, debug);
  for (let i = before; i < this.svgWrapper.paths.length; i++) {
    this.svgWrapper.paths[i].setAttribute("data-edge", String(edgeId));
  }
};

proto.drawAromaticityRing = function (ring) {
  const before = this.svgWrapper.paths.length;
  originalDrawAromaticityRing.call(this, ring);
  for (let i = before; i < this.svgWrapper.paths.length; i++) {
    this.svgWrapper.paths[i].setAttribute("data-ring", String(ring.id));
  }
};

function number(value) {
  return Number(Number(value).toFixed(6));
}

function attribute(element, name) {
  const value = element.getAttribute(name);
  return value === null ? null : value;
}

function mapBondType(type) {
  return {
    "-": "single",
    "=": "double",
    "#": "triple",
    ":": "aromatic",
  }[type] ?? "unknown";
}

function mapStereo(wedge) {
  return {
    up: "up",
    down: "down",
  }[wedge] ?? "none";
}

function parseTransform(value) {
  if (!value) return null;
  const match = value.match(/translateX\(([-\d.eE+]+)px\).*translateY\(([-\d.eE+]+)px\)/);
  if (!match) return null;
  return { x: Number(match[1]), y: Number(match[2]) };
}

function nearestAtom(atoms, point) {
  let best = null;
  let bestDistance = Infinity;
  for (const atom of atoms) {
    const distance = (atom.x - point.x) ** 2 + (atom.y - point.y) ** 2;
    if (distance < bestDistance) {
      bestDistance = distance;
      best = atom;
    }
  }
  return best?.id ?? null;
}

function primitiveFor(element) {
  const kind = element.tagName.toLowerCase();
  if (kind === "line") {
    return {
      kind: "line",
      edge: attribute(element, "data-edge") === null ? null : Number(attribute(element, "data-edge")),
      ring: attribute(element, "data-ring") === null ? null : Number(attribute(element, "data-ring")),
      x1: number(element.getAttribute("x1")),
      y1: number(element.getAttribute("y1")),
      x2: number(element.getAttribute("x2")),
      y2: number(element.getAttribute("y2")),
      stroke: attribute(element, "stroke"),
      strokeWidth: number(element.getAttribute("stroke-width")),
      dash: attribute(element, "stroke-dasharray"),
    };
  }
  if (kind === "polygon") {
    return {
      kind: "polygon",
      edge: attribute(element, "data-edge") === null ? null : Number(attribute(element, "data-edge")),
      ring: attribute(element, "data-ring") === null ? null : Number(attribute(element, "data-ring")),
      points: (element.getAttribute("points") ?? "")
        .trim()
        .split(/\s+/)
        .filter(Boolean)
        .map((pair) => pair.split(",").map(Number)),
      fill: attribute(element, "fill"),
    };
  }
  if (kind === "circle") {
    return {
      kind: "circle",
      edge: attribute(element, "data-edge") === null ? null : Number(attribute(element, "data-edge")),
      ring: attribute(element, "data-ring") === null ? null : Number(attribute(element, "data-ring")),
      cx: number(element.getAttribute("cx")),
      cy: number(element.getAttribute("cy")),
      r: number(element.getAttribute("r")),
      stroke: attribute(element, "stroke"),
      fill: attribute(element, "fill"),
    };
  }
  return { kind };
}

function labelFor(atom, group) {
  const text = group.querySelector("text");
  if (!text) return null;
  const transform = parseTransform(group.getAttribute("style"));
  const point = transform ?? { x: 0, y: 0 };
  return {
    atom,
    text: text.textContent,
    parts: [...text.querySelectorAll("tspan")].map((part) => ({
      text: part.textContent,
      fill: attribute(part, "fill"),
    })),
    direction: attribute(text, "data-direction"),
    x: number(point.x),
    y: number(point.y),
    transform: group.getAttribute("style"),
  };
}

function oracleCommit() {
  try {
    return execFileSync("git", ["-C", checkout, "rev-parse", "HEAD"], {
      encoding: "utf8",
    }).trim();
  } catch {
    return null;
  }
}

function referenceOptions(entry) {
  return entry.reference_options ?? entry.options ?? {};
}

function referenceTheme(entry) {
  return entry.reference_theme ?? entry.theme ?? "light";
}

function validationError(entry) {
  if (!entry.name || typeof entry.name !== "string") return "missing name";
  if (!entry.smiles || typeof entry.smiles !== "string") return "missing smiles";
  if (!Array.isArray(entry.features) || entry.features.length === 0) {
    return "missing features";
  }
  if (typeof entry.raster !== "boolean") return "missing raster flag";
  return null;
}

function renderReference(entry) {
  resetRandom(0x12345678);
  const data = SmilesDrawer.Parser.parse(entry.smiles);
  const options = referenceOptions(entry);
  const theme = referenceTheme(entry);
  const drawer = new SmilesDrawer.SvgDrawer(options);
  const svg = drawer.draw(data, "svg", theme);
  const processor = drawer.preprocessor;
  const atoms = processor.graph.vertices.map((vertex) => ({
    id: vertex.id,
    x: number(vertex.position.x),
    y: number(vertex.position.y),
    element: vertex.value.element,
    hydrogens: number(vertex.value.countImplicitHydrogens?.() ?? 0),
    charge: number(vertex.value.bracket?.charge ?? 0),
    isotope: number(vertex.value.bracket?.isotope ?? 0),
    isDrawn: Boolean(vertex.value.isDrawn),
    isAromatic: Boolean(vertex.value.isPartOfAromaticRing),
    stereocenter: Boolean(vertex.value.isStereoCenter),
    forcePositioned: Boolean(vertex.forcePositioned),
    neighbours: vertex.neighbours.slice(),
  }));
  const edges = processor.graph.edges.map((edge) => ({
    id: edge.id,
    a: edge.sourceId,
    b: edge.targetId,
    order: mapBondType(edge.bondType),
    stereo: mapStereo(edge.wedge),
    aromatic: Boolean(edge.isPartOfAromaticRing),
  }));
  const rings = processor.rings.map((ring) => ({
    id: ring.id,
    atoms: ring.members.slice(),
    aromatic: Boolean(processor.isRingAromatic(ring)),
    cx: number(ring.center.x),
    cy: number(ring.center.y),
  }));

  const primitives = drawer.svgWrapper.paths.map(primitiveFor);
  const labels = drawer.svgWrapper.vertices
    .map((group) => {
      const text = group.querySelector("text");
      if (!text) return null;
      const transform = parseTransform(group.getAttribute("style"));
      if (!transform) return null;
      const atom = nearestAtom(atoms, transform);
      return labelFor(atom, group);
    })
    .filter(Boolean);
  const masks = drawer.svgWrapper.maskElements.map((mask) => ({
    cx: number(mask.getAttribute("cx")),
    cy: number(mask.getAttribute("cy")),
    r: number(mask.getAttribute("r")),
  }));
  const points = drawer.svgWrapper.vertices
    .filter((element) => element.tagName.toLowerCase() === "circle")
    .map((element) => ({
      cx: number(element.getAttribute("cx")),
      cy: number(element.getAttribute("cy")),
      r: number(element.getAttribute("r")),
      fill: attribute(element, "fill"),
    }));
  const gradients = drawer.svgWrapper.gradients.map((gradient) => {
    const entry = { kind: gradient.tagName.toLowerCase(), id: gradient.getAttribute("id") };
    for (const attr of ["x1", "y1", "x2", "y2", "cx", "cy", "r"]) {
      const value = gradient.getAttribute(attr);
      if (value !== null) entry[attr] = number(value);
    }
    entry.stops = [...gradient.querySelectorAll("stop")].map((stop) => ({
      offset: stop.getAttribute("offset"),
      color: stop.getAttribute("stop-color"),
    }));
    return entry;
  });
  const viewBox = (svg.getAttribute("viewBox") ?? "").trim().split(/\s+/).map(Number);
  return {
    reference: {
      name: "smiles-drawer",
      version: SmilesDrawer.Version,
      source: "smilesDrawer/dist/smiles-drawer.mjs",
      commit: oracleCommit(),
      theme,
      options,
    },
    name: entry.name,
    smiles: entry.smiles,
    features: entry.features,
    raster: entry.raster,
    chem_options: entry.chem_options ?? {},
    known_gap: entry.known_gap ?? null,
    atoms,
    edges,
    rings,
    primitives,
    labels,
    masks,
    points,
    gradients,
    viewBox,
    width: number(svg.getAttribute("width")),
    height: number(svg.getAttribute("height")),
    minX: number(drawer.svgWrapper.minX),
    minY: number(drawer.svgWrapper.minY),
    svg: svg.outerHTML,
  };
}

const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
mkdirSync(fixtureDir, { recursive: true });
for (const entry of manifest.fixtures) {
  const error = validationError(entry);
  if (error) {
    throw new Error(`${entry.name ?? "<unnamed>"}: ${error}`);
  }
  const reference = renderReference(entry);
  const path = resolve(fixtureDir, `${entry.name}.json`);
  writeFileSync(path, `${JSON.stringify(reference, null, 2)}\n`);
  console.log(`${entry.name}: ${reference.atoms.length} atoms, ${reference.edges.length} edges, ${reference.primitives.length} primitives`);
}
