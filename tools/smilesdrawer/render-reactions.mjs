#!/usr/bin/env node
/*
 * Development-only SmilesDrawer reaction reference generator.
 *
 * Unlike the molecule generator, this script instruments SvgDrawer.draw while
 * ReactionDrawer lays out a reaction.  Each component keeps its SmilesDrawer
 * coordinates, graph, and final nested-SVG placement.  Reaction composition
 * (plus signs, arrow, text) is captured as ordered layout parts.
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
const fixtureDir = resolve(root, "test/fixtures/smilesdrawer-reactions");
const manifestPath = resolve(fixtureDir, "manifest.json");

const dom = new JSDOM("<!doctype html><html><body></body></html>");
Object.assign(globalThis, {
  window: dom.window,
  document: dom.window.document,
  SVGElement: dom.window.SVGElement,
  SVGSVGElement: dom.window.SVGSVGElement,
  HTMLCanvasElement: dom.window.HTMLCanvasElement,
});
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
  return entry.reference_options ?? {};
}

function referenceTheme(entry) {
  return entry.reference_theme ?? "light";
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

function componentData(preprocessor) {
  const atoms = preprocessor.graph.vertices.map((vertex) => ({
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
  const edges = preprocessor.graph.edges.map((edge) => ({
    id: edge.id,
    a: edge.sourceId,
    b: edge.targetId,
    order: mapBondType(edge.bondType),
    stereo: mapStereo(edge.wedge),
    aromatic: Boolean(edge.isPartOfAromaticRing),
  }));
  const rings = preprocessor.rings.map((ring) => ({
    id: ring.id,
    atoms: ring.members.slice(),
    aromatic: Boolean(preprocessor.isRingAromatic(ring)),
    cx: number(ring.center.x),
    cy: number(ring.center.y),
  }));
  return { atoms, edges, rings };
}

function renderReference(entry) {
  resetRandom(0x12345678);
  const reaction = SmilesDrawer.ReactionParser.parse(entry.smiles);
  const moleculeOptions = referenceOptions(entry);
  const reactionOptions = entry.reaction_options ?? {};
  const theme = referenceTheme(entry);
  const textAbove = entry.text_above ?? "{reagents}";
  const textBelow = entry.text_below ?? "";

  let recording = false;
  const captures = [];
  const originalDraw = SmilesDrawer.SvgDrawer.prototype.draw;
  SmilesDrawer.SvgDrawer.prototype.draw = function (...args) {
    const result = originalDraw.apply(this, args);
    if (recording) {
      captures.push({
        data: componentData(this.preprocessor),
        target: args[1],
      });
    }
    return result;
  };

  let svg;
  try {
    const drawer = new SmilesDrawer.ReactionDrawer(
      reactionOptions,
      moleculeOptions,
    );
    recording = true;
    svg = drawer.draw(reaction, null, theme, null, textAbove, textBelow);
  } finally {
    recording = false;
    SmilesDrawer.SvgDrawer.prototype.draw = originalDraw;
  }

  const reactantCount = reaction.reactants.length;
  const productCount = reaction.products.length;
  if (captures.length !== reactantCount + productCount) {
    throw new Error(
      `${entry.name}: expected ${reactantCount + productCount} component draws, got ${captures.length}`,
    );
  }

  const targets = new Map();
  const components = captures.map((capture, index) => {
    const target = capture.target;
    targets.set(target, index);
    const viewBox = (target.getAttribute("viewBox") ?? "")
      .trim()
      .split(/\s+/)
      .map(Number);
    return {
      kind: index < reactantCount ? "reactant" : "product",
      index,
      x: number(target.getAttribute("x")),
      y: number(target.getAttribute("y")),
      width: number(target.getAttribute("width")),
      height: number(target.getAttribute("height")),
      viewBox,
      ...capture.data,
    };
  });

  const flow = [];
  for (const child of [...svg.children]) {
    if (child.tagName.toLowerCase() !== "svg") continue;
    const id = attribute(child, "id");
    if (targets.has(child)) {
      const component = components[targets.get(child)];
      flow.push({
        kind: component.kind,
        component: component.index,
        x: component.x,
        y: component.y,
        width: component.width,
        height: component.height,
      });
    } else if (id === "plus") {
      flow.push({
        kind: "plus",
        x: number(child.getAttribute("x")),
        y: number(child.getAttribute("y")),
        width: number(child.getAttribute("width")),
        height: number(child.getAttribute("height")),
      });
    } else if (id === "arrow") {
      flow.push({
        kind: "arrow",
        x: number(child.getAttribute("x")),
        y: number(child.getAttribute("y")),
        width: number(child.getAttribute("width")),
        height: number(child.getAttribute("height")),
      });
    } else {
      const text = child.querySelector("text.text");
      if (text) {
        const lines = [...text.querySelectorAll("tspan")].map(
          (part) => part.textContent ?? "",
        );
        const content = lines.length > 0 ? lines.join("\n") : (text.textContent ?? "");
        if (content.trim() !== "") {
          flow.push({
            kind: "text",
            text: content,
            lines,
            x: number(child.getAttribute("x")),
            y: number(child.getAttribute("y")),
            width: number(child.getAttribute("width")),
            height: number(child.getAttribute("height")),
          });
        }
      }
    }
  }

  const arrow = flow.find((part) => part.kind === "arrow");
  if (!arrow) {
    throw new Error(`${entry.name}: reaction has no arrow`);
  }
  const arrowCenter = arrow.y + arrow.height / 2.0;
  const texts = flow.filter((part) => part.kind === "text");
  for (const text of texts) {
    text.role = text.y + text.height / 2.0 < arrowCenter ? "above" : "below";
  }

  const viewBox = (svg.getAttribute("viewBox") ?? "")
    .trim()
    .split(/\s+/)
    .map(Number);

  return {
    reference: {
      name: "smiles-drawer",
      version: SmilesDrawer.Version,
      source: "smilesDrawer/dist/smiles-drawer.mjs",
      commit: oracleCommit(),
      theme,
      molecule_options: moleculeOptions,
      reaction_options: reactionOptions,
      text_above: textAbove,
      text_below: textBelow,
    },
    name: entry.name,
    smiles: entry.smiles,
    features: entry.features,
    raster: entry.raster,
    known_gap: entry.known_gap ?? null,
    reactant_count: reactantCount,
    product_count: productCount,
    components,
    flow,
    texts,
    viewBox,
    width: number(svg.getAttribute("width")),
    height: number(svg.getAttribute("height")),
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
  console.log(
    `${entry.name}: ${reference.reactant_count} reactants, ${reference.product_count} products, ${reference.flow.length} flow parts, ${reference.texts.length} text parts`,
  );
}
