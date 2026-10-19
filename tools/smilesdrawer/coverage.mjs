#!/usr/bin/env node
/*
 * Development-only fixture coverage report.
 *
 * Reads the manifest, so it does not require SmilesDrawer or jsdom.
 */

import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const manifestPath = resolve(here, "../../test/fixtures/smilesdrawer/manifest.json");
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const fixtures = manifest.fixtures;

const featureCounts = new Map();
for (const fixture of fixtures) {
  for (const feature of fixture.features) {
    featureCounts.set(feature, (featureCounts.get(feature) ?? 0) + 1);
  }
}

const raster = fixtures.filter((fixture) => fixture.raster);
const gaps = fixtures.filter((fixture) => fixture.known_gap);
const noFeatures = fixtures.filter((fixture) => !fixture.features?.length);
const duplicateNames = fixtures
  .map((fixture) => fixture.name)
  .filter((name, index, names) => names.indexOf(name) !== index);

console.log(`fixtures: ${fixtures.length}`);
console.log(`raster: ${raster.length} (${raster.map((fixture) => fixture.name).join(", ")})`);
console.log(`known gaps: ${gaps.length}`);
console.log(`missing feature tags: ${noFeatures.length}`);
console.log(`duplicate names: ${duplicateNames.length}`);
console.log("");
console.log("feature counts:");

for (const [feature, count] of [...featureCounts.entries()].sort((a, b) =>
  a[0].localeCompare(b[0]),
)) {
  console.log(`  ${feature}: ${count}`);
}

if (noFeatures.length > 0) {
  console.log("");
  console.log("fixtures without feature tags:");
  for (const fixture of noFeatures) console.log(`  ${fixture.name}`);
}

if (gaps.length > 0) {
  console.log("");
  console.log("declared known gaps:");
  for (const fixture of gaps) console.log(`  ${fixture.name}: ${fixture.known_gap}`);
}
