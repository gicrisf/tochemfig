#!/usr/bin/env node
/*
 * Development-only reaction fixture coverage report.
 *
 * Reads only the reaction manifest and generated fixture JSON.  It does not
 * require SmilesDrawer or jsdom.
 */

import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const fixtureDir = resolve(here, "../../test/fixtures/smilesdrawer-reactions");
const manifest = JSON.parse(readFileSync(resolve(fixtureDir, "manifest.json"), "utf8"));
const fixtures = manifest.fixtures;

const featureCounts = new Map();
for (const fixture of fixtures) {
  for (const feature of fixture.features ?? []) {
    featureCounts.set(feature, (featureCounts.get(feature) ?? 0) + 1);
  }
}

const raster = fixtures.filter((fixture) => fixture.raster);
const gaps = fixtures.filter((fixture) => fixture.known_gap);
const noFeatures = fixtures.filter((fixture) => !fixture.features?.length);
const duplicateNames = fixtures
  .map((fixture) => fixture.name)
  .filter((name, index, names) => names.indexOf(name) !== index);

let components = 0;
let flowParts = 0;
let textParts = 0;
let missingFixtures = 0;
for (const fixture of fixtures) {
  try {
    const generated = JSON.parse(
      readFileSync(resolve(fixtureDir, `${fixture.name}.json`), "utf8"),
    );
    components += generated.components?.length ?? 0;
    flowParts += generated.flow?.length ?? 0;
    textParts += generated.texts?.length ?? 0;
  } catch {
    missingFixtures += 1;
  }
}

console.log(`reaction fixtures: ${fixtures.length}`);
console.log(`raster gates: ${raster.length} (${raster.map((fixture) => fixture.name).join(", ")})`);
console.log(`known gaps: ${gaps.length}`);
console.log(`missing feature tags: ${noFeatures.length}`);
console.log(`duplicate names: ${duplicateNames.length}`);
console.log(`missing generated fixtures: ${missingFixtures}`);
console.log(`raster coverage: ${raster.length}/${fixtures.length}`);
console.log("");
console.log("fixture totals:");
console.log(`  components: ${components}`);
console.log(`  flow parts: ${flowParts}`);
console.log(`  text parts: ${textParts}`);
console.log("");
console.log("feature counts:");

for (const [feature, count] of [...featureCounts.entries()].sort((a, b) =>
  a[0].localeCompare(b[0]),
)) {
  console.log(`  ${feature}: ${count}`);
}

const singletons = [...featureCounts.entries()]
  .filter(([, count]) => count === 1)
  .map(([feature]) => feature)
  .sort();

console.log("");
console.log(`singleton features: ${singletons.length}`);
console.log(`  ${singletons.join(", ")}`);

if (noFeatures.length > 0) {
  console.log("");
  console.log("fixtures without feature tags:");
  for (const fixture of noFeatures) console.log(`  ${fixture.name}`);
}

if (missingFixtures > 0) {
  console.log("");
  console.log("fixtures with missing generated JSON:");
  for (const fixture of fixtures) {
    try {
      readFileSync(resolve(fixtureDir, `${fixture.name}.json`));
    } catch {
      console.log(`  ${fixture.name}`);
    }
  }
}

if (gaps.length > 0) {
  console.log("");
  console.log("declared known gaps:");
  for (const fixture of gaps) console.log(`  ${fixture.name}: ${fixture.known_gap}`);
}
