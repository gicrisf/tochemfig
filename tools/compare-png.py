#!/usr/bin/env python3
"""Advisory raster comparison for the SVG parity report.

Usage: compare-png.py REFERENCE.png NATIVE.png OUTPUT.png

Requires Pillow.  Prints mean absolute RGB error, RMS error, and edge-map
error.  The output image is a side-by-side comparison for manual review.
"""

from __future__ import annotations

import sys

from PIL import Image, ImageChops, ImageFilter, ImageStat


def rgb(path: str) -> Image.Image:
    return Image.open(path).convert("RGB")


def main() -> int:
    if len(sys.argv) != 4:
        raise SystemExit("usage: compare-png.py REFERENCE.png NATIVE.png OUTPUT.png")
    reference_path, native_path, output_path = sys.argv[1:]
    reference = rgb(reference_path)
    native = rgb(native_path)
    if native.size != reference.size:
        native = native.resize(reference.size, Image.Resampling.LANCZOS)

    difference = ImageChops.difference(reference, native)
    stats = ImageStat.Stat(difference)
    mae = sum(stats.mean) / 3.0
    rms = (sum(value * value for value in stats.rms) / 3.0) ** 0.5

    reference_edges = reference.convert("L").filter(ImageFilter.FIND_EDGES)
    native_edges = native.convert("L").filter(ImageFilter.FIND_EDGES)
    edge_stats = ImageStat.Stat(ImageChops.difference(reference_edges, native_edges))
    edge_error = edge_stats.mean[0]

    width, height = reference.size
    side_by_side = Image.new("RGB", (width * 2, height), "white")
    side_by_side.paste(reference, (0, 0))
    side_by_side.paste(native, (width, 0))
    side_by_side.save(output_path)

    print(f"mae={mae:.3f} rms={rms:.3f} edge_error={edge_error:.3f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
