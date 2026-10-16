#!/usr/bin/env python3
"""Generate the test corpus and the golden chemfig output files.

The golden files are produced by the reference mol2chemfigPy3 implementation
and are committed to the repository.  The Elisp parity test suite compares
`tochemfig` output against these files, so `eldev test` does not need Python.

Corpus molecules with explicit 2D coordinates are stored as MOL files.  These
make the comparison independent of the Indigo layout implementation: both the
reference and emacs-indigo read the very same coordinates back, so any
difference in output is a genuine algorithm difference.

Run through tools/gen-goldens.sh (which provides the right Python environment).
"""

import os
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, "mol2chemfigPy3"))

CORPUS_DIR = os.path.join(ROOT, "test", "corpus")
GOLDEN_DIR = os.path.join(ROOT, "test", "golden")
CHEBI_SRC = os.path.join(ROOT, "emacs-indigo", "test", "data", "molecules", "chebi")

from indigo import Indigo  # noqa: E402
from mol2chemfigPy3 import process  # noqa: E402


# --- Corpus -----------------------------------------------------------------

# Curated molecules, chosen to exercise the code generator: chains, branches,
# rings, fused/hetero aromatics, charges, radicals, stereo and fragments.
CURATED = {
    "ethanol": "CCO",
    "acetic_acid": "CC(=O)O",
    "isobutane": "CC(C)C",
    "neopentane": "CC(C)(C)C",
    "acetaldehyde": "CC=O",
    "acetonitrile": "CC#N",
    "cyclohexane": "C1CCCCC1",
    "benzene": "c1ccccc1",
    "naphthalene": "c1ccc2ccccc2c1",
    "pyridine": "c1ccncc1",
    "pyrrole": "c1cc[nH]c1",
    "furan": "c1ccoc1",
    "phenol": "Oc1ccccc1",
    "indole": "c1ccc2[nH]ccc2c1",
    "glycine": "NCC(=O)O",
    "ammonium": "C[N+](C)(C)C",
    "acetate": "CC(=O)[O-]",
    "nitrobenzene": "O=[N+]([O-])c1ccccc1",
    "phosphate": "OP(=O)(O)O",
    "caffeine": "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
    "alanine": "N[C@@H](C)C(=O)O",
    "butene_e": "C/C=C/C",
    "butene_z": "C/C=C\\C",
    "salt_nacl": "[Na+].[Cl-]",
    "salt_ammonium_chloride": "[NH4+].[Cl-]",
    "mixture_ethanol": "CCO.CCO",
    "methyl_radical": "[CH3]",
    "oxygen_radical": "[O]",
}

# A subset rich enough to exercise the per-feature profiles.
RICH = [
    "ethanol", "isobutane", "benzene", "naphthalene", "pyridine",
    "glycine", "ammonium", "acetate", "caffeine", "alanine",
    "salt_nacl", "oxygen_radical",
]

# A batch of real-world structures pulled from emacs-indigo's ChEBI test data.
CHEBI_BATCH = [
    "issue230", "ChEBI_12800", "ChEBI_7871", "ChEBI_12791",
    "ChEBI_12821", "ChEBI_10909", "ChEBI_10305", "ChEBI_12820",
]


# --- Profiles ---------------------------------------------------------------
# Option names match the `tochemfig` option plist.  The reference is invoked
# through equivalent command line flags.

FLAG = {
    "terse": "-z",
    "relative-angles": "-v",
    "flip": "-p",
    "flop": "-q",
    "show-carbons": "-c",
    "show-methyls": "-m",
    "aromatic-circles": "-o",
    "fancy-bonds": "-f",
    "atom-numbers": "-n",
    "wrap-chemfig": "-w",
}

VALUE = {
    "angle": "-a",
    "indent": "-d",
    "markers": "-g",
    "bond-scale": "-s",
    "bond-stretch": "-t",
    "submol-name": "-l",
    "entry-atom": "-e",
    "exit-atom": "-x",
    "cross-bond": "-k",
    "hydrogens": "-y",
}

BASIC_PROFILES = {
    "default": {},
    "terse": {"terse": True},
}

RICH_PROFILES = {
    "rotate": {"angle": 30.0, "flip": True},
    "relative": {"relative-angles": True},
    "fancy": {"fancy-bonds": True},
    "circles": {"aromatic-circles": True},
    "atomnums": {"atom-numbers": True},
    "markers": {"markers": "a"},
    "carbons": {"show-carbons": True},
    "methyls": {"show-methyls": True},
    "scale-keep": {"bond-scale": "keep"},
    "scale-scale": {"bond-scale": "scale", "bond-stretch": 2.0},
    "scale-normalize": {"bond-scale": "normalize", "bond-stretch": 1.5},
    "hydrogens-add": {"hydrogens": "add"},
    "hydrogens-delete": {"hydrogens": "delete"},
    "wrap": {"wrap-chemfig": True},
    "submol": {"submol-name": "mol"},
}

# Molecule specific extras (entry/exit atoms, cross bonds).
EXTRA = [
    ("benzene", "entryexit", {"entry-atom": 1, "exit-atom": 4}),
    ("benzene", "cross", {"cross-bond": [(1, 2)]}),
]


def options_to_args(options):
    args = []
    for key, value in options.items():
        if key in FLAG:
            args.append(FLAG[key])
        elif key == "cross-bond":
            args.extend([VALUE[key], ",".join("%d-%d" % (a, b) for a, b in value)])
        elif key in VALUE:
            args.extend([VALUE[key], str(value)])
        else:
            raise KeyError(f"no CLI flag for option {key}")
    return args


def indigo():
    return Indigo()


def smiles_to_mol(smiles, path):
    ind = indigo()
    mol = ind.loadMolecule(smiles)
    mol.layout()
    # Write Kekule bond orders.  Aromatic order 4 is encoded by Indigo with an
    # SD data tag (MRV_IMPLICIT_H) that older Indigo versions (as used by
    # emacs-indigo) cannot read, which would make the two sides disagree on
    # implicit hydrogens.  Kekulizing the input removes that ambiguity.
    try:
        mol.dearomatize()
    except Exception:
        pass
    mol.saveMolfile(path)


def normalize_mol(src, dst):
    ind = indigo()
    mol = ind.loadMoleculeFromFile(src)
    try:
        mol.dearomatize()
    except Exception:
        pass
    mol.saveMolfile(dst)


def elisp_value(value):
    if value is True:
        return "t"
    if value is False:
        return "nil"
    if isinstance(value, str):
        return '"%s"' % value
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        return repr(value)
    if isinstance(value, list):
        return "(" + " ".join("(%d . %d)" % (a, b) for a, b in value) + ")"
    raise TypeError(f"cannot encode {value!r}")


def elisp_options(options):
    parts = []
    for key in options:
        parts.append(":%s %s" % (key, elisp_value(options[key])))
    return "(" + " ".join(parts) + ")"


def generate(cases):
    manifest = []
    for mol_path, name, opts in cases:
        args = ["--input", "file"] + options_to_args(opts) + [mol_path]
        success, result = process(raw_args=args, inline=True)
        if not success:
            raise SystemExit(f"reference failed for {name}:\n{result}")
        golden = result.render_user()
        out = os.path.join(GOLDEN_DIR, name + ".tex")
        with open(out, "w", encoding="utf-8") as fh:
            fh.write(golden)
        manifest.append(
            "  (:name %s\n   :input %s\n   :options %s\n   :golden %s)"
            % (
                elisp_value(name),
                elisp_value(os.path.relpath(mol_path, ROOT)),
                elisp_options(opts),
                elisp_value(os.path.relpath(out, ROOT)),
            )
        )
        print(f"  {name}")
    return manifest


def main():
    os.makedirs(CORPUS_DIR, exist_ok=True)
    os.makedirs(GOLDEN_DIR, exist_ok=True)

    # 1. Build MOL corpus from curated SMILES.
    print("building MOL corpus...")
    paths = {}
    for name, smiles in CURATED.items():
        path = os.path.join(CORPUS_DIR, name + ".mol")
        smiles_to_mol(smiles, path)
        paths[name] = path

    # 2. Copy the ChEBI batch.
    for name in CHEBI_BATCH:
        src = os.path.join(CHEBI_SRC, name + ".mol")
        dst = os.path.join(CORPUS_DIR, name + ".mol")
        normalize_mol(src, dst)
        paths[name] = dst

    # 3. Generate golden files.
    print("generating goldens...")
    cases = []
    for name, path in paths.items():
        for profile, opts in BASIC_PROFILES.items():
            cases.append((path, f"{name}__{profile}", opts))
    for name in RICH:
        for profile, opts in RICH_PROFILES.items():
            cases.append((paths[name], f"{name}__{profile}", opts))
    for name, label, opts in EXTRA:
        cases.append((paths[name], f"{name}__{label}", opts))

    manifest = generate(cases)

    with open(os.path.join(GOLDEN_DIR, "manifest.el"), "w", encoding="utf-8") as fh:
        fh.write(";;; Generated by tools/gen-goldens.py -- do not edit by hand.\n")
        fh.write("(\n")
        fh.write("\n".join(manifest))
        fh.write(")\n")
    print(f"wrote {len(manifest)} golden files")


if __name__ == "__main__":
    main()
