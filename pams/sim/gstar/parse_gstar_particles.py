#!/usr/bin/env python
"""
parse_gstar_particles.py

Parses pams/sim/gstar/gstar_part.g, extracts all GEANT3 particle definitions,
groups them by particle family, and writes GSTAR_PARTICLE_TABLE.md.

Usage:
    python parse_gstar_particles.py [--input <path>] [--output <path>]
"""

import re
import sys
import os
import argparse

# ---------------------------------------------------------------------------
# Particle family groupings: (group_label, matcher)
# matcher is a function(code, name) -> bool
# Evaluated in order; first match wins.
# ---------------------------------------------------------------------------

def _code_in(codes):
    return lambda code, name: code in codes

def _code_range(lo, hi):
    return lambda code, name: lo <= code <= hi

def _name_re(pattern):
    rx = re.compile(pattern, re.IGNORECASE)
    return lambda code, name: bool(rx.search(name))

GROUPS = [
    # ---- Hyperon decay helpers (codes 92-94, used as constrained daughters) ----
    ("Hyperon decay helpers",
     lambda c, n: c in (92, 93, 94)),

    # ---- d-Hyperon pseudoparticles (must come before Ds mesons to avoid name clash) ----
    ("d-Hyperon pseudoparticles",
     lambda c, n: c in (60100, 60101, 60102, 60103)),

    # ---- Dalitz / pi0 ----
    ("Dalitz / pi0 variants",
     lambda c, n: n.lower() in ("dalitz",) or c in (149, 10007)),

    # ---- eta ----
    ("eta variants",
     lambda c, n: n.lower().startswith("eta_") or n.lower() == "eta"),

    # ---- Light vector mesons ----
    ("rho mesons",
     lambda c, n: n.lower().startswith("rho")),

    ("omega(782)",
     lambda c, n: n.lower() == "omega" and c in (150, 10150)),

    ("phi mesons",
     lambda c, n: n.lower().startswith("phi") or n.lower().startswith("_phi")),

    # ---- Kaons ----
    ("K0 / K-short / K-long",
     lambda c, n: n.lower() in ("k0", "anti_k0", "k0short", "stk0s")
                  or c in (95, 155, 156, 707, 10010, 10110)),

    ("K*(892)",
     lambda c, n: "k0star" in n.lower() or "kstar" in n.lower()
                  or c in (10013, 10113, 10014, 10015)),

    ("K+/K- decay modes",
     lambda c, n: "kaon_plus" in n.lower() or "kaon_minus" in n.lower()
                  or c in (96,)),

    # ---- Charmed mesons ----
    ("D mesons",
     lambda c, n: (n.lower().startswith("d_plus")
                   or n.lower().startswith("d_minus")
                   or n.lower().startswith("d0")
                   or n.lower() in ("d_star_plus", "d_star_minus",
                                    "d_star_0", "d_star_0_bar"))
                  and c in (35, 36, 37, 38, 60, 61, 62, 63, 10060, 10061)),

    ("Ds mesons",
     lambda c, n: "d_s" in n.lower() or "_phi_to_kk_" in n.lower()
                  or c in (99, 10039, 10040)),

    ("J/psi and psi(2S)",
     lambda c, n: "jpsi" in n.lower() or "psi2s" in n.lower()
                  or c in (160, 167, 168, 169)),

    # ---- Bottom mesons ----
    ("B mesons",
     lambda c, n: (n.lower().startswith("b_plus")
                   or n.lower().startswith("b_minus")
                   or n.lower().startswith("b0"))
                  or c in (70, 71, 72, 73)),

    ("Upsilon",
     lambda c, n: "upsilon" in n.lower() or n.lower().startswith("ups")
                  or c in (161, 162, 163, 164, 165, 166)),

    # ---- Lambda baryons ----
    ("Lambda baryons",
     lambda c, n: (n.lower().startswith("lambda")
                   or n.lower().startswith("lambdabar")
                   or n.lower().startswith("fastlambda")
                   or n.lower() in ("_lam_to_p_piminus_", "_lam_to_pb_piplus_"))
                  or c in (97, 98, 10018, 10026, 11018, 11026)),

    ("Lambda(1520)",
     lambda c, n: "lambda1520" in n.lower() or "lambdabar1520" in n.lower()
                  or c in (995, 996)),

    # ---- Sigma / Sigma(1385) ----
    ("Sigma(1385) baryons",
     lambda c, n: "s1385" in n.lower() or c in (701, 702, 703, 704)),

    # ---- Omega baryons (must come BEFORE omega mesons catch-all) ----
    ("Omega baryons",
     lambda c, n: c in (40001, 40002)),

    # ---- Xi baryons ----
    ("Xi baryons",
     lambda c, n: c in (40003, 40004, 40005, 40006, 40007, 40008)),

    # ---- Lambda_c charmed baryon ----
    ("Lambda_c charmed baryon",
     lambda c, n: n.lower().startswith("lac2") or c in (207, 208)),

    # ---- Hypertriton / hypernuclei ----
    ("H3Lambda hypertriton",
     lambda c, n: "hypertriton" in n.lower() or "h3_lambda" in n.lower()
                  or c in (52, 61053, 62053, 63053, 61054, 62054, 63054)),

    ("H4/He4/He5 hypernuclei",
     lambda c, n: c in (61055, 61056, 61057, 61059)),

    # ---- Dibaryons / exotic bound states ----
    ("Dibaryons and exotic bound states",
     lambda c, n: c in (60001, 60002, 60003, 60004, 60005, 60006, 60007)),

    # ---- Pentaquarks ----
    ("Pentaquarks",
     lambda c, n: "pq1730" in n.lower() or c in (60008, 60009)),

    # ---- Anti-nuclei ----
    ("Anti-nuclei",
     lambda c, n: ("anti" in n.lower()
                   and ("deuteron" in n.lower() or "triton" in n.lower()
                        or "alpha" in n.lower() or "helium" in n.lower()))
                  or c in (53, 54, 50045, 50046, 50047, 50049)),

    # ---- Nuclei / heavy ions ----
    ("Nuclei and heavy ions",
     lambda c, n: c in (51045,) or n.lower() == "deuteron"),

    ("Beryllium-8 / Carbon-12 excited states",
     lambda c, n: c in (50060, 50061, 50062, 50063)),

    # ---- Strangelets ----
    ("Strangelets",
     lambda c, n: "strangelet" in n.lower() or c in (60801,)),

    # ---- Special / Geantinos ----
    ("Special / Geantinos",
     lambda c, n: c in (170, 171)),
]

FALLBACK_GROUP = "Other / Unclassified"


# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

def _strip_comments(text):
    """Remove Fortran-style line comments (lines starting with *) and inline ! comments."""
    lines = []
    for line in text.splitlines():
        stripped = line.lstrip()
        if stripped.startswith("*"):
            continue
        # Remove inline ! comments but preserve the line
        idx = line.find("!")
        if idx >= 0:
            line = line[:idx]
        lines.append(line)
    return "\n".join(lines)


def _extract_string_comment(text):
    """Pull the quoted description from  Particle name "description" code=... syntax.
    Only returns the first quoted string that appears before any keyword= parameter.
    Skips short purely-numeric quoted fragments (e.g. mass = 1.115 "683").
    """
    # Find position of first keyword=value (signals end of name/description area)
    kw_match = re.search(r'\b(?:code|mass|charge|tlife|pdg|trktyp|bratio|mode)\s*=',
                         text, re.IGNORECASE)
    search_region = text[:kw_match.start()] if kw_match else text

    m = re.search(r'"([^"]*)"', search_region)
    if not m:
        return ""
    candidate = m.group(1).strip()
    # Reject purely numeric fragments (e.g. "683")
    if re.fullmatch(r'[\d.eE+\-]+', candidate):
        return ""
    return candidate


def _parse_value(text, key):
    """Return the string value of key=<value> from a block of text (case-insensitive)."""
    # Handles both key=value and key = value with possible trailing comma/space
    pat = re.compile(
        r'\b' + re.escape(key) + r'\s*=\s*([^\s,}]+)',
        re.IGNORECASE,
    )
    m = pat.search(text)
    return m.group(1).strip() if m else None


def _parse_bratio(text):
    """Extract the bratio list."""
    m = re.search(r'\bbratio\s*=\s*\{([^}]*)\}', text, re.IGNORECASE)
    if not m:
        return []
    raw = m.group(1)
    return [v.strip() for v in raw.split(",") if v.strip()]


def _parse_mode(text):
    """Extract the mode list."""
    m = re.search(r'\bmode\s*=\s*\{([^}]*)\}', text, re.IGNORECASE)
    if not m:
        return []
    raw = m.group(1)
    return [v.strip() for v in re.split(r'[,\s]+', raw) if v.strip()]


def _safe_float(s):
    if s is None:
        return None
    s = s.rstrip(",")
    # Handle expressions like kHBAR/0.0473 — just store as string
    try:
        return float(s)
    except ValueError:
        return s  # return string for expressions


def parse_particles(path):
    """Return list of dicts, one per Particle definition."""
    with open(path) as fh:
        raw = fh.read()

    # Split into logical lines — continuation is implicit in .g (Mortran) syntax,
    # but the comma at end of line is the continuation marker.  We join continued
    # lines into single logical records to make regex easier.
    #
    # Strategy: find every Particle keyword and grab everything up to the next
    # Particle keyword or End/Module/Subroutine.

    # Remove Fortran * comment lines; keep ! inline comments for label extraction
    # We will do a two-pass: one with comments for label, one without for values.
    no_star = "\n".join(
        line for line in raw.splitlines()
        if not line.lstrip().startswith("*")
    )

    # Match each Particle block: everything from "Particle <name>" up to the
    # next "Particle", "End", "Module", "Subroutine", "Call GSPART" or EOF.
    # We use a lazy .*? in DOTALL mode, terminated by a lookahead.
    # Note: some blocks use "PARTICLE" (case varies).

    stop_pat = (
        r'(?=\s*(?:PARTICLE|Particle|particle)\s+'
        r'|End\s*!'
        r'|\bEnd\b\s*$'
        r'|\bModule\b'
        r'|\bSubroutine\b'
        r'|\bCall\s+GSPART\b)'
    )

    particle_pat = re.compile(
        r'(?:^|(?<=\n))\s*(?:PARTICLE|Particle|particle)\s+'
        r'(.*?)(?=' + stop_pat[3:],  # strip the outer (?=
        re.DOTALL | re.MULTILINE
    )

    # Simpler approach: split on PARTICLE keyword boundaries
    # Find all starts
    starts = [(m.start(), m.end(), m.group(0))
              for m in re.finditer(
                  r'\bPARTICLE\b|\bParticle\b|\bparticle\b',
                  no_star, re.IGNORECASE)]

    particles = []
    for i, (start, end, kw) in enumerate(starts):
        # Get the text from after the keyword to the next keyword (or EOF)
        if i + 1 < len(starts):
            block_end = starts[i + 1][0]
        else:
            block_end = len(no_star)
        block = no_star[end:block_end]

        # First token is the particle name (may have a quoted description after it)
        # e.g.: D_plus    code=35  ...
        # or:   PQ1730 "Pentaquark..." code=60008 ...
        first_line_m = re.match(r'\s*(\S+)(.*)', block, re.DOTALL)
        if not first_line_m:
            continue
        name = first_line_m.group(1)
        rest = first_line_m.group(2)

        # Skip if this looks like a keyword (MODULE, IS, etc.)
        if name.upper() in ("IS", "MODULE", "SUBROUTINE", "FUNCTION",
                             "END", "DATA", "INTEGER", "REAL", "CALL"):
            continue

        description = _extract_string_comment(rest)

        code_str = _parse_value(rest, "code")
        if code_str is None:
            continue
        try:
            code = int(code_str)
        except ValueError:
            continue

        mass_str  = _parse_value(rest, "mass")
        charge_str = _parse_value(rest, "charge")
        tlife_str = _parse_value(rest, "tlife")
        pdg_str   = _parse_value(rest, "pdg")
        trktyp_str = _parse_value(rest, "trktyp")

        mass   = _safe_float(mass_str)  if mass_str  else None
        charge = _safe_float(charge_str) if charge_str else None
        tlife  = _safe_float(tlife_str) if tlife_str  else None
        pdg    = pdg_str  # keep as string (may be expression)

        bratio = _parse_bratio(rest)
        mode   = _parse_mode(rest)

        particles.append({
            "name":        name,
            "code":        code,
            "description": description,
            "mass":        mass,
            "charge":      charge,
            "tlife":       tlife,
            "pdg":         pdg,
            "trktyp":      trktyp_str,
            "bratio":      bratio,
            "mode":        mode,
        })

    return particles


# ---------------------------------------------------------------------------
# Grouping
# ---------------------------------------------------------------------------

def assign_group(code, name):
    for label, matcher in GROUPS:
        if matcher(code, name):
            return label
    return FALLBACK_GROUP


# ---------------------------------------------------------------------------
# Table formatting
# ---------------------------------------------------------------------------

def _fmt_tlife(tlife):
    if tlife is None:
        return ""
    if isinstance(tlife, float):
        if tlife >= 1e14:
            return "stable"
        return f"{tlife:.3e}"
    return str(tlife)


# Mapping from named GEANT3 tracking-type constants to numeric value and description
_TRKTYP_MAP = {
    "kgtgama":  (1, "photon"),
    "kgtelec":  (2, "e±"),
    "kgtneut":  (3, "neutral hadron"),
    "kgthadr":  (4, "charged hadron"),
    "kgtmuon":  (5, "muon"),
    "kgtnino":  (6, "geantino"),
    "kgtckov":  (7, "Cherenkov photon"),
    "kgthion":  (8, "heavy ion"),
    # 9 is used for Monopole (not a standard GEANT3 type)
}


def _fmt_trktyp(raw):
    """Return a human-readable TrkTyp string, e.g. '4 (charged hadron)'."""
    if raw is None:
        return ""
    key = raw.strip().lower().rstrip(",")
    entry = _TRKTYP_MAP.get(key)
    if entry:
        return f"{entry[0]} ({entry[1]})"
    # Already numeric
    try:
        n = int(key)
        labels = {1: "photon", 2: "e±", 3: "neutral hadron", 4: "charged hadron",
                  5: "muon", 6: "geantino", 7: "Cherenkov photon", 8: "heavy ion"}
        desc = labels.get(n, "")
        return f"{n} ({desc})" if desc else str(n)
    except ValueError:
        return raw


def _fmt_float(v, fmt=".4f"):
    if v is None:
        return ""
    if isinstance(v, float):
        return format(v, fmt)
    return str(v)


def build_table(particles):
    """
    Returns a list of (group, [row_dict, ...]) in group order.
    Within each group rows are sorted by ascending code.
    """
    grouped = {}  # group_label -> list of particles
    group_order = []

    for p in particles:
        g = assign_group(p["code"], p["name"])
        if g not in grouped:
            grouped[g] = []
            group_order.append(g)
        grouped[g].append(p)

    result = []
    for g in group_order:
        rows = sorted(grouped[g], key=lambda p: p["code"])
        result.append((g, rows))

    return result


def render_markdown(table_groups):
    lines = []
    lines.append("# GSTAR Particle Table")
    lines.append("")
    lines.append(
        "Particle definitions from `pams/sim/gstar/gstar_part.g`.  "
        "Particles are grouped by family and sorted within each group by "
        "ascending GEANT3 ID."
    )
    lines.append("")

    col_headers = [
        "GEANT3 ID", "Name", "PDG ID", "Mass (GeV)", "Charge",
        "TrkTyp", "Lifetime (s)", "Decay Mode(s)", "Description",
    ]
    sep = "| " + " | ".join("---" for _ in col_headers) + " |"
    header_row = "| " + " | ".join(col_headers) + " |"

    for group_label, rows in table_groups:
        lines.append(f"## {group_label}")
        lines.append("")
        lines.append(header_row)
        lines.append(sep)
        for p in rows:
            mode_str = ", ".join(p["mode"]) if p["mode"] else ""
            br_str   = ", ".join(p["bratio"]) if p["bratio"] else ""
            decay_col = ""
            if mode_str:
                decay_col = f"{mode_str}"
                if br_str:
                    decay_col += f" (BR: {br_str})"

            pdg_col = p["pdg"] if p["pdg"] and p["pdg"] != "0" and p["pdg"] != "UNDEFINED" else ""

            row = "| {} | {} | {} | {} | {} | {} | {} | {} | {} |".format(
                p["code"],
                p["name"],
                pdg_col,
                _fmt_float(p["mass"]),
                _fmt_float(p["charge"], ".0f") if p["charge"] is not None else "",
                _fmt_trktyp(p["trktyp"]),
                _fmt_tlife(p["tlife"]),
                decay_col,
                p["description"],
            )
            lines.append(row)
        lines.append("")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Verification
# ---------------------------------------------------------------------------

def verify(particles, md_path, gstar_path):
    """
    1. Check every code in the particle list is present in gstar_part.g
    2. Check every code in GSTAR_PARTICLE_TABLE.md is present in gstar_part.g
    Returns (ok, messages)
    """
    messages = []
    ok = True

    # Collect codes from parsed particles
    parsed_codes = {p["code"] for p in particles}

    # Collect codes from the markdown table  (look for integer IDs in first column)
    with open(md_path) as fh:
        md_text = fh.read()

    md_codes = set()
    for m in re.finditer(r'^\|\s*(\d+)\s*\|', md_text, re.MULTILINE):
        md_codes.add(int(m.group(1)))

    # Re-read raw gstar text, strip Fortran * comment lines, collect all code= values
    with open(gstar_path) as fh:
        raw_lines = fh.readlines()

    active_lines = [
        line for line in raw_lines
        if not line.lstrip().startswith("*")
    ]
    active_text = "".join(active_lines)

    gstar_codes = set()
    for m in re.finditer(r'\bcode\s*=\s*(\d+)', active_text, re.IGNORECASE):
        gstar_codes.add(int(m.group(1)))

    # Check 1: every code in parsed list is in gstar
    missing_from_gstar = parsed_codes - gstar_codes
    if missing_from_gstar:
        ok = False
        messages.append(
            f"ERROR: codes in parsed list but NOT found in gstar_part.g: "
            f"{sorted(missing_from_gstar)}"
        )
    else:
        messages.append("OK: all parsed GEANT3 IDs are present in gstar_part.g")

    # Check 2: every code in MD table is in gstar
    missing_md_from_gstar = md_codes - gstar_codes
    if missing_md_from_gstar:
        ok = False
        messages.append(
            f"ERROR: codes in GSTAR_PARTICLE_TABLE.md but NOT in gstar_part.g: "
            f"{sorted(missing_md_from_gstar)}"
        )
    else:
        messages.append("OK: all GEANT3 IDs in GSTAR_PARTICLE_TABLE.md are present in gstar_part.g")

    # Check 3: every code in gstar is in the MD table
    missing_from_md = gstar_codes - md_codes
    if missing_from_md:
        ok = False
        messages.append(
            f"ERROR: codes in gstar_part.g but NOT in GSTAR_PARTICLE_TABLE.md: "
            f"{sorted(missing_from_md)}"
        )
    else:
        messages.append("OK: all GEANT3 IDs in gstar_part.g are present in GSTAR_PARTICLE_TABLE.md")

    return ok, messages


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    repo_root  = os.path.abspath(os.path.join(script_dir, "..", "..", ".."))

    default_input  = os.path.join(script_dir, "gstar_part.g")
    default_output = os.path.join(repo_root,  "GSTAR_PARTICLE_TABLE.md")

    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--input",  default=default_input,
                    help="Path to gstar_part.g")
    ap.add_argument("--output", default=default_output,
                    help="Path to write GSTAR_PARTICLE_TABLE.md")
    args = ap.parse_args()

    print(f"Parsing: {args.input}")
    particles = parse_particles(args.input)
    print(f"  Found {len(particles)} particle definitions")

    table_groups = build_table(particles)
    md = render_markdown(table_groups)

    print(f"Writing: {args.output}")
    with open(args.output, "w") as fh:
        fh.write(md)
        fh.write("\n")

    print("\nVerification:")
    ok, msgs = verify(particles, args.output, args.input)
    for msg in msgs:
        print(f"  {msg}")

    if not ok:
        sys.exit(1)

    print("\nDone.")


if __name__ == "__main__":
    main()
