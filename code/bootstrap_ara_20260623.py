#!/usr/bin/env python3
"""Reproducible binary/validation steps for the distractor-control-in-autism ARA.

Run from the project root:
    uv run --with matplotlib,pyyaml code/bootstrap_ara_20260623.py

This script handles the *mechanical* parts of the ARA bootstrap that should be
reproducible: (1) copying manuscript figure PNGs into ara/evidence/figures/,
(2) copying real source code into ara/src/execution/, (3) rendering compact
table-snapshot PNGs for Table 1 and Table A1 from transcribed values, and
(4) a lightweight Seal Level 1 validation pass writing ara/validation/level1_report.json.

The grounded prose ARA files (PAPER.md, logic/*, evidence/*.md, trace/*) are
authored as version-controlled Markdown/YAML alongside this script; this
generator does not overwrite them. Re-running is idempotent.
"""
from __future__ import annotations
import json
import re
import shutil
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
ARA = ROOT / "ara"
EVID_FIG = ARA / "evidence" / "figures"
EVID_TBL = ARA / "evidence" / "tables"
SRC_EXEC = ARA / "src" / "execution"

# ---------------------------------------------------------------------------
# 1. Copy manuscript figure PNGs  (high-res rendered figures -> Figure N)
# ---------------------------------------------------------------------------
# NOTE: figures/image1-6.png and the docx media/image*.png are low-res OLDER
# renders of the analysis plots and do NOT correspond to figure order. The
# descriptively-named fig_*.png are the current high-res manuscript figures.
# Figure 1 (the trial-procedure schematic) is NOT a committed asset anywhere in
# the repo (it lives only in the original manuscript document); it is therefore
# accounted for in evidence/README.md rather than filed with a screenshot.
FIGURE_MAP = {
    "fig_combined_RT_interference.png": "figure2.png",        # RT means + interference
    "fixations_example.png": "figure3.png",                   # example scan paths (qualitative)
    "fig_first_fixation_stacked.png": "figure4.png",          # first-fixation distribution
    "fig_combined_nfixations_interference.png": "figure5.png",# number of fixations + interference
    "fig_refixations_combined.png": "figure6.png",            # target/distractor fixations
    "fig_trait_correlations_combined.png": "figureA1.png",    # trait correlations (appendix)
}


def copy_binaries() -> None:
    EVID_FIG.mkdir(parents=True, exist_ok=True)
    SRC_EXEC.mkdir(parents=True, exist_ok=True)
    for src_name, dst_name in FIGURE_MAP.items():
        src = ROOT / "figures" / src_name
        if src.exists():
            shutil.copy2(src, EVID_FIG / dst_name)
    for fname in ("helper_functions.R", "data_ana.qmd"):
        src = ROOT / fname
        if src.exists():
            shutil.copy2(src, SRC_EXEC / fname)


# ---------------------------------------------------------------------------
# 2. Render compact table-snapshot PNGs (rendered transcriptions, NOT DOCX crops)
# ---------------------------------------------------------------------------
TABLE1_ROWS = [
    ["Measure", "ASD (n=28)", "TD (n=31)", "p", "Effect size"],
    ["Age", "29.8 (7.1)", "28.4 (5.6)", "0.548", "r = -0.09"],
    ["Landolt", "5.8 (0.5)", "5.6 (1.1)", "0.974", "r = 0.005"],
    ["Ishihara", "23.2 (2.3)", "23.2 (1.4)", "0.286", "r = 0.14"],
    ["IQ (CFT-20-R)", "114.9 (20.3)", "116.8 (11.2)", "0.655", "d = -0.12"],
    ["D2 - speed", "102.4 (13.5)", "104.7 (10.5)", "0.464", "d = -0.19"],
    ["D2 - concentration", "102.8 (13.7)", "104.5 (9.1)", "0.666", "r = -0.07"],
    ["BDI", "16.7 (13.2)", "5.19 (5.2)", "<.001 *", "r = -0.63"],
    ["RADS", "33.0 (8.0)", "4.1 (4.0)", "<.001 *", "r = -0.99"],
    ["IU", "67.2 (9.8)", "36.4 (9.4)", "<.001 *", "r = -0.96"],
    ["STAI - trait", "51.9 (11.0)", "31.4 (9.6)", "<.001 *", "d = 1.99"],
    ["STAI - state (pre)", "42.9 (11.5)", "24.4 (7.1)", "<.001 *", "d = 1.93"],
    ["STAI - state (post)", "39.4 (13.3)", "23.2 (7.1)", "<.001 *", "r = -0.72"],
]

TABLEA1_ROWS = [
    ["Trait", "Model", "AIC", "BIC", "Trait_Beta", "Trait_p",
     "Group_Beta", "Group_p", "Prev_Beta", "Prev_p", "Inter_Beta", "Inter_p"],
    ["BDI", "1: Trait", "1383.0", "1391.3", "15.821", ".043", "—", "—", "—", "—", "—", "—"],
    ["BDI", "2: +Group+Prev", "1378.6", "1392.5", "2.927", ".738", "25.124", ".005", "-1.455", ".846", "—", "—"],
    ["BDI", "3: +Trait×Group", "1380.6", "1397.2", "4.590", ".713", "24.337", ".014", "-1.455", ".847", "-2.348", ".851"],
    ["STAI-Trait", "1: Trait", "1379.7", "1388.0", "20.922", ".007", "—", "—", "—", "—", "—", "—"],
    ["STAI-Trait", "2: +Group+Prev", "1378.6", "1392.5", "3.851", ".720", "23.881", ".027", "-1.455", ".846", "—", "—"],
    ["STAI-Trait", "3: +Trait×Group", "1379.5", "1396.2", "4.755", ".659", "23.636", ".029", "-1.455", ".846", "-10.990", ".309"],
    ["IU", "1: Trait", "1376.8", "1385.1", "24.515", ".002", "—", "—", "—", "—", "—", "—"],
    ["IU", "2: +Group+Prev", "1378.6", "1392.4", "6.406", ".657", "21.171", ".143", "-1.455", ".846", "—", "—"],
    ["IU", "3: +Trait×Group", "1380.5", "1397.2", "6.416", ".658", "21.115", ".146", "-1.455", ".847", "1.073", ".941"],
]


def render_table_png(rows, out_path: Path, title: str, col_w=None) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    n_rows = len(rows)
    n_cols = len(rows[0])
    fig_w = max(7, n_cols * 1.25)
    fig_h = max(2.2, n_rows * 0.34 + 0.6)
    fig, ax = plt.subplots(figsize=(fig_w, fig_h))
    ax.axis("off")
    ax.set_title(title, fontsize=10, fontweight="bold", loc="left", pad=10)
    tbl = ax.table(cellText=rows[1:], colLabels=rows[0], loc="center", cellLoc="center")
    tbl.auto_set_font_size(False)
    tbl.set_fontsize(7.5)
    tbl.scale(1, 1.25)
    for (r, c), cell in tbl.get_celld().items():
        if r == 0:
            cell.set_facecolor("#34495e")
            cell.set_text_props(color="white", fontweight="bold")
        elif r % 2 == 0:
            cell.set_facecolor("#f2f4f5")
        cell.set_edgecolor("#cccccc")
    fig.text(0.01, 0.01,
             "Rendered transcription snapshot (generated from manuscript text) — NOT a DOCX/PDF crop.",
             fontsize=6, style="italic", color="#888888")
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close(fig)


def render_tables() -> None:
    EVID_TBL.mkdir(parents=True, exist_ok=True)
    render_table_png(TABLE1_ROWS, EVID_TBL / "table1.png",
                     "Table 1. Descriptive characteristics for ASD and TD groups")
    render_table_png(TABLEA1_ROWS, EVID_TBL / "tableA1.png",
                     "Table A1. Model comparison for traits BDI, STAI-Trait, IU")


# ---------------------------------------------------------------------------
# 3. Lightweight Seal Level 1 validation
# ---------------------------------------------------------------------------
MANDATORY = [
    "PAPER.md",
    "logic/problem.md", "logic/claims.md", "logic/concepts.md",
    "logic/experiments.md", "logic/related_work.md",
    "logic/solution/constraints.md",
    "src/environment.md",
    "trace/exploration_tree.yaml",
    "evidence/README.md",
]


def validate() -> dict:
    report = {"checks": [], "failures": [], "counts": {}}

    def check(name, ok, detail=""):
        report["checks"].append({"check": name, "pass": bool(ok), "detail": detail})
        if not ok:
            report["failures"].append(f"{name}: {detail}")

    # mandatory files
    for rel in MANDATORY:
        p = ARA / rel
        check(f"exists:{rel}", p.exists() and p.stat().st_size > 0,
              "missing or empty" if not (p.exists() and p.stat().st_size > 0) else "")

    claims_txt = (ARA / "logic/claims.md").read_text() if (ARA / "logic/claims.md").exists() else ""
    exp_txt = (ARA / "logic/experiments.md").read_text() if (ARA / "logic/experiments.md").exists() else ""

    claim_ids = set(re.findall(r"^## (C\d{2}):", claims_txt, re.M))
    exp_ids = set(re.findall(r"^## (E\d{2}):", exp_txt, re.M))
    report["counts"]["claims"] = len(claim_ids)
    report["counts"]["experiments"] = len(exp_ids)
    check("claims>=1", len(claim_ids) >= 1, f"{len(claim_ids)} claims")
    check("experiments>=3", len(exp_ids) >= 3, f"{len(exp_ids)} experiments")

    # claim Proof -> experiments resolve
    for cid in sorted(claim_ids):
        block = claims_txt.split(f"## {cid}:", 1)[1].split("\n## ", 1)[0]
        proof_refs = set(re.findall(r"E\d{2}", block.split("Proof")[1].split("\n")[0])) if "Proof" in block else set()
        missing = proof_refs - exp_ids
        check(f"proof-resolves:{cid}", not missing, f"unresolved {missing}")

    # experiment Verifies -> claims resolve
    for eid in sorted(exp_ids):
        block = exp_txt.split(f"## {eid}:", 1)[1].split("\n## ", 1)[0]
        ver_refs = set(re.findall(r"C\d{2}", block.split("Verifies")[1].split("\n")[0])) if "Verifies" in block else set()
        missing = ver_refs - claim_ids
        check(f"verifies-resolves:{eid}", not missing, f"unresolved {missing}")

    # experiments contain no exact numeric RESULTS (reported values, not metric names).
    # Flags p = .NNN, F(df, and "η... = .NN"; a bare "partial η²" metric name is allowed.
    exp_numbers = re.findall(r"(p\s*[<=]\s*\.?\d|F\(\d|η[^\n]{0,8}=\s*\.?\d)", exp_txt)
    check("experiments-directional", not exp_numbers,
          f"found reported values: {exp_numbers[:5]}")

    # evidence: every figureN/tableN png has a sibling md and vice versa
    pngs = sorted(EVID_FIG.glob("*.png")) + sorted(EVID_TBL.glob("*.png"))
    for png in pngs:
        md = png.with_suffix(".md")
        check(f"evidence-pair:{png.parent.name}/{png.stem}", md.exists(),
              "missing sibling .md")
    mds = sorted(EVID_FIG.glob("*.md")) + sorted(EVID_TBL.glob("*.md"))
    for md in mds:
        png = md.with_suffix(".png")
        check(f"evidence-png:{md.parent.name}/{md.stem}", png.exists(),
              "missing sibling .png")
    report["counts"]["evidence_figures"] = len(list(EVID_FIG.glob("*.md")))
    report["counts"]["evidence_tables"] = len(list(EVID_TBL.glob("*.md")))

    # YAML parses + nodes have support_level
    try:
        import yaml
        tree = yaml.safe_load((ARA / "trace/exploration_tree.yaml").read_text())

        def walk(nodes):
            cnt = 0
            for n in nodes or []:
                cnt += 1
                if "support_level" not in n:
                    report["failures"].append(f"tree node {n.get('id')} missing support_level")
                cnt += walk(n.get("children"))
            return cnt

        n_nodes = walk(tree.get("tree", []))
        report["counts"]["trace_nodes"] = n_nodes
        check("yaml-parses", True, f"{n_nodes} nodes")
    except Exception as e:  # noqa: BLE001
        check("yaml-parses", False, str(e))

    concepts_txt = (ARA / "logic/concepts.md").read_text() if (ARA / "logic/concepts.md").exists() else ""
    report["counts"]["concepts"] = len(re.findall(r"^## ", concepts_txt, re.M))

    report["passed"] = len(report["failures"]) == 0
    return report


def main() -> None:
    copy_binaries()
    render_tables()
    report = validate()
    (ARA / "validation").mkdir(parents=True, exist_ok=True)
    (ARA / "validation/level1_report.json").write_text(json.dumps(report, indent=2))
    print(json.dumps(report, indent=2))
    print("\nPASSED" if report["passed"] else f"\nFAILED ({len(report['failures'])} issues)")


if __name__ == "__main__":
    main()
