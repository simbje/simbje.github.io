# Research Trends Radar

Bi-weekly arXiv scan across three areas — **data science** (`stat.ML`, `cs.LG`, `stat.ME`, `stat.AP`), **finance** (`q-fin.*`), and **social sciences** (`econ.*`). A Claude AI agent picks the **top-5 most-trending papers per area** and clusters all candidates into named topics. The Quarto page (`index.qmd`) reads every accumulated snapshot to render the three top-5 lists, a topic timeline, and an interactive `visNetwork` mind graph.

> **Note** — the folder is still named `ssrn-research/` for historical reasons (an earlier draft included SSRN). The pipeline now uses arXiv only.

## Files

| Path | Purpose |
|---|---|
| `scan_ssrn.R` | Bi-weekly scanner (Phase A: three arXiv queries; Phase B: Claude tool-use clustering) |
| `index.qmd` | Quarto page reading all snapshots → 3 top-5 lists, timeline, mind graph |
| `r-packages.txt` | Package list for the GitHub Actions install step |
| `snapshots/YYYY-MM-DD.json` | One per run; the timeline + graph accumulate from these |

## Manual triggers

```powershell
# Phase A only — no API key, no LLM cost. Verifies fetchers + JSON schema.
Rscript ssrn-research/scan_ssrn.R --dry-run

# Full run with the agent.
$env:ANTHROPIC_API_KEY = "sk-ant-..."
Rscript ssrn-research/scan_ssrn.R

# Render only this page after a successful run:
quarto render ssrn-research/index.qmd
```

The script is idempotent: re-running on the same day skips if today's snapshot already exists.

## Schedule

`.github/workflows/ssrn-research.yml` runs at **08:00 UTC on the 1st and 15th** of each month, plus on `workflow_dispatch`. The workflow only writes snapshots — pushing to `main` triggers `deploy.yml` which renders and deploys the site.

## Failure modes

All paths exit 0 (no Action failure) and write a stub snapshot where possible:

- arXiv returns no papers in any bucket → stub snapshot with `candidates_count: 0`.
- `ANTHROPIC_API_KEY` missing → Phase A only, stub snapshot.
- Anthropic API down → no snapshot written, error logged to `_last_error.txt`.
- Today's snapshot already exists → skip.
