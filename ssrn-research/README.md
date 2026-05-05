# Research Trends Radar

Bi-weekly scan of recent research in data analytics, data science, finance, and quantitative finance. A Claude AI agent clusters arXiv (and best-effort SSRN) papers from the last 14 days into named topics and surfaces the top-3 most-trending pieces. The Quarto page (`index.qmd`) reads every accumulated snapshot to render a topic timeline and an interactive `visNetwork` mind graph.

## Files

| Path | Purpose |
|---|---|
| `scan_ssrn.R` | Bi-weekly scanner (Phase A fetch + Phase B Claude tool-use clustering) |
| `index.qmd` | Quarto page reading all snapshots → top-3, timeline, mind graph, table |
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

- arXiv + SSRN both empty → stub snapshot with `candidates_count: 0`.
- `ANTHROPIC_API_KEY` missing → Phase A only, stub snapshot.
- Anthropic API down → no snapshot written, error logged to `_last_error.txt`.
- Today's snapshot already exists → skip.
