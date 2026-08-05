# simbje.github.io — Personal Website

A Quarto personal website with an automated SSB analysis blog (a new analysis every 3 days, plus a machine-learning forecasting special every Friday), powered by the Anthropic API.

## Structure

```
├── index.qmd                    # Homepage
├── cv.qmd                       # Curriculum Vitae
├── blog/
│   ├── index.qmd                # Blog listing
│   └── posts/                   # Manual blog posts
├── projects/
│   ├── index.qmd                # Projects gallery
│   └── *.qmd
├── ssb-daily/
│   ├── index.qmd                # SSB Analysis listing page (auto)
│   ├── generate_post.R          # AI post generator (every 3 days)
│   ├── generate_special.R       # Friday ML forecasting special (Opus)
│   ├── r-packages.txt           # R deps for GH Actions cache
│   └── posts/
│       └── YYYY-MM-DD/          # Generated posts
│           └── index.qmd
├── assets/styles.scss           # Custom theme
├── _quarto.yml                  # Site config
└── .github/workflows/deploy.yml # SSB automation
```

---

## Setup (one time)

### 1. Fork / clone and configure

```bash
git clone https://github.com/simbje/simbje.github.io.git
```

Edit `_quarto.yml` with your repo name, and `index.qmd` / `cv.qmd` with your details.

### 2. Add your Anthropic API key to GitHub Secrets

Go to **Settings → Secrets and variables → Actions → New repository secret**:

```
Name:  ANTHROPIC_API_KEY
Value: sk-ant-...your key...
```

### 3. Enable GitHub Pages

Go to **Settings → Pages → Source → GitHub Actions**

### 4. Push to main

The workflow triggers on push — your site goes live. After that, a new SSB analysis post is generated automatically every 3 days, with a machine-learning forecasting special every Friday.

---

## How the Post Generation Works

```
06:00 UTC · every 3 days (ML special on Fridays)
      ↓
GitHub Actions runner starts
      ↓
generate_post.R (or generate_special.R on Fridays) runs
  - Checks if today's post already exists (skips if so)
  - Sends a prompt to Claude (claude-opus-4-5) via Anthropic API
  - Claude picks SSB datasets, writes full .qmd with R code + analysis
  - Saves to ssb-daily/posts/YYYY-MM-DD/index.qmd
      ↓
git commit & push the new post file
      ↓
quarto render (renders entire site including new post)
      ↓
Deploy to GitHub Pages
      ↓
New post live at yourusername.github.io/ssb-daily/
```

Claude is instructed to:
- Pick different datasets each run from a curated SSB catalogue
- Use different chart types (ridgelines, waffle, lollipop, maps, beeswarm, etc.)
- Write with a TidyTuesday-inspired journalistic style
- Always include 3–5 ggplot2 charts with cohesive color palettes
- Add prose commentary explaining the analytical choices

### Manually triggering a post

Go to **Actions → SSB Analysis + Deploy → Run workflow**.  
Toggle "Force regenerate" if you want to replace today's post.

### Customising the prompt

Edit `ssb-daily/generate_post.R` — the `SYSTEM_PROMPT` and `SSB_CATALOGUE` variables 
control what Claude knows and what style it writes in.

---

## Adding a Manual Blog Post

```bash
mkdir blog/posts/2025-04-my-topic
```

Create `blog/posts/2025-04-my-topic/index.qmd`:

```yaml
---
title: "My Post"
description: "Short description"
date: "2025-04-01"
categories: [R, Trading]
---
Your content here...
```

Commit and push — it appears in the blog listing automatically.

---

## Theming

Edit `assets/styles.scss`. Key variables:

```scss
$primary:   #1a1a2e;   // Dark navy
$highlight: #e94560;   // Red accent
$accent:    #0f3460;   // Mid navy
```

---

## Key R Packages

| Package | Purpose |
|---|---|
| `httr2` | Anthropic API calls |
| `PxWebApiData` | SSB data fetching |
| `ggplot2` + extensions | All visualizations |
| `MetBrewer` | Beautiful color palettes |
| `patchwork` | Combining charts |
| `ggtext` | Markdown in plot titles |
| `gganimate` | Animated charts |
| `sf` + `rnaturalearth` | Norwegian maps |
