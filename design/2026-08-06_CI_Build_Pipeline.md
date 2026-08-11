# Building the xGx site with CI

**Status:** live — Pages source switched to GitHub Actions 2026-08-11, deployment enabled
**Date:** 2026-08-06, updated 2026-08-11
**Needs from someone else:** nothing outstanding; the Pages setting has been changed

---

## The one-sentence version

Stop committing the rendered website into the repository. Have GitHub render it
from the `.Rmd` source on every push and publish the result directly. The public
URL does not change.

---

## 1. What "CI" means here

CI is *continuous integration*. On GitHub the product is called **GitHub
Actions**. The idea is simply: **a computer at GitHub runs the build for you,
automatically, every time you push.**

**How the site is built today**

```
edit  Rmarkdown/Multiple_Ascending_Dose_PK.Rmd     (your laptop)
  |
run   Rmarkdown/000_render_site.R                  (your laptop)
  |
      renders into Rmarkdown/www/, then shell-copies
      ~27 .html files + 17 *_files/ dirs + site_libs/ + SiteResources/
      up into the repository root
  |
commit the rendered HTML
  |
GitHub Pages serves whatever HTML is committed at the root of master
```

The website *is* the set of files you committed. Your laptop is the build
server. This is why the root directory is full: **it is a build output
directory that also happens to be where the source lives.**

**How it would work with CI**

```
edit  Rmarkdown/Multiple_Ascending_Dose_PK.Rmd
  |
commit ONLY the .Rmd
  |
GitHub boots a clean Linux machine, installs R and the packages,
runs render_site(), assembles the site
  |
publishes it straight to Pages -- the HTML never enters the repository
```

Vocabulary you will see: a **workflow** is the YAML file describing those steps
(`.github/workflows/build-site.yml`); a **runner** is the temporary machine.
It is free for public repositories.

---

## 2. Why do this

### 2.1 It dissolves the organisation problem instead of relocating it

This is the main reason. Every other layout we considered just moves the mess.

| Approach | Build output in git | Root contents | Needs admin |
| --- | --- | --- | --- |
| Today | yes, at the root | ~47 generated entries + source | no |
| Move output to `docs/` | yes, in `docs/` | `docs/` + source | yes |
| **CI build** | **none** | **`Rmarkdown/ Data/ Resources/ dev/ README.md`** | yes |

There is no clever directory layout that fixes this while the rendered site
lives in the repository, because the rendered site *has* to sit at the path
Pages serves. Removing it from the repository is the fix.

### 2.2 The build is currently unverified

The site could not be built from a clean checkout by anyone, including its
maintainer. `PKPD_Datasets.Rmd` required `RxODE`, which left CRAN in 2022 and is
not installed on the maintainer's machine either — the committed HTML simply
outlived anyone's ability to reproduce it. There was also no dependency
manifest; the package list had to be recovered by grepping every `.Rmd`.

Both are now fixed (§4.1, §5.2). CI makes a clean-machine build a precondition
of every change, which is the only way this stays true.

### 2.3 Committed output silently drifts

`SiteResources/README.html` is committed HTML that references
`site_libs/highlightjs-1.1/` and `site_libs/jquery-1.11.3/` — library versions
that a later render replaced and that no longer exist in the repository. It has
been broken since. In fairness this is a mild example: the file is orphaned,
nothing links to it. But it is the failure mode exactly — a page rendered once,
never regenerated, quietly rotting — and it is structurally impossible when
every page is rebuilt from source on every push.

### 2.4 Contributors stop needing your toolchain

Today, Alison or anyone else needs the full R stack working locally just to
check whether a plot renders. With CI, a pull request builds the whole site and
the result is downloadable from the run.

### 2.5 The repository stops growing

`.git` is ~588 MB. A meaningful part is rendered PNGs recommitted on every
render, plus ~170 MB of `.mp4` stored in *four* places (`SiteResources/` and
`Rmarkdown/SiteResources/` are byte-identical duplicates, both committed). CI
does not shrink existing history, but it stops adding to it.

---

## 3. What is needed

### 3.1 From an admin — one setting — DONE 2026-08-11

**Settings → Pages → Source: "Deploy from a branch" → "GitHub Actions"**

That was the entire ask, and it has been done. Configuration now:

```
build_type: workflow
html_url:   https://opensource.nibr.com/xgx/
```

Note the consequence, because it is what forces the rest of the cutover to
follow promptly: with `build_type: workflow`, the legacy `pages-build-deployment`
job no longer runs. Committing HTML to master no longer changes the live site.
Until the `deploy` job below runs on master, Pages keeps serving the last
legacy build — the site stays up, but frozen.

The `github-pages` environment restricts deployments to `master` only, which is
a useful second lock: a branch cannot publish to production even by accident.

Repo admins as of 2026-08-06: `kliatsko`, `HEBERAN2`, `orladoylenvs`.

**The public URL does not change.** `opensource.nibr.com/xgx/` comes from the
org-level domain on `Novartis/Novartis.github.io` plus this repository's name.
Neither is affected by how the content is built.

Worth asking in the same conversation: whether the Novartis org restricts
GitHub Actions on public repositories (allowed-actions policies are common). If
Actions is blocked, fall back to the `docs/` layout, which needs a different
value of the same setting.

### 3.2 From us — already prototyped

| File | Purpose |
| --- | --- |
| `.github/workflows/build-site.yml` | The workflow. Deployment deliberately disabled. |
| `dev/ci/render_site_ci.R` | Renders and assembles the publish directory without touching the repo root |
| `dev/ci/check_links.py` | Fails the build if a link breaks |
| `DESCRIPTION` | The authoritative dependency manifest |

Deployment was gated on `github.repository_owner != 'Novartis'` while the
pipeline was being proven, so that it ran end to end in the fork
(`https://iamstein.github.io/xgx/`, the same `/xgx/` path depth as production)
without touching the live site. That gate served its purpose and is gone.

**As of 2026-08-11 the condition is `github.ref == 'refs/heads/master'`**, on
both the Pages upload step and the deploy job. Pull requests build and produce
a downloadable artifact; only master publishes.

---

## 4. How the pipeline works

### 4.1 Dependencies

**Resolved 2026-08-06.** Dependencies now live in a `DESCRIPTION` file at the
repository root, which CI reads with `dependencies: '"hard"'`. That is the single
authoritative list, and `pak::local_install_deps()` syncs a local machine from
the same source. `rxode2` sits in `Suggests`, so CI never builds it.

The list was originally recovered by grepping every `.Rmd` for `library()` and
`pkg::`:

```
DT GGally RxODE binom broom caTools dplyr ggplot2 gridExtra htmltools knitr
lubridate plyr rmarkdown scales stringr survival survminer tidyr tidyverse
xgxr zoo
```

`use-public-rspm: true` pulls prebuilt Ubuntu binaries, turning what would be a
~40 minute source build into a few minutes.

### 4.2 Assembling the publish directory

This is the part that preserves every existing URL. `render_site()` emits HTML,
`*_files/`, `site_libs/` and `SiteResources/` into `Rmarkdown/www/`. But the
generated pages also link *directly at source folders*:

| Target | References from published HTML | Purpose |
| --- | --- | --- |
| `Data/*.csv` | 22 | dataset downloads |
| `Rmarkdown/*.Rmd` | 17 | "view the source" links |
| `Resources/*.pdf,.pptx` | 8 | cheat sheets, decks |

So `render_site_ci.R` copies those three into `www/` after rendering. The result
is a directory whose internal structure is byte-for-byte the same shape as
today's repository root, which is why
`opensource.nibr.com/xgx/Data/mt12345.csv` and
`opensource.nibr.com/xgx/Rmarkdown/Adverse_Events.Rmd` keep working.

### 4.3 Caching

The site leans on knitr `_cache` directories so re-rendering is quick. Each CI
run starts on a clean machine, so caches are carried between runs with
`actions/cache`, keyed on the hash of the `.Rmd` files. A cold build — no cache,
compiling ODE models, ~27 pages — is the 90 minute timeout case; a warm build
should be far quicker. **This is the least certain part of the design and needs
a real run to characterise.**

### 4.4 Link checking

`check_links.py` walks the built site, resolves every local `href`/`src`, and
fails on anything missing. Two targets are baselined as already broken
(`Oncology_Efficacy_Data.csv` and `Oncology_Efficacy_Dose.csv`, which
`Oncology_Efficacy_Plots.html` links at the site root though they live in
`Data/`), so the build fails on regressions rather than on inherited debt.
Include fragments under `SiteResources/` and vendored bundles under `site_libs/`
are not crawled — their links resolve against the embedding page, not their own
directory.

---

## 5. Known problems

### 5.1 Datasets are a build side effect — RESOLVED 2026-08-06

> **Resolved.** The first CI run failed on exactly this, and the fix is in.
> `PKPD_Datasets.Rmd` now shows its generation code without evaluating it
> unless `XGX_REGENERATE_DATA=true`, and renders its tables from the committed
> CSVs instead. `RxODE` is gone from the dependency list, which also removes
> the only compiled dependency from the build. The original analysis follows.



`Rmarkdown/PKPD_Datasets.Rmd` **writes into `Data/` while rendering**:

```r
write.csv(Single_Ascending_Dose_Dataset,   "../Data/Single_Ascending_Dose_Dataset.csv",  ...)   # line 148
write.csv(Single_Ascending_Dose_Dataset2,  "../Data/Single_Ascending_Dose_Dataset2.csv", ...)   # line 171
write.csv(Multiple_Ascending_Dose_Dataset, "../Data/Multiple_Ascending_Dose_Dataset.csv", ...)  # line 352
write.csv(Multiple_Ascending_Dose_Dataset2,"../Data/Multiple_Ascending_Dose_Dataset2.csv", ...) # line 381
```

Those CSVs are committed, and every other page reads them. So rendering the site
regenerates its own inputs. It is seeded (`set.seed(12345666)`), so it is
*mostly* deterministic — but it depends on the R version, the RNG
implementation, and RxODE's solver behaviour, none of which are pinned. A CI
machine may well produce different numbers from a 2019 laptop, which would
silently change the data underneath every plot on the site.

**What was done.** Rather than duplicating ~350 lines into a separate script,
where the two copies would drift, the code stays where it is and becomes
opt-in. A `setup` chunk defines `regenerate <- Sys.getenv("XGX_REGENERATE_DATA")
== "true"`, the three generation chunks carry `eval = regenerate`, and a hidden
chunk reads the committed CSVs so the `DT::datatable` displays still work. The
page looks the same and still documents how the data was made; it just no
longer executes it. Regeneration is documented on the page itself.

### 5.2 RxODE — RESOLVED 2026-08-06

`RxODE` was the only compiled dependency and was used solely by
`PKPD_Datasets.Rmd`.

**It was removed from CRAN on 2022-10-10** ("archived as issues were not
corrected in time"), so `pak` could not resolve it at all, and because one
package in the set was unsolvable it reported `dependency conflict` against all
21 others — a single root cause presenting as total failure.

Two things worth drawing out:

* The site has depended on an uninstallable package for nearly four years, and
  nobody knew, because it only ever had to build on one laptop that already had
  an old copy. This is §2.2 in concrete form.
* Removing it means the build now has **no compiled dependencies at all**. No C
  toolchain, no ODE solver. Every remaining package is available as a prebuilt
  Ubuntu binary from RSPM, which should make builds substantially faster.

If the datasets ever do need regenerating, the migration is `RxODE` →
`rxode2`, and that is a deliberate piece of work rather than something every
build has to carry.

### 5.3 No dependency manifest

See §4.1. The package list is a hand-maintained copy of what the `.Rmd` files
happen to use.

---

## 6. Sequence

1. **Done** — `f1cdea7`, move developer-only material into `dev/`. No published
   file moved; all 248 local link targets verified unchanged.
2. **Done** — prototype the workflow, build-only, deployment disabled.
3. Push the branch, let the workflow run, and fix what breaks. **This is where
   the real work is** and it has nothing to do with permissions.
4. Fix §5.1 — split dataset generation out of `PKPD_Datasets.Rmd`.
5. **Done** — compare the CI-built site against the live site. Reviewed by
   Alison, 2026-08-11.
6. **Done 2026-08-11** — Pages source switched to "GitHub Actions".
7. **Done 2026-08-11** — `deploy` job enabled; both fork-only gates replaced
   with `github.ref == 'refs/heads/master'`.
8. **Next** — delete the committed build output from the repository root. Root
   becomes `Rmarkdown/ Data/ Resources/ dev/ design/ README.md LICENSE.md`.
   Safe only *after* a green deploy from master, since until then the committed
   HTML is still what Pages is serving.
9. Retire `000_render_site.R` and its two variants, or reduce them to a thin
   local-preview wrapper around `render_site_ci.R`.

Steps 3–5 were completed *before* step 6, so the request came with a working
pipeline attached rather than a plan.

---

## 7. Decisions taken 2026-08-06

**Where the project lives: unchanged.** xgx and xgxr both stay under
`Novartis`. A personal fork, or a new GitHub org alongside `synpmx` and
`TrinityMetrics`, were both considered and rejected — the 29 stars, 12 forks,
eight years of history, and the `opensource.nibr.com/xgx/` URL cited in
publications and in xgxr's CRAN listing are not worth rebuilding. `iamstein/xgx`
exists solely as a test bed for this pipeline, not as a future home.

Everything in this document applies unchanged either way, and was worth doing
regardless.

**xgxr will not restore the `"h"` / `"d"` unit abbreviations.** They were
removed deliberately and have been gone for some time. The fix belongs in this
repository, and is done — six call sites across
`Multiple_Ascending_Dose_PD_count.Rmd` and
`Multiple_Ascending_Dose_PKPD_count.Rmd` now pass `"hour"` and `"day"`
(commit `2d54f80`).

---

## 8. Asking for the setting

Draft email in `design/2026-08-06_CI_Email_to_Alison_and_Orla.md`. Not to be
sent until a green deploy has been demonstrated in the fork.
