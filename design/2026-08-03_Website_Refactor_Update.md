## Plan for Update (as of 2026-08-11) for first gen update

Everything below was on branch `andy-reorg-nochange`, which **merged to
`Novartis/xgx` master on 2026-08-11** as PR #62. Steps 0–2 are complete and the
live site is built by CI. Detail and rationale live in
`design/2026-08-06_CI_Build_Pipeline.md`.

### Already done on the branch

- **CI pipeline built and proven.** `.github/workflows/build-site.yml` renders
  all 27 pages from source and checks links. Demonstrated end to end on the
  fork: https://iamstein.github.io/xgx/ (build 4m47s, deploy 19s).
- **RxODE removed.** Migrated to `rxode2`, dataset generation made opt-in, and
  the committed datasets in `Data/` are now treated as source (`12a2a77`,
  `1a7ad39`). RxODE left CRAN 2022-10-10, so `PKPD_Datasets` had become
  unbuildable by anyone.
- **`xgx_scale_x_time_units` fixed** in six call sites for xgxr 1.1.6, which
  rejects the `"h"`/`"d"` abbreviations (`2d54f80`). Decided: xgxr will *not*
  restore the abbreviations — they were removed deliberately, so the fix
  belongs here.
- **JavaScript references fixed** in `SiteResources/README.html`. It pointed at
  `jquery-1.11.3` and `highlightjs-1.1`, neither of which is still in
  `site_libs/`, and every asset path was also one directory level too shallow.
  Repointed to `jquery-3.6.0` / `highlightjs-9.12.0` and to `../site_libs/`, in
  both the source copy under `Rmarkdown/SiteResources/` and the published copy
  at the root. All eight now resolve.
- **Developer-only material moved** into `dev/` (`f1cdea7`) — `Rlib/`, `Test/`
  and the CI scripts. This is the part of the reorganisation that was safe to
  do before CI is live.

### Step 0 — before anything else, Monday morning

**Does the Novartis org restrict GitHub Actions on public repositories?**
Allowed-actions policies are common. `repos/Novartis/xgx/actions/permissions`
is admin-only and returns 403, so this has to come from Orla or another admin.
If Actions is restricted, the cutover below needs a different approach and it
is better to know that before Alison spends time reviewing.

### Step 1 — cutover to CI — DONE 2026-08-11

1. **Done** — Alison reviewed the branch, including `PKPD_Datasets.Rmd`.
2. **Done** — both deploy guards flipped from
   `github.repository_owner != 'Novartis'` to `github.ref == 'refs/heads/master'`
   (`cc3355a`).
3. **Done** — merged to master as PR #62 (`e790042`). Build green, deploy 27s.
4. **Done** — Pages Source set to "GitHub Actions". Confirmed via the API as
   `build_type: workflow`. It is a maintain-level setting; no admin was needed.
   Worth recording: the `github-pages` environment separately restricts
   deployments to `master`, so a branch cannot publish to production.
5. **Done** — verified against the done criteria below.

One ordering hazard, since the note above got it backwards in practice: flipping
the Pages setting *before* the workflow lands stops the legacy branch build
without putting anything in its place. The site keeps serving its last legacy
deployment, but goes stale — committing HTML no longer changes it. The window is
harmless as long as the merge follows promptly, which is what happened.

Rollback **was** changing Pages Source back to "Deploy from a branch"
(`master`, `/root`), which worked while the committed HTML was still in the
repository. Step 2 removed it, so that route is gone; rollback now means
reverting the merge, or `git revert` of the deletion commit to restore the root
HTML first.

### Step 2 — clean-up — DONE 2026-08-11

Done the same day as Step 1 rather than after a release cycle, at Andy's call,
once the CI-built site had been verified serving correctly. 292 files removed.

Before deleting, every root `*.html` was checked against `Rmarkdown/*.Rmd`:
all 27 had a source, so CI regenerates the lot and no page was lost.

1. **Done** — deleted the ~27 `*.html` and their 17 `*_files/` companions, plus
   `site_libs/`.
2. **Done** — deleted the published copy of `SiteResources/` at the root.
3. **Done** — deleted `SiteResources/README.html` outright, in both the root
   copy and the source copy under `Rmarkdown/SiteResources/`. Nothing
   references it; confirmed by grep across `.Rmd`, `.R`, `.yml`, `.html` and
   `.py`.
4. **Done** — root is now exactly `Rmarkdown/ Data/ Resources/ dev/ design/
   README.md DESCRIPTION LICENSE.md .github/ .gitignore`.

Two things done alongside, because the cleanup does not hold without them:

* **`.gitignore` now ignores `/*.html`, `/*_files/`, `/site_libs/` and
  `/SiteResources/`.** At the time, `Rmarkdown/000_render_site.R` still
  shell-copied `www/` into the root, so without this the next person to run the
  old local build recreated all 292 files and could recommit them.
* **`README.md` rewritten** — it described the root as the published website
  and told contributors to run `000_render_site.R`.

### Step 3 — retire the old local build — DONE 2026-08-11

`000_render_site.R` and both `_clear_cache` variants deleted. They were the
mechanism that put build output in the root, so leaving them in place would
have made Step 2 a matter of time rather than a fix.

Replaced by `dev/render_site_local.R`:

```r
source("dev/render_site_local.R")
render_xgx_site()                     # incremental
render_xgx_site(clear_cache = TRUE)   # from scratch
```

It wraps `dev/ci/render_site_ci.R` — the same script CI runs, so local and CI
builds cannot drift — and writes only into the gitignored `Rmarkdown/www/`.
Cache clearing, the one thing the variants added that was worth keeping, clears
both `*_cache` and `*_files`; clearing only the first is the trap that yields a
build that looks fine while serving stale plots.

`000_render_site_clear_cache_install_xgxr.R` was deleted rather than ported: it
installed the vendored `dev/Rlib/xgxr_1.0.2.tar.gz`, and since `2d54f80` fixed
the site for xgxr 1.1.6, running it would downgrade xgxr and break the build.

This orphans `dev/Rlib/` — a vendored xgxr 1.0.2 install plus its tarball, ~35
`.html` help pages among them. Nothing references it now; xgxr comes from CRAN
via `DESCRIPTION`. Deleting it is available whenever someone wants to.

Note this does not shrink `.git`, which is ~588 MB largely from rendered images
recommitted on every render. It stops it growing, which is the achievable win.

### Constraints — these do not move

**`Data/`, `Rmarkdown/` and `Resources/` stay exactly where they are.** The
published pages link directly into all three, and those URLs are cited in
publications and from xgxr's CRAN listing. "Reorganise the folder structure"
means *remove generated output from the root* — it does not mean relocating
source directories. Verified link shapes that must keep working:

    /xgx/Data/mt12345.csv
    /xgx/Rmarkdown/Adverse_Events.Rmd
    /xgx/Resources/Presentation_Checklist_v2.03.pdf

The public URL `opensource.nibr.com/xgx/` is unaffected by any of this — it
comes from the org-level Pages domain plus the repository name, neither of
which changes.

### Done criteria

- All 27 pages return 200.
- The three link shapes above return 200.
- `dev/ci/check_links.py` passes and the build is green.
- No *generated website* `.html` committed anywhere outside
  `Rmarkdown/SiteResources/`, which keeps only the three fragments the build
  includes: `header.html`, `body.html`, `icon_nav.html`.

  The one exception, deliberate: `dev/Rlib/xgxr/` contains ~35 `.html` help
  pages belonging to a vendored xgxr package install. Those are a local R
  library, not site output, and nothing publishes them — `dev/` is not copied
  into `www/`. Whether that vendored copy should exist at all is a separate
  question from this cleanup.

### Not in this generation

Andy is handling the **data checking page** update separately.

Explicitly deferred to a later generation, and tracked in the sections below:
the new `xgx_breaks_x_time_units()` function in xgxr, the simpler caption, the
AI/skill files, synpmx, and the RECIST dosing code improvements.

## xGx Refactor

For many reasons, we'd like to update the xGx website.  The key guiding principle for the update though is that nothing should break.  Every page should continue to compile.  

This should all be doen in a new branch.

Before anything begins, look through the xGx MS Teams space and todos and issues and see what else should be added.

## Organization

- Should this site be organized a bit better?  I think so.  I don't like how so many files and folders are in the top directory.  Can this be reorganized without breaking anything.  
- Should the html files be committed in git or compiled in git and git-pages?
- The next version should be developde to enable AI development/use (i.e. use the pages as templates) but also there will be skill files to help with data checking and plot interpretation.

## xGx usage

- use a new caption that's a bit simpler.
- change the way xgx_scale_x_time_units is used.  Scaling time in the ggplot() object and just using xgx_scale_x_time_units to set breaks and ticks.  Ah, maybe even create a new function for this, to put in xgxr.  like xgx_breaks_x_time_units() or something like that
- look for places where code might be improved - like the dosing in the tumor size RECIST plots

## Added functionality

- The data checking and data exploration.  I had generated ideas for additional plots, maybe from dose finding toolbox.  Improve that.  Also, look for ideas for how to improve.  Think about if synpmx sohuld relate in some ways here as there is a validate function.  I've purposefully gone away from functions because they do make it harder for people to run and understand code line by line...  Revisit that idea