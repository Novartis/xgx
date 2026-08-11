## Plan for Update (as of 2026-08-11) for first gen update

Everything below is on branch `andy-reorg-nochange`, which has **not** been
pushed to `Novartis/xgx`. Detail and rationale live in
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

### Step 1 — cutover to CI

Ordered, and each step is reversible on its own.

1. **Alison reviews the branch** — blocking. Specifically `PKPD_Datasets.Rmd`,
   since the dataset generation is her code and the engine underneath it has
   changed.
2. **Flip the two deploy guards** in `build-site.yml`. Both the Pages artifact
   upload (line ~121) and the `deploy` job (line ~136) are currently gated
   `github.repository_owner != 'Novartis'`, which scoped them to the fork.
   These become `github.ref == 'refs/heads/master'`. *Pushing the branch without
   this change produces a green build that deploys nothing.*
3. **Push the branch to `Novartis/xgx` and merge to master.**
4. **Set Settings → Pages → Source to "GitHub Actions."** This is a maintain-level
   setting, not admin — no admin needed. (It was flipped on 2026-08-11 and
   reverted the same day, to keep the setting in step with the code until the
   workflow actually lands.)
5. **Verify against the done criteria below before touching anything else.**

Rollback at any point is changing Pages Source back to "Deploy from a branch"
(`master`, `/root`). The committed HTML is still in the repository at this
stage, so the previous site returns immediately.

### Step 2 — clean-up, only after CI has been live for one release cycle

This is the irreversible part, and it is deliberately separated from Step 1.
Nothing here should happen until the CI-built site has been serving correctly
and someone has clicked through it.

1. **Delete the generated HTML from the repository root** — the ~27 `*.html`
   files and their `*_files/` companions, plus `site_libs/`. CI regenerates all
   of it; from here on generated output never enters the repository.
2. **Delete the published copy of `SiteResources/` at the root.** The source
   copy under `Rmarkdown/SiteResources/` is the one the build uses.
3. **Consider deleting `SiteResources/README.html` outright** rather than
   keeping it fixed. It has no `.Rmd` source, an empty `<title>`, nothing links
   to it, and `_site.yml` does not reference it — only `header.html` and
   `body.html`. Its internal nav links are still wrong (they assume it sits at
   the root). It is a stale render from the original bulk upload.
4. **Confirm the root is down to** `Rmarkdown/ Data/ Resources/ dev/ README.md`
   plus `DESCRIPTION`, `LICENSE.md` and `.github/`.

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
- No `.html` committed anywhere outside `Rmarkdown/SiteResources/`.

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