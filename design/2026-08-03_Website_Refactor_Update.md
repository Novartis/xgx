# xGx Website Refactor — open items and next steps

**Updated 2026-08-11.** The first-generation update is finished: the site is
built from source by GitHub Actions on every push to master, no rendered output
is committed, and the data checking page has been rewritten as an IDA report.

Completed work is not described here. It is in git, as PRs
[#62](https://github.com/Novartis/xgx/pull/62) (CI cutover and the `dev/`
reorganisation), [#63](https://github.com/Novartis/xgx/pull/63) (deleting the
committed build output), [#64](https://github.com/Novartis/xgx/pull/64) and
[#65](https://github.com/Novartis/xgx/pull/65) (the data checking page), and
[#66](https://github.com/Novartis/xgx/pull/66). The live description of how the
build works is `.github/workflows/build-site.yml` and `dev/ci/`, not a document.

What follows is only what is still open.

---

## Guiding principle — does not move

**Nothing should break. Every page must continue to compile.**

**`Data/`, `Rmarkdown/` and `Resources/` stay exactly where they are.** The
published pages link directly into all three, and those URLs are cited in
publications and from xgxr's CRAN listing. "Reorganise the folder structure"
means *remove generated output from the root* — it does not mean relocating
source directories. Link shapes that must keep working:

    /xgx/Data/mt12345.csv
    /xgx/Rmarkdown/Adverse_Events.Rmd
    /xgx/Resources/Presentation_Checklist_v2.03.pdf

The public URL `opensource.nibr.com/xgx/` is unaffected by any of this — it
comes from the org-level Pages domain plus the repository name, neither of
which changes.

---

## Open items

Found during the cleanup and deliberately not taken. These are editorial or
content calls rather than build work, so they were left for someone who knows
the material.

### Content

* **Three published pages are unreachable from the navigation** —
  `Multiple_Ascending_Dose_PD_receptor_occupancy`,
  `Multiple_Ascending_Dose_PKPD_receptor_occupancy` and
  `Presentations_Publications`. They render, they are live, and nothing in
  `_site.yml` or on any page links to them.

  The two receptor-occupancy pages are ~43-line stubs carrying
  `status = "DRAFT"` that load no data at all — work started and parked.
  `Presentations_Publications` is the odd one: it is complete, and it is where
  the ACoP videos are cited from, yet nothing reaches it. Adding it to the nav
  is more likely right than deleting it.

* **Six unreferenced files in `Resources/`, ~15.7 MB.** Published, with public
  URLs, and nothing on the site links them:

  | File | Size |
  | --- | --- |
  | `Uncertainty_Assessment_Presentation.pdf` | 14 MB |
  | `Graphics_Principles_Cheat_Sheet_v1.1.pdf` | 880 KB |
  | `Giving_and_Receiving_Feedback.docx` | 628 KB |
  | `ContextOfUse_Table_1page.pdf` | 124 KB |
  | `Presentation_Checklist_v2.04.docx` | 72 KB |
  | `Presentation_Outline_Template.pptx` | 28 KB |

  Documents, not build output, so the question is editorial: meant for the
  Resources page and never linked, or superseded?
  `Presentation_Checklist_v2.04.docx` is the sharpest case — the page links
  `v2.03.pdf`, so a newer revision sits unlinked beside the old one.

* **Five unreferenced images** in `Rmarkdown/SiteResources/`:
  `AE_vs_AUC_boxplots.png`, `Count_Hazard_Figure.png`, `Kaplan_Meier.png`,
  `Lab_Marker_Pct_of_ULN.png`, `Safety_icon.png`. ~250 KB. `Safety_icon` is
  suggestive — an icon for a nav section that does not exist.

* **`dev/Test/`** — two scratch files, kept deliberately in `f1cdea7`.

### Data

* **Defects in `Data/Data_Checking.csv`**, surfaced by the integrity checks
  added to the data checking page. The page reports them; the data has not been
  fixed.

  - Four covariates vary within a subject. `AGEB` takes nine distinct values
    within subject 1, though it is a *baseline* covariate.
  - Three duplicate event records on `USUBJID + CMT + EVID + TIME`.
  - One subject has no observation records.
  - `CENS` is set only on pre-dose records whose values sit *above* the LLOQ,
    while 15 observations below the LLOQ are unflagged — so `CENS` in this
    dataset is not marking BLQ, and `LLOQ = 10` is applied to the PD marker as
    well as PK.

### Repository size

* **The two `.mp4` files are 177 MB of the working tree**, both genuinely used
  by the ACoP poster and tutorial pages. Deleting the root `SiteResources/`
  copy already halved this.

* **`.git` is ~588 MB**, largely rendered images recommitted over the years.
  The cutover stopped it growing but did not shrink it. Shrinking means
  rewriting history, which breaks every existing clone and fork, and is not
  obviously worth it.

---

## Next generation

Deferred deliberately, not forgotten.

### Repository and tooling

- Before picking any of this up, look through the xGx MS Teams space and todos
  and issues and see what else should be added.
- The next version should be developed to enable AI development/use (i.e. use
  the pages as templates) but also there will be skill files to help with data
  checking and plot interpretation.

### xGx usage

- Use a new caption that's a bit simpler.
- Change the way `xgx_scale_x_time_units` is used. Scaling time in the
  `ggplot()` object and just using `xgx_scale_x_time_units` to set breaks and
  ticks. Maybe even create a new function for this, to put in xgxr — like
  `xgx_breaks_x_time_units()` or something like that.
- Look for places where code might be improved — like the dosing in the tumor
  size RECIST plots.

### Data checking and exploration

The data checking page itself is done. What remains is the wider idea behind it:

- Additional plots, maybe from the dose finding toolbox. Look for ideas on how
  to improve.
- Think about whether synpmx should relate here, as it has a validate function.
- Revisit the decision to avoid functions. Going away from functions was
  deliberate — they make it harder for people to run and understand code line
  by line — but it is worth revisiting.
- The page's own parking lot carries the concrete follow-ups: checking a dataset
  against a company data specification (a job for an agent, since spec formats
  vary by company), an AI-assisted review of the exported summary, and the
  `NMdata` / `apmx` / `pointblank` / `dataquieR` tooling options.
