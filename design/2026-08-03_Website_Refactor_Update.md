## Guiding principle 

**Nothing should break. Every page must continue to compile.**

This site should enable AI development/use (i.e. use the pages as templates) together with `synpmx` package
There could ultimately be created some skill files to help use with adapting these sites as templates. 

---

## Open items found during cleanup

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

### Repository size

* **The two `.mp4` files are 177 MB of the working tree**, both genuinely used
  by the ACoP poster and tutorial pages. Deleting the root `SiteResources/`
  copy already halved this.

* **`.git` is ~588 MB**, largely rendered images recommitted over the years.
  The cutover stopped it growing but did not shrink it. Shrinking means
  rewriting history, which breaks every existing clone and fork, and is not
  obviously worth it.

---

## Next Set of Improvements

### xGx usage

- Use a new caption that's a bit simpler.
- Change the way `xgx_scale_x_time_units` is used. Scaling time in the
  `ggplot()` `aes` call and just using `xgx_scale_x_time_units` to set breaks and
  ticks. Maybe even create a new function for this, to put in xgxr — like
  `xgx_breaks_x_time_units()` or something like that.
- Look for places where code might be improved — like the dosing in the tumor
  size RECIST plots.

### AI SKILL files together with synpmx

- begin implementing AI workflow with skill files for agents, once synpmx approved for use.