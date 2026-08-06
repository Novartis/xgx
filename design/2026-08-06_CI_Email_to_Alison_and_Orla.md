# Draft email: asking for the Pages setting change

**Date:** 2026-08-06
**To:** Alison Margolskee, Orla Doyle
**Purpose:** ask for one GitHub Pages setting to be changed on `Novartis/xgx`

This is a draft. Edit freely before sending — particularly the tone, which is
pitched at "colleague explaining a small ask", not at a formal change request.

Background for the ask is in `design/2026-08-06_CI_Build_Pipeline.md`.

---

**Subject:** xgx website — one GitHub setting, and why it's worth changing

Hi Alison, Orla,

I've been modernising how the xgx website gets built, and I'd like to ask for
one small change that I can't make myself. Short version: **the site should be
built automatically by GitHub rather than by me pasting rendered HTML into the
repository, and switching that on is a single setting.**

## The ask

On `github.com/Novartis/xgx` → **Settings → Pages → Source**, change
*"Deploy from a branch"* to *"GitHub Actions"*.

That's it. Orla, I believe you're one of the three people with admin on the repo
(along with `kliatsko` and `HEBERAN2`), so it'd need to be you or one of them —
either making the change, or granting me admin so I can.

**The public URL does not change.** `opensource.nibr.com/xgx/` comes from the
Novartis org-level Pages domain plus the repository name, and neither is
affected by how the content is built. Every existing page URL, dataset download
link and `.Rmd` source link keeps working. I've verified this carefully, because
xgxr's CRAN listing points at that URL and it's cited in our publications.

## Why — three things that were quietly broken

Today the repository stores ~27 rendered HTML files that I generate on my laptop
and commit. The site is whatever was last committed. Nothing rebuilds it, so
nothing checks it.

I built a pipeline that rebuilds the site from source on a clean machine, and on
its first attempts it found three defects that had been live on the site for
years. None were introduced by this work — all three were invisible *because*
the output was stored rather than rebuilt:

1. **`PKPD_Datasets` could not be rebuilt by anyone.** It depends on `RxODE`,
   which was removed from CRAN on 2022-10-10. It isn't installed on my machine
   either. The published page has simply outlived our ability to reproduce it.
   Fixed: the dataset generation is now opt-in and migrated to `rxode2`, and the
   committed datasets are treated as source.

2. **Two pages show plots that current code cannot reproduce.** They call
   `xgx_scale_x_time_units(units_dataset = "h", units_plot = "d")`, and current
   xgxr rejects the abbreviated unit names. The published plots were rendered
   against an older xgxr. Fixed on the site side, in six call sites.

3. **`SiteResources/README.html` references JavaScript libraries that no longer
   exist** in the repository — versions a later render replaced. Minor, since
   nothing links to it, but the same failure mode.

The site was last actually rebuilt on **28 March 2026**. Every change since then
has modified source without regenerating output.

## What we get

- **Nothing can silently rot.** Every page is rebuilt from source on every
  change, and a link checker fails the build if anything breaks.
- **Anyone can contribute.** Alison — today you'd need my entire R toolchain
  working locally just to check whether a plot renders. With this, you open a
  pull request and GitHub renders the whole site for you.
- **The repository gets dramatically simpler.** The root directory currently
  holds ~47 generated files and folders mixed in with the actual source. With
  CI, generated output never enters the repository at all, and the root becomes
  `Rmarkdown/ Data/ Resources/ README.md`. This has been the single most
  confusing thing about the repo for newcomers.
- **It stops growing.** `.git` is currently ~588 MB, largely rendered images
  recommitted on every render. This doesn't shrink history, but it stops adding
  to it.
- **The dependencies are finally written down**, in a `DESCRIPTION` file, rather
  than being recoverable only by grepping every `.Rmd` for `library()` calls.

## What I've already done

The work is on a branch and has been tested end to end in a personal fork, so
this isn't a request to try something and see. A full clean rebuild produces all
27 pages with no errors, all links resolving, and output matching the published
pages. **`Novartis/xgx` itself is untouched** — I haven't pushed anything to it,
and the deploy step is explicitly disabled on the Novartis repository so it
cannot affect the live site even by accident.

The one piece I haven't been able to test is deployment itself, because GitHub
Actions has been in a major outage today. I'll confirm that in the fork before
anyone changes a setting.

## One thing to check

Does the Novartis org restrict GitHub Actions on public repositories? Allowed-
actions policies are common and I can't see that setting. If Actions is
restricted, there's a fallback that needs a different value of the same Pages
setting — happy to go that route instead.

## What I'd like

- **Orla:** the Pages source change above, or admin on the repo so I can make
  it. Whenever convenient — nothing breaks in the meantime.
- **Alison:** a look over the branch before we switch anything, especially the
  changes to `PKPD_Datasets.Rmd`, since the dataset generation is your code.

Thanks both,
Andy

---

## Notes for me, not for the email

- Do not send until a green deploy has been demonstrated in `iamstein/xgx`.
- If Orla asks what happens on rollback: reverting is changing the Pages source
  back to "Deploy from a branch". The committed HTML is still in git history, so
  the previous site can be restored by checking it out. Worth keeping the
  committed output in the repository for one release cycle after cutover rather
  than deleting it immediately (step 8 in the pipeline doc).
- Alison's most recent commits are the substantive ones on the branch's history,
  so framing the `PKPD_Datasets.Rmd` change as "your code, please review" is
  accurate rather than just polite.
- Decisions taken 2026-08-06: xgxr will **not** restore the `"h"`/`"d"`
  abbreviations — they were removed deliberately and the fix belongs in the
  site, which is done. xgx and xgxr both stay under `Novartis`.
