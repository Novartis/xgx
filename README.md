# Overview

This site contains the code used to generate the [xGx website](https://opensource.nibr.com/xgx/) for Exploratory Graphics.

This repository displays suggested plots to pursue when exploring different PK/PD datasets, with a focus on exploring the Dose-Exposure-Response relationship. This site is a collection of exploratory plots and code, and could serve as a checklist of graphs someone might create for certain projects. 

Many of the codes on the site use functions that we have found to be helpful while exploring PK/PD data. We compiled these helpful functions into the xgxr R-package, which is available on [CRAN](https://cran.r-project.org/web/packages/xgxr/index.html), and [GitHub](https://github.com/Novartis/xgxr).

## Repository layout

**The repository contains source only.**  The website is built by GitHub Actions on
every push to `master` and published straight to Pages - see
`.github/workflows/build-site.yml`.  No rendered HTML is committed.

It did not always work this way.  Until 2026-08-11 the root directory *was* the
published website: ~27 `*.html` files, their `*_files/` folders, `site_libs/` and a
copy of `SiteResources/`, all generated locally and committed.  Those are gone from
the root now.  If you see them reappear, something copied `Rmarkdown/www/` up a
level - do not commit them.

Directories you will actually edit:

| Directory      | Contents |
| -------------- | -------- |
| `Rmarkdown/`   | The Rmarkdown source for every page, plus `_site.yml` and `SiteResources/` |
| `Data/`        | The datasets |
| `Resources/`   | Cheat sheets, checklists and slides linked from the Resources page |
| `design/`      | Design notes and refactor plans |
| `dev/`         | Developer-only material, not part of the website: `dev/R` (older standalone functions, superseded by the xgxr package), `dev/Test` (scratch tests), `dev/Rlib` (local xgxr install target) |

Note that `Data/`, `Rmarkdown/` and `Resources/` are **both source and published web
assets** - the generated pages link directly to the `.csv`, `.Rmd` and `.pdf` files inside
them, and the build copies all three into the published site at the same paths. Their
names are part of the public URLs (`/xgx/Data/mt12345.csv`,
`/xgx/Rmarkdown/Adverse_Events.Rmd`), which are cited in publications, so they do not
move.

## Setting up a machine

The packages the site needs are recorded in `DESCRIPTION` at the repository root.
That file is not a real R package - it exists so there is one authoritative list
instead of one recovered by grepping the Rmarkdown sources. To install them:

```r
install.packages("pak")
pak::local_install_deps()
```

`rxode2` is deliberately in `Suggests` rather than `Imports`: it is needed only
to regenerate the datasets in `Data/` (see `Rmarkdown/PKPD_Datasets.Rmd`), not
to build the site, and it requires C and Fortran compilers. `pak::pak("rxode2")`
if you actually need it.

## Rebuilding the site

**You do not need to build the site to publish it.**  Edit the `.Rmd` files, commit,
and push - GitHub Actions renders everything and deploys.  A pull request builds too,
and attaches the whole rendered site to the run as a downloadable `xgx-site` artifact,
so you can check a change without an R toolchain at all.

To render locally, from the repository root:

```r
source("dev/ci/render_site_ci.R")
```

That renders into `Rmarkdown/www/`, which is gitignored.  Open
`Rmarkdown/www/index.html` to preview.

> `Rmarkdown/000_render_site.R` and its two variants are the **old** local build.  They
> shell-copy `Rmarkdown/www/` into the repository root, which is exactly the committed
> build output that was removed on 2026-08-11.  Prefer `render_site_ci.R`; the old
> scripts are kept for now only because they are what people have in muscle memory.

Most of the Rmarkdown scripts use caching so that recompiling is quick.  However, when developing the site, if you change any dependencies (e.g. data in the Data folder) you should delete the cache.  This means deleting all the folders in Rmarkdown that end in "_cache"  DO NOT DELETE SiteResources.

In order to add links to the website, do the following:
* Edit the Rmarkdown/_site.yml.  Under the menu of interest, add a text and href entry - follow other examples
* Edit the Rmarkdown/SiteResources/icon_nav.html.  Under the menu of interest, add the link and name.  It is an HTML *fragment*, not a document - no `<html>` or `<body>` wrapper.
* Commit and push; CI rebuilds the site.
