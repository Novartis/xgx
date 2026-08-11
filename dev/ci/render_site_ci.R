# Render the xGx website for CI / GitHub Pages.
#
# It never copies anything into the repository root. It renders into
# Rmarkdown/www and then assembles a self-contained publish directory, so no
# build output is ever committed to git. That was the whole point of replacing
# the old Rmarkdown/000_render_site.R, which shell-copied www/ up a level and
# is why the rendered site used to live in git (retired 2026-08-11).
#
# For local previews use dev/render_site_local.R, which wraps this script and
# adds cache clearing. Both paths run the code below, so they cannot drift.
#
# The published pages link directly at Data/, Resources/ and Rmarkdown/, so
# those have to ship alongside the HTML at the same relative depth in order for
# every existing public URL to keep resolving.

publish <- "Rmarkdown/www"

#########################################################
## record the datasets as committed, before rendering
#########################################################
# PKPD_Datasets.Rmd regenerates Data/*.csv as a side effect of rendering.
# We snapshot them so the workflow can detect drift and warn about it.
data_before <- tools::md5sum(list.files("Data", full.names = TRUE))

#########################################################
## render
#########################################################
# _site.yml lives in Rmarkdown/, and the .Rmd files reference "../Data/..." ,
# so the render has to run from inside that directory.
owd <- setwd("Rmarkdown")
rmarkdown::render_site()
setwd(owd)

if (!dir.exists(publish)) {
  stop("render_site() produced no output at ", publish)
}

#########################################################
## assemble the publish directory
#########################################################
# Copied AFTER the render, so regenerated datasets are the ones published.
for (d in c("Data", "Resources")) {
  file.copy(d, publish, recursive = TRUE)
}

# The pages offer their own source for download, e.g.
# opensource.nibr.com/xgx/Rmarkdown/Multiple_Ascending_Dose_PK.Rmd
dir.create(file.path(publish, "Rmarkdown"), showWarnings = FALSE)
file.copy(Sys.glob("Rmarkdown/*.Rmd"), file.path(publish, "Rmarkdown"))

#########################################################
## report dataset drift
#########################################################
data_after <- tools::md5sum(list.files("Data", full.names = TRUE))
common <- intersect(names(data_before), names(data_after))
changed <- common[data_before[common] != data_after[common]]

if (length(changed) > 0) {
  cat("\n::warning::Rendering regenerated these committed datasets:\n")
  cat(paste0("  ", changed, collapse = "\n"), "\n")
  cat("The datasets in Data/ are source, not build output. Rendering should",
      "not change them:\n  PKPD_Datasets.Rmd only regenerates them when",
      "XGX_REGENERATE_DATA=true.\n")
}

cat("\nPublish directory assembled at ", publish, ":\n", sep = "")
cat("  ", length(Sys.glob(file.path(publish, "*.html"))), " html pages\n", sep = "")
