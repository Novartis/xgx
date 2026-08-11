# Local preview build for the xGx website.
#
# Replaces Rmarkdown/000_render_site.R and its two variants, retired 2026-08-11.
# Those scripts shell-copied Rmarkdown/www/ up into the repository root, which is
# how ~292 files of rendered output came to be committed in the first place.
# Nothing here writes outside Rmarkdown/www/, which is gitignored.
#
# This script does not publish anything and cannot. GitHub Actions builds and
# deploys the live site on every push to master -- see
# .github/workflows/build-site.yml. Use this only to look at a change locally
# before you push it.
#
# Run from the repository root:
#
#   source("dev/render_site_local.R")
#   render_xgx_site()                     # incremental; reuses the knitr cache
#   render_xgx_site(clear_cache = TRUE)   # from scratch
#
# Clear the cache when you change something the cache cannot see -- most often
# the datasets in Data/, since knitr keys on the .Rmd chunk source, not on the
# files a chunk happens to read. A cold build takes a while; the whole site is
# ~27 pages.

render_xgx_site <- function(clear_cache = FALSE, preview = TRUE) {
  if (!dir.exists("Rmarkdown") || !file.exists("dev/ci/render_site_ci.R")) {
    stop("Run this from the repository root, not from Rmarkdown/.")
  }

  if (clear_cache) {
    # Both, and that is not optional. On a cache hit knitr skips chunk
    # evaluation, which means it also does not rewrite the figures. Clearing
    # *_cache but leaving *_files behind gives you a build that looks fine and
    # is silently serving stale plots. Same trap the CI cache config documents.
    stale <- c(Sys.glob("Rmarkdown/*_cache"), Sys.glob("Rmarkdown/*_files"))
    if (length(stale)) {
      cat("Clearing", length(stale), "cache/figure directories\n")
      unlink(stale, recursive = TRUE)
    }
  }

  # The one build. CI runs this exact script, so a green preview here means the
  # same thing CI will do -- there is no second code path to drift out of sync.
  source("dev/ci/render_site_ci.R")

  index <- file.path("Rmarkdown/www", "index.html")
  if (!file.exists(index)) {
    stop("Render finished but produced no ", index)
  }

  cat("\nPreview: ", normalizePath(index), "\n", sep = "")
  cat("Nothing was copied into the repository root. Push to publish.\n")

  if (preview && interactive()) {
    utils::browseURL(normalizePath(index))
  }

  invisible(normalizePath("Rmarkdown/www"))
}
