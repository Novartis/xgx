#!/usr/bin/env python3
"""Check that every local link in the built site resolves to a real file.

Usage:  python3 dev/ci/check_links.py <publish_dir>

Exits non-zero if any local href/src target is missing. Known-broken targets
that predate the CI pipeline are listed in BASELINE and reported but tolerated,
so the build fails only on regressions.
"""
import sys
import os
import re
import urllib.parse

# Already broken on master as of 2026-08-06. Reported, but not build-failing,
# so that CI fails only on regressions rather than on inherited debt.
BASELINE = {
    # Oncology_Efficacy_Plots.html links these at the site root, but the files
    # actually live in Data/
    "Oncology_Efficacy_Data.csv",
    "Oncology_Efficacy_Dose.csv",
}

LINK = re.compile(r'(?:src|href)="([^"]+)"')

# Directories whose HTML we do not crawl. We still verify links *into* them.
#   site_libs     third-party bundles ship demo pages we neither control nor publish
#   SiteResources header/body/icon_nav are include fragments, not standalone pages:
#                 their hrefs resolve against the page that embeds them, not against
#                 their own directory, so they are validated via the rendered pages
#   dev           developer-only, never published
SKIP_DIRS = {"site_libs", "SiteResources", "dev", ".git"}


def main(root):
    missing = {}
    checked = set()

    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in SKIP_DIRS]
        for fn in filenames:
            if not fn.endswith(".html"):
                continue
            page = os.path.join(dirpath, fn)
            with open(page, encoding="utf-8", errors="replace") as fh:
                html = fh.read()

            for raw in LINK.findall(html):
                # strip fragment and query, ignore absolute and generated links
                target = urllib.parse.unquote(raw.split("#")[0].split("?")[0])
                if not target or target.startswith(
                    ("http://", "https://", "//", "mailto:", "data:", "'")
                ):
                    continue

                resolved = os.path.normpath(os.path.join(dirpath, target))
                checked.add(resolved)
                if not os.path.exists(resolved):
                    rel = os.path.relpath(resolved, root)
                    missing.setdefault(rel, set()).add(os.path.relpath(page, root))

    regressions = {k: v for k, v in missing.items() if k not in BASELINE}
    tolerated = sorted(set(missing) & BASELINE)

    print(f"checked {len(checked)} unique local link targets under {root}")
    if tolerated:
        print(f"tolerated (broken before CI): {len(tolerated)}")
        for t in tolerated:
            print(f"    {t}")

    if regressions:
        print(f"\nFAIL: {len(regressions)} broken link target(s) introduced:")
        for target in sorted(regressions):
            print(f"    {target}")
            for page in sorted(regressions[target])[:4]:
                print(f"        linked from {page}")
        return 1

    print("\nOK: no broken links beyond the known baseline")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else "Rmarkdown/www"))
