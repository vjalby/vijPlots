# https://github.com/jamovi/jmvplots/blob/main/tests/testthat/helper-vdiffr.R
# jamovi Team (Ravi Selker) code
# 2026-08-05


# Visual-regression snapshots (vdiffr) are a LOCAL check, not a CI gate.
# Pixel/SVG output is not reproducible across the macOS/Windows/Linux x
# ggplot2-version CI matrix, so comparing it there only produces noise.
#
# On CI we still render the plot — so render-time errors surface on every
# matrix leg — but skip the pixel comparison. Locally we run the full vdiffr
# visual diff. The deterministic CI gate for plot correctness is
# expect_plot_equivalent() in helper-syntax-equivalence.R.
expect_plot_snapshot <- function(title, fig, ...) {
    if (isTRUE(as.logical(Sys.getenv("CI")))) {
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        print(fig)
        testthat::succeed()
    } else {
        vdiffr::expect_doppelganger(title, fig, ...)
    }
}
