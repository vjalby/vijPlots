test_that("fixed-length palettes are interpolated beyond their number of colors", {
    theme <- list(palette = "jmv")
    for (pal in c("Set1", "ggsci::jama", "custom::lemovice")) {
        palette <- vijColorPalette(pal, "fill", theme)
        nl <- vijColorPaletteNlevels(palette)
        # Within the palette: exact colors, in order
        expect_equal(palette(nl)[1:3], palette(3))
        # Beyond: no NA, no duplicate, no warning
        expect_no_warning(colors <- palette(nl + 3))
        expect_length(colors, nl + 3)
        expect_false(anyNA(colors))
        expect_equal(anyDuplicated(colors), 0)
    }
})

test_that("vijHideAxis: transparent ticks, blank when the theme has none", {
    # jmvcore's themes are lists (a theme and scales): added to a plot, as in .plot()
    ticks <- function(ggtheme) {
        p <- ggplot2::ggplot() + ggtheme + vijHideAxis(x = TRUE)
        ggplot2::calc_element("axis.ticks.x.bottom", ggplot2::theme_grey() + p$theme)
    }
    expect_equal(ticks(jmvcore::theme_default())$colour, "transparent")
    expect_s3_class(ticks(jmvcore::theme_min()), "element_blank")
})
