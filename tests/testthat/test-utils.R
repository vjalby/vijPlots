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

test_that("vijNudgePosition: the position's own placement, then the nudge", {
    d <- data.frame(x = factor(c("a", "b", "b")), y = 1:3, g = factor(c("u", "u", "v")))
    xs <- function(position) ggplot2::layer_data(ggplot2::ggplot(d, ggplot2::aes(x, y, group = g)) +
                                                 ggplot2::geom_point(position = position))
    for (position in list(ggplot2::position_jitter(width = 0.02, height = 0, seed = 42), ggplot2::position_dodge(width = 0.75))) {
        plain <- xs(position)
        nudged <- xs(vijNudgePosition(position, 0.1))
        expect_equal(nudged$x, plain$x + 0.1)
        expect_equal(nudged$y, plain$y)
    }
    # The nudge is read when the position is created, not when the plot is rendered
    nx <- 0.1
    position <- vijNudgePosition(ggplot2::position_identity(), nx)
    nx <- 0.5
    expect_equal(xs(position)$x, c(1.1, 2.1, 2.1))
})
