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
