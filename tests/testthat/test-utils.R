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

test_that("vijPlotTheme(): every theme builds, jamovi's SPSS without deprecation warning", {
    withr::local_options(lifecycle_verbosity = "warning")
    for (th in c("default", "jmvDefault", "jmvMinimal", "jmvHadley", "jmvSpss", "ggBw", "ggLight", "ggLinedraw", "ggDark"))
        expect_no_warning(expect_s3_class(vijPlotTheme(list(plotTheme = th, fontFamily = "mono")), "theme"))
    expect_equal(vijFontFamily(list()), "")
    expect_equal(vijFontFamily(list(fontFamily = "default")), "")
    expect_equal(vijFontFamily(list(fontFamily = "serif")), "serif")
})
