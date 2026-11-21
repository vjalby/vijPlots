# Renders a results' plot through jmvcore (as jamovi does) and returns the ggplot drawn
renderedPlot <- function(results) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(results$plot)
    ggplot2::last_plot()
}

# Text layers of a ggplot (ggplot2's and ggrepel's)
textLayers <- function(p) {
    Filter(function(l) inherits(l$geom, c("GeomText", "GeomLabel", "GeomTextRepel", "GeomLabelRepel")), p$layers)
}

# Checks a plot drawn with plotTheme = "ggBw" and fontFamily = "serif": the theme's frame (unless the
# analysis removes it), the serif font in the theme and in every text layer (at least nText of them)
expectBwSerif <- function(p, nText = 0, frame = TRUE) {
    border <- ggplot2::calc_element("panel.border", p$theme)
    expect_true(inherits(border, if (frame) "element_rect" else "element_blank"))
    expect_equal(ggplot2::calc_element("plot.title", p$theme)$family, "serif")
    expect_equal(ggplot2::calc_element("legend.text", p$theme)$family, "serif")
    layers <- textLayers(p)
    expect_gte(length(layers), nText)
    for (l in layers)
        expect_equal(l$aes_params$family, "serif")
}
