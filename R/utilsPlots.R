# Helpers used by vijPlots only (utils.R is shared with vijMulti, keep it identical there)

# Theme adding major grid lines on the displayed x and/or y axis (with coord_flip(),
# theme elements follow the displayed axes). Nothing is added with the jamovi themes
# that already draw a grid (hadley, minimal).
vijGridLines = function(themeName, x = FALSE, y = FALSE) {
    gridTheme <- ggplot2::theme()
    if (themeName %in% c("hadley", "minimal"))
        return(gridTheme)
    gridLine <- ggplot2::element_line(colour = "#E8E8E8", linewidth = 0.73)
    if (x)
        gridTheme <- gridTheme + ggplot2::theme(panel.grid.major.x = gridLine)
    if (y)
        gridTheme <- gridTheme + ggplot2::theme(panel.grid.major.y = gridLine)
    return(gridTheme)
}

# Theme hiding the line and the ticks of the displayed x and/or y axis (with
# coord_flip(), theme elements follow the displayed axes). The axis text is kept, and the
# ticks are made transparent rather than blank so that their length still keeps the text
# away from the panel; unless the theme has no ticks (minimal): they then stay blank, with no length.
vijHideAxis = function(x = FALSE, y = FALSE) {
    axisTheme <- ggplot2::theme()
    invisibleTicks <- ggplot2::element_line(colour = "transparent", inherit.blank = TRUE)
    if (x)
        axisTheme <- axisTheme + ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                                                axis.ticks.x = invisibleTicks)
    if (y)
        axisTheme <- axisTheme + ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                                                axis.ticks.y = invisibleTicks)
    return(axisTheme)
}

# Position nudged horizontally by x after the given position (e.g. position_jitter()) placed the data
vijNudgePosition = function(position, x) {
    force(x)
    ggplot2::ggproto(NULL, position,
        compute_layer = function(self, data, params, layout) {
            data <- ggplot2::ggproto_parent(position, self)$compute_layer(data, params, layout)
            ggplot2::transform_position(data, function(xx) xx + x)
        }
    )
}
