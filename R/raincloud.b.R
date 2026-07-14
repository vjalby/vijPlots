
# This file is a generated template, your changes will not be overwritten

raincloudClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "raincloudClass",
    inherit = raincloudBase,
    private = list(
        .init = function() {
            # Stretchable dimensions
            if (!is.null(self$options$groupOne))
                k <- nlevels( self$data[[self$options$groupOne]])
            else
                k <- 0
            if (self$options$horizontal) {
                height <- 300 + min(400, k*100)
                width <- 400
            } else {
                height <- 400
                width <- 300 + min(400, k*100)
            }
            # Fixed dimensions
            if (self$options$horizontal) {
                fixed_height <- 50 # X-Axis legend
                if (!is.null(self$options$groupOne))
                    fixed_width <- 100
                else
                    fixed_width <- 0
            } else {
                fixed_width <- 50 # Y-Axis legend
                if (!is.null(self$options$groupOne))
                    fixed_height <- 50
                else
                    fixed_height <- 0
            }
            if (!is.null(self$options$groupTwo)) {
                if (self$options$legendPosition %in% c('top','bottom'))
                    fixed_height <- fixed_height + 50
                else
                    fixed_width <- fixed_width + 100
            }
            # Set the image dimensions
            image <- self$results$plot
            if (is.null(image[['setSize2']])) { # jamovi < 2.7.16
                image$setSize(width + fixed_width, height + fixed_height)
            } else {
                image$setSize2(width, height, fixed_width, fixed_height)
            }
        },
        .run = function() {
            if (is.null(self$options$aVar))
                return()
            plotData <- self$data[c(self$options$aVar, self$options$groupOne, self$options$groupTwo)]
            plotData[[self$options$aVar]] <- jmvcore::toNumeric(plotData[[self$options$aVar]])
            # Remove case with missing group
            if (!is.null(self$options$groupOne) & self$options$ignoreNA) {
                plotData <- subset(plotData, !is.na(plotData[self$options$groupOne]))
            }
            if (!is.null(self$options$groupTwo) & self$options$ignoreNA) {
                plotData <- subset(plotData, !is.na(plotData[self$options$groupTwo]))
            }
            if (nrow(plotData) == 0)
                return()
            image <- self$results$plot
            image$setState(plotData)
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            aVar <- self$options$aVar
            aVar <- ensym(aVar)

            if (!is.null(self$options$groupOne)) {
                groupOne <- self$options$groupOne
                groupOne <- ensym(groupOne)
            } else {
                groupOne <- NULL
            }

            if (!is.null(self$options$groupTwo)) {
                groupTwo <- self$options$groupTwo
                groupTwo <- ensym(groupTwo)
            } else {
                groupTwo <- NULL
            }

            alphaC <- as.numeric(self$options$alphaC)

            reverse <- self$options$reverse

            ifrev <- function(tt,ff) {
                if (reverse)
                    return(tt)
                else
                    return(ff)
            }

            rainSide <- ifrev("left","right")

            if (self$options$nudgeBoxplot) {
                boxplotPosition <- position_dodge2(padding = 0.5)
                boxplotWidth <- 0.05
                boxplotWidth2 <- 0.15
                boxplotAlpha <- 1.
            } else {
                boxplotPosition <- position_nudge(x = 0)
                boxplotWidth <- 0.03
                boxplotWidth2 <- 0.09
                boxplotAlpha <- alphaC
            }

            # One color only
            nbColors <- attr(vijPalette(self$options$colorPalette, "fill"),"nlevels")
            colorNo <- self$options$colorNo
            if (self$options$singleColor) {
                oneColorOfPalette <- vijPalette(self$options$colorPalette, "fill")(nbColors)[min(colorNo,nbColors)]
            } else {
                oneColorOfPalette <- vijPalette(self$options$colorPalette, "fill")(nbColors)[1]
            }

            if (is.null(groupOne)) {
                if (is.null(groupTwo)) {
                    jitter_nudge <- ggpp::position_jitternudge(width = 0.03, height = 0,
                                                         seed = 42, x = ifrev(0.08,-0.08),
                                                         nudge.from = "jittered")
                    plot <- ggplot(plotData, aes(x = 1, y = !!aVar))
                    plot <- plot + geom_boxplot(width = 0.05, outlier.shape = NA, fill = oneColorOfPalette)
                    plot <- plot + geom_point(position = jitter_nudge, size=1.2)
                    plot <- plot + ggdist::stat_halfeye(side = rainSide, justification = ifrev(1.25,-0.25), width = 0.2, .width = 0,
                                                        point_color = NA, slab_color = "black", slab_linewidth = 0.5, fill = oneColorOfPalette)
                    plot <- plot + guides(fill = "none")
                } else {
                    # show.legend needed to display unused colors, guide size & slab_color needed to hide extra legends
                    jitter_nudge <- ggpp::position_jitternudge(width = 0.02, height = 0,
                                                         seed = 42, x = ifrev(+0.05,-0.05),
                                                         nudge.from = "jittered")
                    plot <- ggplot(plotData, aes(x = 1, y = !!aVar, fill = !!groupTwo, color = !!groupTwo, slab_color = !!groupTwo))
                    plot <- plot + geom_boxplot(position = boxplotPosition, width = boxplotWidth, outlier.shape = NA, color="black", alpha = boxplotAlpha, key_glyph = draw_key_rect, show.legend = TRUE)
                    plot <- plot + geom_point(aes(color = !!groupTwo), position = jitter_nudge, size=1.2, show.legend = FALSE)
                    plot <- plot + ggdist::stat_halfeye(side = rainSide, justification = ifrev(1.1,-0.1), width = 0.3, .width = 0, point_color=NA,
                                                     alpha = alphaC, slab_linewidth = 0.8, show.legend = FALSE)
                    plot <- plot + guides(fill = guide_legend(override.aes = list(alpha = 1)), size = "none", slab_color = "none")
                }
                plot <- plot + labs(x = "") + scale_x_continuous(breaks = NULL)
            } else {
                if (is.null(groupTwo)) {
                    jitter_nudge <- ggpp::position_jitternudge(width = 0.02, height = 0,
                                                         seed = 42, x = ifrev(+0.12,-0.12),
                                                         nudge.from = "jittered")
                    if (self$options$singleColor) {
                        plot <- ggplot(plotData, aes(x = !!groupOne, y = !!aVar))
                        plot <- plot + geom_boxplot(position = position_nudge(), width = 0.09, outlier.shape = NA, fill = oneColorOfPalette)
                        plot <- plot + ggdist::stat_halfeye(side = rainSide, justification = ifrev(1.30,-0.30), width = 0.3, .width = 0,
                                                            point_color = NA, slab_color = "black", fill = oneColorOfPalette, slab_linewidth = 0.5)
                    } else {
                        plot <- ggplot(plotData, aes(x = !!groupOne, y = !!aVar, fill = !!groupOne))
                        plot <- plot + geom_boxplot(position = position_nudge(), width = 0.09, outlier.shape = NA)
                        plot <- plot + ggdist::stat_halfeye(side = rainSide, justification = ifrev(1.30,-0.30), width = 0.3, .width = 0,
                                                            point_color = NA, slab_color = "black", slab_linewidth = 0.5)
                    }
                    plot <- plot + geom_point(position = jitter_nudge, size=1.2) + guides(fill = "none")


                } else {
                    jitter_nudge <- ggpp::position_jitternudge(width = 0.05, height = 0,
                                                         seed = 42, x = ifrev(+0.15,-0.15),
                                                         nudge.from = "jittered")
                    plot <- ggplot(plotData, aes(x = !!groupOne, y = !!aVar, fill =  !!groupTwo, color = !!groupTwo, slab_color = !!groupTwo))
                    plot <- plot + geom_boxplot(position = boxplotPosition, width = boxplotWidth2, outlier.shape = NA, color = "black", alpha = boxplotAlpha, key_glyph = draw_key_rect, show.legend = TRUE)
                    plot <- plot + geom_point(aes(color = !!groupTwo), position = jitter_nudge, size=1.2, show.legend = FALSE)
                    plot <- plot + ggdist::stat_halfeye(side = rainSide, justification = ifrev(1.30,-0.30), width = 0.3, .width = 0, point_color = NA,
                                                     slab_alpha = alphaC, slab_linewidth = 0.8, show.legend = FALSE)
                    plot <- plot + guides(fill = guide_legend(override.aes = list(alpha = 1)), size = "none", slab_color = "none")
                }
                plot <- plot + scale_x_discrete(drop = FALSE) # keep unused levels
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "color", drop = FALSE) +
                                    vijScale(self$options$colorPalette, "fill", drop = FALSE) +
                                    vijScale(self$options$colorPalette, "slab_color", drop = FALSE)

            # Ticks
            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot  + scale_y_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1))
            }
            if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot  + scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1))
            }

            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + coord_flip(ylim = c(self$options$xAxisRangeMin, self$options$xAxisRangeMax))
                } else {
                    plot <- plot + coord_flip()
                }
            } else if (self$options$yAxisRangeType == "manual") { # Horizontal and manual
                plot <- plot + coord_cartesian(ylim = c(self$options$yAxisRangeMin, self$options$yAxisRangeMax))
            }

            # Legend spacing
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            # Titles & Labels
            defaults <- list(y = aVar, x = groupOne, legend = groupTwo)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)

            return(plot)
        })
)
