
# This file is a generated template, your changes will not be overwritten

raincloudClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "raincloudClass",
    inherit = raincloudBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            if (self$options$horizontal) {
                height <- 300
                width <- 400
            } else {
                height <- 400
                width <- 500
            }
            if (!is.null(self$options$groupOne))
                k <- nlevels( self$data[[self$options$groupOne]])
            else
                k <- 0
            # Compute the size according to facet
            if (userWidth * userHeight == 0)
                if (self$options$horizontal)
                    height <- height + min(200, k*100)
                else
                    width <- width + min(200, k*100)
            if (!is.null(self$options$groupTwo)) {
                if (self$options$horizontal) {
                    height <- height + 50
                    width <- width + 100
                } else {
                    width <- width + 50
                }
            }
            if (userWidth >0)
                width = userWidth
            if (userHeight >0)
                height = userHeight
            image <- self$results$plot
            image$setSize(width, height)
        },

        .run = function() {
            if (! is.null(self$options$aVar)) {
                plotData <- self$data[c(self$options$aVar, self$options$groupOne, self$options$groupTwo)]
                plotData[[self$options$aVar]] <- jmvcore::toNumeric(plotData[[self$options$aVar]])
                image <- self$results$plot
                image$setState(plotData)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(self$options$aVar))
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

            plotData <- jmvcore::naOmit(plotData)

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

            if (is.null(groupOne)) {
                if (is.null(groupTwo)) {
                    jitter_nudge <- ggpp::position_jitternudge(width = 0.03, height = 0,
                                                         seed = 42, x = ifrev(0.08,-0.08),
                                                         nudge.from = "jittered")
                    plot <- ggplot(plotData, aes(x = 1, y = !!aVar, fill = "1")) +
                                geom_boxplot(width = 0.05, outlier.shape = NA) +
                                geom_point(position = jitter_nudge, size=1.2) +
                                ggdist::stat_halfeye(side = rainSide, justification = ifrev(1.25,-0.25), width = 0.2, .width = 0, point_color = NA,
                                                        slab_color = "black", slab_linewidth = 0.5) +
                                guides(fill = FALSE)
                } else {
                    jitter_nudge <- ggpp::position_jitternudge(width = 0.02, height = 0,
                                                         seed = 42, x = ifrev(+0.05,-0.05),
                                                         nudge.from = "jittered")
                    plot <- ggplot(plotData, aes(x = 1, y = !!aVar, fill = !!groupTwo, color = !!groupTwo, slab_color = !!groupTwo)) +
                                geom_boxplot(position = boxplotPosition, width = boxplotWidth, outlier.shape = NA, color="black", alpha = boxplotAlpha, key_glyph = draw_key_rect) +
                                geom_point(aes(color = !!groupTwo), position = jitter_nudge, size=1.2, show.legend = FALSE) +
                                ggdist::stat_halfeye(side = rainSide, justification = ifrev(1.1,-0.1), width = 0.3, .width = 0, point_color=NA,
                                                     alpha = alphaC, slab_linewidth = 0.8, show.legend = FALSE)
                    plot <- plot + guides(fill = guide_legend(override.aes = list(alpha = 1)))
                }
                plot <- plot + labs(x = "") + scale_x_continuous(breaks = NULL)
            } else {
                if (is.null(groupTwo)) {
                    jitter_nudge <- ggpp::position_jitternudge(width = 0.02, height = 0,
                                                         seed = 42, x = ifrev(+0.12,-0.12),
                                                         nudge.from = "jittered")
                    plot <- ggplot(plotData, aes(x = !!groupOne, y = !!aVar, fill = !!groupOne)) +
                                geom_boxplot(position = position_nudge(), width = 0.09, outlier.shape = NA) +
                                geom_point(aes(color = !!groupTwo), position = jitter_nudge, size=1.2) +
                                ggdist::stat_halfeye(side = rainSide, justification = ifrev(1.30,-0.30), width = 0.3, .width = 0, point_color = NA,
                                                     slab_color = "black", slab_linewidth = 0.5) + guides(fill = FALSE)

                } else {
                    jitter_nudge <- ggpp::position_jitternudge(width = 0.05, height = 0,
                                                         seed = 42, x = ifrev(+0.15,-0.15),
                                                         nudge.from = "jittered")
                    plot <- ggplot(plotData, aes(x = !!groupOne, y = !!aVar, fill =  !!groupTwo, color = !!groupTwo, slab_color = !!groupTwo)) +
                                geom_boxplot(position = boxplotPosition, width = boxplotWidth2, outlier.shape = NA, color = "black", alpha = boxplotAlpha, key_glyph = draw_key_rect) +
                                geom_point(aes(color = !!groupTwo), position = jitter_nudge, size=1.2, show.legend = FALSE) +
                                ggdist::stat_halfeye(side = rainSide, justification = ifrev(1.30,-0.30), width = 0.3, .width = 0, point_color = NA,
                                                     slab_alpha = alphaC, slab_linewidth = 0.8, show.legend = FALSE)
                    plot <- plot + guides(fill = guide_legend(override.aes = list(alpha = 1)))
                }
            }

            if (self$options$horizontal)
                plot <- plot + coord_flip()

            plot <- plot + ggtheme

            if (self$options$colorPalette != 'jmv') {
                plot <- plot + scale_fill_brewer(palette = self$options$colorPalette) + scale_color_brewer(palette = self$options$colorPalette) +
                    scale_color_brewer(palette = self$options$colorPalette, aesthetic = "slab_color")
            } else {
                plot <- plot + scale_color_manual(values = colorPalette(pal = theme$palette, type="color"), aesthetics = "slab_color")
            }

            return(plot)

        })
)
