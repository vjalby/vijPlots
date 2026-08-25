scatterplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "scatterplotClass",
    inherit = scatterplotBase,
    private = list(
        .init = function() {
            # Stretchable dimensions
            if (!is.null(self$options$facet)) {
                nbOfFacet <- nlevels(self$data[[self$options$facet]])
                if (self$options$facetBy == "column") {
                    nbOfColumn <- self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn)
                } else {
                    nbOfRow <- self$options$facetNumber
                    nbOfColumn <- ceiling(nbOfFacet / nbOfRow)
                }
                width <- max(350, 350*nbOfColumn)
                height <- max(350,350*nbOfRow)
            } else {
                width <- 450
                height <- 450
            }
            # Fixed dimensions
            fixed_height <- 50 # X-Axis legend
            fixed_width <- 75 # Y-Axis legend
            if( !is.null(self$options$group) || !is.null(self$options$ptSize) ) {
                if (self$options$legendPosition %in% c('top','bottom')) {
                    if( !is.null(self$options$group) && !is.null(self$options$ptSize) )
                        fixed_height <- fixed_height + 100 # two legends
                    else
                        fixed_height <- fixed_height + 50 # one legend
                } else {
                    fixed_width <- fixed_width + 100
                }
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
            xaxis <- self$options$xaxis
            yaxis <- self$options$yaxis
            groupVar <- self$options$group
            labelVar <- self$options$labelVar
            sizeVar <- self$options$ptSize

            if (is.null(xaxis) || is.null(yaxis))
                return(FALSE)

            plotData <- jmvcore::select(self$data, c(xaxis, yaxis, groupVar, labelVar, sizeVar, self$options$facet))
            plotData[[xaxis]] <- jmvcore::toNumeric(plotData[[xaxis]])
            plotData[[yaxis]] <- jmvcore::toNumeric(plotData[[yaxis]])
            if(!is.null(sizeVar))
                plotData[[sizeVar]] <- jmvcore::toNumeric(plotData[[sizeVar]])

            if (!self$options$keepNA)
                plotData <- jmvcore::naOmit(plotData)

            if (nrow(plotData) == 0)
                return(FALSE)

            image <- self$results$plot
            image$setState(plotData)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            plotData <- image$state

            #### Set variables ####

            xaxis <- rlang::sym(self$options$xaxis)
            yaxis <- rlang::sym(self$options$yaxis)
            if( !is.null(self$options$group) ) {
                groupVar <- rlang::sym(self$options$group)
            } else {
                groupVar <- NULL
            }
            if( !is.null(self$options$labelVar) ) {
                labelVar <- rlang::sym(self$options$labelVar)
            } else {
                labelVar <- NULL
            }
            if( !is.null(self$options$ptSize) ) {
                sizeVar <- rlang::sym(self$options$ptSize)
            } else {
                sizeVar <- NULL
            }
            if (!is.null(self$options$facet)) {
                facetVar <- rlang::sym(self$options$facet)
            } else {
                facetVar <- NULL
            }

            if (self$options$groupShapes) {
                shapeVar <- groupVar
            } else {
                shapeVar <- NULL
            }

            #### Main plot ####

            plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = !!xaxis, y = !!yaxis, color = !!groupVar, fill = !!groupVar))

            if (!is.null(sizeVar)) {
                if (is.null(groupVar)) {
                    plot <- plot + ggplot2::geom_point(ggplot2::aes(size = !!sizeVar), color = self$options$singleColor)
                } else {
                    plot <- plot + ggplot2::geom_point(ggplot2::aes(size = !!sizeVar, shape = !!shapeVar))
                }
            } else {
                if (is.null(groupVar)) {
                    plot <- plot + ggplot2::geom_point(color = self$options$singleColor, size = self$options$pointSize)
                } else {
                    plot <- plot + ggplot2::geom_point(ggplot2::aes(shape = !!shapeVar), size = self$options$pointSize)
                }
            }

            #### Regression line ####

            if (self$options$regLine) {
                plot <- plot + ggplot2::geom_smooth(method = self$options$lineMethod, se = self$options$lineSE, formula = y ~ x,
                                                    linewidth = self$options$lineSize, show.legend = FALSE)
            }

            # order must be set explicitly on every merged aesthetic (color/fill/shape),
            # not just size: an unset order (0) is broken by an internal hash rather than
            # code order, so its position relative to size's order=99 is not deterministic.
            plot <- plot + ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size=4), order = 1),
                                           fill = ggplot2::guide_legend(order = 1),
                                           shape = ggplot2::guide_legend(order = 1),
                                           size = ggplot2::guide_legend(order = 99))

            #### Labels ####

            if (!is.null(labelVar)) {
                x_scale <- max(plotData[[xaxis]], na.rm=TRUE) - min(plotData[[xaxis]], na.rm = TRUE)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = !!labelVar), color = "black",
                                         na.rm = TRUE, size = self$options$labelTextSize / ggplot2::.pt, hjust = 0, nudge_x = 0.02*x_scale,
                                         check_overlap = self$options$overlap)
                # Enlarge graphic by 10% at right (for labels)
                maxx <- max(plotData[[xaxis]], na.rm=T) + 0.1*x_scale
                plot <- plot + ggplot2::expand_limits(x = maxx)
            }

            #### Reference lines ####

            if (self$options$hline)
                plot <- plot + ggplot2::geom_hline(yintercept = self$options$yinter,  color="black", linewidth = self$options$lineSize/2)
            if (self$options$vline)
                plot <- plot + ggplot2::geom_vline(xintercept = self$options$xinter,  color="black", linewidth = self$options$lineSize/2)

            #### Colors and axes ####

            # Theme and colors
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "color", theme, drop = FALSE) + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE)

            # Shapes
            if (self$options$groupShapes && !is.null(groupVar)) {
                #plot <- plot + ggplot2::scale_shape_manual(values = c(16, 17, 15, 18, 8, 3, 4,16, 17, 15, 18, 8, 3, 4))
                plot <- plot + ggplot2::scale_shape_manual(values = c(16, 17, 15, 18, 1, 2, 0, 5, 6, 3, 4), na.value = 7)
            }

            # Axis Limits
            if (self$options$yAxisRangeType == "manual")
                yLim <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
            else
                yLim <- NULL
            if (self$options$xAxisRangeType == "manual")
                xLim <- c(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
            else
                xLim <- NULL
            plot <- plot + ggplot2::coord_cartesian(ylim = yLim, xlim = xLim)

            # Ticks
            if (self$options$xTicks > 0) {
                plot <- plot  + ggplot2::scale_x_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1))
            }
            if (self$options$yTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1))
            }

            # Border
            if( self$options$plotBorder ) {
                plot <- plot + ggplot2::theme(axis.line = ggplot2::element_line(linewidth = 0),
                                              panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)
                                        )
            }

            # Facet
            if (!is.null(facetVar)) {
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            # Titles & Labels
            defaults <- list(y = yaxis, x = xaxis, legend = groupVar, sizeLegend = sizeVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options)

            vijDebugPlot(self, plot)

            return(plot)
        }

        )
)
