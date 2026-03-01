
# This file is a generated template, your changes will not be overwritten

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

            if ( is.null(xaxis) || is.null(yaxis))
                return(FALSE)

            data <- jmvcore::select(self$data, c(xaxis,yaxis,groupVar,labelVar,sizeVar, self$options$facet))
            data[[xaxis]] <- jmvcore::toNumeric(data[[xaxis]])
            data[[yaxis]] <- jmvcore::toNumeric(data[[yaxis]])
            if(!is.null(sizeVar))
                data[[sizeVar]] <- jmvcore::toNumeric(data[[sizeVar]])

            if( ! self$options$keepNA )
                data <- jmvcore::naOmit(data)

            image <- self$results$plot
            image$setState(data)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) {
                return(FALSE)
            }
            xaxis <- self$options$xaxis
            yaxis <- self$options$yaxis
            groupVar <- self$options$group
            labelVar <- self$options$labelVar
            sizeVar <- self$options$ptSize

            if (!is.null(self$options$facet)) {
                facetVar <- self$options$facet
                facetVar <- ensym(facetVar)
            } else {
                facetVar <- NULL
            }

            xaxis <- ensym(xaxis)
            yaxis <- ensym(yaxis)
            if( !is.null(labelVar) ) {
                labelVar <- ensym(labelVar)
            }
            if( !is.null(groupVar) ) {
                groupVar <- ensym(groupVar)
            }
            if( !is.null(sizeVar) ) {
                sizeVar <- ensym(sizeVar)
            }

            plotData <- image$state

            if( !is.null(sizeVar) ) {
                plot <- ggplot(plotData, aes(x = !!xaxis, y = !!yaxis, size = !!sizeVar, color = !!groupVar, fill = !!groupVar))
                if( is.null(groupVar)) {
                    plot <- plot + geom_point(color = self$options$singleColor)
                } else {
                    plot <- plot + geom_point(aes(color = !!groupVar), show.legend = TRUE)
                }
            } else {
                plot <- ggplot(plotData, aes(x = !!xaxis, y = !!yaxis, color = !!groupVar, fill = !!groupVar))
                if( is.null(groupVar)) {
                    plot <- plot + geom_point(color = self$options$singleColor, size = self$options$pointSize)
                } else {
                    plot <- plot + geom_point(aes(color = !!groupVar), size = self$options$pointSize, show.legend = TRUE)
                }
            }

            if (self$options$regLine) {
                plot <- plot + ggplot2::geom_smooth(method = self$options$lineMethod, se = self$options$lineSE, formula = y ~ x,
                                                    size = self$options$lineSize, show.legend = FALSE)
            }

            plot <- plot + guides(color = guide_legend(override.aes = list(size=4)))

            if( !is.null(labelVar) ) {
                x_scale <- max(plotData[[xaxis]], na.rm=TRUE) - min(plotData[[xaxis]], na.rm=TRUE)
                plot <- plot + geom_text(aes(label = !!labelVar), color="black",
                                         na.rm=TRUE, size=4, hjust = 0, nudge_x = 0.02*x_scale,
                                         check_overlap = self$options$overlap)
                # Enlarge graphic by 10% at right (for labels)
                maxx <- max(plotData[[xaxis]], na.rm=T) + 0.1*(max(plotData[[xaxis]], na.rm=T) - min(plotData[[xaxis]], na.rm=T))
                plot <- plot + expand_limits(x = maxx)
            }
            if (self$options$hline)
                plot <- plot + geom_hline(yintercept= self$options$yinter,  color="black", size = self$options$lineSize/2)
            if (self$options$vline)
                plot <- plot + geom_vline(xintercept= self$options$xinter,  color="black", size = self$options$lineSize/2)

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "color", drop = FALSE) + vijScale(self$options$colorPalette, "fill", drop = FALSE)

            # Axis Limits
            if (self$options$yAxisRangeType == "manual")
                yLim <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
            else
                yLim <- NULL
            if (self$options$xAxisRangeType == "manual")
                xLim <- c(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
            else
                xLim <- NULL
            plot <- plot + coord_cartesian(ylim = yLim, xlim = xLim)

            if( self$options$plotBorder ) {
                plot <- plot + theme(axis.line = element_line(linewidth = 0), panel.border = element_rect(color = "black", fill = NA, size = 1))
            }

            # Facet
            if (!is.null(facetVar)) {
                if (self$options$facetBy == "column")
                    plot <- plot + facet_wrap(vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + facet_wrap(vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            # Titles & Labels
            defaults <- list(y = yaxis, x = xaxis, legend = groupVar, sizeLegend = sizeVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)

            return(plot)
        }

        )
)
