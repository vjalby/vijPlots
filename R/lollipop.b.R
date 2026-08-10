lollipopClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lollipopClass",
    inherit = lollipopBase,
    private = list(
        .init = function() {
            if (!is.null(self$options$group))
                nbOfLevel <- nlevels(self$data[[self$options$group]])
            else
                nbOfLevel <- 5

            # Stretchable dimensions
            if (self$options$horizontal) {
                width <- 400
                height <- min(max(250,nbOfLevel*50),650)
            } else {
                width <- min(max(350,nbOfLevel*75),600)
                height <- 350
            }
            # With facets
            if (!is.null(self$options$facet)) {
                nbOfFacet <- nlevels(self$data[[self$options$facet]])
                if (self$options$facetBy == "column") {
                    nbOfColumn <- self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn)
                } else {
                    nbOfRow <- self$options$facetNumber
                    nbOfColumn <- ceiling(nbOfFacet / nbOfRow)
                }
                width <- max(width, 0.75*width*nbOfColumn)
                height <- max(height, 0.75*height*nbOfRow)
            }
            # Fixed dimension
            if (self$options$horizontal) {
                fixed_width <- 100
                fixed_height <- 50
            } else {
                fixed_width <- 75
                fixed_height <- 50
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
            if (is.null(self$options$aVar) || is.null(self$options$group) || nrow(self$data) == 0)
                return(FALSE)

            plotData <- self$data[c(self$options$aVar, self$options$group, self$options$facet)]
            plotData[[self$options$aVar]] <- jmvcore::toNumeric(plotData[[self$options$aVar]])

            plotData <- jmvcore::naOmit(plotData)
            if (nrow(plotData) == 0)
                return(FALSE)

            image <- self$results$plot
            image$setState(plotData)
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            aVar <- rlang::sym(self$options$aVar)
            groupVar <- rlang::sym(self$options$group)

            if (!is.null(self$options$facet)) {
                facetVar <- rlang::sym(self$options$facet)
            } else {
                facetVar <- NULL
            }

            orderFun <- self$options$yaxis
            if (orderFun == "minmax" || orderFun == "identity")
                orderFun <- max

            if (self$options$order == "decreasing")
                plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = forcats::fct_reorder(!!groupVar,!!aVar, .fun = orderFun, .desc = TRUE) , y = !!aVar))
            else if (self$options$order == "increasing")
                plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = forcats::fct_reorder(!!groupVar,!!aVar, .fun = orderFun, .desc = FALSE) , y = !!aVar))
            else
                plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = !!groupVar, y = !!aVar))

            summaryFun <- self$options$yaxis
            if (summaryFun == "minmax") {
                mainColor <- self$options$dotColor
                lightColor <- colorspace::lighten(mainColor, 0.4)
                darkColor <- colorspace::darken(mainColor, 0.2)
                plot <- plot +
                    ggplot2::stat_summary(geom = "linerange", fun.min = "min", fun.max = "max", linewidth = self$options$lineWidth, color = self$options$lineColor) +
                    ggplot2::stat_summary(geom = "point", fun = "min", size = self$options$dotSize, color = lightColor) +
                    ggplot2::stat_summary(geom = "point", fun = "max", size = self$options$dotSize, color = darkColor)
            } else {
                plot <- plot +
                    ggplot2::stat_summary(geom = "segment", fun = summaryFun, ggplot2::aes(yend = 0), linewidth = self$options$lineWidth, color = self$options$lineColor)+
                    ggplot2::stat_summary(geom = "point", fun = summaryFun, size = self$options$dotSize, color = self$options$dotColor)
            }

            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + ggplot2::coord_flip(ylim = c(self$options$xAxisRangeMin, self$options$xAxisRangeMax))
                } else {
                    plot <- plot + ggplot2::coord_flip()
                }
            } else if (self$options$yAxisRangeType == "manual") { # Vertical and manual
                plot <- plot + ggplot2::coord_cartesian(ylim = c(self$options$yAxisRangeMin, self$options$yAxisRangeMax))
            }

            # Ticks
            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1))
            }
            if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1))
            }

            plot <- plot + ggplot2::scale_x_discrete(drop = FALSE) # keep unused levels

            # Axis Labels
            if (self$options$yaxis == "mean")
                ylabel = jmvcore::format(.("Mean of {var}"), var = aVar)
            else if (self$options$yaxis == "median")
                ylabel = jmvcore::format(.("Median of {var}"), var = aVar)
            else if (self$options$yaxis == "min")
                ylabel = jmvcore::format(.("Minimum of {var}"), var = aVar)
            else if (self$options$yaxis == "max")
                ylabel = jmvcore::format(.("Maximum of {var}"), var = aVar)
            else
                ylabel = aVar

            # facet
            if (!is.null(facetVar)) {
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            # Theme and colors
            plot <- plot + ggtheme

            # Titles & Labels
            defaults <- list(y = ylabel, x = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options, showLegend = FALSE)

            return(plot)
        })
)
