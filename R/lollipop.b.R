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

            aName <- self$options$aVar
            groupName <- self$options$group
            facetName <- self$options$facet

            # .X, .Y, .YMIN and .YMAX are the columns computed below
            reservedVar <- intersect(c(aName, groupName, facetName), c(".X", ".Y", ".YMIN", ".YMAX"))
            if (length(reservedVar) > 0)
                vijErrorMessage(self, jmvcore::format(.("\"{var}\" is a reserved name. Please rename this variable."), var = reservedVar[1]))

            plotData <- jmvcore::select(self$data, c(aName, groupName, facetName))
            plotData[[aName]] <- jmvcore::toNumeric(plotData[[aName]])

            plotData <- jmvcore::naOmit(plotData)
            if (nrow(plotData) == 0)
                return(FALSE)

            yaxis <- self$options$yaxis

            # Position of the lollipops on the axis (.X): sorted on the raw data, before it is summarised.
            if (self$options$order != "none") {
                orderFun <- switch(yaxis, mean = mean, median = stats::median, min = min, max)
                plotData[[".X"]] <- forcats::fct_reorder(plotData[[groupName]], plotData[[aName]], .fun = orderFun,
                                                         .desc = (self$options$order == "decreasing"))
            } else {
                plotData[[".X"]] <- plotData[[groupName]]
            }

            # Summarize per group/facet: the state is saved into the .omv, so store one row per
            # lollipop rather than one row per case ("identity" draws the values themselves).
            cellFun <- switch(yaxis,
                mean = function(x) data.frame(.Y = mean(x)),
                median = function(x) data.frame(.Y = stats::median(x)),
                min = function(x) data.frame(.Y = min(x)),
                max = function(x) data.frame(.Y = max(x)),
                minmax = function(x) data.frame(.YMIN = min(x), .YMAX = max(x)),
                identity = function(x) data.frame(.Y = x))
            plotData <- plotData |>
                dplyr::group_by(dplyr::pick(dplyr::all_of(c(".X", facetName)))) |>
                dplyr::reframe(cellFun(.data[[aName]])) |>
                as.data.frame()

            image <- self$results$plot
            image$setState(plotData)
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            aVar <- self$options$aVar
            groupVar <- self$options$group
            facetVar <- vijVar(self$options$facet)

            plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data[[".X"]]))

            if (self$options$yaxis == "minmax") {
                mainColor <- self$options$dotColor
                lightColor <- scales::col_mix(mainColor, "white", 0.4, space = "lab")
                darkColor <- scales::col_mix(mainColor, "black", 0.2, space = "lab")
                plot <- plot +
                    ggplot2::geom_linerange(ggplot2::aes(ymin = .data[[".YMIN"]], ymax = .data[[".YMAX"]]), linewidth = self$options$lineWidth, color = self$options$lineColor) +
                    ggplot2::geom_point(ggplot2::aes(y = .data[[".YMIN"]]), size = self$options$dotSize, color = lightColor) +
                    ggplot2::geom_point(ggplot2::aes(y = .data[[".YMAX"]]), size = self$options$dotSize, color = darkColor)
            } else {
                plot <- plot +
                    ggplot2::geom_segment(ggplot2::aes(y = .data[[".Y"]], yend = 0), linewidth = self$options$lineWidth, color = self$options$lineColor) +
                    ggplot2::geom_point(ggplot2::aes(y = .data[[".Y"]]), size = self$options$dotSize, color = self$options$dotColor)
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
                ylabel <- jmvcore::format(.("Mean of {var}"), var = aVar)
            else if (self$options$yaxis == "median")
                ylabel <- jmvcore::format(.("Median of {var}"), var = aVar)
            else if (self$options$yaxis == "min")
                ylabel <- jmvcore::format(.("Minimum of {var}"), var = aVar)
            else if (self$options$yaxis == "max")
                ylabel <- jmvcore::format(.("Maximum of {var}"), var = aVar)
            else
                ylabel <- aVar

            # facet
            if (!is.null(facetVar)) {
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            # Theme and colors
            plot <- plot + ggtheme

            # Grid lines (value axis only)
            plot <- plot + vijGridLines(self$options$theme,
                                        x = self$options$horizontal && self$options$xGridLines,
                                        y = !self$options$horizontal && self$options$yGridLines)

            # Titles & Labels
            defaults <- list(y = ylabel, x = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options, showLegend = FALSE)

            vijDebugPlot(self, plot)

            return(plot)
        })
)
