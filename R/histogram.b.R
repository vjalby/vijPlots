histogramClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "histogramClass",
    inherit = histogramBase,
    private = list(
        .init = function() {
            # Default size
            # single facet : w = 550 + 50, h = 350 + 50
            # multiple facets : w = 450*ncol + 50 , h = 300*nrow + 50
            # legend :  w + 100 if left/right, h + 50 if top/bttom

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
                width <- max(550, 450*nbOfColumn)
                height <- max(350,300*nbOfRow)
            } else {
                width <- 550
                height <- 350
            }
            # Fixed dimension
            fixed_width <- 50 # Y-Axis legend
            fixed_height <- 50 # X-Axis legend
            if (!is.null(self$options$group)) {
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
        	if (is.null(self$options$aVar) || nrow(self$data) == 0)
        		return(FALSE)

            varNames <- c(self$options$aVar, self$options$group, self$options$facet)
            plotData <- jmvcore::select(self$data, varNames)
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

			#### Set variables ####

            xVar <- rlang::sym(self$options$aVar)

            if (!is.null(self$options$group)) {
                groupVar <- rlang::sym(self$options$group)
            } else {
                groupVar <- NULL
            }

            if (!is.null(self$options$facet)) {
                facetVar <- rlang::sym(self$options$facet)
            } else {
                facetVar <- NULL
            }

			#### Set options ####

            # Bin width
			if (self$options$binWidth == 0) { # ggplot default
                x_vals <- plotData[[xVar]]
                x_range <- range(x_vals, na.rm = TRUE)
                binWidth <- (x_range[2] - x_range[1]) / 30
            } else {
            	binWidth <- self$options$binWidth
            }

			# Boundary
            if (self$options$binBoundary == 0)
                binBoundary <- NULL
            else
                binBoundary <- self$options$binBoundary

            # Border color
            if (self$options$borderColor == "none") {
                borderColor <- NA
            } else {
                borderColor <- self$options$borderColor
            }
            # Fill color
            if (self$options$fillColor == "none") {
                fillColor <- NA
            } else {
                fillColor <- self$options$fillColor
            }

            # Transparency
            if (!is.null(groupVar) && self$options$groupingN == "identity") {
                histAlpha <- self$options$binOpacity #0.5
            } else {
            	histAlpha <- NA
            }

            # Legend glyph priority: filled square if bins/density shown, colored line otherwise
            legendAsSquare <- self$options$showBins || self$options$density

			#### Build histogram ####

			plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = !!xVar, fill = !!groupVar, color = !!groupVar))

            if (self$options$histtype == "density") {
                histAes <- ggplot2::aes(y = ggplot2::after_stat(density))
            } else {
            	histAes <- ggplot2::aes() # no change
            }

            #### Bins ####

            if (self$options$showBins) {
                if (!is.null(groupVar)) {
                    if (self$options$usePalette == "forFilling") { # fixed border color
                        plot <- plot + ggplot2::geom_histogram(
                            mapping = histAes,
                            binwidth = binWidth,
                            boundary = binBoundary,
                            position = self$options$groupingN,
                            alpha = histAlpha,
                            color = borderColor,
                            show.legend = TRUE
                        )
                    } else { # fixed fill color
                        plot <- plot + ggplot2::geom_histogram(
                            mapping = histAes,
                            binwidth = binWidth,
                            boundary = binBoundary,
                            position = self$options$groupingN,
                            alpha = histAlpha,
                            fill = fillColor,
                            show.legend = TRUE
                        )
                    }
                } else { # no group / fixed colors
                    plot <- plot + ggplot2::geom_histogram(
                        mapping = histAes,
                        binwidth = binWidth,
                        boundary = binBoundary,
                        fill = fillColor,
                        color = borderColor,
                        alpha = histAlpha
                    )
                }
            }

            #### Lines ####

            if (self$options$showLines) {
                plot <- plot + ggplot2::geom_freqpoly(
                    mapping = histAes,
                    binwidth = binWidth,
                    boundary = binBoundary,
                    position = self$options$groupingN,
                    linewidth = self$options$lineLineSize,
                    show.legend = !legendAsSquare
                )
            }

            #### Normal Curve ####

            if (self$options$normalCurve) {
                lineType <- ifelse(self$options$dashedDensity,2,1)
                lineSize <- self$options$normalCurveLineSize

                if (self$options$histtype == "density") {
                	normalCurveAes <- ggplot2::aes() # no change
                } else {
                    normalCurveAes <- ggplot2::aes(y = ggplot2::after_stat(count) * binWidth)
                }

				if (is.null(groupVar)) {
                    plot <- plot + ggh4x::stat_theodensity(
                        mapping = normalCurveAes,
                        na.rm = TRUE,
                        color = 'red',
                        linewidth = lineSize,
                        linetype = lineType
                    )
                } else {
                    plot <- plot + ggh4x::stat_theodensity(
                        mapping = normalCurveAes,
                        na.rm = TRUE,
                        linewidth = lineSize,
                        linetype = lineType,
                        show.legend = !legendAsSquare,
                        position = self$options$groupingN
                    )
                }
             }

            #### Density ####

            if (self$options$density) {
            	densityAlpha <- self$options$densityOpacity
                densitySize  <- self$options$densityLineSize

            	if (self$options$histtype == "density") {
                    densityAes <- ggplot2::aes(y = ggplot2::after_stat(density))
                } else {
                    densityAes <- ggplot2::aes(y = ggplot2::after_stat(count) * binWidth)
                }

                if (is.null(groupVar)) {
                    plot <- plot + ggplot2::geom_density(
                        mapping = densityAes,
                        fill = fillColor,
                        alpha = densityAlpha,
                        linewidth = densitySize
                    )
                } else {
                    plot <- plot + ggplot2::geom_density(
                        mapping = densityAes,
                        alpha = densityAlpha,
                        linewidth = densitySize,
                        position = self$options$groupingN,
                        show.legend = legendAsSquare
                    )
                }
            }

            #### Mean/Median Lines ####

            if (self$options$meanLine)
                plot <- plot + private$.summaryLine(plotData, xVar, groupVar, facetVar, mean, "dashed", "µ")
            if (self$options$medianLine)
                plot <- plot + private$.summaryLine(plotData, xVar, groupVar, facetVar, stats::median, "dotted", "Med")

            #### Axes ####

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

            if (self$options$summaryLineLabel && (self$options$meanLine || self$options$medianLine)) {
                expand_arg <- ggplot2::expansion(mult = c(0.05, 0.1))
            } else {
                expand_arg <- ggplot2::waiver()
            }

            # Ticks
            if (self$options$xTicks > 0) {
                plot <- plot  + ggplot2::scale_x_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1))
            }
            if (self$options$yTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1),
                                                            expand = expand_arg)
            } else {
                plot <- plot + ggplot2::scale_y_continuous(expand = expand_arg)
            }

            # Facet
            if (!is.null(facetVar)) {
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            # Theme and colors
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE) +
                                    vijColorScale(self$options$colorPalette, "color", theme, drop = FALSE)

            # Titles & Labels
            yLab <- ifelse(self$options$histtype == "density", .("Density"), .("Count"))
            defaults <- list(legend = groupVar, x = xVar, y = yLab)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) +
                            vijTitleAndLabelFormat(self$options)

            # Legend position
            plot <- plot + ggplot2::theme(legend.key.spacing.y = grid::unit(1, "mm"), legend.byrow = TRUE)

            vijDebugPlot(self, plot)

            return(plot)
        },
        .summaryLine = function(plotData, xVar, groupVar, facetVar, fun, linetype, symbol) {
            lineData <- plotData |>
                dplyr::group_by(!!groupVar, !!facetVar) |>
                dplyr::summarise(.value = fun(!!xVar, na.rm = TRUE), .groups = "drop")

            if (is.null(groupVar)) {
                vline <- ggplot2::geom_vline(
                    data = lineData,
                    mapping = ggplot2::aes(xintercept = .value),
                    color = "black", linetype = linetype, linewidth = 0.8
                )
            } else {
                vline <- ggplot2::geom_vline(
                    data = lineData,
                    mapping = ggplot2::aes(xintercept = .value, color = !!groupVar),
                    linetype = linetype, linewidth = 0.8, show.legend = FALSE
                )
            }

            if (!self$options$summaryLineLabel)
                return(vline)

            lineData$.label <- sprintf("%s = %.2f", symbol, lineData$.value)

            if (is.null(groupVar)) {
                # when both mean and median lines are shown, offset the label away
                # from the other value's line so the two don't collide when close
                hjust <- -0.1
                if (self$options$meanLine && self$options$medianLine) {
                    meanValue <- mean(plotData[[xVar]], na.rm = TRUE)
                    medianValue <- stats::median(plotData[[xVar]], na.rm = TRUE)
                    if ((identical(fun, mean) && meanValue <= medianValue) ||
                            (identical(fun,stats::median) && medianValue < meanValue )) {
                        hjust <- 1.1
                    }
                }
                label <- ggplot2::geom_text(
                    data = lineData,
                    mapping = ggplot2::aes(x = .value, y = Inf, label = .label),
                    color = "black", vjust = 1.5, size = 4, hjust = hjust
                )
            } else {
                label <- ggrepel::geom_text_repel(
                    data = lineData,
                    mapping = ggplot2::aes(x = .value, y = Inf, label = .label, color = !!groupVar),
                    vjust = 1.5, size = 4, show.legend = FALSE,
                    direction = "x", seed = 123, min.segment.length = 100, point.padding = 1
                )
            }

            list(vline, label)
        }
    )
)
