linechartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "linechartClass",
    inherit = linechartBase,
    private = list(
        .init = function() {
            # Stretchable dimensions
            width <- 425
            height <- 350
            # Fixed dimension
            fixed_width <- 75 # Y-Axis legend
            fixed_height <- 50 # X-Axis legend
            if (!is.null(self$options$group) || length(self$options$vars) > 1) {
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
            timeVar <- self$options$timeVar
            depVars <- self$options$vars
            groupVar <- self$options$group
            varNames <- c(timeVar, depVars, groupVar)

            if (length(depVars) == 0 || is.null(timeVar))
                return()

            plotData <- jmvcore::select(self$data, varNames)

            # Be sure dep var are numeric
            for (aVar in depVars)
                plotData[[aVar]] <- jmvcore::toNumeric(plotData[[aVar]])
            # Delete row with missing time
            plotData <- plotData[!is.na(plotData[[timeVar]]),]

            # Ignore NA
            if (!is.null(groupVar) && self$options$ignoreNA)
                plotData <- plotData[!is.na(plotData[[groupVar]]),]

            if (nrow(plotData) == 0)
                return()

            # Time format
            timeVarIsNumeric <- is.numeric(plotData[[timeVar]])
            timeVarIsDate <- (self$options$isDate && !timeVarIsNumeric)

            # Check time format
            if (self$options$isDate && timeVarIsNumeric) {
                vijWarningMessage(self,.("A date variable must be in text format."))
            }

            # Convert timeVar as date
            if (timeVarIsDate) {
                timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], self$options$dateFormat)
                if (!is.null(timeVarAsDate)) {
                    plotData[[timeVar]] <- timeVarAsDate
                } else {
                    errorMessage <- jmvcore::format(.("{var} doesn't have a valid date format."), var = self$options$timeVar)
                    vijWarningMessage(self, errorMessage)
                    timeVarIsDate <- FALSE
                }
            }

            # Check axis options
            if (!timeVarIsNumeric && self$options$xTicks > 0)
                vijWarningMessage(self, .("\"Tick Count\" option for the X-axis is only available for numeric variables."))
            if (!timeVarIsNumeric && !timeVarIsDate && self$options$xAxisRangeType == "manual")
                vijWarningMessage(self, .("\"Range\" option for the X-axis is only available for numeric and date variables."))

            image <- self$results$plot
            image$setState(plotData)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            timeVar <- rlang::sym(self$options$timeVar)
            depVars <- self$options$vars
            if (!is.null(self$options$group))
                groupVar <- rlang::sym(self$options$group)
            else
                groupVar <- NULL

            # timeVar format
            timeVarIsNumeric <- is.numeric(plotData[[timeVar]])
            timeVarIsDate <- inherits(plotData[[timeVar]], "Date")

            dotSize <- self$options$dotSize
            lineWidth <- self$options$lineWidth

            if (length(depVars) > 1) {
                plotDataLong <- tidyr::pivot_longer(
                    plotData,
                    cols = tidyselect::all_of(depVars),
                    names_to = ".variable",
                    values_to = ".value"
                )
                if (is.null(groupVar)) {
                    plot <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = !!timeVar, y = .value, color = .variable, group = .variable))
                } else {
                    plot <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = !!timeVar, y = .value, color = .variable, linetype = !!groupVar, group = interaction(.variable, !!groupVar)))
                }
                plot <- plot + ggplot2::geom_line(linewidth = lineWidth)
                if (self$options$showPoint) {
                    plot <- plot + ggplot2::geom_point(size = dotSize)
                }
            } else {
                # A single variable
                aVar <- rlang::sym(depVars[1])
                if (is.null(groupVar)) {
                    plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = !!timeVar, y = !!aVar, group = 1, color = "aVar")) +
                                ggplot2::geom_line(linewidth = lineWidth)
                    if (self$options$showPoint) {
                        plot <- plot + ggplot2::geom_point(size = dotSize)
                    }
                } else {
                    plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = !!timeVar, y = !!aVar, color = !!groupVar, group = !!groupVar)) +
                                ggplot2::geom_line(linewidth = lineWidth)
                    if (self$options$showPoint) {
                        plot <- plot + ggplot2::geom_point(size = dotSize)
                    }
                }
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "color")

            if (length(depVars) > 1 ){
                plot <- plot + ggplot2::guides(color = ggplot2::guide_legend(order = 1), linetype = ggplot2::guide_legend(order = 2))
                yLab <- .("Values")
                gLab <- .("Variables")
            } else {
                yLab <- depVars
                gLab <- self$options$group
            }
            if (length(depVars) > 1 && is.null(groupVar))
                plot <- plot + ggplot2::labs(color = '')

            if (length(depVars) == 1 && is.null(groupVar))
                showLegend <- FALSE
            else
                showLegend <- TRUE

            # Date range/scale
            if (timeVarIsDate) {
                if (self$options$xAxisRangeType == "manual") { # Date and manual
                    plot <- plot + ggplot2::scale_x_date(labels = private$.myDateLabel, date_breaks = self$options$dateBreak,
                                                        limits = private$.convertToDate(c(self$options$xAxisRangeMin,self$options$xAxisRangeMax), self$options$dateFormat),
                                                        expand = c(0, 0))
                } else {
                    plot <- plot + ggplot2::scale_x_date(labels = private$.myDateLabel, date_breaks = self$options$dateBreak)
                }
            }

            # Axis Ticks (always set ticks before range)
            if (timeVarIsNumeric && self$options$xTicks > 0) {
                plot <- plot  + ggplot2::scale_x_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1))
            }
            if (self$options$yTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1))
            }

            # Axis ranges
            if (timeVarIsNumeric && self$options$xAxisRangeType == "manual")
                xLim <- c(as.double(self$options$xAxisRangeMin), as.double(self$options$xAxisRangeMax))
            else
                xLim <- NULL
            if (self$options$yAxisRangeType == "manual")
                yLim <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
            else
                yLim <- NULL
            if (!is.null(xLim) || !is.null(yLim))
                plot <- plot + ggplot2::coord_cartesian(xlim = xLim, ylim = yLim)

            # Titles & Labels
            defaults <- list(y = yLab, x = timeVar, legend = gLab)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options, showLegend = showLegend)
            plot <- plot + ggplot2::theme(legend.key.spacing.y = grid::unit(1, "mm"), legend.byrow = TRUE)

            vijDebugPlot(self, plot)

            return(plot)
        },
        .myDateLabel = function(dateVector) {
            mapply(private$.formatDate, dateVector)
        },
        .formatDate = function(aDate) {
            longMonths <- c(.("January"), .("February"), .("March"), .("April"), .("May"), .("June"),
                            .("July"), .("August"), .("September"), .("October"), .("November"), .("December"))
            shortMonths <- c(.("Jan"), .("Feb"), .("Mar"), .("Apr"), .("May"), .("Jun"),
                             .("Jul"), .("Aug"), .("Sep"), .("Oct"), .("Nov"), .("Dec"))
            shortMonth <- shortMonths[as.integer(format.Date(aDate, "%m"))]
            longMonth <- longMonths[as.integer(format.Date(aDate, "%m"))]
            firstUp <- function(s){paste0(toupper(substring(s, 1, 1)), substring(s, 2))}
            if (self$options$displayFormat == "%Y %B %e")
                return(paste(format.Date(aDate, "%Y"), longMonth, trimws(format.Date(aDate, "%e"))))
            else if (self$options$displayFormat == "%e %B %Y")
                return(paste(format.Date(aDate, "%e"), longMonth, format.Date(aDate, "%Y")))
            else if (self$options$displayFormat == "%B %Y")
                return(paste(firstUp(longMonth), format.Date(aDate, "%Y")))
            else if (self$options$displayFormat == "%B %y")
                return(paste(firstUp(longMonth), format.Date(aDate, "%y")))
            else if (self$options$displayFormat == "%b %Y")
                return(paste(firstUp(shortMonth), format.Date(aDate, "%Y")))
            else if (self$options$displayFormat == "%b %y")
                return(paste(firstUp(shortMonth), format.Date(aDate, "%y")))
            else if (self$options$displayFormat == "%Y %B")
                return(paste(format.Date(aDate, "%Y"), longMonth))
            else if (self$options$displayFormat == "%Y %b")
                return(paste(format.Date(aDate, "%Y"), shortMonth))
            else if (self$options$displayFormat == "%b")
                return(firstUp(shortMonth))
            else if (self$options$displayFormat == "%B")
                return(firstUp(longMonth))
            else
                return(format.Date(aDate, self$options$displayFormat))
        },
        .convertToDate = function(dAsString, fmt) {
            if (fmt == "auto") {
                dAsDate <- private$.convertToDateBase(dAsString, "iso")
                if (is.null(dAsDate)) {
                    dAsDate <- private$.convertToDateBase(dAsString, "us")
                    if (is.null(dAsDate)) {
                        dAsDate <- private$.convertToDateBase(dAsString, "eu")
                    }
                }
            } else {
                dAsDate <- private$.convertToDateBase(dAsString, fmt)
            }
            return(dAsDate)
        },
        .convertToDateBase = function(dAsString, fmt) {
            n <- length(stats::na.omit(dAsString))
            if (fmt == "iso")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d"))
            else if (fmt == "us")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%m-%d-%Y", "%m/%d/%Y", "%m.%d.%Y", "%m%d%Y"))
            else if (fmt== "eu")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y", "%d%m%Y"))
            else
                dAsDate <- NULL
            if (length(stats::na.omit(dAsDate)) != n || min(as.numeric(format(dAsDate, "%Y")), na.rm=T) < 100)
                dAsDate <- NULL
            return(dAsDate)
        })
)
