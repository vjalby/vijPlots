
# This file is a generated template, your changes will not be overwritten

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

            if (!self$options$isDate && !is.numeric(plotData[[timeVar]]) && (self$options$xTicks > 0 || self$options$xAxisRangeType == "manual"))
                vijWarningMessage(self,"\"Tick Count\" and \"Range\" options for the X-axis are only available for numeric variables.")

            if (self$options$isDate && is.numeric(plotData[[timeVar]]))
                vijWarningMessage(self,"A date variable must be in text format.")

            # Be sure dep var are numeric
            for (aVar in depVars)
                plotData[[aVar]] <- jmvcore::toNumeric(plotData[[aVar]])
            # Delete row with missing time
            plotData <- subset(plotData, !is.na(plotData[timeVar]))
            # Ignore NA
            if (!is.null(groupVar) && self$options$ignoreNA)
                plotData <- subset(plotData, !is.na(plotData[groupVar]))
            if (nrow(plotData) == 0)
                return()
            image <- self$results$plot
            image$setState(plotData)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            timeVar <- self$options$timeVar
            groupVar <- self$options$group
            depVars <- self$options$vars

            timeVar <- ensym(timeVar)
            if (!is.null(groupVar))
                groupVar <- ensym(groupVar)

            # Time format
            timeIsNumeric <- is.numeric(plotData[[timeVar]])
            timeVarIsDate <- (self$options$isDate && !timeIsNumeric)

            if (timeVarIsDate) {
                timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], self$options$dateFormat)
                if (!is.null(timeVarAsDate)) {
                    plotData[[timeVar]] <- timeVarAsDate
                } else {
                    errorMessage <- jmvcore::format(.("{var} doesn't have a valid date format."), var = self$options$timeVar)
                    vijErrorMessage(self, errorMessage)
                    return(TRUE)
                }
            }

            dotSize <- self$options$dotSize
            lineWidth <- self$options$lineWidth

            if (is.null(groupVar))
                plot <- ggplot(plotData, aes(x = !!timeVar, group = 1))
            else
                plot <- ggplot(plotData, aes(x = !!timeVar, group = !!groupVar))

            for (varName in depVars) {
                aVar <- ensym(varName)

                if (is.null(groupVar)) {
                    plot <- plot + geom_line(aes(y = !!aVar, color = !!varName), linewidth = lineWidth)
                    if (self$options$showPoint)
                        plot <- plot + geom_point(aes(y = !!aVar, color = !!varName), size = dotSize)
                } else {
                    if (length(depVars) > 1) {
                        plot <- plot + geom_line(aes(y = !!aVar, color = !!varName, linetype = !!groupVar), linewidth = lineWidth)
                        if (self$options$showPoint)
                            plot <- plot + geom_point(aes(y = !!aVar, color = !!varName), size = dotSize)
                    } else {
                        plot <- plot + geom_line(aes(y = !!aVar, color = !!groupVar), linewidth = lineWidth)
                        if (self$options$showPoint)
                            plot <- plot + geom_point(aes(y = !!aVar, color = !!groupVar), size = dotSize)
                    }
                }
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "color")

            if (length(depVars) > 1 ){
                plot <- plot + guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2))
                yLab <- .("Values")
                gLab <- .("Variables")
            } else {
                yLab <- depVars
                gLab <- self$options$group
            }
            if (length(depVars) > 1 && is.null(groupVar))
                plot <- plot + labs(color = '')

            if (length(depVars) == 1 && is.null(groupVar))
                showLegend <- FALSE
            else
                showLegend <- TRUE

            # Date range/scale
            if (timeVarIsDate) {
                if (self$options$xAxisRangeType == "manual") { # Date and manual
                    plot <- plot + scale_x_date(labels = private$.myDateLabel, date_breaks = self$options$dateBreak,
                                                limits = private$.convertToDate(c(self$options$xAxisRangeMin,self$options$xAxisRangeMax), self$options$dateFormat),
                                                expand = c(0, 0))
                } else {
                    plot <- plot + scale_x_date(labels = private$.myDateLabel, date_breaks = self$options$dateBreak)
                }
            }

            # Axis Ticks (always set ticks before range)
            if (timeIsNumeric && self$options$xTicks > 0) {
                plot <- plot  + scale_x_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1))
            }
            if (self$options$yTicks > 0) {
                plot <- plot  + scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1))
            }

            # Axis ranges
            if (!timeVarIsDate && timeIsNumeric && self$options$xAxisRangeType == "manual")
                xLim <- c(as.double(self$options$xAxisRangeMin), as.double(self$options$xAxisRangeMax))
            else
                xLim <- NULL
            if (self$options$yAxisRangeType == "manual")
                yLim <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
            else
                yLim <- NULL
            if (!is.null(xLim) || !is.null(yLim))
                plot <- plot + coord_cartesian(xlim = xLim, ylim = yLim)

            # Titles & Labels
            defaults <- list(y = yLab, x = timeVar, legend = gLab)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options, showLegend = showLegend)
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            #self$results$text$setContent(plot)

            return(plot)
        },
        .myDateLabel = function(dateVector) {
            mapply(private$.formatDate, dateVector)
        },
        .formatDate = function(aDate) {
            longMonths <- c(.("January"), .("February"), .("March"), .("April"), .("May"), .("June"),
                            .("July"), .("August"), .("September"), .("October"), .("November"), .("December"))
            shortMonths <- c(.("Jan"), .("Feb"), .("Mar"), .("Apr"), .("May"), .("Jun"),
                             .("Jul"), .("Aug"), .("Sept"), .("Oct"), .("Nov"), .("Dec"))
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
            n <- length(na.omit(dAsString))
            if (fmt == "iso")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d"))
            else if (fmt == "us")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%m-%d-%Y", "%m/%d/%Y", "%m.%d.%Y", "%m%d%Y"))
            else if (fmt== "eu")
                dAsDate <- as.Date(dAsString, optional = TRUE, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y", "%d%m%Y"))
            else
                dAsDate <- NULL
            if (length(na.omit(dAsDate)) != n || min(as.numeric(format(dAsDate, "%Y")), na.rm=T) < 100)
                dAsDate <- NULL
            return(dAsDate)
        })
)
