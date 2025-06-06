
# This file is a generated template, your changes will not be overwritten

areachartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "areachartClass",
    inherit = areachartBase,
    private = list(
        .init = function() {
            # Set the size of the plot
            userWidth <- as.numeric(self$options$plotWidth)
            userHeight <- as.numeric(self$options$plotHeight)
            width <- 600
            height <- 400
            # Compute the size according to options
            if (userWidth * userHeight == 0) {
                if (!is.null(self$options$group) || length(self$options$vars) > 1)
                    width <- 650
                if (self$options$rotateLabels)
                    height <- 450
            }
            if (userWidth >0)
                width = userWidth
            if (userHeight >0)
                height = userHeight
            image <- self$results$plot
            image$setSize(width, height)
        },
        .run = function() {
            oneVariable <- (self$options$mode == "oneVariable")
            if (oneVariable) {
                timeVar <- self$options$timeVar
                depVars <- self$options$var
                groupVar <- self$options$group
                varNames <- c(timeVar, depVars, groupVar)
            } else {
                timeVar <- self$options$timeVar1
                depVars <- self$options$vars
                varNames <- c(timeVar, depVars)
            }
            if (length(depVars) == 0 || is.null(timeVar))
                return()
            data <- jmvcore::select(self$data, varNames)
            # Delete row with missing time
            data <- subset(data, !is.na(data[timeVar]))
            # Be sure dep var are numeric
            for (aVar in depVars)
                data[[aVar]] <- jmvcore::toNumeric(data[[aVar]])
            image <- self$results$plot
            image$setState(data)
        },
        .plot = function(image, ggtheme, theme, ...) {
            plotData <- image$state
            oneVariable <- (self$options$mode == "oneVariable")
            if (oneVariable) {
                timeVar <- self$options$timeVar
                depVar <- self$options$var
                groupVar <- self$options$group
            } else {
                timeVar <- self$options$timeVar1
                if (length(self$options$vars) > 0 && !is.null(timeVar)) {
                    plotData <- plotData %>%
                                    gather(key = "Variables", value = "Values", -timeVar)
                    # Transform "Variables" as factor to keep the variable order.
                    plotData$Variables <- factor(plotData$Variables, levels = self$options$vars)
                    depVar <- "Values"
                    groupVar <- "Variables"
                } else {
                    depVar <- NULL
                }
            }

            if (is.null(depVar) || is.null(timeVar))
                return(FALSE)

            timeVar <- ensym(timeVar)
            depVar <- ensym(depVar)
            if (!is.null(groupVar))
                groupVar <- ensym(groupVar)

            # Time format
            timeVarIsDate <- self$options$isDate
            if (timeVarIsDate) {
                if (self$options$dateFormat == "auto") {
                    timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], "iso")
                    if (is.null(timeVarAsDate)) {
                        timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], "us")
                        if (is.null(timeVarAsDate)) {
                            timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], "eu")
                        }
                    }
                } else {
                    timeVarAsDate <- private$.convertToDate(plotData[[timeVar]], self$options$dateFormat)
                }

                if (!is.null(timeVarAsDate)) {
                    plotData[[timeVar]] <- timeVarAsDate
                } else {
                    jmvcore::reject(paste0(self$options$timeVar, .(" doesn't have a valid date format.")))
                    timeVarIsDate <- FALSE
                }
            }

            # Plot options (lines)
            if (self$options$showLine)
                lineWidth <- self$options$lineWidth
            else
                lineWidth <- 0

            # Plot options (position)
            if (self$options$position == "identity")
                alpha = 0.5
            else
                alpha = 1

            if (is.null(groupVar))
                plot <- ggplot(plotData, aes(x = !!timeVar, group = 1)) +
                            geom_area(aes(y = !!depVar, fill = "OneVar"),
                                      position = self$options$position,
                                      color='black', size = lineWidth,
                                      alpha = alpha, show.legend = FALSE)
            else
                plot <- ggplot(plotData, aes(x = !!timeVar, group = !!groupVar)) +
                            geom_area(aes(y = !!depVar, fill = !!groupVar),
                                      position = self$options$position,
                                      color='black', size = lineWidth,
                                      alpha = alpha)

            plot <- plot + ggtheme + scale_fill_brewer(palette = self$options$colorPalette)

            if (timeVarIsDate) {
                Sys.setlocale("LC_TIME", .("en_US.utf-8"))
                plot <- plot + scale_x_date(date_labels = self$options$displayFormat, date_breaks = self$options$dateBreak)
            }

            if (self$options$rotateLabels)
                plot <- plot + theme(axis.text.x=element_text(angle=60, hjust=0.5))

            if (self$options$position == "fill")
                plot <- plot + scale_y_continuous(labels=percent_format())

            if (!oneVariable)
                plot <- plot + labs(fill='')

            if (!is.null(self$options$ylabel) && self$options$ylabel != "")
                plot <- plot + labs(y = self$options$ylabel)

            return(plot)
        },
        .convertToDate = function(dAsString, fmt) {
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
