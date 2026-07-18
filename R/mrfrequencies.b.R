mrfrequenciesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mrfrequenciesClass",
    inherit = mrfrequenciesBase,
    private = list(
        .init = function() {
            morevar <- (self$options$mode == "morevar")
            # Show help message (and hide results)
            if ((!morevar && is.null(self$options$repVar)) || (morevar && length(self$options$resps) < 1)) {
                private$.showHelpMessage()
                return()
            }
            table <- self$results$responses
            # Set custom name for options column
            if (morevar)
                table$getColumn('var')$setTitle(self$options$optionname)
            else
                table$getColumn('var')$setTitle(self$options$repVar)
            # Set the rows
            if (morevar) {
                for (i in seq_along(self$options$resps))
                    table$addRow(rowKey = as.character(i))
            } else {
                aCol <- self$data[[self$options$repVar]]
                uniqueValues <- private$.oneHotEncoding(aCol, self$options$separator, init = TRUE)
                for (i in seq_along(uniqueValues))
                    table$addRow(rowKey = as.character(i))
            }
            # Add the "total" row
            if (self$options$showTotal) {
                table$addRow(rowKey = '.total', values = list(var="Total"))
                table$addFormat(rowKey = ".total", col = 1, jmvcore::Cell.BEGIN_GROUP)
            }
            # Set the image dimensions
            width <- 425
            height <- 350
            fixed_width <- 75
            fixed_height <- 50
            image <- self$results$plot
            if (is.null(image[['setSize2']])) { # jamovi < 2.7.16
                image$setSize(width + fixed_width, height + fixed_height)
            } else {
                image$setSize2(width, height, fixed_width, fixed_height)
            }
        },
        .run = function() {
            if (self$options$mode == "morevar") { # Several dychotomous variables
                if (length(self$options$resps) < 1 || nrow(self$data) == 0) {
                    return()
                } else {
                    myresult <- private$.multipleResponse(self$data, self$options$resps, self$options$endorsed, self$options$order)
                }
            } else { # One Multiple Value Variables
                if (is.null(self$options$repVar) || self$options$separator == '' || nrow(self$data) == 0) {
                    return()
                } else {
                    rawData <- self$data[[self$options$repVar]]
                    oneHotData <- private$.oneHotEncoding(rawData, self$options$separator, self$options$emptyAsNA)
                    myresult <- private$.multipleResponse(oneHotData, names(oneHotData), 1, self$options$order)
                }
            }

            table <- self$results$responses
            for(i in seq_len(nrow(myresult$df) - 1)) {
                table$setRow(rowKey = as.character(i),
                             values = list(var = myresult$df[i,1],
                                         freq = myresult$df[i,2],
                                         responsepercent = myresult$df[i,3],
                                         casepercent = myresult$df[i,4]))
            }
            if ( self$options$showTotal ) {
                i <- nrow(myresult$df)
                table$setRow(rowKey = ".total",
                             values = list(var = "Total",
                                         freq = myresult$df[i,2],
                                         responsepercent = myresult$df[i,3],
                                         casepercent = myresult$df[i,4]))
            }

            table$setNote('noc', paste(.("Number of cases:"), myresult$nrOfCases) , init = FALSE)

            image <- self$results$plot
            image$setState(myresult$df[seq_len(nrow(myresult$df)-1),])

        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            plotData <- image$state

            # to be sure the factor ordering is kept
            plotData$Option <- factor(plotData$Option, levels = plotData$Option)

            # Percent format (scales)
            doPercent <- scales::label_percent(
                accuracy = as.numeric(self$options$accuracy),
                suffix = '\u2009%',
                decimal.mark = self$options[['decSymbol']])

            if (self$options$horizontal)
                doNumber <- function(x) formatC(x, width = 2, format = "d")
            else
                doNumber <- as.character

            # Border color
            if (self$options$borderColor == "none")
                borderColor <- NA
            else
                borderColor <- self$options$borderColor

            #### Doing the plot ####
            if (self$options$yaxis == "responses") {
                plot <- ggplot(plotData, aes(Option, Responses, label = doPercent(Responses)))
                labelFnct <- scales::label_percent(suffix = '\u2009%', decimal.mark = self$options[['decSymbol']])
                yLab <- .("% of Responses")
                yScaleFactor <- 100 # yScaleFactor is used for manual range computation (1 = count, 100 = percent)
            } else if (self$options$yaxis == "cases") {
                plot <- ggplot(plotData, aes(Option, Cases, label = doPercent(Cases)))
                labelFnct <- scales::label_percent(suffix = '\u2009%', decimal.mark = self$options[['decSymbol']])
                yLab <- .("% of Cases")
                yScaleFactor <- 100
            } else {
                plot <- ggplot(plotData, aes(Option, Frequency, label = doNumber(Frequency)))
                labelFnct <- waiver()
                yLab <- .("Counts")
                yScaleFactor <- 1
            }

            if (self$options$singleColor) {
                nbColors <- attr(vijPalette(self$options$colorPalette, "fill"),"nlevels")
                colorNo <- self$options$colorNo
                oneColorOfPalette <- vijPalette(self$options$colorPalette, "fill")(nbColors)[min(colorNo,nbColors)]
                plot <- plot + geom_col(fill = oneColorOfPalette, color = borderColor)
            } else {
                plot <- plot + geom_col(aes(fill = Option), color = borderColor) + guides(fill = "none")
            }

            if (self$options$labelPosition == "top")
                textColor <- "black"
            else if (self$options$textColor == "auto" && self$options$singleColor)
                textColor <- ggstats::hex_bw(oneColorOfPalette)
            else
                textColor <- self$options$textColor

            #### Labels ####
            if (self$options$labelPosition == "middle") {
                vjust1 <- 0.5
                hjust2 <- 0.5
                vjust2 <- 0.5
            } else {
                vjust1 <- 1
                if (self$options$horizontal) {
                    hjust2 <- -0.2
                    vjust2 <- 0.5
                } else {
                    hjust2 <- 0.5
                    vjust2 <- -0.6
                }
            }

            if (self$options$showLabels) {
                if (self$options$textColor == "auto" && self$options$labelPosition == "middle" && !self$options$singleColor) {
                    plot <- plot + geom_text(aes(fill = Option, color = after_scale(ggstats::hex_bw(.data$fill))),
                             position = position_stack(vjust = vjust1), vjust = vjust2, hjust = hjust2,
                             fontface = "bold", size = self$options$labelFontSize / .pt)
                } else {
                    plot <- plot + geom_text(color = textColor,
                                             position = position_stack(vjust = vjust1), vjust = vjust2, hjust = hjust2,
                                             fontface = "bold", size = self$options$labelFontSize / .pt)
                }
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill")

            #### Axis Limits & flip ####
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + coord_flip(ylim = c(self$options$xAxisRangeMin/yScaleFactor, self$options$xAxisRangeMax/yScaleFactor))
                } else {
                    plot <- plot + coord_flip(clip = "off")
                }
            } else {
                if (self$options$yAxisRangeType == "manual") { # Horizontal and manual
                    plot <- plot + coord_cartesian(ylim = c(self$options$yAxisRangeMin/yScaleFactor, self$options$yAxisRangeMax/yScaleFactor))
                } else {
                    plot <- plot + coord_cartesian(clip = "off")
                }
            }

            #### Ticks & Axis Expansion ####
            expand_arg <- ggplot2::waiver() # Default ggplot behavior
            if (self$options$showLabels && self$options$labelPosition == "top" && self$options$xAxisRangeType == "auto") {
                expand_arg <- expansion(mult = c(0.05, 0.1)) # same expansion for horizontal and vertical modes.
            }
            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot  + scale_y_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1), labels = labelFnct, expand = expand_arg)
            } else if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot  + scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1), labels = labelFnct, expand = expand_arg)
            } else {
                plot <- plot  + scale_y_continuous(labels = labelFnct, expand = expand_arg)
            }

            # Titles & Labels
            defaults <- list(y = yLab, x = "")
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options, showLegend = FALSE)

            return(plot)

        },
        .multipleResponse = function (data, items = NULL, endorsedOption = 1, order='none') {
            # From userfriendlyscience package
            data <- data[, items, drop = FALSE]
            nrOfEndorsements <- sum(data == endorsedOption, na.rm = TRUE)
            if( length(items) == 1 ) {
                endorsementsPerItem <- nrOfEndorsements
                names(endorsementsPerItem) <- items[1]
                nrOfCases <- sum(!is.na(data))
            } else {
                endorsementsPerItem <- colSums(data == endorsedOption, na.rm = TRUE)
                nrOfCases <- sum(rowSums(!is.na(data)) > 0)
            }
            totals <- as.numeric(c(endorsementsPerItem, nrOfEndorsements))
            res <- data.frame(c(names(endorsementsPerItem), "Total"),
                              totals, (totals/nrOfEndorsements),
                              (totals/nrOfCases))
            names(res) <- c("Option", "Frequency", "Responses", "Cases")
            # Sort
            n <- length(items)
            if (order == 'decreasing') {
                res <- res[c(order(res$Frequency[1:n], decreasing = TRUE),n+1),]
            } else if (order == 'increasing') {
                res <- res[c(order(res$Frequency[1:n], decreasing = FALSE),n+1),]
            }

            return( list('nrOfCases' = nrOfCases, 'df' = res) )
        },
        .oneHotEncoding = function (aCol, separator, na = TRUE, init = FALSE) {
            # List of values
            uniqueValues <- unique(unlist(strsplit(levels(aCol), split = separator, fixed = TRUE)))
            uniqueValues <- trimws(uniqueValues)
            uniqueValues <- unique(uniqueValues)  # le trim peut créer des doublons
            uniqueValues <- uniqueValues[uniqueValues != ""]
            # Return the list of values/columns for table init
            if (init)
                return(uniqueValues)
            # Build the encoded table
            rawValues <- as.character(aCol)
            # split then clean (heading/trailing spaces) the values
            splitted <- lapply(
                strsplit(rawValues, split = separator, fixed = TRUE),
                trimws
            )
            # build the TRUE/FALSE matrix
            encoded <- sapply(uniqueValues, function(opt) {
                vapply(splitted, function(x) opt %in% x, logical(1))
            })
            # sapply returns a vector if there's only one uniqueValues. Convert it back to matrix
            if (length(uniqueValues) == 1)
                encoded <- matrix(encoded, ncol = 1, dimnames = list(NULL, uniqueValues))
            # + OL converts TRUE/FALSE to 1/0
            onehotDF <- as.data.frame(encoded + 0L)
            # NA/empty rawvalues set to NA
            if (na) {
                isMissing <- is.na(aCol) | trimws(rawValues) == ""
                onehotDF[isMissing, ] <- NA
            }
            return(onehotDF)
        },
        .showHelpMessage = function() {
            helpMsg <- .('<p>This module computes frequencies for multiple response questions, also known as "Check All That Apply" (CATA) questions.</p>
<p>Questions may be coded as:</p>
<ul>
<li>several <strong>dummy (or indicator) variables</strong> using 0/1, 1/2, Y/N for each possible answer;</li>
<li>a single <strong>multi-valued variable</strong> containing the checked answers, separated by a symbol (usually "," or ";").</li>
</ul>
<p>A sample file is included at Open > Data Library > vijPlots > Credit Cards</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)
