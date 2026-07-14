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
                    table$addRow(rowKey = i)
            } else {
                aCol <- self$data[[self$options$repVar]]
                uniqueValues <- unique(unlist(strsplit(levels(aCol), split = self$options$separator, fixed = TRUE)))
                uniqueValues <- uniqueValues[uniqueValues != ""]
                for (i in seq_along(uniqueValues))
                    table$addRow(rowKey = i)
            }
            # Add the "total" row
            if (self$options$showTotal) {
                table$addRow(rowKey='.total', values=list(var="Total"))
                table$addFormat(rowKey=".total", col=1, jmvcore::Cell.BEGIN_GROUP)
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
            for(i in 1:(nrow(myresult$df)-1))
                table$setRow(rowNo=i,
                             values=list(var=myresult$df[i,1],
                                         freq=myresult$df[i,2],
                                         responsepercent=myresult$df[i,3],
                                         casepercent=myresult$df[i,4]))

            if ( self$options$showTotal ) {
                i <- nrow(myresult$df)
                table$setRow(rowKey=".total",
                             values=list(var="Total",
                                         freq=myresult$df[i,2],
                                         responsepercent=myresult$df[i,3],
                                         casepercent=myresult$df[i,4]))
            }

            table$setNote('noc', paste(.("Number of cases:"), myresult$nrOfCases) , init=FALSE)

            image <- self$results$plot
            image$setState(myresult$df[1:(nrow(myresult$df)-1),])

        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            plotData <- image$state

            # to be sure the factor ordering is kept
            plotData$Option <- factor(plotData$Option, levels=plotData$Option)

            # Percent format (scales)
            doPercent <- scales::label_percent(
                accuracy = as.numeric(self$options$accuracy),
                suffix = '\u2009%',
                decimal.mark = self$options[['decSymbol']])

            # Border color
            if (self$options$borderColor == "none")
                borderColor = NA
            else
                borderColor = self$options$borderColor

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
                plot <- ggplot(plotData, aes(Option, Frequency, label = Frequency))
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

            # Labels
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

            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + coord_flip(ylim = c(self$options$xAxisRangeMin/yScaleFactor, self$options$xAxisRangeMax/yScaleFactor))
                } else {
                    if (self$options$showLabels && self$options$labelPosition == "top")
                        plot <- plot + coord_flip(clip = "off", ylim = layer_scales(plot)$y$get_limits()*1.1) # Gives more room for labels !
                    else
                        plot <- plot + coord_flip(clip = "off")
                }
            } else {
                if (self$options$yAxisRangeType == "manual") { # Horizontal and manual
                    plot <- plot + coord_cartesian(ylim = c(self$options$yAxisRangeMin/yScaleFactor, self$options$yAxisRangeMax/yScaleFactor))
                } else {
                    plot <- plot + coord_cartesian(clip = "off")
                }
            }

            # Ticks
            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot  + scale_y_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1), labels = labelFnct)
            } else if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot  + scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1), labels = labelFnct)
            } else {
                plot <- plot  + scale_y_continuous(labels = labelFnct)
            }

            # Titles & Labels
            defaults <- list(y = yLab, x = "")
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options, showLegend = FALSE)

            #self$results$text$setContent(plot) # Show debug messages !

            return(plot)

        },
        .multipleResponse = function (data, items = NULL, endorsedOption = 1, order='none') {
            # From userfriendlyscience package
            data = data[, items]
            nrOfEndorsements = sum(data == endorsedOption, na.rm = TRUE)
            if( length(items) == 1 ) {
                endorsementsPerItem <- nrOfEndorsements
                names(endorsementsPerItem) <- items[1]
                nrOfCases <- sum(!is.na(data))
            } else {
                endorsementsPerItem = colSums(data == endorsedOption, na.rm = TRUE)
                nrOfCases = sum(!apply(apply(data, 1, is.na), 2, all))
            }
            totals = as.numeric(c(endorsementsPerItem, nrOfEndorsements))
            res <- data.frame(c(names(endorsementsPerItem), "Total"),
                              totals, (totals/nrOfEndorsements),
                              (totals/nrOfCases))
            names(res) <- c("Option", "Frequency", "Responses", "Cases")
            # Sort
            n <- length(items)
            if (order == 'decreasing') {
                res<-res[c(order(res$Frequency[1:n], decreasing = TRUE),n+1),]
            } else if (order == 'increasing') {
                res<-res[c(order(res$Frequency[1:n], decreasing = FALSE),n+1),]
            }

            return( list('nrOfCases'=nrOfCases, 'df'=res) )

        },
        .oneHotEncoding = function (aCol, separator, na = TRUE) {
            uniqueValues <- unique(unlist(strsplit(levels(aCol), split = separator, fixed = TRUE)))
            uniqueValues <- uniqueValues[uniqueValues != ""]
            onehotDF <- data.frame("X__priVate__X" = 1:length(aCol))
            for(j in uniqueValues) {
                onehotDF[, j] <- ifelse(grepl(j, aCol, fixed = TRUE),1,0)
            }
            if (na)
                onehotDF[is.na(aCol),] <- NA
            onehotDF[,"X__priVate__X"] <- NULL
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
