mrcrosstabsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mrcrosstabsClass",
    inherit = mrcrosstabsBase,
    private = list(
        .init = function() {
            morevar <- (self$options$mode == "morevar")
            # Show help message (and hide results)
            if ((!morevar && (is.null(self$options$repVar) || self$options$separator == '')) || (morevar && length(self$options$resps) < 1)) {
                private$.showHelpMessage()
                return()
            }
            table <- self$results$crosstab
            # Table title
            if ( self$options$computedValues == "options" ) {
                table$setTitle(.("Crosstab (% by row)"))
            } else if (self$options$computedValues == "cases") {
                table$setTitle(.("Crosstab (% of cases)"))
            } else if (self$options$computedValues == "responses") {
                table$setTitle(.("Crosstab (% of responses)"))
            }
            # Table rows
            if (morevar) {
                for (i in seq_along(self$options$resps))
                    table$addRow(rowKey = as.character(i), list(var = self$options$resps[i]))
            } else {
                aCol <- self$data[[self$options$repVar]]
                uniqueValues <- private$.oneHotEncoding(aCol, self$options$separator, init = TRUE)
                for (i in seq_along(uniqueValues))
                    table$addRow(rowKey = as.character(i), list(var = uniqueValues[i]))
            }
            # Add the "total" row here (to prevent flickering)
            if ( self$options$totalRow ) {
                table$addRow(rowKey='.total', values=list(var="Total"))
                table$addFormat(rowKey=".total", col=1, jmvcore::Cell.BEGIN_GROUP)
            }
            if ( self$options$showNbOfCases && ( self$options$computedValues == "count" || self$options$computedValues == "options") ) {
                table$addRow(rowKey='.nbofcases', values=list(var=.("Number of cases")))
                table$addFormat(rowKey=".nbofcases", col=1, jmvcore::Cell.BEGIN_GROUP)
            }
            # Set custom name for options column
            if (morevar)
                table$getColumn('var')$setTitle(self$options$optionname)
            else
                table$getColumn('var')$setTitle(self$options$repVar)
            # Cell Type
            if ( self$options$computedValues == "count" ) {
                cellType = "integer"
                cellFormat = ""
            } else {
                cellType = "number"
                cellFormat = "pc"
            }
            # Columns
            if (morevar)
                groupVar <- self$options$group
            else
                groupVar <- self$options$group2
            if ( ! is.null(groupVar) ) {
                gLevels <- levels(self$data[,groupVar])
                for(i in seq_along(gLevels))
                    table$addColumn(name = gLevels[i], type=cellType, format=cellFormat, superTitle=groupVar)
            }
            table$addColumn(name = "Total", title=.("Overall"), type=cellType, format=cellFormat, visible=self$options$overall)

            # Set the image dimensions
            width <- 425
            height <- 350
            fixed_width <- 75
            fixed_height <- 50
            if (self$options$legendPosition %in% c('top','bottom'))
                fixed_height <- fixed_height + 50
            else
                fixed_width <- fixed_width + 100
            image <- self$results$plot
            if (is.null(image[['setSize2']])) { # jamovi < 2.7.16
                image$setSize(width + fixed_width, height + fixed_height)
            } else {
                image$setSize2(width, height, fixed_width, fixed_height)
            }
        },

        .run = function() {
            if (self$options$mode == "morevar") { # Several dichotomous variables
                if (length(self$options$resps) < 1 || is.null(self$options$group) || nrow(self$data) == 0) {
                    return()
                } else {
                    nGroups <- nlevels(self$data[,self$options$group])
                    nAnswers <- length(self$options$resps)
                    crosstab <- private$.crossTab(self$data, self$options$resps, self$options$group,
                                                  self$options$endorsed, self$options$order, self$options$computedValues)
                }
            } else { # One Multiple Value Variables
                if (is.null(self$options$repVar) || self$options$separator == '' || is.null(self$options$group2) || nrow(self$data) == 0) {
                    return()
                } else {
                    nGroups <- nlevels(self$data[,self$options$group2])
                    rawData <- self$data[[self$options$repVar]]
                    oneHotData <- private$.oneHotEncoding(rawData, self$options$separator, self$options$emptyAsNA)
                    answerList <- names(oneHotData)
                    nAnswers <- length(answerList)
                    oneHotData[, self$options$group2] <- self$data[,self$options$group2]
                    crosstab <- private$.crossTab(oneHotData, answerList, self$options$group2,
                                                          1, self$options$order, self$options$computedValues)
                }
            }

            #### Fill the table ####
            table <- self$results$crosstab
            n <- nrow(crosstab)
            for(i in seq_len(n-2)) {
                table$setRow(rowKey = as.character(i),
                             values = c(var = rownames(crosstab)[i], crosstab[i,]))
            }
            if ( self$options$totalRow ) {
                table$setRow(rowKey = '.total', values = c(var="Total", crosstab[n-1,]))
            }
            if ( self$options$showNbOfCases && (self$options$computedValues == "count" || self$options$computedValues == "options") ) {
                table$setRow(rowKey = '.nbofcases', values = append(list(var=.("Number of cases")), crosstab[n,]))
            }
            image <- self$results$plot
            image$setState(crosstab[1:nAnswers,1:nGroups])
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            if (self$options$mode == "morevar") {
                groupVar <- self$options$group
                optionName <- self$options$optionname
            } else {
                groupVar <- self$options$group2
                optionName <- self$options$repVar
            }

            # Percent format (scales)
            doPercent <- scales::label_percent(
                accuracy = as.numeric(self$options$accuracy),
                suffix = '\u2009%',
                decimal.mark = self$options[['decSymbol']])

            # Percent format (scales)
            if (self$options$computedValues == "count")
                doNumber <- function (x){x}
            else
                doNumber <- doPercent

            # Border color
            if (self$options$borderColor == "none")
                borderColor <- NA
            else
                borderColor <- self$options$borderColor

            # Reverse stack option
            bartype <- self$options$bartype
            reverseStack <- (!self$options$reverseStack && bartype == "stack")
            if (reverseStack)
                bartype <- position_stack(reverse = TRUE)

            # Data
            plotData <- cbind("Options" = factor(rownames(image$state), levels=rownames(image$state)),image$state)
            plotData <- tidyr::pivot_longer(plotData, cols=colnames(image$state), names_to = groupVar, values_to = "Count")
            plotData[[groupVar]] <- factor(plotData[[groupVar]], levels = names(image$state) )
            # Plot
            optionsVar <- "Options"
            if (self$options$xaxis == "xcols") {
                xVarName <- ensym(groupVar)
                zVarName <- ensym(optionsVar)
                xLab <- groupVar
                gLab <- optionName
            } else {
                xVarName <- ensym(optionsVar)
                zVarName <- ensym(groupVar)
                xLab <- optionName
                gLab <- groupVar
            }
            plot <- ggplot(plotData, aes(x=!!xVarName, y = Count, label = doNumber(Count)))
            plot <- plot + geom_col( aes(fill=!!zVarName), position = bartype, color = borderColor)

            #### Labels ####
            if( self$options$showLabels ) {
                vjust2 <- 0.5
                hjust2 <- 0.5
                vfactor <- 1

                if (self$options$bartype == "stack") {
                    if(reverseStack)
                        labelPosition <- position_stack(vjust = 0.5, reverse = TRUE)
                    else
                        labelPosition <- position_stack(vjust = 0.5)
                } else {
                    if (self$options$labelPosition == "middle") {
                        labelPosition <- position_dodge(width = 0.9)
                        vfactor <- 2
                    } else {
                        labelPosition <- position_dodge(width = 0.9)
                        if (self$options$horizontal) {
                            hjust2 <- -0.2
                        } else {
                            vjust2 <- -0.6
                        }
                    }
                }

                if (self$options$labelPosition == "top")
                    textColor <- "black"
                else
                    textColor <- self$options$textColor


                if (self$options$textColor == "auto" && self$options$labelPosition == "middle") { # using hex_bw
                    plot <- plot + geom_text(aes(fill = !!zVarName, y = Count / vfactor, color = after_scale(ggstats::hex_bw(.data$fill))),
                                             position = labelPosition, vjust = vjust2, hjust = hjust2,
                                             fontface = "bold", size = self$options$labelFontSize / .pt)
                } else {
                    plot <- plot + geom_text(aes(fill = !!zVarName, y = Count / vfactor), color = textColor,
                                             position = labelPosition, vjust = vjust2, hjust = hjust2,
                                             fontface = "bold", size = self$options$labelFontSize / .pt)
                }
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill")

            # Y scale and lab
            if (self$options$computedValues == "responses") {
                labelFnct <- scales::label_percent(suffix = '\u2009%', decimal.mark = self$options[['decSymbol']])
                yLab <- .("% of Responses")
                yScaleFactor <- 100 # yScaleFactor is used for manual range computation (1 = count, 100 = percent)
            } else if (self$options$computedValues == "cases") {
                labelFnct <- scales::label_percent(suffix = '\u2009%', decimal.mark = self$options[['decSymbol']])
                yLab <- .("% of Cases")
                yScaleFactor <- 100
            } else if (self$options$computedValues == "options") {
                labelFnct <- scales::label_percent(suffix = '\u2009%', decimal.mark = self$options[['decSymbol']])
                yLab <- paste(.("% within"), optionName)
                yScaleFactor <- 100
            } else {
                labelFnct <- waiver()
                yLab <- .("Count")
                yScaleFactor <- 1
            }

            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + coord_flip(ylim = c(self$options$xAxisRangeMin/yScaleFactor, self$options$xAxisRangeMax/yScaleFactor))
                } else {
                    plot <- plot + coord_flip(clip = "off")
                }
            } else {
                if (self$options$yAxisRangeType == "manual") {
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
            defaults <- list(y = yLab, x = xLab, legend = gLab)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            return(plot)
        },

        .crossTab = function (data, items = NULL, group = NULL, endorsedOption = 1, order='none', values='count') {
            options = data[, items]
            groups = list(data[,group])
            # CrossTab
            crossTab <- aggregate(options == endorsedOption, groups, sum, na.rm = TRUE)
            # Save group names
            groupNames <- crossTab[,1]
            # Transpose the content of the table
            crossTab <- as.data.frame(t(crossTab[,-1]))
            # Rename the columns of the table
            names(crossTab) <- groupNames
            # Compute the number of cases by group
            if (length(items) > 1) {
                NbOfCases <- aggregate( rowSums(!is.na(options)) > 0, groups, sum )
            } else {
                NbOfCases <- aggregate( !is.na(options), groups, sum )
            }
            NbOfCases <- c( NbOfCases[,2], sum(NbOfCases[,2]))
            # Add the margin column to crosstab
            crossTab <- cbind(crossTab, "Total" = rowSums(crossTab))
            # Sorting crosstab
            if (order == 'decreasing') {
                crossTab <- crossTab[order(crossTab$Total, decreasing = TRUE),]
            } else if (order == 'increasing') {
                crossTab <- crossTab[order(crossTab$Total, decreasing = FALSE),]
            }
            # add margin row to  crosstab
            NbOfResps <- colSums(crossTab)
            crossTab <- rbind(crossTab, "Total" = NbOfResps)
            #
            if (values == 'cases') {
                crossTab <- sweep( crossTab , 2, NbOfCases, "/")
                crossTab <- rbind(crossTab, "Nb of Cases"=NbOfCases)
            } else if (values == 'responses' ) {
                crossTab <- sweep( crossTab , 2, NbOfResps, "/")
                crossTab <- rbind(crossTab, "Nb of Cases"=NbOfCases)
            } else if (values == 'options' ) {
                crossTab <- rbind(crossTab, "Nb of Cases"=NbOfCases)
                crossTab <- sweep( crossTab , 1, crossTab$Total, "/")
            } else {
                crossTab <- rbind(crossTab, "Nb of Cases"=NbOfCases)
            }
            # Return result
            return(crossTab)
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
            helpMsg <- .('<p>This module computes two-way frequencies for multiple response questions, also known as "Check All That Apply" (CATA) questions.</p>
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
