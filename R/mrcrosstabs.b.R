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
                nOptions <- length(self$options$resps)
                for (i in seq_len(nOptions))
                    table$addRow(rowKey = as.character(i), list(var = self$options$resps[i]))
            } else {
                aCol <- self$data[[self$options$repVar]]
                uniqueValues <- vijOneHotEncoding(aCol, self$options$separator, init = TRUE)
                nOptions <- length(uniqueValues)
                for (i in seq_len(nOptions))
                    table$addRow(rowKey = as.character(i), list(var = uniqueValues[i]))
            }
            if (nOptions > 0) {
                table$addFormat(rowKey = "1", col = 1, jmvcore::Cell.BEGIN_GROUP)
                table$addFormat(rowKey = as.character(nOptions), col = 1, jmvcore::Cell.END_GROUP)
            }
            # Add the "total" row here (to prevent flickering)
            if ( self$options$totalRow ) {
                table$addRow(rowKey='.total', values=list(var=.("Total")))
                table$addFormat(rowKey=".total", col=1, jmvcore::Cell.BEGIN_END_GROUP)
            }
            if ( self$options$showNbOfCases && ( self$options$computedValues == "count" || self$options$computedValues == "options") ) {
                # The number of cases by group, or (% by row) its distribution among the groups,
                # i.e. the % of cases, comparable to the option rows
                nbOfCasesLabel <- if (self$options$computedValues == "options") .("% of cases") else .("Number of cases")
                table$addRow(rowKey='.nbofcases', values=list(var=nbOfCasesLabel))
                table$addFormat(rowKey=".nbofcases", col=1, jmvcore::Cell.BEGIN_END_GROUP)
            }
            # Set custom name for options column
            if (morevar)
                table$getColumn('var')$setTitle(self$options$optionname)
            else
                table$getColumn('var')$setTitle(self$options$repVar)
            # Cell Type
            if ( self$options$computedValues == "count" ) {
                cellType <- "integer"
                cellFormat <- ""
            } else {
                cellType <- "number"
                cellFormat <- "pc"
            }
            # Columns
            if (morevar)
                groupVar <- self$options$group
            else
                groupVar <- self$options$group2
            if ( ! is.null(groupVar) ) {
                gLevels <- levels(self$data[,groupVar])
                # Column names: the level prefixed by ".G." (the level as title), so that a level can never take
                # the name of another column ("var", ".TOTAL"); .run() names crosstab's columns the same way
                for(i in seq_along(gLevels))
                    table$addColumn(name = paste0(".G.", gLevels[i]), title = gLevels[i], type=cellType, format=cellFormat, superTitle=groupVar)
            }
            # Margin column named ".TOTAL", not "Total": a group level may be named "Total"
            table$addColumn(name = ".TOTAL", title=.("Overall"), type=cellType, format=cellFormat, visible=self$options$overall)

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
                } else if (all(is.na(self$data[, self$options$resps, drop = FALSE])) || all(is.na(self$data[[self$options$group]]))) {
                    return()
                } else if (self$options$endorsed == '') {
                    vijErrorMessage(self, .("Counted value string may not be empty."))
                } else {
                    # Encode as 0/1 by comparing each level's text to the endorsed value.
                    encodedData <- as.data.frame(lapply(self$data[, self$options$resps, drop = FALSE], function(col) {
                        as.integer(as.character(col) == self$options$endorsed)
                    }), check.names = FALSE)
                    crosstab <- private$.crossTab(encodedData, self$options$resps, self$data[[self$options$group]],
                                                  1, self$options$order, self$options$computedValues)
                }
            } else { # One Multiple Value Variables
                if (is.null(self$options$repVar) || is.null(self$options$group2) || nrow(self$data) == 0) {
                    return()
                } else if (all(is.na(self$data[[self$options$repVar]])) || all(is.na(self$data[[self$options$group2]]))) {
                    return()
                } else if (self$options$separator == '') {
                    vijErrorMessage(self, .("Separator string may not be empty."))
                } else {
                    rawData <- self$data[[self$options$repVar]]
                    oneHotData <- vijOneHotEncoding(rawData, self$options$separator, self$options$emptyAsNA)
                    if (length(oneHotData) == 0) {
                        vijErrorMessage(self, .("No distinct values found in the selected variable."))
                    }
                    answerList <- names(oneHotData)
                    crosstab <- private$.crossTab(oneHotData, answerList, self$data[[self$options$group2]],
                                                          1, self$options$order, self$options$computedValues)
                }
            }

            #### Fill the table ####
            table <- self$results$crosstab
            # The columns by their names in the table (.init()): each group column prefixed by ".G.", then the margin
            values <- crosstab
            names(values) <- c(paste0(".G.", names(crosstab)[-ncol(crosstab)]), ".TOTAL")
            n <- nrow(crosstab)
            for(i in seq_len(n-2)) {
                table$setRow(rowKey = as.character(i),
                             values = c(var = rownames(crosstab)[i], values[i,]))
            }
            if ( self$options$totalRow ) {
                table$setRow(rowKey = '.total', values = as.list(values[n-1,]))   # label set in .init()
            }
            if ( self$options$showNbOfCases && (self$options$computedValues == "count" || self$options$computedValues == "options") ) {
                table$setRow(rowKey = '.nbofcases', values = as.list(values[n,]))   # label set in .init()
            }
            image <- self$results$plot
            # Plot data: the options x groups block, without the margins (the Total column, the last two rows
            # Total and Nb of Cases): every level of the group, drop = FALSE for a single option or group
            image$setState(crosstab[seq_len(nrow(crosstab) - 2), seq_len(ncol(crosstab) - 1), drop = FALSE])
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Long by design (reviewed 2026-10-09): a linear `plot <- plot + ...` build; splitting
            # it would scatter the plot's construction without simplifying it.
            if (is.null(image$state))
                return(FALSE)

            if (self$options$mode == "morevar") {
                groupVar <- self$options$group
                optionName <- self$options$optionname
            } else {
                groupVar <- self$options$group2
                optionName <- self$options$repVar
            }
            # Labels at the bars' end: "top" is ignored on stacked bars (centred in each segment), even when
            # left selected (the option is only disabled in the UI)
            labelTop <- (self$options$labelPosition == "top" && self$options$bartype != "stack")
            # In horizontal mode, add 2 spaces before label "at top"
            labelPrefix <- if (self$options$horizontal && labelTop) "  " else ""
            # Label formats
            percentFormat <- scales::label_percent(accuracy = as.numeric(self$options$accuracy), suffix = ' %',
                                                   decimal.mark = self$options[['decSymbol']], prefix = labelPrefix)
            countFormat <- vijFormatCount(self$options[['decSymbol']], prefix = labelPrefix)
            # Labels of the bars: counts, or percentages (computedValues). No label for an empty bar (an
            # option nobody chose in a group): NA, not drawn (na.rm in geom_text), the bar keeping its place
            if (self$options$computedValues == "count")
                valueLabel <- function(x) ifelse(x == 0, NA_character_, countFormat(x))
            else
                valueLabel <- function(x) ifelse(x == 0, NA_character_, percentFormat(x))

            # Border color
            if (self$options$borderColor == "none")
                borderColor <- NA
            else
                borderColor <- self$options$borderColor

            # Reverse stack option
            bartype <- self$options$bartype
            reverseStack <- (self$options$reverseStack && bartype == "stack")
            if (reverseStack)
                bartype <- ggplot2::position_stack(reverse = TRUE)

            # Data, in long format: one row per option x group, with internal column names (.OPTION, .GROUP,
            # .COUNT) whatever the names of the group variable and of its levels (e.g. a group variable "Count")
            state <- image$state
            plotData <- data.frame(.OPTION = factor(rep(rownames(state), each = ncol(state)), levels = rownames(state)),
                                   .GROUP = factor(rep(names(state), times = nrow(state)), levels = names(state)),
                                   .COUNT = as.vector(t(as.matrix(state))))
            # Plot
            if (self$options$xaxis == "xcols") {
                xVarName <- rlang::sym(".GROUP")
                zVarName <- rlang::sym(".OPTION")
                xLab <- groupVar
                gLab <- optionName
            } else {
                xVarName <- rlang::sym(".OPTION")
                zVarName <- rlang::sym(".GROUP")
                xLab <- optionName
                gLab <- groupVar
            }
            plot <- ggplot2::ggplot(plotData, ggplot2::aes(x=!!xVarName, y = .data[[".COUNT"]], label = valueLabel(.data[[".COUNT"]])))
            plot <- plot + ggplot2::geom_col(ggplot2::aes(fill=!!zVarName), position = bartype, color = borderColor)

            #### Labels ####
            if( self$options$showLabels ) {
                vjust2 <- 0.5
                hjust2 <- 0.5
                vfactor <- 1

                if (self$options$bartype == "stack") {
                    if(reverseStack)
                        labelPosition <- ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
                    else
                        labelPosition <- ggplot2::position_stack(vjust = 0.5)
                } else {
                    if (self$options$labelPosition == "middle") {
                        labelPosition <- ggplot2::position_dodge(width = 0.9)
                        vfactor <- 2
                    } else {
                        labelPosition <- ggplot2::position_dodge(width = 0.9)
                        if (self$options$horizontal) {
                            hjust2 <- 0
                        } else {
                            vjust2 <- -0.6
                        }
                    }
                }

                if (labelTop)
                    textColor <- "black"
                else
                    textColor <- self$options$textColor


                if (self$options$textColor == "auto" && !labelTop) { # using hex_bw
                    plot <- plot + ggplot2::geom_text(ggplot2::aes(fill = !!zVarName, y = .data[[".COUNT"]] / vfactor, color = ggplot2::after_scale(ggstats::hex_bw(.data$fill))),
                                                        position = labelPosition, vjust = vjust2, hjust = hjust2,
                                                        fontface = "bold", size = self$options$labelFontSize / ggplot2::.pt, na.rm = TRUE)
                } else {
                    plot <- plot + ggplot2::geom_text(ggplot2::aes(fill = !!zVarName, y = .data[[".COUNT"]] / vfactor), color = textColor,
                                                        position = labelPosition, vjust = vjust2, hjust = hjust2,
                                                        fontface = "bold", size = self$options$labelFontSize / ggplot2::.pt, na.rm = TRUE)
                }
            }

            # Theme and colors
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "fill", theme)

            # Grid lines (value axis only)
            plot <- plot + vijGridLines(self$options$theme,
                                        x = self$options$horizontal && self$options$xGridLines,
                                        y = !self$options$horizontal && self$options$yGridLines)

            # Y scale and lab
            if (self$options$computedValues == "responses") {
                labelFnct <- scales::label_percent(suffix = ' %', decimal.mark = self$options[['decSymbol']])
                yLab <- .("% of Responses")
                yScaleFactor <- 100 # yScaleFactor is used for manual range computation (1 = count, 100 = percent)
            } else if (self$options$computedValues == "cases") {
                labelFnct <- scales::label_percent(suffix = ' %', decimal.mark = self$options[['decSymbol']])
                yLab <- .("% of Cases")
                yScaleFactor <- 100
            } else if (self$options$computedValues == "options") {
                labelFnct <- scales::label_percent(suffix = ' %', decimal.mark = self$options[['decSymbol']])
                yLab <- jmvcore::format(.("% within {category}"), category = optionName)
                yScaleFactor <- 100
            } else {
                labelFnct <- ggplot2::waiver()
                yLab <- .("Count")
                yScaleFactor <- 1
            }

            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + ggplot2::coord_flip(ylim = c(self$options$xAxisRangeMin/yScaleFactor, self$options$xAxisRangeMax/yScaleFactor))
                } else {
                    plot <- plot + ggplot2::coord_flip(clip = "off")
                }
            } else {
                if (self$options$yAxisRangeType == "manual") {
                    plot <- plot + ggplot2::coord_cartesian(ylim = c(self$options$yAxisRangeMin/yScaleFactor, self$options$yAxisRangeMax/yScaleFactor))
                } else {
                    plot <- plot + ggplot2::coord_cartesian(clip = "off")
                }
            }

            #### Ticks & Axis Expansion ####
            expand_arg <- ggplot2::waiver() # Default ggplot behavior
            # Room for the labels at the bars' end, unless the range of the value axis is set by hand
            rangeType <- if (self$options$horizontal) self$options$xAxisRangeType else self$options$yAxisRangeType
            if (self$options$showLabels && labelTop && rangeType == "auto") {
                expand_arg <- ggplot2::expansion(mult = c(0.05, 0.1)) # same expansion for horizontal and vertical modes.
            }
            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1), labels = labelFnct, expand = expand_arg)
            } else if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1), labels = labelFnct, expand = expand_arg)
            } else {
                plot <- plot  + ggplot2::scale_y_continuous(labels = labelFnct, expand = expand_arg)
            }

            # Titles & Labels
            defaults <- list(y = yLab, x = xLab, legend = gLab)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options)
            plot <- plot + ggplot2::theme(legend.key.spacing.y = grid::unit(1, "mm"), legend.byrow = TRUE)

            vijDebugPlot(self, plot)

            return(plot)
        },

        # group: the values of the grouping variable (one per row of data), not its name. Added as a column
        # of data, it would overwrite a response of the same name (e.g. a group variable "Age" and a response "Age")
        .crossTab = function (data, items = NULL, group = NULL, endorsedOption = 1, order='none', values='count') {
            options <- data[, items, drop = FALSE]
            groups <- list(group)
            # CrossTab
            # Every level of the group, in its order, as the table's columns (.init()): drop = FALSE keeps a level
            # with no case at all (unused level retained in jamovi), whose sum is NA: counted 0
            crossTab <- aggregate(options == endorsedOption, groups, sum, na.rm = TRUE, drop = FALSE)
            crossTab[is.na(crossTab)] <- 0
            # Save group names
            groupNames <- crossTab[,1]
            # Transpose the content of the table
            crossTab <- as.data.frame(t(crossTab[, -1, drop = FALSE]))
            # Rename the columns of the table
            names(crossTab) <- groupNames
            # Compute the number of cases by group
            # (a case: a row with at least one non-missing response)
            NbOfCases <- aggregate( rowSums(!is.na(options)) > 0, groups, sum, drop = FALSE )
            NbOfCases <- NbOfCases[,2]
            NbOfCases[is.na(NbOfCases)] <- 0   # a level with no case
            NbOfCases <- c(NbOfCases, sum(NbOfCases))
            # Add the margin column to crosstab
            crossTab <- cbind(crossTab, ".TOTAL" = rowSums(crossTab))
            # Sorting crosstab
            if (order == 'decreasing') {
                crossTab <- crossTab[order(crossTab$.TOTAL, decreasing = TRUE),]
            } else if (order == 'increasing') {
                crossTab <- crossTab[order(crossTab$.TOTAL, decreasing = FALSE),]
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
                crossTab <- sweep( crossTab , 1, crossTab$.TOTAL, "/")
            } else {
                crossTab <- rbind(crossTab, "Nb of Cases"=NbOfCases)
            }
            # Percentages with a zero denominator (a group with no response or no case, an option nobody chose):
            # missing (an empty cell) rather than NaN. The counts are never missing, so is.na() only finds those NaN
            crossTab[is.na(crossTab)] <- NA
            # Return result
            return(crossTab)
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
