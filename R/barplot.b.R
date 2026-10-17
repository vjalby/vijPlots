barplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "barplotClass",
    inherit = barplotBase,
    #### Active bindings ---- from jmv/conttables.b.R
    active = list(
        countsName = function() {
            if ( ! is.null(self$options$counts)) {
                return(self$options$counts)
            } else if ( ! is.null(attr(self$data, "jmv-weights-name"))) {
                return (attr(self$data, "jmv-weights-name"))
            }
            NULL
        }
    ),
    private = list(
        .init = function() {
            # Weight message
            countsName <- self$countsName
            if (!is.null(countsName)) {
                warningMessage <- ..('The data is weighted by the variable {}.', countsName)
                vijWarningMessage(self, warningMessage, '.weights')
            }
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
                width <- max(450, 200*nbOfColumn)
                height <- max(350, 300*nbOfRow)
            } else {
                width <- 450
                height <- 350
            }
            # Fixed dimension
            fixed_width <- 50 # Y-Axis legend
            fixed_height <- 50 # X-Axis legend
            if( !is.null(self$options$columns)) {
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
            # Frequency table columns: percentages are computed within each "outer" block
            table <- self$results$frequencies
            blockNames <- private$.tableBlocks()
            if (!is.null(self$options$facet))
                table$getColumn('facet')$setTitle(self$options$facet)
            if (!is.null(blockNames$outer))
                table$getColumn('outer')$setTitle(blockNames$outer)
            if (!is.null(blockNames$inner))
                table$getColumn('inner')$setTitle(blockNames$inner)
            # Cumulative percentages only make sense for a single variable
            table$getColumn('cumPercent')$setVisible(is.null(self$options$columns))
        },
        .tableBlocks = function() {
            # Rows are grouped by the variable the percentages are computed within (as in the plot).
            # In count mode, percentWithin is disabled in the UI, so it has no effect: within groups.
            if (is.null(self$options$columns))
                list(outer = NULL, inner = self$options$rows)
            else if (self$options$yaxis == "count" || self$options$percentWithin == "group")
                list(outer = self$options$columns, inner = self$options$rows)
            else
                list(outer = self$options$rows, inner = self$options$columns)
        },
        .run = function() {
            if (is.null(self$options$rows))
                return()

            varNames <- c(self$options$rows, self$options$columns, self$options$facet)

            # ".COUNTS" is the column name used for the weights
            reservedVar <- intersect(varNames, ".COUNTS")
            if (length(reservedVar) > 0)
                vijErrorMessage(self, jmvcore::format(.("\"{var}\" is a reserved name. Please rename this variable."), var = reservedVar[1]))

            plotData <- jmvcore::select(self$data, varNames)

            # Weight data
            countsName <- self$options$counts
            if (!is.null(countsName)) {
                # vijPlots weights
                plotData[['.COUNTS']] <- jmvcore::toNumeric(self$data[[countsName]])
            } else if (!is.null(attr(self$data, "jmv-weights"))) {
                # jamovi built-in weights
                plotData[['.COUNTS']] <- jmvcore::toNumeric(attr(self$data, "jmv-weights"))
            } else {
                # no weights
                plotData[['.COUNTS']] <- as.integer(rep(1, nrow(plotData)))
            }

            # Excluded observations (weighted; rows without a weight can't be counted), for the table note
            nMissing <- 0
            if (self$options$ignoreNA) {
                isIncomplete <- !stats::complete.cases(plotData) & !is.na(plotData$.COUNTS)
                nMissing <- sum(plotData$.COUNTS[isIncomplete])
                plotData <- jmvcore::naOmit(plotData)
            }
            if (nrow(plotData) == 0)
                return()

            # Validate .COUNTS (NA / non negative / not infinite / not all zero)
            if (any(is.na(plotData$.COUNTS)))  {
                vijErrorMessage(self, .('Counts may not contain missing values.'))
            }
            if (any(plotData$.COUNTS < 0)) {
                vijErrorMessage(self, .('Counts may not be negative.'))
            }
            if (any(is.infinite(plotData$.COUNTS))) {
                vijErrorMessage(self, .('Counts may not be infinite.'))
            }
            if (sum(plotData$.COUNTS) == 0) {
                vijErrorMessage(self, .('Counts may not all be zero.'))
            }

            # Sum the counts per combination of variables: the state is saved into the .omv,
            # so store one row per bar/slice rather than one row per case
            plotData <- dplyr::count(plotData, dplyr::pick(dplyr::all_of(varNames)), wt = .data[[".COUNTS"]], name = ".COUNTS")

            # Sort the categories by overall count
            categoryName <- self$options$rows
            if (self$options$order == "decreasing")
                plotData[[categoryName]] <- forcats::fct_infreq(plotData[[categoryName]], w = plotData$.COUNTS)
            else if (self$options$order == "increasing")
                plotData[[categoryName]] <- forcats::fct_rev(forcats::fct_infreq(plotData[[categoryName]], w = plotData$.COUNTS))

            if (self$options$frequencyTable)
                private$.fillFrequencyTable(plotData, nMissing)

            image <- self$results$plot
            image$setState(plotData)
        },
        .fillFrequencyTable = function(plotData, nMissing) {
            table <- self$results$frequencies
            facetName <- self$options$facet
            blockNames <- private$.tableBlocks()
            # Values of a variable in the plot's order: all levels, then NA last when missing
            # values are kept (ignoreNA unchecked). %in% rather than == to match NA.
            valuesOf <- function(name) {
                values <- levels(plotData[[name]])
                if (anyNA(plotData[[name]]))
                    values <- c(values, NA)
                values
            }
            labelOf <- function(value) if (is.na(value)) "NA" else value
            # No facet/group: a single block containing all the rows
            facetValues <- if (is.null(facetName)) list(NULL) else as.list(valuesOf(facetName))
            outerValues <- if (is.null(blockNames$outer)) list(NULL) else as.list(valuesOf(blockNames$outer))
            innerValues <- valuesOf(blockNames$inner)
            rowNo <- 0
            for (facetValue in facetValues) {
                inFacet <- if (is.null(facetValue)) TRUE else plotData[[facetName]] %in% facetValue
                # Only facets with data (as facet_wrap)
                if (!any(inFacet))
                    next
                for (outerValue in outerValues) {
                    inBlock <- inFacet & (if (is.null(outerValue)) TRUE else plotData[[blockNames$outer]] %in% outerValue)
                    counts <- vapply(innerValues, function(value) sum(plotData$.COUNTS[inBlock & plotData[[blockNames$inner]] %in% value]), numeric(1))
                    total <- sum(counts)
                    # A group/category without data in this facet has no bars: skip it
                    if (!is.null(outerValue) && total == 0)
                        next
                    blockValues <- list(
                        facet = if (is.null(facetValue)) "" else labelOf(facetValue),
                        outer = if (is.null(outerValue)) "" else labelOf(outerValue))
                    cumCounts <- cumsum(counts)
                    for (i in seq_along(innerValues)) {
                        rowNo <- rowNo + 1
                        table$addRow(rowKey = rowNo, values = c(blockValues, list(
                            inner = labelOf(innerValues[[i]]), counts = counts[[i]],
                            percent = counts[[i]] / total, cumPercent = cumCounts[[i]] / total)))
                        # Space above each new block (jamovi only renders BEGIN_GROUP, as a top padding)
                        if (i == 1 && rowNo > 1)
                            for (col in c('facet', 'outer', 'inner', 'counts', 'percent', 'cumPercent'))
                                table$addFormat(rowKey = rowNo, col = col, jmvcore::Cell.BEGIN_GROUP)
                    }
                    rowNo <- rowNo + 1
                    table$addRow(rowKey = rowNo, values = c(blockValues, list(
                        inner = .("Total"), counts = total, percent = 1, cumPercent = "")))
                    for (col in c('inner', 'counts', 'percent', 'cumPercent'))
                        table$addFormat(rowKey = rowNo, col = col, jmvcore::Cell.BEGIN_GROUP)
                }
            }
            # Observations left out because of missing values (ignoreNA checked), overall
            if (nMissing > 0)
                table$setNote('missing', jmvcore::format(.('Missing values: {n}'), n = format(nMissing)))
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Long by design (reviewed 2026-10-09): a linear `plot <- plot + ...` build; splitting
            # it would scatter the plot's construction without simplifying it.
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            #### Variables ####

            categoryVar <- rlang::sym(self$options$rows)
            groupVar <- vijVar(self$options$columns)

            if (self$options$borderColor == "none")
                borderColor <- NA
            else
                borderColor <- self$options$borderColor

            positionStack <- (self$options$barType == "stack")
            if (self$options$barType == "dodge2")
                position <- ggplot2::position_dodge2(preserve = "single", width = 0.9)
            else if(self$options$barType == "dodge")
                position <- ggplot2::position_dodge(width = 0.9)
            else
                position <- self$options$barType

            yaxis <- self$options$yaxis

            # ggstats::StatProp drops the rows whose fill/by is NA (stats::aggregate), unlike stat = "count":
            # make NA an explicit level (still drawn with the scale's na.value) to keep the NA bars.
            # Done after ordering, so that NA stays last, and only if NA is present (an empty NA level
            # would otherwise be shown, as scales use drop = FALSE).
            if (yaxis == "percent") {
                for (varName in c(self$options$rows, self$options$columns)) {
                    if (anyNA(plotData[[varName]]))
                        plotData[[varName]] <- forcats::fct_na_value_to_level(plotData[[varName]])
                }
            }

            reverseStack <- (self$options$reverseStack && positionStack)

            if (reverseStack)
                position <- ggplot2::position_stack(reverse = TRUE)

            # Percent format (scales)
            doPercent <- scales::label_percent(
                            accuracy = as.numeric(self$options$accuracy),
                            suffix = ' %',
                            decimal.mark = self$options[['decSymbol']])
            if (self$options$horizontal)
                doNumber <- function(x) formatC(x, width = 2, format = "d")
            else
                doNumber <- as.character

            # Correct Single color option
            if (positionStack || !is.null(groupVar))
                singleColor <- FALSE
            else
                singleColor <- self$options$singleColor

            if (singleColor)
                oneColorOfPalette <- vijOneColorOfPalette(self$options$colorPalette, "fill", theme, self$options$colorNo)

            # Correct labelPosition option
            if (positionStack)
                labelPosition <- "middle"
            else
                labelPosition <- self$options$labelPosition

            # Correct textColor option
            if (labelPosition == "top")
                textColor <- "black"
            else if (self$options$textColor == "auto" && singleColor)
                textColor <- ggstats::hex_bw(oneColorOfPalette)
            else
                textColor <- self$options$textColor

            #### AES ####

            if (is.null(groupVar)) { # No group
                byVar <- 1
                fillVar <- categoryVar
                if (positionStack)
                    xVar <- 1
                else
                    xVar <- categoryVar
            } else { # with group
                xVar <- categoryVar
                fillVar <- groupVar
                if (self$options$percentWithin == "group")
                    byVar <- groupVar
                else
                    byVar <- categoryVar
            }

            plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = !!xVar, fill = !!fillVar, by = !!byVar, weight = .data[[".COUNTS"]]))

            #### Bars ####

            geomBar <- function(...) ggplot2::geom_bar(..., position = position, color = borderColor)

            if (yaxis == "count") {
                if (singleColor)
                    plot <- plot + geomBar(ggplot2::aes(y = ggplot2::after_stat(count)), stat = "count", fill = oneColorOfPalette)
                else
                    plot <- plot + geomBar(ggplot2::aes(y = ggplot2::after_stat(count)), stat = "count", show.legend = TRUE) # show.legend needed to display unused levels
            } else {
                if (singleColor)
                    plot <- plot + geomBar(ggplot2::aes(y = ggplot2::after_stat(prop)), stat = ggstats::StatProp, fill = oneColorOfPalette)
                else
                    plot <- plot + geomBar(ggplot2::aes(y = ggplot2::after_stat(prop)), stat = ggstats::StatProp, show.legend = TRUE)
            }

            #### Labels ####

            if (self$options$showLabels) {
                # Default justifications
                vjust2 <- 0.5
                hjust2 <- 0.5
                vfactor <- 1

                # Change justifications
                if (positionStack) {
                    if (reverseStack)
                        labPosition <- ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
                    else
                        labPosition <- ggplot2::position_stack(vjust = 0.5)
                } else {
                    if(self$options$barType == "dodge2")
                        labPosition <- ggplot2::position_dodge2(preserve = "single", width = 0.9)
                    else
                        labPosition <- ggplot2::position_dodge(width = 0.9)
                    if (self$options$labelPosition == "middle") {
                        vfactor <- 2
                    } else {
                        if (self$options$horizontal) {
                            hjust2 <- -0.2
                        } else {
                            vjust2 <- -0.6
                        }
                    }
                }

                # Count labels

                geomText <- function(...) ggplot2::geom_text(...,
                                                             position = labPosition, vjust = vjust2, hjust = hjust2,
                                                             fontface = "bold", size = self$options$labelFontSize / ggplot2::.pt)

                if (yaxis == "count") {
                    if (textColor == "auto") {
                        plot <- plot + geomText(ggplot2::aes(y = ggplot2::after_stat(count)/vfactor, label = doNumber(ggplot2::after_stat(count)),
                                                             color = ggplot2::after_scale(ggstats::hex_bw(.data$fill))),
                                                 stat = "count")
                    } else {
                        plot <- plot + geomText(ggplot2::aes(y = ggplot2::after_stat(count)/vfactor, label = doNumber(ggplot2::after_stat(count))),
                                                 stat = "count", color = textColor)
                    }
                } else { # Percent
                    if (textColor == "auto") {
                        plot <- plot + geomText(ggplot2::aes(y = ggplot2::after_stat(prop)/vfactor, label = doPercent(ggplot2::after_stat(prop)),
                                                             color = ggplot2::after_scale(ggstats::hex_bw(.data$fill))),
                                                 stat = ggstats::StatProp)
                    } else {
                        plot <- plot + geomText(ggplot2::aes(y = ggplot2::after_stat(prop)/vfactor, label = doPercent(ggplot2::after_stat(prop))),
                                                 stat = ggstats::StatProp, color = textColor)
                    }
                }
            }

            #### Finishing ####

            # Legend
            if (is.null(groupVar) && !positionStack)
                plot <- plot + ggplot2::guides(fill = "none")

            # Axis labels and scales
            if (yaxis == "count") {
                yLab <- .("Count")
                yScaleFactor <- 1
                labelFnct <- ggplot2::waiver()
            } else {
                yLab <- .("Percent")
                yScaleFactor <- 100
                labelFnct <- scales::label_percent(suffix = ' %', decimal.mark = self$options[['decSymbol']])
            }

            # Show unused levels (if checked in data/var setting)
            plot <- plot + ggplot2::scale_x_discrete(drop = FALSE)

            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") { # Horizontal and manual
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
            if (self$options$showLabels && self$options$labelPosition == "top") {
                if (self$options$horizontal  && self$options$xAxisRangeType == "auto")
                    expand_arg <- ggplot2::expansion(mult = c(0.05, 0.1))
                else if (!self$options$horizontal  && self$options$yAxisRangeType == "auto")
                    expand_arg <- ggplot2::expansion(mult = c(0.05, 0.1))
            }

            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1), labels = labelFnct, expand = expand_arg)
            } else if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1), labels = labelFnct, expand = expand_arg)
            } else {
                plot <- plot + ggplot2::scale_y_continuous(labels = labelFnct, expand = expand_arg)
            }

            # facet
            if (!is.null(self$options$facet)) {
                facetVar <- rlang::sym(self$options$facet)
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            # Theme and colors
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE) # drop to include unused levels in color scales

            # Titles & Labels
            defaults <- list(y = yLab, x = categoryVar, legend = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options)

            # Legend position
            plot <- plot + ggplot2::theme(legend.key.spacing.y = grid::unit(1, "mm"), legend.byrow = TRUE)

            vijDebugPlot(self, plot)

            return(plot)

        })
)
