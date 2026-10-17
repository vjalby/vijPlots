piechartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "piechartClass",
    inherit = piechartBase,
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
                width <- max(400, 200*nbOfColumn)
                height <- max(500, 300*nbOfRow)
            } else {
                width <- 400
                height <- 400
            }
            # Fixed dimension
            if (self$options$legendPosition %in% c('top','bottom')) {
                fixed_width <- 0
                fixed_height <- 50
            } else {
                fixed_width <- 100
                fixed_height <- 0
            }
            # Set the image dimensions
            image <- self$results$plot
            if (is.null(image[['setSize2']])) { # jamovi < 2.7.16
                image$setSize(width + fixed_width, height + fixed_height)
            } else {
                image$setSize2(width, height, fixed_width, fixed_height)
            }
            # Frequency table columns
            table <- self$results$frequencies
            if (!is.null(self$options$aVar))
                table$getColumn('category')$setTitle(self$options$aVar)
            if (!is.null(self$options$facet))
                table$addColumn(name = 'facet', title = self$options$facet, type = 'text',
                                combineBelow = TRUE, index = 1)
        },
        .run = function() {
            if (is.null(self$options$aVar) || nrow(self$data) == 0) {
                vijWarningMessage(self, .("Pie charts are for educational use only. Please do not use Pie charts!"))
                return(FALSE)
            }

            varNames <- c(self$options$aVar, self$options$facet)

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
                return(FALSE)

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

            # Sort the categories by overall count (slices, legend and frequency table)
            aVarName <- self$options$aVar
            if (self$options$order == "decreasing")
                plotData[[aVarName]] <- forcats::fct_infreq(plotData[[aVarName]], w = plotData$.COUNTS)
            else if (self$options$order == "increasing")
                plotData[[aVarName]] <- forcats::fct_rev(forcats::fct_infreq(plotData[[aVarName]], w = plotData$.COUNTS))

            if (self$options$frequencyTable)
                private$.fillFrequencyTable(plotData, nMissing)

            image <- self$results$plot
            image$setState(plotData)
        },
        .fillFrequencyTable = function(plotData, nMissing) {
            table <- self$results$frequencies
            aVarName <- self$options$aVar
            facetName <- self$options$facet
            # Values of a variable in the plot's order: all levels, then NA last when missing
            # values are kept (ignoreNA unchecked). %in% rather than == to match NA.
            valuesOf <- function(name) {
                values <- levels(plotData[[name]])
                if (anyNA(plotData[[name]]))
                    values <- c(values, NA)
                values
            }
            labelOf <- function(value) if (is.na(value)) "NA" else value
            # All categories are listed (as in the legend), only non-empty facets (as in the plot)
            categories <- valuesOf(aVarName)
            if (is.null(facetName)) {
                facetLevels <- list(NULL)
            } else {
                facetLevels <- Filter(function(value) any(plotData[[facetName]] %in% value), as.list(valuesOf(facetName)))
            }
            rowNo <- 0
            for (facetLevel in facetLevels) {
                if (is.null(facetLevel)) {
                    inFacet <- rep(TRUE, nrow(plotData))
                    facetValue <- NULL
                } else {
                    inFacet <- plotData[[facetName]] %in% facetLevel
                    facetValue <- list(facet = labelOf(facetLevel))
                }
                counts <- vapply(categories, function(cat) sum(plotData$.COUNTS[inFacet & plotData[[aVarName]] %in% cat]), numeric(1))
                total <- sum(counts)
                cumCounts <- cumsum(counts)
                for (i in seq_along(categories)) {
                    rowNo <- rowNo + 1
                    table$addRow(rowKey = rowNo, values = c(facetValue, list(
                        category = labelOf(categories[[i]]), counts = counts[[i]], percent = counts[[i]] / total,
                        cumPercent = cumCounts[[i]] / total)))
                    # Space above each new facet (jamovi only renders BEGIN_GROUP, as a top padding)
                    if (i == 1 && rowNo > 1)
                        for (col in c('facet', 'category', 'counts', 'percent', 'cumPercent'))
                            table$addFormat(rowKey = rowNo, col = col, jmvcore::Cell.BEGIN_GROUP)
                }
                rowNo <- rowNo + 1
                table$addRow(rowKey = rowNo, values = c(facetValue, list(
                    category = .("Total"), counts = total, percent = 1, cumPercent = "")))
                for (col in c('category', 'counts', 'percent', 'cumPercent'))
                    table$addFormat(rowKey = rowNo, col = col, jmvcore::Cell.BEGIN_GROUP)
            }
            # Observations left out because of missing values (ignoreNA checked), overall
            if (nMissing > 0)
                table$setNote('missing', jmvcore::format(.('Missing values: {n}'), n = format(nMissing)))
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            plotData <- image$state

            # ggstats::StatProp (percent labels) drops the rows whose fill is NA (stats::aggregate):
            # make NA an explicit level (still drawn with the scale's na.value) to keep the NA slice
            # in the percentages. Only if NA is present (an empty NA level would show in the legend).
            if (self$options$labels %in% c("percent", "group+percent") && anyNA(plotData[[self$options$aVar]]))
                plotData[[self$options$aVar]] <- forcats::fct_na_value_to_level(plotData[[self$options$aVar]])

            #### Set variables ####
            aVar <- rlang::sym(self$options$aVar)
            facetVar <- vijVar(self$options$facet)

            #### Plot options ####

            if (self$options$borderColor == "none") {
                borderColor <- NA
            } else {
                borderColor <- self$options$borderColor
            }

            doPercent <- scales::label_percent(
                accuracy = as.numeric(self$options$accuracy),
                suffix = ' %',
                decimal.mark = self$options[['decSymbol']])

            #### Build the plot ####

            if(self$options$donut) {
                plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = 10, fill = !!aVar, by = 1, weight = .data[[".COUNTS"]])) + ggplot2::xlim(c(8.5,NA))
                xOffset <- 10
            } else {
                plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = "", fill = !!aVar, by = 1, weight = .data[[".COUNTS"]]))
                xOffset <- 1
            }

            plot <- plot + ggplot2::geom_bar(position = "fill", color = borderColor, show.legend = TRUE) + ggplot2::coord_polar("y", direction = if (self$options$clockwise) -1 else 1)

            #### Labels ####

            if (self$options$labels != "none") {
                # label geom
                if (self$options$overlap) {
                    if (self$options$labType == "text")
                        baseGeomLab <- function(...) ggrepel::geom_text_repel(..., direction = "both", seed = 123, min.segment.length = 1)
                    else
                        baseGeomLab <- function(...) ggrepel::geom_label_repel(..., direction = "both", seed = 123, min.segment.length = 1)
                } else {
                    if (self$options$labType == "text")
                        baseGeomLab <- ggplot2::geom_text
                    else
                        baseGeomLab <- ggplot2::geom_label
                }

                # label color
                if (self$options$textColor != "auto") {
                    geomLab <- function(...) baseGeomLab(..., color = self$options$textColor)
                } else {
                    geomLab <- baseGeomLab
                }

                # label format (percentages of small slices are hidden: NA labels are dropped by na.rm)
                hideBelow <- if (self$options$hideLabelsBelow) 0.05 else 0
                labelExpr <- rlang::expr(switch(self$options$labels,
                                   "count" = ggplot2::after_stat(count),
                                   "percent" = ifelse(ggplot2::after_stat(prop) < !!hideBelow, NA_character_,
                                                      doPercent(ggplot2::after_stat(prop))),
                                   "group" = .data$fill,
                                   "group+count" = paste0(.data$fill, "\n", ggplot2::after_stat(count)),
                                   "group+percent" = ifelse(ggplot2::after_stat(prop) < !!hideBelow, as.character(.data$fill),
                                                            paste0(.data$fill, "\n", doPercent(ggplot2::after_stat(prop))))))

                # Label stat and position
                statLab <- switch(self$options$labels, "percent" = ggstats::StatProp,
                                  "group+percent" = ggstats::StatProp,
                                  "count")

                labX <- self$options$labOffset/10 + xOffset

                # Label AES
                if (self$options$textColor == "auto") {
                    labMapping <- ggplot2::aes(x = !!labX, label = !!labelExpr,
                                               color = ggplot2::after_scale(ggstats::hex_bw(.data$fill)))
                } else {
                    labMapping <- ggplot2::aes(x = !!labX, label = !!labelExpr)
                }

                # Plot label
                plot <- plot + geomLab(labMapping, stat = statLab,
                                       position = ggplot2::position_fill(vjust = 0.5),
                                       fontface = "bold",
                                       size = self$options$labSize / ggplot2::.pt,
                                       show.legend = FALSE, na.rm = TRUE)
            }

            #### Facet ####

            if (!is.null(facetVar) ) {
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber), scales = "free")
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber), scales = "free")
            }

            #### Theme & colors ####

            # Theme and colors
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE)

            # Guide
            if (self$options$labels %in% c("group","group+count","group+percent"))
                plot <- plot + ggplot2::guides(fill = "none")

            #### Axes and titles ####

            # Titles & Labels
            defaults <- list(y = "", x = "", legend = aVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options)
            plot <- plot + ggplot2::theme(legend.key.spacing.y = grid::unit(1, "mm"), legend.byrow = TRUE)

            # Labs
            plot <- plot + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                                 axis.line.x = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank(),
                                 axis.text.x = ggplot2::element_blank(),axis.text.y = ggplot2::element_blank(),
                                 panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

            vijDebugPlot(self, plot)

            return(plot)
        }

    )
)
