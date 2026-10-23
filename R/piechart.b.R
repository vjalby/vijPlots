piechartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "piechartClass",
    inherit = piechartBase,
    private = list(
        .init = function() {
            # Weight message
            vijWeightsMessage(self, self$options$counts)
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
                table$getColumn('inner')$setTitle(self$options$aVar)
            if (!is.null(self$options$facet))
                table$getColumn('facet')$setTitle(self$options$facet)
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

            weighted <- vijWeightedData(self, varNames, countsName = self$options$counts,
                                        ignoreNA = self$options$ignoreNA)
            if (is.null(weighted))
                return(FALSE)

            # Sum the counts per combination of variables: the state is saved into the .omv,
            # so store one row per bar/slice rather than one row per case
            plotData <- dplyr::count(weighted$data, dplyr::pick(dplyr::all_of(varNames)), wt = .data[[".COUNTS"]], name = ".COUNTS")

            # Sort the categories by overall count (slices, legend and frequency table)
            aVarName <- self$options$aVar
            if (self$options$order == "decreasing")
                plotData[[aVarName]] <- forcats::fct_infreq(plotData[[aVarName]], w = plotData$.COUNTS)
            else if (self$options$order == "increasing")
                plotData[[aVarName]] <- forcats::fct_rev(forcats::fct_infreq(plotData[[aVarName]], w = plotData$.COUNTS))

            if (self$options$frequencyTable)
                vijFrequencyTable(self, plotData, inner = aVarName,
                                  facet = self$options$facet, nMissing = weighted$nMissing)

            image <- self$results$plot
            image$setState(plotData)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            plotData <- image$state

            # Missing values (ignoreNA unchecked) shown as "(Missing)": legend, facet strips, text labels
            labelNA <- vijLabelNA(self)

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
            doCount <- vijFormatCount(self$options[['decSymbol']])

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
                                   "count" = doCount(ggplot2::after_stat(count)),
                                   "percent" = ifelse(ggplot2::after_stat(prop) < !!hideBelow, NA_character_,
                                                      doPercent(ggplot2::after_stat(prop))),
                                   "group" = labelNA(.data$fill),
                                   "group+count" = paste0(labelNA(.data$fill), "\n", doCount(ggplot2::after_stat(count))),
                                   "group+percent" = ifelse(ggplot2::after_stat(prop) < !!hideBelow, labelNA(.data$fill),
                                                            paste0(labelNA(.data$fill), "\n", doPercent(ggplot2::after_stat(prop))))))

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
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber), scales = "free",
                                                       labeller = ggplot2::as_labeller(labelNA))
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber), scales = "free",
                                                       labeller = ggplot2::as_labeller(labelNA))
            }

            #### Theme & colors ####

            # Theme and colors
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE, labels = labelNA)

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
