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
        },
        .run = function() {
            if (is.null(self$options$rows))
                return()

            varNames <- c(self$options$rows, self$options$columns, self$options$facet)
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

            if (self$options$ignoreNA)
                plotData <- jmvcore::naOmit(plotData)
            if (nrow(plotData) == 0)
                return()

            # Validate .COUNTS (NA / non negative / not infinite)
            if (any(is.na(plotData$.COUNTS)))  {
                vijErrorMessage(self, .('Counts may not contain missing values.'))
            }
            if (any(plotData$.COUNTS < 0)) {
                vijErrorMessage(self, .('Counts may not be negative.'))
            }
            if (any(is.infinite(plotData$.COUNTS))) {
                vijErrorMessage(self, .('Counts may not be infinite.'))
            }

            image <- self$results$plot
            image$setState(plotData)
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            #### Variables ####

            categoryVar <- rlang::sym(self$options$rows)

            if (!is.null(self$options$columns))
                groupVar <- rlang::sym(self$options$columns)
            else
                groupVar <- NULL

            if (self$options$borderColor == "none")
                borderColor = NA
            else
                borderColor = self$options$borderColor

            positionStack <- (self$options$barType == "stack")
            if (self$options$barType == "dodge2")
                position <- ggplot2::position_dodge2(preserve = "single", width = 0.9)
            else if(self$options$barType == "dodge")
                position <- ggplot2::position_dodge(width = 0.9)
            else
                position <- self$options$barType

            yaxis <- self$options$yaxis

            if (self$options$order != "none") {
                if (self$options$order == "decreasing")
                    plotData[[categoryVar]] <- forcats::fct_infreq(plotData[[categoryVar]], w = plotData$.COUNTS)
                else
                    plotData[[categoryVar]] <- forcats::fct_rev(forcats::fct_infreq(plotData[[categoryVar]], w = plotData$.COUNTS))
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

            if (singleColor) {
                selectedColorPalette <- vijPalette(self$options$colorPalette, "fill")
                nbColors <- vijPaletteNlevels(selectedColorPalette)
                colorNo <- as.numeric(self$options$colorNo)
                oneColorOfPalette <- selectedColorPalette(nbColors)[min(colorNo,nbColors)]
            }

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

            if (yaxis == "count") {
                if (singleColor)
                    plot <- plot + ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(count)), stat = "count", position = position,
                                            color = borderColor, fill = oneColorOfPalette)
                else
                    plot <- plot + ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(count)), stat = "count", position = position,
                                            color = borderColor, show.legend = TRUE) # show.legend needed to display unused levels
            } else {
                if (singleColor)
                    plot <- plot + ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(prop)), stat = ggstats::StatProp, position = position,
                                            color = borderColor, fill = oneColorOfPalette)
                else
                    plot <- plot + ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(prop)), stat = ggstats::StatProp, position = position,
                                            color = borderColor, show.legend = TRUE)
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

                # geom_text
                if (yaxis == "count") {
                    if (textColor == "auto") {
                        plot <- plot + ggplot2::geom_text(ggplot2::aes(y = ggplot2::after_stat(count)/vfactor, label = doNumber(ggplot2::after_stat(count)),
                                                                                        color = ggplot2::after_scale(ggstats::hex_bw(.data$fill))),
                                                 stat = "count", position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 fontface = "bold", size = self$options$labelFontSize / ggplot2::.pt)
                    } else {
                        plot <- plot + ggplot2::geom_text(ggplot2::aes(y = ggplot2::after_stat(count)/vfactor, label = doNumber(ggplot2::after_stat(count))),
                                                 stat = "count", position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 color = textColor, fontface = "bold", size = self$options$labelFontSize / ggplot2::.pt)
                    }
                } else { # Percent
                    if (textColor == "auto") {
                        plot <- plot + ggplot2::geom_text(ggplot2::aes(y = ggplot2::after_stat(prop)/vfactor, label = doPercent(ggplot2::after_stat(prop)),
                                                                                        color = ggplot2::after_scale(ggstats::hex_bw(.data$fill))),
                                                 stat = ggstats::StatProp, position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 fontface = "bold", size = self$options$labelFontSize / ggplot2::.pt)
                    } else {
                        plot <- plot + ggplot2::geom_text(ggplot2::aes(y = ggplot2::after_stat(prop)/vfactor, label = doPercent(ggplot2::after_stat(prop))),
                                                 stat = ggstats::StatProp, position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 color = textColor, fontface = "bold", size = self$options$labelFontSize / ggplot2::.pt)
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
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill", drop = FALSE) # drop to include unused levels in color scales

            # Titles & Labels
            defaults <- list(y = yLab, x = categoryVar, legend = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options)

            # Legend position
            plot <- plot + ggplot2::theme(legend.key.spacing.y = grid::unit(1, "mm"), legend.byrow = TRUE)

            vijDebugPlot(self, plot)

            return(plot)

        })
)
