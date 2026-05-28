
# This file is a generated template, your changes will not be overwritten

barplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "barplotClass",
    inherit = barplotBase,
    private = list(
        .init = function() {
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
            if (!is.null(self$options$rows) && nrow(self$data) != 0) {
                plotData <- self$data[c(self$options$rows, self$options$columns, self$options$facet)]
                if( self$options$ignoreNA )
                    plotData <- jmvcore::naOmit(plotData)
                image <- self$results$plot
                image$setState(plotData)
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            #### Variables ####

            category <- self$options$rows
            category <- ensym(category)

            columns <- self$options$columns
            if (!is.null(columns))
                group <- ensym(columns)
            else
                group <- NULL

            if (self$options$borderColor == "none")
                borderColor = NA
            else
                borderColor = self$options$borderColor

            positionStack <- (self$options$barType == "stack")
            if(self$options$barType == "dodge2")
                position <- position_dodge2(preserve = "single")
            else
                position <- self$options$barType

            yaxis <- self$options$yaxis


            if (self$options$order == "decreasing")
                plotData[[category]] <- forcats::fct_infreq(plotData[[category]])
            else if (self$options$order == "increasing")
                plotData[[category]] <- forcats::fct_rev(forcats::fct_infreq(plotData[[category]]))

            reverseStack <- (!self$options$reverseStack && positionStack)

            if (reverseStack)
                position <- position_stack(reverse = TRUE)

            # Percent format (scales)
            doPercent <- scales::label_percent(
                            accuracy = as.numeric(self$options$accuracy),
                            suffix = '\u2009%',
                            decimal.mark = self$options[['decSymbol']])
            if (self$options$horizontal)
                doNumber <- function(x){ifelse(x<10, paste0(" ",x), x)}
            else
                doNumber <- as.character

            # Correct Single color option
            if (positionStack || !is.null(group))
                singleColor <- FALSE
            else
                singleColor <- self$options$singleColor

            if (singleColor) {
                nbColors <- attr(vijPalette(self$options$colorPalette, "fill"),"nlevels")
                colorNo <- self$options$colorNo
                oneColorOfPalette <- vijPalette(self$options$colorPalette, "fill")(nbColors)[min(colorNo,nbColors)]
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

            if (is.null(group)) { # No group
                bby <- 1
                ffill <- category
                if (positionStack)
                    xx <- 1
                else
                    xx <- category
            } else { # with group
                xx <- category
                ffill <- group
                if (self$options$percentWithin == "group")
                    bby <- group
                else
                    bby <- category
            }

            plot <- ggplot(plotData, aes(x = !!xx, fill = !!ffill, by = !!bby))

            #### Bars ####

            if (yaxis == "count") {
                if (singleColor)
                    plot <- plot + geom_bar(aes(y = after_stat(count)), stat = "count", position = position,
                                            color = borderColor, fill = oneColorOfPalette)
                else
                    plot <- plot + geom_bar(aes(y = after_stat(count)), stat = "count", position = position,
                                            color = borderColor, show.legend = TRUE) # show.legend needed to display unused levels
            } else {
                if (singleColor)
                    plot <- plot + geom_bar(aes(y = after_stat(prop)), stat = ggstats::StatProp, position = position,
                                            color = borderColor, fill = oneColorOfPalette)
                else
                    plot <- plot + geom_bar(aes(y = after_stat(prop)), stat = ggstats::StatProp, position = position,
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
                        labPosition <- position_stack(vjust = 0.5, reverse = TRUE)
                    else
                        labPosition <- position_stack(vjust = 0.5)
                } else {
                    if(self$options$barType == "dodge2")
                        labPosition <- position_dodge2(preserve = "single", width = 0.9)
                    else
                        labPosition <- position_dodge(width = 0.9)
                    if (self$options$labelPosition == "middle") {
                        #labPosition <- position_dodge(width = 0.9)
                        vfactor <- 2
                    } else {
                        #labPosition <- position_dodge(width = 0.9)
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
                        plot <- plot + geom_text(aes(y = after_stat(count)/vfactor, label = doNumber(after_stat(count)),
                                                                                        color = after_scale(ggstats::hex_bw(.data$fill))),
                                                 stat = "count", position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 fontface = "bold", size = self$options$labelFontSize / .pt)
                    } else {
                        plot <- plot + geom_text(aes(y = after_stat(count)/vfactor, label = doNumber(after_stat(count))),
                                                 stat = "count", position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 color = textColor, fontface = "bold", size = self$options$labelFontSize / .pt)
                    }
                } else { # Percent
                    if (textColor == "auto") {
                        plot <- plot + geom_text(aes(y = after_stat(prop)/vfactor, label = doPercent(after_stat(prop)),
                                                                                        color = after_scale(ggstats::hex_bw(.data$fill))),
                                                 stat = ggstats::StatProp, position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 fontface = "bold", size = self$options$labelFontSize / .pt)
                    } else {
                        plot <- plot + geom_text(aes(y = after_stat(prop)/vfactor, label = doPercent(after_stat(prop))),
                                                 stat = ggstats::StatProp, position = labPosition, vjust = vjust2, hjust = hjust2,
                                                 color = textColor, fontface = "bold", size = self$options$labelFontSize / .pt)
                    }
                }
            }

            #### Finishing ####

            # Legend
            if (is.null(group) && !positionStack)
                plot <- plot + guides(fill = "none")

            # Axis labels and scales
            if (yaxis == "count") {
                yLab <- .("Count")
                yScaleFactor <- 1
                labelFnct <- waiver()
            } else {
                yLab <- .("Percent")
                yScaleFactor <- 100
                labelFnct <- scales::label_percent(suffix = '\u2009%', decimal.mark = self$options[['decSymbol']])
            }

            # Show unused levels (if checked in data/var setting)
            plot <- plot + scale_x_discrete(drop = FALSE) #+ scale_fill_manual(drop = FALSE)

            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") { # Horizontal and manual
                    plot <- plot + coord_flip(ylim = c(self$options$xAxisRangeMin/yScaleFactor, self$options$xAxisRangeMax/yScaleFactor))
                } else {
                    if (self$options$showLabels && self$options$labelPosition == "top")
                        plot <- plot + coord_flip(clip = "off", ylim = layer_scales(plot)$y$get_limits()*1.1) # Gives more room for labels !
                    else
                        plot <- plot + coord_flip(clip = "off")
                }
            } else {
                if (self$options$yAxisRangeType == "manual") {
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

            # facet
            if (!is.null(self$options$facet)) {
                facetVar <- self$options$facet
                facetVar <- ensym(facetVar)
                if (self$options$facetBy == "column")
                    plot <- plot + facet_wrap(vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + facet_wrap(vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill", drop = FALSE) # drop to include unused levels in color scales

            # Titles & Labels
            defaults <- list(y = yLab, x = category, legend = group)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)

            # Legend position
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            #self$results$text$setContent(plot)

            return(plot)

        })
)
