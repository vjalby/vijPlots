# This file is a generated template, your changes will not be overwritten

barchartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "barchartClass",
    inherit = barchartBase,
    private = list(
        .init = function() {
            if (!is.null(self$options$xVar))
                nbOfLevel <- nlevels(self$data[[self$options$xVar]])
            else
                nbOfLevel <- 5

            # Stretchable dimensions
            if (self$options$horizontal) {
                width <- 400
                height <- min(max(250,nbOfLevel*50),650)
            } else {
                width <- min(max(400,nbOfLevel*75),700)
                height <- 350
            }
            # Fixed dimension
            if (self$options$horizontal) {
                fixed_width <- 100
                fixed_height <- 50
            } else {
                fixed_width <- 75
                fixed_height <- 50
            }
            # Legend
            if( !is.null(self$options$group)) {
                if (self$options$legendPosition %in% c('top','bottom'))
                    fixed_height <- fixed_height + 50
                else
                    fixed_width <- fixed_width + 100
            }
            # Set the image dimensions
            image <- self$results$plot
            if (is.null(image$setSize2)) { # jamovi < 2.7.16
                image$setSize(width + fixed_width, height + fixed_height)
            } else {
                image$setSize2(width, height, fixed_width, fixed_height)
            }
        },
        .run = function() {
            if (!is.null(self$options$yVar) && !is.null(self$options$xVar) && nrow(self$data) != 0) {
                plotData <- self$data[c(self$options$yVar, self$options$xVar, self$options$group, self$options$facet)]
                plotData[[self$options$yVar]] <- jmvcore::toNumeric(plotData[[self$options$yVar]])
                # missing data
                plotData <- subset(plotData, !is.na(plotData[self$options$yVar]))
                # Remove case with missing group
                if (!is.null(self$options$xVar) & self$options$ignoreNA) {
                    plotData <- subset(plotData, !is.na(plotData[self$options$xVar]))
                }
                if (!is.null(self$options$group) & self$options$ignoreNA) {
                    plotData <- subset(plotData, !is.na(plotData[self$options$group]))
                }
                if (!is.null(self$options$facet) & self$options$ignoreNA) {
                    plotData <- subset(plotData, !is.na(plotData[self$options$facet]))
                }
                #plotData <- jmvcore::naOmit(plotData)
                image <- self$results$plot
                image$setState(plotData)
            }
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            yVar <- self$options$yVar
            yVar <- ensym(yVar)
            xVar <- self$options$xVar
            xVar <- ensym(xVar)
            groupVar <- self$options$group
            if (!is.null(groupVar)) {
                groupVar <- ensym(groupVar)
                ffill <- groupVar
            } else {
                ffill <- xVar
            }

            # barType / Position
            position <- self$options$barType
            if (is.null(groupVar))
                position <- "dodge"

            stacked <- (position == "stack")
            if (stacked) {
                if (self$options$reverseStack) {
                    position <- position_stack()
                    labPosition <- position_stack(vjust = 0.5)
                } else {
                    position <- position_stack(reverse = TRUE)
                    labPosition <- position_stack(vjust = 0.5, reverse = TRUE)
                }
            } else {
                labPosition <- position_dodge(width = 0.9)
            }

            # Single color
            singleColor <- self$options$singleColor
            if (!is.null(groupVar))
                singleColor <- FALSE
            if (singleColor) {
                nbColors <- attr(vijPalette(self$options$colorPalette, "fill"),"nlevels")
                colorNo <- self$options$colorNo
                oneColorOfPalette <- vijPalette(self$options$colorPalette, "fill")(nbColors)[min(colorNo,nbColors)]
            }
            # Border color
            if (self$options$borderColor == "none")
                borderColor = NA
            else
                borderColor = self$options$borderColor

            if (!is.null(self$options$facet)) {
                facetVar <- self$options$facet
                facetVar <- ensym(facetVar)
            } else {
                facetVar <- NULL
            }

            # Order
            orderFun <- self$options$yaxis
            if (self$options$order == "decreasing")
                plot <- ggplot(plotData, aes(x = forcats::fct_reorder(!!xVar,!!yVar, .fun = orderFun, .desc = TRUE), y = !!yVar, group = !!groupVar, fill = !!ffill))
            else if (self$options$order == "increasing")
                plot <- ggplot(plotData, aes(x = forcats::fct_reorder(!!xVar,!!yVar, .fun = orderFun, .desc = FALSE), y = !!yVar, group = !!groupVar, fill = !!ffill))
            else
                plot <- ggplot(plotData, aes(x = !!xVar, y = !!yVar, group = !!groupVar, fill = !!ffill))

            summaryFun <- self$options$yaxis

            if(singleColor)
                plot <- plot + stat_summary(fun = summaryFun, geom = "bar", position = position,
                                            color = borderColor, fill = oneColorOfPalette)
            else
                plot <- plot + stat_summary(fun = summaryFun, geom = "bar", position = position,
                                            color = borderColor)

            # Value labels
            if (self$options$showLabels) {
                if (stacked) {
                    vjust <- 0.5
                    hjust <- 0.5
                } else if (self$options$horizontal) {
                    vjust <- 0.5
                    hjust <- -0.5
                } else {
                    vjust <- -0.5
                    hjust <- 0.5
                }
                textColor <- self$options$textColor
                if (!stacked && textColor == "auto")
                    textColor <- "black"
                if (stacked)
                    fontFace <- "bold"
                else
                    fontFace = "plain"
                if (textColor == "auto")
                    plot <- plot + stat_summary(fun = summaryFun, geom = "text",
                                                aes(label = round(after_stat(y), self$options$decimalPrecision), color = after_scale(ggstats::hex_bw(.data$fill))),
                                                size = self$options$labelTextSize /.pt, fontface = fontFace,
                                                position = labPosition, vjust = vjust, hjust = hjust)
                else
                    plot <- plot + stat_summary(fun = summaryFun, geom = "text",
                                                aes(label = round(after_stat(y), self$options$decimalPrecision)),
                                                size = self$options$labelTextSize /.pt, fontface = fontFace,
                                                position = labPosition, vjust = vjust, hjust = hjust,
                                                color = textColor)
                # Label for stacked sum
                if (stacked && self$options$yaxis == "sum") {
                    if (self$options$horizontal) {
                        vjust2 <- 0.5
                        hjust2 <- -0.5
                    } else {
                        vjust2 <- -0.5
                        hjust2 <- 0.5
                    }
                    textColor2 <- ifelse(textColor %in% c("auto","white"), "black", textColor)

                    plot <- plot + stat_summary(fun = summaryFun, geom = "text",
                                                aes(y = !!yVar, label = round(after_stat(y), self$options$decimalPrecision), group = NULL, fill = NULL),
                                                size = self$options$labelTextSize /.pt, vjust = vjust2, hjust = hjust2,
                                                color = textColor2)
                }
            }

            # ErrorBars
            errorBars <- self$options$errorBars
            if (stacked || orderFun != "mean")
                errorBars <- "none"

            if (errorBars == "sd") {
                funData <- mean_sdl
                funArgs <- list(mult = 1)
            } else if (errorBars == "se") {
                funData <- mean_cl_normal
                funArgs <- list(mult = 1)
            } else if (errorBars == "ci") {
                funData <- ifelse(self$options$bootstrap, mean_cl_boot, mean_cl_normal)
                funArgs <- list(conf.int = self$options$ciLevel/100)
            }
            if (errorBars != "none")
                plot <- plot +  stat_summary(fun.data = funData, fun.args = funArgs, geom = "errorbar",
                                             width = self$options$errorBarWidth, size = self$options$errorBarLineSize,
                                             color = "black",
                                             position = position_dodge(width = 0.8))

            # Axis Limits & flip
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + coord_flip(ylim = c(self$options$xAxisRangeMin, self$options$xAxisRangeMax))
                } else {
                    if ((self$options$showLabels && !stacked) || (self$options$showLabels && summaryFun == "sum" && stacked))
                        plot <- plot + coord_flip(clip = "off", ylim = layer_scales(plot)$y$get_limits()*1.2)
                    else
                        plot <- plot + coord_flip()
                }
            } else if (self$options$yAxisRangeType == "manual") { # Horizontal and manual
                plot <- plot + coord_cartesian(ylim = c(self$options$yAxisRangeMin, self$options$yAxisRangeMax))
            }

            plot <- plot + scale_x_discrete(drop = FALSE) # keep unused levels

            # Axis Labels
            if (self$options$yaxis == "mean") {
                if (errorBars == "none")
                    ylabel <- jmvcore::format(.('Mean of {var}'), var = yVar)
                else if (errorBars == "sd")
                    ylabel <- jmvcore::format(.('{var} (Mean ± SD)'), var = yVar)
                else if (errorBars == "se")
                    ylabel <- jmvcore::format(.('{var} (Mean ± SE)'), var = yVar)
                else if (errorBars == "ci")
                    ylabel <- jmvcore::format(.('{var} (Mean ± {level}%  CI)'), var = yVar, level = self$options$ciLevel)
            } else if (self$options$yaxis == "median")
                ylabel <- jmvcore::format(.('Median of {var}'), var = yVar)
            else if (self$options$yaxis == "min")
                ylabel <- jmvcore::format(.('Minimum of {var}'), var = yVar)
            else if (self$options$yaxis == "max")
                ylabel <- jmvcore::format(.('Maximum of {var}'), var = yVar)
            else if (self$options$yaxis == "sum")
                ylabel <- jmvcore::format(.('Sum of {var}'), var = yVar)
            else
                ylabel <- yVar

            # Ticks
            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot  + scale_y_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1))
            } else if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot  + scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1))
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
            defaults <- list(y = ylabel, x = xVar, legend = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options, showLegend = !is.null(groupVar))

            #self$results$text$setContent(plot)

            return(plot)
        })
)
