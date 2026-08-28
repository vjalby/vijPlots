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
            if (is.null(image[['setSize2']])) { # jamovi < 2.7.16
                image$setSize(width + fixed_width, height + fixed_height)
            } else {
                image$setSize2(width, height, fixed_width, fixed_height)
            }
        },
        .run = function() {
            if (is.null(self$options$yVar) || is.null(self$options$xVar))
                return()

            varNames <- c(self$options$yVar, self$options$xVar, self$options$group, self$options$facet)
            plotData <- jmvcore::select(self$data, varNames)

            plotData[[self$options$yVar]] <- jmvcore::toNumeric(plotData[[self$options$yVar]])
            # missing data
            plotData <- plotData[!is.na(plotData[[self$options$yVar]]),]
            # Remove case with missing group
            if (!is.null(self$options$xVar) && self$options$ignoreNA) {
                plotData <- plotData[!is.na(plotData[[self$options$xVar]]),]
            }
            if (!is.null(self$options$group) && self$options$ignoreNA) {
                plotData <- plotData[!is.na(plotData[[self$options$group]]),]
            }
            if (!is.null(self$options$facet) && self$options$ignoreNA) {
                plotData <- plotData[!is.na(plotData[[self$options$facet]]),]
            }
            if (nrow(plotData) == 0)
                return()
            image <- self$results$plot
            image$setState(plotData)
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            yVar <- rlang::sym(self$options$yVar)
            xVar <- rlang::sym(self$options$xVar)
            if (is.null(self$options$group)) {
                groupVar <- NULL
                fillVar <- xVar
            } else {
                groupVar <- rlang::sym(self$options$group)
                fillVar <- groupVar
            }

            #### barType / Position ####
            position <- self$options$barType
            if (is.null(groupVar))
                position <- "dodge"

            stacked <- (position == "stack")
            dodge2 <- (position == "dodge2")

            if (stacked) {
                if (self$options$reverseStack) {
                    position <- ggplot2::position_stack(reverse = TRUE)
                    labPosition <- ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
                } else {
                    position <- ggplot2::position_stack()
                    labPosition <- ggplot2::position_stack(vjust = 0.5)
                }
            } else if (dodge2) {
                position <- ggplot2::position_dodge2(preserve = "single", width = 0.9)
                labPosition <- ggplot2::position_dodge2(preserve = "single", width = 0.9)
            } else {
                position <- ggplot2::position_dodge(width = 0.9)
                labPosition <- ggplot2::position_dodge(width = 0.9)
            }

            #### Single color ####
            singleColor <- self$options$singleColor
            if (!is.null(groupVar))
                singleColor <- FALSE
            if (singleColor)
                oneColorOfPalette <- vijOneColorOfPalette(self$options$colorPalette, "fill", theme, self$options$colorNo)

            #### Border color ####
            if (self$options$borderColor == "none")
                borderColor <- NA
            else
                borderColor <- self$options$borderColor

            #### Order ####
            orderFun <- self$options$yaxis
            if (self$options$order == "decreasing")
                plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = forcats::fct_reorder(!!xVar,!!yVar, .fun = orderFun, .desc = TRUE), y = !!yVar, group = !!groupVar, fill = !!fillVar))
            else if (self$options$order == "increasing")
                plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = forcats::fct_reorder(!!xVar,!!yVar, .fun = orderFun, .desc = FALSE), y = !!yVar, group = !!groupVar, fill = !!fillVar))
            else
                plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = !!xVar, y = !!yVar, group = !!groupVar, fill = !!fillVar))

            summaryFun <- self$options$yaxis

            if(singleColor)
                plot <- plot + ggplot2::stat_summary(fun = summaryFun, geom = "bar", position = position,
                                            color = borderColor, fill = oneColorOfPalette)
            else
                plot <- plot + ggplot2::stat_summary(fun = summaryFun, geom = "bar", position = position,
                                            color = borderColor)

            #### Value labels ####
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
                    plot <- plot + ggplot2::stat_summary(fun = summaryFun, geom = "text",
                                                ggplot2::aes(label = round(ggplot2::after_stat(y), self$options$decimalPrecision),
                                                             color = ggplot2::after_scale(ggstats::hex_bw(.data$fill))),
                                                size = self$options$labelTextSize /ggplot2::.pt, fontface = fontFace,
                                                position = labPosition, vjust = vjust, hjust = hjust)
                else
                    plot <- plot + ggplot2::stat_summary(fun = summaryFun, geom = "text",
                                                ggplot2::aes(label = round(ggplot2::after_stat(y), self$options$decimalPrecision)),
                                                size = self$options$labelTextSize /ggplot2::.pt, fontface = fontFace,
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

                    plot <- plot + ggplot2::stat_summary(fun = summaryFun, geom = "text",
                                                ggplot2::aes(y = !!yVar, label = round(ggplot2::after_stat(y), self$options$decimalPrecision), group = NULL, fill = NULL),
                                                size = self$options$labelTextSize /ggplot2::.pt, vjust = vjust2, hjust = hjust2,
                                                color = textColor2)
                }
            }

            #### ErrorBars ####
            errorBars <- self$options$errorBars
            if (stacked || orderFun != "mean")
                errorBars <- "none"

            if (errorBars == "sd") {
                funData <- ggplot2::mean_sdl
                funArgs <- list(mult = 1)
            } else if (errorBars == "se") {
                funData <- ggplot2::mean_cl_normal
                funArgs <- list(mult = 1)
            } else if (errorBars == "ci" && self$options$bootstrap) {
                funData <- ggplot2::mean_cl_boot
                funArgs <- list(conf.int = self$options$ciLevel/100)
            } else if (errorBars == "ci" && !self$options$bootstrap) {
                funData <- ggplot2::mean_cl_normal
                funArgs <- list(conf.int = self$options$ciLevel/100)
            }
            if (errorBars != "none")
                plot <- plot +  ggplot2::stat_summary(fun.data = funData, fun.args = funArgs, geom = "errorbar",
                                             width = self$options$errorBarWidth, size = self$options$errorBarLineSize,
                                             color = "black",
                                             position = ggplot2::position_dodge(width = 0.9))

            # Show unused levels (if checked in data/var setting)
            plot <- plot + ggplot2::scale_x_discrete(drop = FALSE)

            #### Axis Limits & flip ####
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + ggplot2::coord_flip(ylim = c(self$options$xAxisRangeMin, self$options$xAxisRangeMax))
                } else {
                    plot <- plot + ggplot2::coord_flip(clip = "off")
                }
            } else {
                if (self$options$yAxisRangeType == "manual") {
                    plot <- plot + ggplot2::coord_cartesian(ylim = c(self$options$yAxisRangeMin, self$options$yAxisRangeMax))
                } else {
                    plot <- plot + ggplot2::coord_cartesian(clip = "off")
                }
            }

            #### Ticks & Axis Expansion ####
            expand_arg <- ggplot2::waiver() # Default ggplot behavior
            if ((self$options$showLabels && !stacked) || (self$options$showLabels && summaryFun == "sum" && stacked)) {
                if (self$options$horizontal && self$options$xAxisRangeType == "auto")
                    expand_arg <- ggplot2::expansion(mult = c(0.05, 0.2))
                else if (!self$options$horizontal && self$options$yAxisRangeType == "auto")
                    expand_arg <- ggplot2::expansion(mult = c(0.05, 0.1))
            }
            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1), expand = expand_arg)
            } else if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1), expand = expand_arg)
            } else if(!inherits(expand_arg, "waiver")) {
                plot <- plot  + ggplot2::scale_y_continuous(expand = expand_arg)
            }

            #### Axis Labels ####
            ylabel <- private$.getYLabel(yVar, errorBars)

            #### facet ####
            if (!is.null(self$options$facet)) {
                facetVar <- rlang::sym(self$options$facet)
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            #### Theme and colors ####
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE) # drop to include unused levels in color scales

            #### Titles & Labels ####
            defaults <- list(y = ylabel, x = xVar, legend = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options, showLegend = !is.null(groupVar))

            vijDebugPlot(self, plot)

            return(plot)
        },
        .getYLabel = function(yVar, errorBars) {
            if (self$options$yaxis == "mean") {
                if (errorBars == "sd")
                    jmvcore::format(.('{var} (Mean ± SD)'), var = yVar)
                else if (errorBars == "se")
                    jmvcore::format(.('{var} (Mean ± SE)'), var = yVar)
                else if (errorBars == "ci")
                    jmvcore::format(.('{var} (Mean ± {level}% CI)'), var = yVar, level = self$options$ciLevel)
                else
                    jmvcore::format(.('Mean of {var}'), var = yVar)
            } else if (self$options$yaxis == "median")
                jmvcore::format(.('Median of {var}'), var = yVar)
            else if (self$options$yaxis == "min")
                jmvcore::format(.('Minimum of {var}'), var = yVar)
            else if (self$options$yaxis == "max")
                jmvcore::format(.('Maximum of {var}'), var = yVar)
            else if (self$options$yaxis == "sum")
                jmvcore::format(.('Sum of {var}'), var = yVar)
            else
                yVar
        })
)
