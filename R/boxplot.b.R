
# This file is a generated template, your changes will not be overwritten

boxplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "boxplotClass",
    inherit = boxplotBase,
    private = list(
        .init = function() {
            # Stretchable dimensions
            if( is.null(self$options$group) ) {
                if (self$options$horizontal) {
                    width <- 600
                    height <- min(max(100*length(self$options$vars), 300), 600)
                } else {
                    width <- min(max(150*length(self$options$vars), 400), 800)
                    height <- 400
                }
            } else {
                if (self$options$horizontal) {
                    width <- 600
                    height <- min(max(75*length(self$options$vars) * nlevels(self$data[[self$options$group]]), 300), 600)
                } else {
                    width <- min(max(100*length(self$options$vars) * nlevels(self$data[[self$options$group]]),400), 800)
                    height <- 400
                }
            }
            # Facet
            if (!is.null(self$options$facet)) {
                nbOfFacet <- nlevels(self$data[[self$options$facet]])
                if (self$options$facetBy == "column") {
                    nbOfColumn <- self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn)
                } else {
                    nbOfRow <- self$options$facetNumber
                    nbOfColumn <- ceiling(nbOfFacet / nbOfRow)
                }
                width <- max(width, (width-100)*nbOfColumn)
                height <- max(height, (height-75)*nbOfRow)
            }
            # Fixed dimension
            fixed_width <- 50 # Y-Axis legend
            fixed_height <- 50 # X-Axis legend
            if (!is.null(self$options$group) && length(self$options$vars) > 1) {
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
            if( length(self$options$vars) == 0 || nrow(self$data) == 0)
                return()
            varNames <- c(self$options$label,self$options$group,self$options$facet,self$options$vars)
            data <- jmvcore::select(self$data, varNames)
            # Be sure dep var are numeric
            for (varName in self$options$vars)
                data[[varName]] <- jmvcore::toNumeric(data[[varName]])
            # Remove case with missing group
            if (!is.null(self$options$group) && self$options$ignoreNA) {
                data <- data[!is.na(data[[self$options$group]]),]
            }
            if (!is.null(self$options$facet) && self$options$ignoreNA) {
                data <- data[!is.na(data[[self$options$facet]]),]
            }

            #### Compute the outliers ####
            if (!is.null(self$options$label)) {
                labelVar <- rlang::sym(self$options$label)
                groupVar <- if (!is.null(self$options$group)) rlang::sym(self$options$group) else NULL
                facetVar <- if (!is.null(self$options$facet)) rlang::sym(self$options$facet) else NULL
                for (varName in self$options$vars) {
                    outlierVar <- rlang::sym(paste0(".outliers_",varName))
                    aVar <- rlang::sym(varName)
                    data <- data |>
                        dplyr::group_by(!!groupVar, !!facetVar) |>
                        dplyr::mutate(!!outlierVar := ifelse(private$.isOutlier(!!aVar), as.character(!!labelVar), NA)) |>
                        dplyr::ungroup()
                }
            }

            image <- self$results$plot
            image$setState(data)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            #### Set variables ####
            depVarNames <- self$options$vars

            if (!is.null(self$options$label)) {
                labelVar = rlang::sym(self$options$label)
            } else {
                labelVar <- NULL
            }
            if (!is.null(self$options$group)) {
                groupVar = rlang::sym(self$options$group)
            } else {
                groupVar <- NULL
            }
            if (!is.null(self$options$facet)) {
                facetVar <- rlang::sym(self$options$facet)
            } else {
                facetVar <- NULL
            }

            #### Set boxplot staples and notches ####

            if (self$options$staple)
                stapleWidth <- as.numeric(self$options$stapleWidth)
            else
                stapleWidth <- 0

            notches <- self$options$notches
            notchWidth <- as.numeric(self$options$notchWidth)

            #### Plot options ####

            if (self$options$horizontal)
                labAngle = 60
            else
                labAngle = 0

            if (self$options$horizontal)
                 nudgeX <- 0.02
            else
                 nudgeX <- 0.015

            if(is.null(groupVar) || length(depVarNames) > 1)
                nudgeX <- nudgeX * length(depVarNames)
            else
                nudgeX <- nudgeX * max(1,nlevels(plotData[[groupVar]]))

            # One color only
            if (self$options$singleColor)
                oneColorOfPalette <- vijOneColorOfPalette(self$options$colorPalette, "fill", theme, self$options$colorNo)

            #### Building the plot ####

            plot <- ggplot2::ggplot(plotData)

            for (varName in depVarNames) {
                yVar <- rlang::sym(varName)

                # set xVar
                if (!is.null(groupVar) && length(depVarNames) == 1) {
                    xVar <- groupVar
                } else {
                    xVar <- varName
                }

                # multiGroup if several variables and several groups
                multiGroup <- (!is.null(groupVar) && length(depVarNames) > 1)

                # fill color
                if (multiGroup) {
                    fillMapping <- groupVar
                    fillStatic <- NULL
                } else if (self$options$singleColor) {
                    fillMapping <- NULL
                    fillStatic <- oneColorOfPalette
                } else {
                    fillMapping <- if (is.null(groupVar)) varName else groupVar
                    fillStatic <- NULL
                }

                # Boxplot
                if (!is.null(fillMapping)) {
                    plot <- plot + ggplot2::geom_boxplot(
                        ggplot2::aes(y = !!yVar, x = !!xVar, fill = !!fillMapping),
                        outliers = self$options$showOutliers, na.rm = TRUE,
                        staplewidth = stapleWidth, notch = notches, notchwidth = notchWidth,
                        show.legend = multiGroup, key_glyph = ggplot2::draw_key_rect
                    )
                } else {
                    plot <- plot + ggplot2::geom_boxplot(
                        ggplot2::aes(y = !!yVar, x = !!xVar), fill = fillStatic,
                        outliers = self$options$showOutliers, na.rm = TRUE,
                        staplewidth = stapleWidth, notch = notches, notchwidth = notchWidth
                    )
                }

                # Outliers
                if (!is.null(labelVar) && self$options$showOutliers) {
                    outlierVar <- rlang::sym(paste0(".outliers_", varName))

                    if (multiGroup) {
                        plot <- plot + ggplot2::geom_text(
                            ggplot2::aes(x = !!xVar, y = !!yVar, label = !!outlierVar, group = !!groupVar),
                            na.rm = TRUE, hjust = 0, angle = labAngle, size = self$options$labSize / ggplot2::.pt,
                            position = ggpp::position_dodgenudge(x = nudgeX, width = .75)
                        )
                    } else {
                        plot <- plot + ggplot2::geom_text(
                            ggplot2::aes(x = !!xVar, y = !!yVar, label = !!outlierVar),
                            na.rm = TRUE, hjust = 0, nudge_x = nudgeX, angle = labAngle,
                            size = self$options$labSize / ggplot2::.pt
                        )
                    }
                }

                # Means
                if (self$options$showMean) {
                    if (multiGroup) {
                        plot <- plot + ggplot2::stat_summary(
                            ggplot2::aes(y = !!yVar, x = !!xVar, group = !!groupVar),
                            fun = mean, na.rm = TRUE, geom = "point",
                            shape = 15, size = 3,
                            position = ggplot2::position_dodge(.75),
                            show.legend = FALSE
                        )
                    } else {
                        plot <- plot + ggplot2::stat_summary(
                            ggplot2::aes(y = !!yVar, x = !!xVar),
                            fun = mean, na.rm = TRUE, geom = "point",
                            shape = 15, size = 3
                        )
                    }
                }
            }

            # Hide legend if groupVar == NULL
            if (is.null(groupVar)) {
                plot <- plot + ggplot2::guides(fill = "none")
            }

            #### Sort variables / levels by median ####
            if (length(depVarNames) > 1) {
                if (self$options$order == "none") {
                    plot <- plot + ggplot2::scale_x_discrete(limits = depVarNames)
                } else {
                    orderedVars <- order(vapply(plotData[,depVarNames], stats::median, FUN.VALUE = numeric(1), na.rm = TRUE), decreasing = (self$options$order == "decreasing"))
                    plot <- plot + ggplot2::scale_x_discrete(limits = depVarNames[orderedVars])
                }
            } else if (!is.null(groupVar) && self$options$order != "none") {
                aVar <- depVarNames # length(depVarNames) = 1
                orderedLevelsData <- forcats::fct_reorder(plotData[[groupVar]], plotData[[aVar]], .desc = (self$options$order == "decreasing"))
                orderedLevels <- levels(addNA(orderedLevelsData, ifany=TRUE))
                plot <- plot + ggplot2::scale_x_discrete(limits = orderedLevels, drop = FALSE)
            } else if (!is.null(groupVar)) {
                plot <- plot + ggplot2::scale_x_discrete(drop = FALSE) # keep unused levels
            }

            #### Theme and colors ####
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE)

            #### Axis Limits & flip ####
            if (self$options$horizontal) {
                if (self$options$xAxisRangeType == "manual") {
                    plot <- plot + ggplot2::coord_flip(ylim = c(self$options$xAxisRangeMin, self$options$xAxisRangeMax))
                } else {
                    plot <- plot + ggplot2::coord_flip()
                }
            } else if (self$options$yAxisRangeType == "manual") { # Horizontal and manual
                plot <- plot + ggplot2::coord_cartesian(ylim = c(self$options$yAxisRangeMin, self$options$yAxisRangeMax))
            }

            #### Ticks ####
            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1))
            }
            if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1))
            }

            plot <- plot + ggplot2::theme(legend.key.spacing.y = grid::unit(1, "mm"), legend.byrow = TRUE)

            #### Facet ####
            if (!is.null(facetVar)) {
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber))
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber))
            }

            #### Titles & Labels ####
            defaults <- list(legend = groupVar)
            if (!is.null(groupVar) && length(depVarNames) == 1) {
                defaults$x <- groupVar
                defaults$y <- depVarNames
                showLegend <- FALSE
            } else {
                defaults$x = NULL
                showLegend <- TRUE
            }
            if (length(depVarNames) > 1) {
                defaults$x <- NULL
                defaults$y <- NULL
            }
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options, showLegend = showLegend)

            vijDebugPlot(self, plot)

            return(plot)
        },
        .isOutlier = function(x) {
            # Be careful that ggplot2::geom_boxplot and graphics::boxplot use different hinges
            if (length(stats::na.omit(x)) == 0) {
                return(rep(FALSE, length(x)))
            } else {
                q1 <- stats::quantile(x, .25, na.rm = TRUE)
                q3 <- stats::quantile(x, .75, na.rm = TRUE)
                iqr <- q3-q1
                return(x < q1 - 1.5*iqr | x > q3 + 1.5*iqr)
            }
        }
    )
)
