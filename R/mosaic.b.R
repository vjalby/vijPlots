mosaicClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mosaicClass",
    inherit = mosaicBase,
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
            showGroupLegend <- self$options$yAxis != "label" || self$options$alwaysShowGroupLegend
            if (showGroupLegend || (self$options$residualsAsOpacity && self$options$showOpacityLegend)) {
                if (self$options$legendPosition %in% c('top','bottom')) {
                    if (showGroupLegend && self$options$residualsAsOpacity && self$options$showOpacityLegend)
                        fixed_height <- fixed_height + 100
                    else
                        fixed_height <- fixed_height + 50
                } else {
                    fixed_width <- fixed_width + 100
                }
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
            if (is.null(self$options$category) || is.null(self$options$group) || nrow(self$data) == 0)
                return()

            # Get raw data
            categoryName <- self$options$category
            groupName <- self$options$group
            facetName <- self$options$facet
            plotData <- jmvcore::select(self$data, c(categoryName, groupName, facetName))

            # Drop ordered-ness (level order is preserved): nothing downstream relies on
            # it, and chisq.test()$residuals loses it when converted back to a data frame,
            # which then breaks the left_join() in .mosaicData() with an ordered/factor
            # type mismatch if category or group is an ordinal variable
            plotData[[categoryName]] <- factor(plotData[[categoryName]], levels = levels(plotData[[categoryName]]), ordered = FALSE)
            plotData[[groupName]] <- factor(plotData[[groupName]], levels = levels(plotData[[groupName]]), ordered = FALSE)

            # Weight data
            countsName <- self$options$counts
            if (!is.null(countsName)) {
                # vijPlots/Mosaic weights
                plotData[['.COUNTS']] <- jmvcore::toNumeric(self$data[[countsName]])
            } else if (!is.null(attr(self$data, "jmv-weights"))) {
                # jamovi built-in weights
                plotData[['.COUNTS']] <- jmvcore::toNumeric(attr(self$data, "jmv-weights"))
            } else {
                # no weights
                plotData[['.COUNTS']] <- as.integer(rep(1, nrow(plotData)))
            }

            # Remove NA (including weights !)
            if (self$options$ignoreNA)
                plotData <- jmvcore::naOmit(plotData)
            if (nrow(plotData) == 0)
                return(FALSE)

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

            mdf <- private$.mosaicData(plotData, categoryName, groupName, facetName, self$options$order, self$options$reverseStack)

            image <- self$results$plot
            image$setState(mdf)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            #### Build plot ####

            category <- rlang::sym(self$options$category)
            group <- rlang::sym(self$options$group)

            plot <- ggplot2::ggplot(plotData)

            #### Tiles ####

            if (self$options$residualsAsOpacity){
                plot <- plot + ggplot2::geom_tile(ggplot2::aes(x = x_center, y = y_center, width = xwidth, height = percentage, fill = !!group, alpha = abs(residuals)),
                                                  color = "white", linewidth = self$options$borderWidth, show.legend = TRUE) # show.legend needed to display unused levels
                # Alpha scale
                plot <- plot + ggplot2::scale_alpha_continuous(name = .("Residuals"), range = c(0.1, 1))
            } else {
                plot <- plot + ggplot2::geom_tile(ggplot2::aes(x = x_center, y = y_center, width = xwidth, height = percentage, fill = !!group),
                                                  color = "white", linewidth = self$options$borderWidth, show.legend = TRUE) # show.legend needed to display unused levels
            }

            #### Label ####

            doPercent <- scales::label_percent(
                accuracy = as.numeric(self$options$accuracy),
                suffix = ' %',
                decimal.mark = self$options[['decSymbol']])
            doNumber <- scales::label_number(
                accuracy = as.numeric(self$options$accuracy),
                decimal.mark = self$options[['decSymbol']])

            if (self$options$labelType != "none") {
                plotData <- plotData |>
                    dplyr::mutate(
                        label_text = switch(self$options$labelType,
                                            "count"     = as.character(freq),
                                            "percent"   = doPercent(percentage),
                                            "residuals" = doNumber(residuals)
                        )
                    )

                if (self$options$labelColor == "auto") {
                    plot <- plot + ggplot2::geom_text(
                        data = dplyr::filter(plotData, percentage > 0.05 & xwidth > 0.05),
                        mapping = ggplot2::aes(
                                    x = x_center, y = y_center, label = label_text, fill = !!group,
                                    color = ggplot2::after_scale(ggstats::hex_bw(.data$fill))
                                    ),
                        size = self$options$labelFontSize / ggplot2::.pt,
                        fontface = "bold"
                    )
                } else {
                    plot <- plot + ggplot2::geom_text(
                        data = dplyr::filter(plotData, percentage > 0.05 & xwidth > 0.05),
                        mapping = ggplot2::aes(x = x_center, y = y_center, label = label_text),
                        size = self$options$labelFontSize / ggplot2::.pt,
                        fontface = "bold",
                        color = self$options$labelColor
                    )
                }
            }

            #### X-Axis ####
            expFactMult <- ifelse(self$options$hideAxes, 0.0, 0.04)

            if (is.null(self$options$facet)) {
                xAxisData <- unique(plotData[c("x_center", as.character(category), "xwidth")])
                if (self$options$showCategoryPercent) {
                    xAxisLabels <- paste0(xAxisData[[category]], "\n(", doPercent(xAxisData$xwidth), ")")
                } else {
                    xAxisLabels <- xAxisData[[category]]
                }
                plot <- plot + ggplot2::scale_x_continuous(breaks = xAxisData$x_center,
                                                           labels = xAxisLabels,
                                                           expand = ggplot2::expansion(mult = expFactMult))
            }

            #### Horizontal ####
            if (self$options$horizontal) {
                plot <- plot + ggplot2::coord_flip()
            }

            #### Ticks ####
            labelFnct <- scales::label_percent(suffix = ' %', decimal.mark = self$options[['decSymbol']])

            if (self$options$yAxis == "label") {
                if (is.null(self$options$facet)) {
                    # KNOWN LIMITATION: see matching comment in the facet block below — a group with
                    # zero count in the first column is silently missing from the y-axis labels.
                    firstColBreaks <- dplyr::filter(plotData, x_center == min(x_center))
                    plot <- plot + ggplot2::scale_y_continuous(breaks = firstColBreaks$y_center,
                                                                labels = firstColBreaks[[group]],
                                                                expand = ggplot2::expansion(mult = expFactMult))
                }
                # else: per-facet breaks/labels added below via facetted_pos_scales(y = ...)
            } else if (self$options$yAxis == "percent") {
                if (self$options$horizontal && self$options$xTicks > 0) {
                    plot <- plot  + ggplot2::scale_y_continuous(labels = labelFnct,
                                                                breaks = scales::breaks_extended(self$options$xTicks + 1),
                                                                expand = ggplot2::expansion(mult = expFactMult))
                } else if (!self$options$horizontal && self$options$yTicks > 0) {
                    plot <- plot  + ggplot2::scale_y_continuous(labels = labelFnct,
                                                                breaks = scales::breaks_extended(self$options$yTicks + 1),
                                                                expand = ggplot2::expansion(mult = expFactMult))
                } else {
                    plot <- plot  + ggplot2::scale_y_continuous(labels = labelFnct,
                                                                expand = ggplot2::expansion(mult = expFactMult))
                }
            } else { # yAxis == "none
                plot <- plot  + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = expFactMult))
            }

            #### Facet ####

            if (!is.null(self$options$facet)) {
                facetVar <- rlang::sym(self$options$facet)
                # coord_flip() below needs the OPPOSITE free scale from what you'd expect:
                # free_x when horizontal, free_y otherwise (confirmed empirically, see conversation)
                # TEMP: with self$options$yAxis == "label", ggh4x::facetted_pos_scales() only assigns distinct
                # per-panel breaks correctly to BOTH x and y when the facet frees both dimensions
                # (confirmed empirically) — a single free_x/free_y silently reuses one panel's
                # scale for every panel on the non-freed dimension
                freeScale <- if (self$options$yAxis == "label") {
                    "free"
                } else if (self$options$horizontal) {
                    "free_y"
                } else {
                    "free_x"
                }
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber), scales = freeScale)
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber), scales = freeScale)

                # xScales
                facetLevels <- levels(droplevels(plotData[[self$options$facet]]))
                # Column widths are computed per facet, so each panel needs its own x breaks/labels
                xScales <- lapply(facetLevels, function(lv) {
                    subData <- plotData[plotData[[self$options$facet]] == lv, ]
                    xAxisData <- unique(subData[c("x_center", as.character(category), "xwidth")])
                    if (self$options$showCategoryPercent) {
                        xAxisLabels <- paste0(xAxisData[[category]], "\n(", doPercent(xAxisData$xwidth), ")")
                    } else {
                        xAxisLabels <- xAxisData[[category]]
                    }
                    ggplot2::scale_x_continuous(breaks = xAxisData$x_center,
                                                labels = xAxisLabels,
                                                expand = ggplot2::expansion(mult = expFactMult)
                                                )
                })

                # yScales
                if (self$options$yAxis == "label") {
                    # KNOWN LIMITATION: breaks/labels come only from the first (leftmost) column, so a
                    # group with zero count there is silently missing from the y-axis even if present
                    # in other columns.
                    yScales <- lapply(facetLevels, function(lv) {
                        subData <- plotData[plotData[[self$options$facet]] == lv, ]
                        firstColBreaks <- dplyr::filter(subData, x_center == min(x_center))
                        ggplot2::scale_y_continuous(breaks = firstColBreaks$y_center,
                                                    labels = firstColBreaks[[group]],
                                                    expand = ggplot2::expansion(mult = expFactMult))
                    })
                }

                # Apply scales using ggh4x::facetted_pos_scales
                if (self$options$yAxis == "label") {
                    plot <- plot + ggh4x::facetted_pos_scales(x = xScales, y = yScales)
                } else {
                    plot <- plot + ggh4x::facetted_pos_scales(x = xScales)
                }

            }

            #### Theme, Legend & Axes ####

            # Theme and colors
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE) # drop to include unused levels in color scales

            # Titles & Labels
            defaults <- list(y = self$options$group, x = self$options$category, legend = self$options$group)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options)

            # Legend position

            showGroupLegend <- self$options$yAxis != "label" || self$options$alwaysShowGroupLegend
            if (showGroupLegend) {
                plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(linewidth = 0.5), order = 1))
            } else {
                plot <- plot + ggplot2::guides(fill = "none")
            }

            if (self$options$showOpacityLegend) {
                plot <- plot + ggplot2::guides(alpha = ggplot2::guide_legend(override.aes = list(linewidth = 0.5), order = 2))
            } else {
                plot <- plot + ggplot2::guides(alpha = "none")
            }

            if (showGroupLegend || self$options$showOpacityLegend)
                plot <- plot + ggplot2::theme(legend.key.spacing.y = grid::unit(1, "mm"), legend.byrow = TRUE)

            # Hide axes
            if (self$options$hideAxes) {
                plot <- plot + ggplot2::theme(
                    axis.line = ggplot2::element_blank(),
                    axis.ticks = ggplot2::element_blank()
                )
            }
            if (self$options$yAxis == "none") {
                if (self$options$horizontal)
                    plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
                else
                    plot <- plot + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
            }

            vijDebugPlot(self, plot)

            return(plot)
        },
        .mosaicData = function(df, categoryName, groupName, facetName = NULL, order = "none", reverseStack = FALSE) {
            category <- rlang::sym(categoryName)
            group <- rlang::sym(groupName)
            facet <- vijVar(facetName)
            joinBy <- if (is.null(facetName)) categoryName else c(categoryName, facetName)
            residualsJoinBy <- if (is.null(facetName)) c(categoryName, groupName) else c(categoryName, groupName, facetName)

            df <- df |>
                dplyr::group_by(!!category, !!group, !!facet) |>
                dplyr::summarise(freq = sum(.data[[".COUNTS"]]), .groups = 'drop')

            # Pearson residuals (stats::chisq.test()$residuals) from a chi-squared test of
            # category x group independence, computed separately per facet
            residuals <- df |>
                dplyr::group_by(!!facet) |>
                dplyr::group_modify(function(subDf, ...) {
                    # rlang::expr()/!! build the formula from symbols directly, bypassing text
                    # parsing entirely — unlike stats::reformulate(), which breaks on a variable
                    # name containing a space or other non-syntactic character. xtabs() accepts
                    # this (a "call", not a "formula") since it calls as.formula() on it internally.
                    tab <- stats::xtabs(rlang::expr(freq ~ !!category + !!group), data = subDf)
                    # a 2x2-or-larger table is required: below that, chisq.test() either errors
                    # (fully empty table) or silently returns a 1-D vector instead of a matrix
                    # of residuals (single category or single group), which would break the
                    # column renaming below just the same
                    chisqres <- if (nrow(tab) >= 2 && ncol(tab) >= 2) {
                        tryCatch(suppressWarnings(stats::chisq.test(tab)), error = function(e) NULL)
                    } else {
                        NULL
                    }
                    if (is.null(chisqres)) {
                        # keep the same column structure as the success case, just NA,
                        # so group_modify() doesn't choke combining facets across groups
                        residDf <- unique(subDf[c(categoryName, groupName)])
                        residDf$residuals <- NA_real_
                        return(residDf)
                    }
                    residDf <- as.data.frame(chisqres$residuals)
                    names(residDf) <- c(categoryName, groupName, "residuals")
                    residDf
                }) |>
                dplyr::ungroup()

            # Global category total (summed across facets), used to order categories
            globalOrder <- df |>
                dplyr::group_by(!!category) |>
                dplyr::summarise(global_total = sum(freq), .groups = 'drop')

            widths <- df |>
                dplyr::group_by(!!category, !!facet) |>
                dplyr::summarise(x_total = sum(freq), .groups = 'drop') |>
                dplyr::left_join(globalOrder, by = categoryName) |>
                dplyr::group_by(!!facet)

            widths <- switch(order,
                             "increasing" = dplyr::arrange(widths, global_total, .by_group = TRUE),
                             "decreasing" = dplyr::arrange(widths, dplyr::desc(global_total), .by_group = TRUE),
                             dplyr::arrange(widths, !!category, .by_group = TRUE)
            )

            widths <- widths |>
                dplyr::mutate(
                    xwidth = x_total / sum(x_total),
                    x_center = (cumsum(x_total) - x_total / 2) / sum(x_total)
                ) |>
                dplyr::ungroup() |>
                dplyr::select(!!category, !!facet, x_center, xwidth)

            mosaic_data <- df |>
                dplyr::group_by(!!category, !!facet) |>
                dplyr::mutate(x_total = sum(freq)) |>
                dplyr::ungroup() |>
                dplyr::group_by(!!category, !!facet)

            mosaic_data <- if (reverseStack) {
                dplyr::arrange(mosaic_data, !!group, .by_group = TRUE)
            } else {
                dplyr::arrange(mosaic_data, dplyr::desc(!!group), .by_group = TRUE)
            }

            mosaic_data <- mosaic_data |>
                dplyr::mutate(
                    percentage = freq / x_total,
                    y_center = (cumsum(freq) - freq / 2) / x_total
                ) |>
                dplyr::ungroup() |>
                dplyr::left_join(widths, by = joinBy) |>
                dplyr::left_join(residuals, by = residualsJoinBy)
            return(mosaic_data)
        }
    )
)
