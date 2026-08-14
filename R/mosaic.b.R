
# This file is a generated template, your changes will not be overwritten

mosaicClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mosaicClass",
    inherit = mosaicBase,
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
            if (is.null(self$options$category) || is.null(self$options$group) || nrow(self$data) == 0)
                return()

            plotData <- self$data[c(self$options$category, self$options$group, self$options$facet)]
            if( self$options$ignoreNA )
                plotData <- jmvcore::naOmit(plotData)

            mdf <- private$.mosaicData(plotData, self$options$category, self$options$group, self$options$facet, self$options$order)

            image <- self$results$plot
            image$setState(mdf)
        },
        .mosaicData = function(df, categoryName, groupName, facetName = NULL, order = "none") {
            category <- rlang::sym(categoryName)
            group <- rlang::sym(groupName)
            facet <- if (!is.null(facetName)) rlang::sym(facetName) else NULL
            freq <- rlang::sym("Freq")
            joinBy <- if (is.null(facetName)) rlang::as_string(category) else c(rlang::as_string(category), facetName)

            df <- df |>
                dplyr::group_by(!!category, !!group, !!facet) |>
                dplyr::summarise(Freq = dplyr::n(), .groups = 'drop')

            # Global category total (summed across facets), used to order categories
            globalOrder <- df |>
                dplyr::group_by(!!category) |>
                dplyr::summarise(global_total = sum(!!freq), .groups = 'drop')

            widths <- df |>
                dplyr::group_by(!!category, !!facet) |>
                dplyr::summarise(x_total = sum(!!freq), .groups = 'drop') |>
                dplyr::left_join(globalOrder, by = rlang::as_string(category)) |>
                dplyr::group_by(!!facet)

            widths <- switch(order,
                "increasing" = dplyr::arrange(widths, global_total, .by_group = TRUE),
                "decreasing" = dplyr::arrange(widths, dplyr::desc(global_total), .by_group = TRUE),
                dplyr::arrange(widths, !!category, .by_group = TRUE)
            )

            widths <- widths |>
                dplyr::mutate(
                    xmax = cumsum(x_total) / sum(x_total),
                    xmin = xmax - (x_total / sum(x_total)),
                    xwidth = xmax - xmin,
                    x_center = xmin + (xmax - xmin) / 2
                ) |>
                dplyr::ungroup() |>
                dplyr::select(!!category, !!facet, xmin, xmax, x_center, xwidth)

            mosaic_data <- df |>
                dplyr::group_by(!!category, !!facet) |>
                dplyr::mutate(x_total = sum(!!freq)) |>
                dplyr::ungroup() |>
                dplyr::group_by(!!category, !!facet) |>
                dplyr::arrange(desc(!!group), .by_group = TRUE) |>
                dplyr::mutate(
                    ymax = cumsum(!!freq) / x_total,
                    ymin = ymax - (!!freq / x_total),
                    pourcentage = !!freq / x_total,
                    freq = !!freq,
                    y_center = ymin + (ymax - ymin) / 2
                ) |>
                dplyr::ungroup() |>
                dplyr::left_join(widths, by = joinBy)
            return(mosaic_data)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state

            #### Build plot ####

            category <- rlang::sym(self$options$category)
            group <- rlang::sym(self$options$group)

            plot <- ggplot2::ggplot(plotData)

            #### Rectangles ####

            plot <- plot + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = !!group),
                                              color = "white", linewidth = 1)

            #### Label ####

            doPercent <- scales::label_percent(
                accuracy = as.numeric(self$options$accuracy),
                suffix = '\u2009%',
                decimal.mark = self$options[['decSymbol']])

            if (self$options$labelType != "none") {
                if (self$options$labelType == "count") {
                    if (self$options$labelColor == "auto") {
                        labelAes <- ggplot2::aes(x = x_center, y = y_center, label = freq, fill = !!group,
                                                 color = ggplot2::after_scale(ggstats::hex_bw(.data$fill)))
                        textColor <- NULL
                    } else {
                        labelAes <- ggplot2::aes(x = x_center, y = y_center, label = freq)
                        textColor <- self$options$labelColor
                    }
                } else if (self$options$labelType == "percent") {
                    if (self$options$labelColor == "auto") {
                        labelAes <- ggplot2::aes(x = x_center, y = y_center, label = doPercent(pourcentage), fill = !!group,
                                                 color = ggplot2::after_scale(ggstats::hex_bw(.data$fill)))
                        textColor <- NULL
                    } else {
                        labelAes <- ggplot2::aes(x = x_center, y = y_center, label = doPercent(pourcentage))
                        textColor <- self$options$labelColor
                    }
                }
                plot <- plot + ggplot2::geom_text(data = dplyr::filter(plotData, pourcentage > 0.05 &  xwidth > 0.05),
                                                  mapping = labelAes, color = textColor,
                                                  size = self$options$labelFontSize / ggplot2::.pt, fontface = "bold")

            }

            if (is.null(self$options$facet)) {
                plot <- plot + ggplot2::scale_x_continuous(breaks = unique(plotData$x_center), labels = unique(plotData[[category]]))
            }

            # Axis Limits & flip
            if (self$options$horizontal) {
                plot <- plot + ggplot2::coord_flip()
            }

            # Ticks
            labelFnct <- scales::label_percent(suffix = '\u2009%', decimal.mark = self$options[['decSymbol']])

            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(labels = labelFnct, breaks = scales::breaks_extended(self$options$xTicks + 1))
            } else if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(labels = labelFnct, breaks = scales::breaks_extended(self$options$yTicks + 1))
            } else {
                plot <- plot  + ggplot2::scale_y_continuous(labels = labelFnct)
            }

            #### facet ####
            if (!is.null(self$options$facet)) {
                facetVar <- rlang::sym(self$options$facet)

                # Column widths are computed per facet, so each panel needs its own x breaks/labels
                facetLevels <- levels(droplevels(plotData[[self$options$facet]]))
                xScales <- lapply(facetLevels, function(lv) {
                    subData <- plotData[plotData[[self$options$facet]] == lv, ]
                    ggplot2::scale_x_continuous(breaks = unique(subData$x_center), labels = unique(subData[[category]]))
                })

                # coord_flip() below needs the OPPOSITE free scale from what you'd expect:
                # free_x when horizontal, free_y otherwise (confirmed empirically, see conversation)
                freeScale <- if (self$options$horizontal) "free_y" else "free_x"
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber), scales = freeScale)
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber), scales = freeScale)

                plot <- plot + ggh4x::facetted_pos_scales(x = xScales)
            }


            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill", drop = FALSE) # drop to include unused levels in color scales

            # Titles & Labels
            defaults <- list(y = self$options$group, x = self$options$category, legend = self$options$group)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)

            # Legend position
            plot <- plot + ggplot2::theme(legend.key.spacing.y = grid::unit(1, "mm"), legend.byrow = TRUE)
            #plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))

            vijDebugMessage(self, plot)

            return(plot)

        },
        # Demonstration only — not applied to R/mosaic.b.R.
        # .mosaicData() with a `gap` parameter: fraction of the [0,1] axis reserved
        # for EACH inter-rectangle gap (x direction: between categories within a facet;
        # y direction: between group segments within a category+facet).
        # gap = 0 reproduces the exact current geometry.

        .mosaicData2 = function(df, categoryName, groupName, facetName = NULL, gap = 0.01) {
            category <- rlang::sym(categoryName)
            group <- rlang::sym(groupName)
            facet <- if (!is.null(facetName)) rlang::sym(facetName) else NULL
            freq <- rlang::sym("Freq")
            joinBy <- if (is.null(facetName)) rlang::as_string(category) else c(rlang::as_string(category), facetName)

            df <- df |>
                dplyr::group_by(!!category, !!group, !!facet) |>
                dplyr::summarise(Freq = dplyr::n(), .groups = 'drop')

            mosaic_data <- df |>
                dplyr::group_by(!!category, !!facet) |>
                dplyr::mutate(x_total = sum(!!freq)) |>
                dplyr::ungroup() |>
                dplyr::group_by(!!category, !!facet) |>
                dplyr::arrange(desc(!!group), .by_group = TRUE) |>
                dplyr::mutate(
                    n_group = dplyr::n(),
                    pourcentage = !!freq / x_total,
                    # shrink each slice so (n_group - 1) gaps of size `gap` fit in [0,1],
                    # then reinsert the gaps between cumulative positions
                    scaledHeight = pourcentage * pmax(0, 1 - gap * (n_group - 1)),
                    ymax = cumsum(scaledHeight) + gap * (dplyr::row_number() - 1),
                    ymin = ymax - scaledHeight,
                    freq = !!freq,
                    y_center = ymin + (ymax - ymin) / 2
                ) |>
                dplyr::ungroup() |>
                dplyr::select(-n_group, -scaledHeight) |>
                dplyr::left_join(
                    df |>
                        dplyr::group_by(!!category, !!facet) |>
                        dplyr::summarise(x_total = sum(!!freq), .groups = 'drop') |>
                        dplyr::group_by(!!facet) |>
                        dplyr::arrange(!!category, .by_group = TRUE) |>
                        dplyr::mutate(
                            n_cat = dplyr::n(),
                            p = x_total / sum(x_total),
                            scaledWidth = p * pmax(0, 1 - gap * (n_cat - 1)),
                            xmax = cumsum(scaledWidth) + gap * (dplyr::row_number() - 1),
                            xmin = xmax - scaledWidth,
                            xwidth = xmax - xmin,
                            x_center = xmin + (xmax - xmin) / 2
                        ) |>
                        dplyr::ungroup() |>
                        dplyr::select(!!category, !!facet, xmin, xmax, x_center, xwidth),
                    by = joinBy
                )
            return(mosaic_data)
        }

        # --- .plot() geom_rect: linewidth is no longer needed for separation ---
        # plot <- plot + ggplot2::geom_rect(
        #     ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = !!group),
        #     color = NA)   # or a hairline (linewidth = 0.2) purely for anti-aliasing, not for spacing
        )
)
