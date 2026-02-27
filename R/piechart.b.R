
# This file is a generated template, your changes will not be overwritten

piechartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "piechartClass",
    inherit = piechartBase,
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
        },

        .run = function() {
            if (!is.null(self$options$aVar) && nrow(self$data) != 0) {
                plotData <- self$data[c(self$options$aVar, self$options$facet)]
                plotData <- jmvcore::naOmit(plotData)
                image <- self$results$plot
                image$setState(plotData)
            } else {
                vijWarningMessage(self, .("Pie charts are for educational use only. Please do not use Pie charts!"))
            }
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state
            aVar <- self$options$aVar
            aVar <- ensym(aVar)

            if (!is.null(self$options$facet) ) {
                facetVar <- self$options$facet
                facetVar <- ensym(facetVar)
            } else {
                facetVar <- NULL
            }

            # set the border color
            if (self$options$borderColor == "none") {
                borderColor <- NA
            } else {
                borderColor = self$options$borderColor
            }

            # Percent format (scales)
            doPercent <- scales::label_percent(
                accuracy = as.numeric(self$options$accuracy),
                suffix = '\u2009%',
                decimal.mark = self$options[['decSymbol']])

            if(self$options$donut) {
                plot <- ggplot(plotData, aes(x = 10, fill = !!aVar, by = 1)) + xlim(c(8.5,NA))
                xOffset <- 10
            } else {
                plot <- ggplot(plotData, aes(x = "", fill = !!aVar, by = 1))
                xOffset <- 1
            }

            plot <- plot + geom_bar(position = "fill", color = borderColor, show.legend = TRUE) + coord_polar("y")

            if (self$options$labType == "text") {
                if (self$options$overlap)
                    geomLab <- ggrepel::geom_text_repel
                else
                    geomLab <- geom_text
            } else {
                if (self$options$overlap)
                    geomLab <- ggrepel::geom_label_repel
                else
                    geomLab <- geom_label
            }

            if (self$options$labels != "none") {
                if (self$options$textColor == "auto") { # using hex_bw
                    plot <- plot + geomLab(aes(x = self$options$labOffset/10 + xOffset,
                                               label = switch(self$options$labels,
                                                              "count" = after_stat(count),
                                                              "percent" = doPercent(after_stat(prop)),
                                                              "group" = fill,
                                                              "group+count" = paste0(fill, "\n", after_stat(count)),
                                                              "group+percent" = paste0(fill, "\n", doPercent(after_stat(prop)))),
                                                color = after_scale(ggstats::hex_bw(.data$fill))
                                               ),
                                            stat = switch(self$options$labels, "percent" = ggstats::StatProp, "group+percent" = ggstats::StatProp, "count"),
                                            position = position_fill(vjust = 0.5), direction = "both",
                                            fontface = "bold", size = self$options$labSize / .pt,
                                            show.legend = FALSE, seed = 123, min.segment.length = 1)
                } else {
                    plot <- plot + geomLab(aes(x = self$options$labOffset/10 + xOffset,
                                               label = switch(self$options$labels,
                                                              "count" = after_stat(count),
                                                              "percent" = doPercent(after_stat(prop)),
                                                              "group" = fill,
                                                              "group+count" = paste0(fill, "\n", after_stat(count)),
                                                              "group+percent" = paste0(fill, "\n", doPercent(after_stat(prop)))),
                                                ),
                                           stat = switch(self$options$labels, "percent" = ggstats::StatProp, "group+percent" = ggstats::StatProp, "count"),
                                           position = position_fill(vjust = 0.5), direction = "both",
                                           color = self$options$textColor,
                                           fontface = "bold", size = self$options$labSize / .pt,
                                           show.legend = FALSE, seed = 123, min.segment.length = 1)
                }
            }

            # Facet
            if (!is.null(facetVar) ) {
                if (self$options$facetBy == "column")
                    plot <- plot + facet_wrap(vars(!!facetVar), ncol = as.numeric(self$options$facetNumber), scales = "free")
                else
                    plot <- plot + facet_wrap(vars(!!facetVar), nrow = as.numeric(self$options$facetNumber), scales = "free")
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "fill", drop = FALSE)

            # Guide
            if (self$options$labels %in% c("group","group+count","group+percent"))
                plot <- plot + guides(fill = "none")

            # Titles & Labels
            defaults <- list(y = "", x = "", legend = aVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            # Labs
            plot <- plot + theme(axis.ticks = element_blank(),
                                 axis.line.x = element_blank(), axis.line.y = element_blank(),
                                 axis.text.x = element_blank(),axis.text.y = element_blank(),
                                 panel.grid.major = element_blank(), panel.grid.minor = element_blank())

            return(plot)
        }

    )
)
