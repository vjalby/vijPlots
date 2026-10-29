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

            yName <- self$options$yVar
            xName <- self$options$xVar

            # ".X", ".Y", ".YMIN", ".YMAX", ".TOTAL", ".END" and ".PY" are the column names of the summaries computed below
            reservedVar <- intersect(c(yName, xName, self$options$group, self$options$facet),
                                     c(".X", ".Y", ".YMIN", ".YMAX", ".TOTAL", ".END", ".PY"))
            if (length(reservedVar) > 0)
                vijErrorMessage(self, jmvcore::format(.("\"{var}\" is a reserved name. Please rename this variable."), var = reservedVar[1]))

            plotData <- jmvcore::select(self$data, c(yName, xName, self$options$group, self$options$facet))

            plotData[[yName]] <- jmvcore::toNumeric(plotData[[yName]])
            # missing data (and infinite values, as stat_summary() used to drop them)
            plotData <- plotData[is.finite(plotData[[yName]]),]
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

            barFun <- switch(self$options$yaxis,
                mean = mean, median = stats::median, min = min, max = max, sum = sum)

            # Position of the bars on the axis (.X): sorted on the raw data, before it is summarised.
            # The x variable itself keeps its level order, which the fill colours follow.
            if (self$options$order != "none")
                plotData[[".X"]] <- forcats::fct_reorder(plotData[[xName]], plotData[[yName]], .fun = barFun,
                                                         .desc = (self$options$order == "decreasing"))
            else
                plotData[[".X"]] <- plotData[[xName]]

            ciLevel <- self$options$ciLevel / 100
            cellFun <- switch(private$.errorBars(),
                sd = private$.meanSd,
                se = private$.meanSe,
                ci = if (self$options$bootstrap)
                         function(x) private$.meanClBoot(x, conf.int = ciLevel)
                     else
                         function(x) private$.meanClNormal(x, conf.int = ciLevel),
                none = function(x) data.frame(.Y = barFun(x), .YMIN = NA_real_, .YMAX = NA_real_))

            rawData <- plotData

            # Summarize per x-value/group/facet, with its value (.Y) and its error bar limits (.YMIN, .YMAX).
            # xName is kept for the color order
            plotData <- plotData |>
                dplyr::group_by(dplyr::pick(dplyr::all_of(c(self$options$group, ".X", xName, self$options$facet)))) |>
                dplyr::summarise(cellFun(.data[[yName]]), .groups = "drop") |>
                as.data.frame()

            # Stacked sums: net total per x (and facet), and the end of the stack on the side of its sign, where its
            # label goes (ggplot2 stacks positive and negative values apart, on each side of 0)
            totals <- NULL
            if (self$options$barType == "stack" && !is.null(self$options$group) && self$options$yaxis == "sum")
                totals <- plotData |>
                    dplyr::group_by(dplyr::pick(dplyr::all_of(c(".X", self$options$facet)))) |>
                    dplyr::summarise(.TOTAL = sum(.data[[".Y"]]),
                                     .END = if (.TOTAL >= 0) sum(pmax(.data[[".Y"]], 0)) else sum(pmin(.data[[".Y"]], 0)),
                                     .groups = "drop") |>
                    as.data.frame()

            # Individual observations (only when drawn): their x position, value, group and facet
            points <- NULL
            if (private$.showPoints()) {
                points <- rawData[c(".X", self$options$group, self$options$facet)]
                points[[".PY"]] <- rawData[[yName]]
            }

            image <- self$results$plot
            image$setState(list(data = plotData, totals = totals, points = points))
        },
        .plot = function(image, ggtheme, theme, ...) {  # <-- the plot function
            # Long by design (reviewed 2026-10-09): a linear `plot <- plot + ...` build; splitting
            # it would scatter the plot's construction without simplifying it.
            if (is.null(image$state))
                return(FALSE)
            plotData <- image$state$data

            yVar <- rlang::sym(".Y")
            xVar <- rlang::sym(self$options$xVar)
            groupVar <- vijVar(self$options$group)
            # Missing values (ignoreNA unchecked) shown as "(Missing)": axis, legend, facet strips
            labelNA <- vijLabelNA(self)
            fillVar <- groupVar %||% xVar

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
                # no error bars on stacked bars (see .errorBars())
            } else if (dodge2) {
                position <- ggplot2::position_dodge2(preserve = "single", width = 0.9)
                labPosition <- ggplot2::position_dodge2(preserve = "single", width = 0.9)
                # Error bars laid out with the bars' width (0.9): dodge2 places each element according to its
                # own width, so with their narrower width they were offset in a category with fewer bars.
                # padding then draws them narrower around the same centre: a bar is drawn 0.9 of its slot
                # (dodge2's default padding = 0.1), so padding = 1 - 0.9 * errorBarWidth makes the cap
                # errorBarWidth times the bar's width (1 = as wide as the bar)
                errorWidth <- 0.9
                errorPosition <- ggplot2::position_dodge2(preserve = "single", width = 0.9,
                                                          padding = 1 - 0.9 * self$options$errorBarWidth)
            } else {
                position <- ggplot2::position_dodge(width = 0.9)
                labPosition <- ggplot2::position_dodge(width = 0.9)
                # A bar takes its whole slot (0.9 / number of bars), as does an element 0.9 wide: the cap is
                # errorBarWidth times the bar's width (1 = as wide as the bar)
                errorWidth <- 0.9 * self$options$errorBarWidth
                errorPosition <- ggplot2::position_dodge(width = 0.9)
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

            #### Bars ####
            plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data[[".X"]], y = !!yVar, group = !!groupVar, fill = !!fillVar))

            if(singleColor)
                plot <- plot + ggplot2::geom_col(position = position, color = borderColor, fill = oneColorOfPalette)
            else
                plot <- plot + ggplot2::geom_col(position = position, color = borderColor)

            #### Observations ####
            # Over the bars, under the value labels and the error bars (layers are drawn in the order they are added).
            # Laid out like the bars, see .positionJitterDodge()
            if (!is.null(image$state$points)) {
                pointShape <- switch(self$options$pointShape, circle = 16, opencircle = 1, square = 15)
                plot <- plot + ggplot2::geom_point(data = image$state$points, inherit.aes = FALSE,
                                                   ggplot2::aes(x = .data[[".X"]], y = .data[[".PY"]], group = !!groupVar),
                                                   position = private$.positionJitterDodge(single = dodge2),
                                                   shape = pointShape, size = self$options$pointSize, color = self$options$pointColor,
                                                   show.legend = FALSE)
            }

            #### Value labels ####
            if (self$options$showLabels) {
                labelSize <- self$options$labelTextSize / ggplot2::.pt
                digits <- self$options$decimalPrecision
                # Labels drawn outside their bars, beyond the bar's end on the side of the value's sign: above
                # (right) of a positive bar, below (left) of a negative one. Horizontal: hjust 0/1 with two spaces
                # between the bar and the text, i.e. the same gap whatever the label length (a negative hjust is
                # a fraction of the label's own width)
                horizontal <- self$options$horizontal
                outsideText <- function(value) {
                    text <- round(value, digits)
                    if (horizontal) ifelse(value < 0, paste0(text, "  "), paste0("  ", text)) else paste0(text)
                }
                outsideVjust <- function(value) if (horizontal) 0.5 else ifelse(value < 0, 1.5, -0.5)
                outsideHjust <- function(value) if (horizontal) ifelse(value < 0, 1, 0) else 0.5
                # Labels of stacked bars are centred inside each bar
                # No label on a bar (or a stacked segment) whose value rounds to 0
                if (stacked) {
                    labelText <- function(value) ifelse(round(value, digits) == 0, "", paste0(round(value, digits)))
                    labelVjust <- function(value) 0.5
                    labelHjust <- function(value) 0.5
                } else {
                    labelText <- function(value) ifelse(round(value, digits) == 0, "", outsideText(value))
                    labelVjust <- outsideVjust
                    labelHjust <- outsideHjust
                }

                textColor <- self$options$textColor
                if (!stacked && textColor == "auto")
                    textColor <- "black"

                geomText <- function(...) ggplot2::geom_text(..., size = labelSize, fontface = if (stacked) "bold" else "plain",
                                                             position = labPosition)
                if (textColor == "auto")
                    plot <- plot + geomText(ggplot2::aes(label = labelText(!!yVar), vjust = labelVjust(!!yVar), hjust = labelHjust(!!yVar),
                                                         color = ggplot2::after_scale(ggstats::hex_bw(.data$fill))))
                else
                    plot <- plot + geomText(ggplot2::aes(label = labelText(!!yVar), vjust = labelVjust(!!yVar), hjust = labelHjust(!!yVar)),
                                            color = textColor)

                # Label for stacked sum: the net total, at the end of the stack on the side of its sign (see .run()),
                # so that with mixed signs it doesn't fall inside the stack, among the segments' labels
                totals <- image$state$totals
                if (!is.null(totals)) {
                    textColor2 <- ifelse(textColor %in% c("auto", "white"), "black", textColor)
                    plot <- plot + ggplot2::geom_text(data = totals, inherit.aes = FALSE,
                                                      ggplot2::aes(x = .data[[".X"]], y = .data[[".END"]],
                                                                   label = outsideText(.data[[".TOTAL"]]),
                                                                   vjust = outsideVjust(.data[[".TOTAL"]]),
                                                                   hjust = outsideHjust(.data[[".TOTAL"]])),
                                                      size = labelSize, color = textColor2)
                }
            }

            #### ErrorBars ####
            errorBars <- private$.errorBars()
            if (errorBars != "none")
                plot <- plot +  ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[[".YMIN"]], ymax = .data[[".YMAX"]]),
                                             width = errorWidth, linewidth = self$options$errorBarLineSize,
                                             color = self$options$errorBarColor, position = errorPosition)

            # Show unused levels (if checked in data/var setting)
            plot <- plot + ggplot2::scale_x_discrete(drop = FALSE, labels = labelNA)

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
            if (self$options$showLabels && (!stacked || !is.null(image$state$totals))) {
                # Room for the labels beyond the bars' ends: on the positive side, and on the negative side too
                # when a bar (or, stacked, a total) is negative: its label is then drawn below/left of it
                upper <- if (self$options$horizontal) 0.2 else 0.1
                lower <- if (any((image$state$totals$.TOTAL %||% plotData$.Y) < 0, na.rm = TRUE)) upper else 0.05
                if (self$options$horizontal && self$options$xAxisRangeType == "auto")
                    expand_arg <- ggplot2::expansion(mult = c(lower, upper))
                else if (!self$options$horizontal && self$options$yAxisRangeType == "auto")
                    expand_arg <- ggplot2::expansion(mult = c(lower, upper))
            }
            if (self$options$horizontal && self$options$xTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1), expand = expand_arg)
            } else if (!self$options$horizontal && self$options$yTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1), expand = expand_arg)
            } else if(!inherits(expand_arg, "waiver")) {
                plot <- plot  + ggplot2::scale_y_continuous(expand = expand_arg)
            }

            #### Axis Labels ####
            ylabel <- private$.getYLabel(self$options$yVar, errorBars)

            #### facet ####
            if (!is.null(self$options$facet)) {
                facetVar <- rlang::sym(self$options$facet)
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), ncol = as.numeric(self$options$facetNumber),
                                                       labeller = ggplot2::as_labeller(labelNA))
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facetVar), nrow = as.numeric(self$options$facetNumber),
                                                       labeller = ggplot2::as_labeller(labelNA))
            }

            #### Theme and colors ####
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE, labels = labelNA) # drop to include unused levels in color scales

            #### Grid lines (value axis only) ####
            plot <- plot + vijGridLines(self$options$theme,
                                        x = self$options$horizontal && self$options$xGridLines,
                                        y = !self$options$horizontal && self$options$yGridLines)

            #### Titles & Labels ####
            defaults <- list(y = ylabel, x = xVar, legend = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options, showLegend = !is.null(groupVar))

            vijDebugPlot(self, plot)

            return(plot)
        },
        # Error bars actually drawn: none on stacked bars, nor around anything but a mean
        .errorBars = function() {
            stacked <- (self$options$barType == "stack" && !is.null(self$options$group))
            if (stacked || self$options$yaxis != "mean")
                "none"
            else
                self$options$errorBars
        },
        # Observations actually drawn: none on stacked bars with a group (raw values don't belong on a stack of means),
        # nor around a sum, a minimum or a maximum (the bar is then not a typical value of the points)
        .showPoints = function() {
            self$options$showPoints && self$options$yaxis %in% c("mean", "median") &&
                !(self$options$barType == "stack" && !is.null(self$options$group))
        },
        # Position of the observations over the bars: each bar's slot is worked out from the bars (one per x and
        # group), not from the points; the points then get a random sideways spread inside it (60% of the slot).
        # `single` ("dodge2" bars, position_dodge2(preserve = "single")): slot = 0.9 / largest number of bars at any x,
        # bars centred; otherwise ("dodge" bars, position_dodge): slot = 0.9 / number of bars at this x.
        # Not ggplot2's own positions: position_dodge2() would treat every point as a bar, and position_jitterdodge()
        # needs a fill/colour aesthetic to dodge by before ggplot2 4.0 (jamovi 2.7 ships 3.5), and doesn't follow the
        # bars in an incomplete category. The spread of each bar is seeded by its own values (as in .meanClBoot): the
        # same values always give the same points, and changing another bar doesn't move them.
        .positionJitterDodge = function(single, jitterWidth = 0.6) {
            positionClass <- ggplot2::ggproto("PositionJitterDodge", ggplot2::Position,
                required_aes = c("x", "y"),
                width = 0.9,
                single = single,
                jitter.width = jitterWidth,
                setup_params = function(self, data) {
                    bars <- unique(data[c("PANEL", "x", "group")])
                    list(n = max(table(bars$PANEL, as.numeric(bars$x))))
                },
                compute_panel = function(self, data, params, scales) {
                    bars <- unique(data[c("x", "group")])
                    bars <- bars[order(bars$x, bars$group), ]
                    xs <- as.numeric(bars$x)
                    nBars <- stats::ave(bars$group, xs, FUN = length)
                    rank <- stats::ave(bars$group, xs, FUN = seq_along)
                    bars$slot <- self$width / (if (self$single) params$n else nBars)
                    bars$center <- xs + bars$slot * (rank - (nBars + 1) / 2)
                    bar <- match(paste(as.numeric(data$x), data$group), paste(xs, bars$group))
                    jitter <- stats::ave(data$y, bar, FUN = function(y) {
                        withr::with_seed(strtoi(substr(rlang::hash(y), 1, 7), 16L), stats::runif(length(y), -0.5, 0.5))
                    })
                    data$x <- bars$center[bar] + jitter * bars$slot[bar] * self$jitter.width
                    data
                })
            ggplot2::ggproto(NULL, positionClass)
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
        },
        # Error bar summaries (mean and limits of one bar), replacing ggplot2's
        # mean_sdl/mean_cl_normal/mean_cl_boot, which require Hmisc
        .meanSd = function(x) {
            x <- x[!is.na(x)]
            m <- mean(x)
            s <- stats::sd(x)
            data.frame(.Y = m, .YMIN = m - s, .YMAX = m + s)
        },
        .meanSe = function(x) {
            x <- x[!is.na(x)]
            m <- mean(x)
            se <- stats::sd(x) / sqrt(length(x))
            data.frame(.Y = m, .YMIN = m - se, .YMAX = m + se)
        },
        .meanClNormal = function(x, conf.int = 0.95) {
            x <- x[!is.na(x)]
            n <- length(x)
            m <- mean(x)
            if (n < 2)
                return(data.frame(.Y = m, .YMIN = NA_real_, .YMAX = NA_real_))
            h <- stats::qt((1 + conf.int) / 2, n - 1) * stats::sd(x) / sqrt(n)
            data.frame(.Y = m, .YMIN = m - h, .YMAX = m + h)
        },
        .meanClBoot = function(x, conf.int = 0.95, B = 1000) {
            x <- x[!is.na(x)]
            n <- length(x)
            m <- mean(x)
            if (n < 2)
                return(data.frame(.Y = m, .YMIN = NA_real_, .YMAX = NA_real_))
            # Seed derived from the data: the same values always give the same limits,
            # and each bar gets its own resampling sequence
            means <- withr::with_seed(strtoi(substr(rlang::hash(x), 1, 7), 16L),
                                      vapply(seq_len(B), function(i) mean(x[sample.int(n, n, replace = TRUE)]), numeric(1)))
            q <- stats::quantile(means, c((1 - conf.int) / 2, (1 + conf.int) / 2), names = FALSE)
            data.frame(.Y = m, .YMIN = q[1], .YMAX = q[2])
        })
)
