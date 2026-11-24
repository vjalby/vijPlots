qqplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "qqplotClass",
    inherit = qqplotBase,
    private = list(
        .init = function() {
            # Stretchable dimensions (one plot per group: grid from facetBy and facetNumber)
            separate <- !is.null(self$options$group) && self$options$plotPerGroup
            if (separate) {
                nbOfFacet <- nlevels(self$data[[self$options$group]])
                if (self$options$facetBy == "column") {
                    nbOfColumn <- self$options$facetNumber
                    nbOfRow <- ceiling(nbOfFacet / nbOfColumn)
                } else {
                    nbOfRow <- self$options$facetNumber
                    nbOfColumn <- ceiling(nbOfFacet / nbOfRow)
                }
                width <- max(450, 350 * nbOfColumn)
                height <- max(450, 350 * nbOfRow)
            } else {
                width <- 450
                height <- 450
            }
            # Fixed dimensions
            fixed_height <- 50 # X-Axis legend
            fixed_width <- 75 # Y-Axis legend
            # Legend
            if (!is.null(self$options$group) && !separate) {
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
            # Show help
            if (is.null(self$options$dep)) {
                private$.showHelpMessage()
            }
        },
        .run = function() {
            depVar <- self$options$dep
            groupVar <- self$options$group

            if (is.null(depVar) || nrow(self$data) == 0)
                return(FALSE)

            data <- jmvcore::select(self$data, c(depVar,groupVar))
            data[[depVar]] <- vijToNumeric(data[[depVar]])

            data <- jmvcore::naOmit(data)
            if (nrow(data) == 0)
                return(FALSE)
            if (nrow(data) < 2)
                vijErrorMessage(self, .("At least two observations are required."))

            #### Data transformation ####

            if (self$options$transLog) {
                if (min(data[[depVar]]) > 0) {
                    data[[depVar]] <- log(data[[depVar]])
                } else {
                    vijErrorMessage(self, .("Natural Log Transform requires positive (>0) data."))
                }
            }

            if (stats::sd(data[[depVar]]) == 0) {
                vijErrorMessage(self, .("The variance of the variable is equal to zero."))
            }

            byGroup <- !is.null(groupVar) && self$options$paramMethod == "paraEstimate"
            standardizeByGroup <- !is.null(groupVar) && self$options$standardize
            levels <- if (byGroup || standardizeByGroup) levels(droplevels(data[[groupVar]])) else NULL
            for (level in levels) {
                x <- data[[depVar]][data[[groupVar]] == level]
                if (length(x) < 2)
                    vijErrorMessage(self, jmvcore::format(.("At least two observations are required in group {group}."), group = level))
                if (stats::sd(x) == 0)
                    vijErrorMessage(self, jmvcore::format(.("The variance of the variable is equal to zero in group {group}."), group = level))
            }

            if (standardizeByGroup)
                data[[depVar]] <- stats::ave(data[[depVar]], data[[groupVar]], FUN = function(x) (x - mean(x)) / stats::sd(x))
            else if (self$options$standardize)
                data[[depVar]] <- (data[[depVar]] - mean(data[[depVar]]))/stats::sd(data[[depVar]])

            #### Data/distribution compatibility ####

            distrib <- self$options$distrib

            if (self$options$standardize && distrib %in% c("lnorm", "chisq", "f", "gamma", "weibull", "exp", "beta"))
                vijErrorMessage(self, jmvcore::format(.("Standardized data cannot fit {distrib} distribution."), distrib = private$.distTitleName(distrib)))

            varMin <- min(data[[depVar]])
            varMax <- max(data[[depVar]])
            if (varMin <= 0 && distrib %in% c("lnorm", "chisq", "f", "gamma", "weibull"))
                vijErrorMessage(self, jmvcore::format(.("{distrib} distribution requires positive (>0) data."), distrib = private$.distTitleName(distrib)))
            if (varMin < 0 && distrib == "exp")
                vijErrorMessage(self, .("Exponential distribution requires non-negative (≥0) data."))
            if (distrib == "beta" && (varMin <=0 || varMax >= 1))
                vijErrorMessage(self, .("Beta distribution requires data between 0 and 1."))

            #### Parameter estimations ####

            if (self$options$paramMethod == "paraEstimate") {
                if (byGroup)
                    params <- sapply(levels, function(level) private$.estimateParameters(distrib, data[[depVar]][data[[groupVar]] == level], level), simplify = FALSE)
                else
                    params <- private$.estimateParameters(distrib, data[[depVar]])
            } else {
                params <- private$.userParameters(distrib, self$options$param1, self$options$param2)
                if (is.null(params)) {
                    vijErrorMessage(self, .("Wrong parameter values."))
                }
            }

            #### Populate the Parameters table ####

            table <- self$results$paramTable
            table$getColumn('var')$setTitle(private$.distTitleName(distrib))
            paramSets <- if (byGroup) params else list(params)
            if (byGroup)
                table$addColumn('group', title = groupVar, type = 'text')
            for (name in names(paramSets[[1]]))
                table$addColumn(name, type = 'number', format = 'zto')
            varName <- ifelse(self$options$transLog, paste0("LN(", depVar, ")"), depVar)
            for (i in seq_along(paramSets)) {
                values <- c(list(var = varName), if (byGroup) list(group = names(paramSets)[i]), as.list(paramSets[[i]]))
                if (i == 1)
                    table$setRow(rowNo = 1, values)
                else
                    table$addRow(rowKey = i, values = values)
            }
            if (self$options$standardize)
                table$setNote("1", if (standardizeByGroup) .("Values standardized within each group") else .("Standardized values"), init = TRUE)
            if (self$options$paramMethod == "paraEstimate"){
                if (self$options$paramEstMethod == "mle")
                    self$results$paramTable$setTitle(.("Parameter Estimates (MLE)"))
                else
                    self$results$paramTable$setTitle(.("Parameter Estimates (MME)"))
            } else {
                self$results$paramTable$setTitle(.("Parameter Values"))
            }

            #### Points, reference line and band ####

            if (self$options$type == "QQ" && self$options$band && self$options$methodQQ == "ts" && distrib != "norm")
                vijWarningMessage(self, .("Tail-sensitive confidence bands are only valid for the Normal distribution."))

            image <- self$results$plot
            image$setState(list(qq = private$.qqPlotData(data, params, byGroup)))
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            qq <- image$state$qq
            if (is.null(qq)) # State saved by vijPlots <= 1.4.1 (restored by jamovi 2.7)
                qq <- private$.qqPlotData(image$state$data, image$state$params)

            groupVar <- vijVar(self$options$group)
            # One plot per group: no color per group, one panel per group
            colorVar <- if (self$options$plotPerGroup) NULL else groupVar

            distrib <- self$options$distrib

            if (self$options$detrend)
                detrend <- TRUE
            else
                detrend <- FALSE

            identity <- (self$options$refType == "identity")

            # Do the plot
            if (is.null(colorVar))
                plot <- ggplot2::ggplot()
            else
                plot <- ggplot2::ggplot(mapping = ggplot2::aes(color = .data$.GROUP, fill = .data$.GROUP))

            xy <- ggplot2::aes(x = .data$x, y = .data$y)
            band <- ggplot2::aes(x = .data$x, ymin = .data$lower, ymax = .data$upper)
            if (self$options$band) {
                # Outline (drawn with groups only): 0.1 for the Q-Q band, as qqplotr's geom; default width for the P-P band
                if (is.null(colorVar) && self$options$type == "QQ")
                    plot <- plot + ggplot2::geom_ribbon(data = qq$band, band, alpha = 0.5, linewidth = 0.1, fill = "darkgrey")
                else if (is.null(colorVar))
                    plot <- plot + ggplot2::geom_ribbon(data = qq$band, band, alpha = 0.5, fill = "darkgrey")
                else if (self$options$type == "QQ")
                    plot <- plot + ggplot2::geom_ribbon(data = qq$band, band, alpha = 0.5, linewidth = 0.1)
                else
                    plot <- plot + ggplot2::geom_ribbon(data = qq$band, band, alpha = 0.5)
            }
            if (self$options$refLine) {
                if (self$options$type == "PP")
                    plot <- plot + ggplot2::annotate("segment", x = 0, y = 0, xend = 1, yend = ifelse(detrend, 0, 1), color = "darkgrey")
                else if (is.null(colorVar) || identity)
                    plot <- plot + ggplot2::geom_path(data = qq$line, xy, linewidth = 0.8, color = "darkgrey")
                else
                    plot <- plot + ggplot2::geom_path(data = qq$line, xy, linewidth = 0.8)
            }
            plot <- plot + ggplot2::geom_point(data = qq$points, xy, key_glyph = ggplot2::draw_key_rect, show.legend = TRUE)
            if (!is.null(groupVar) && is.null(colorVar)) {
                scales <- if (self$options$facetFreeScales) "free" else "fixed"
                if (self$options$facetBy == "column")
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data$.GROUP), ncol = as.numeric(self$options$facetNumber), scales = scales)
                else
                    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data$.GROUP), nrow = as.numeric(self$options$facetNumber), scales = scales)
            }

            if (self$options$type == "QQ") {
                if (detrend) {
                    if (self$options$standardize)
                        yLab <- .("Standardized Sample Quantiles Deviation")
                    else
                        yLab <- .("Sample Quantiles Deviation")
                } else {
                    if (self$options$standardize)
                        yLab <- .("Standardized Sample Quantiles")
                    else
                        yLab <- .("Sample Quantiles")
                }
                xLab <- .("Theoretical Quantiles")
            } else { # PP
                xLab <- .("Theoretical Probabilities")
                yLab <-  ifelse(detrend, .("Sample Probability Deviation"), .("Sample Probabilities"))
            }

            # Theme and colors
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "color", theme, drop = FALSE) + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE)

            # Grid lines
            plot <- plot + vijGridLines(self$options$theme,
                                        x = self$options$xGridLines,
                                        y = self$options$yGridLines)

            # Hide axis line and ticks
            plot <- plot + vijHideAxis(x = self$options$xHideAxis, y = self$options$yHideAxis)

            # Legend
            if (is.null(colorVar))
                plot <- plot + ggplot2::guides(fill = "none")
            else
                plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))

            # Axis Limits
            if (self$options$yAxisRangeType == "manual")
                yLim <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
            else
                yLim <- NULL
            if (self$options$xAxisRangeType == "manual")
                xLim <- c(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
            else
                xLim <- NULL
            plot <- plot + ggplot2::coord_cartesian(ylim = yLim, xlim = xLim)

            # Ticks
            if (self$options$xTicks > 0) {
                plot <- plot  + ggplot2::scale_x_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1))
            }
            if (self$options$yTicks > 0) {
                plot <- plot  + ggplot2::scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1))
            }

            # Define the title of the plot
            if (detrend) {
                plotTitle <- jmvcore::format(.("Detrended {distribStr} {typeStr} Plot of {varStr}"),
                                             distribStr = private$.distTitleName(distrib),
                                             typeStr = ifelse(self$options$type == "PP", .("P-P"), .("Q-Q")),
                                             varStr = ifelse(self$options$transLog, paste0("LN(",self$options$dep,")"), self$options$dep)
                )
            } else {
                plotTitle <- jmvcore::format(.("{distribStr} {typeStr} Plot of {varStr}"),
                                             distribStr = private$.distTitleName(distrib),
                                             typeStr = ifelse(self$options$type == "PP", .("P-P"), .("Q-Q")),
                                             varStr = ifelse(self$options$transLog, paste0("LN(",self$options$dep,")"), self$options$dep)
                )
            }

            # Titles & Labels
            defaults <- list(title = plotTitle, x = xLab , y = yLab, legend = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plot = plot) + vijTitleAndLabelFormat(self$options)

            # Legend spacing
            plot <- plot + ggplot2::theme(legend.key.spacing.y = grid::unit(1, "mm"), legend.byrow = TRUE)

            vijDebugPlot(self, plot)

            return (plot)
        },
        .distTitleName = function(distName) {
            switch(
                distName,
                beta = .("Beta"),
                cauchy = .("Cauchy"),
                chisq = .("Chi-squared"),
                exp = .("Exponential"),
                f = "F",
                gamma = .("Gamma"),
                lnorm = .("Log-normal"),
                logis = .("Logistic"),
                norm = .("Normal [distribution]"),
                t = .("Student"),
                unif = .("Uniform"),
                weibull = .("Weibull"),
                NULL
            )
        },
        .userParameters = function(distName, p1, p2) {
            params <- NULL
            if (distName == "beta" && p1 > 0 && p2 > 0) {
                params <- list(shape1 = p1, shape2 = p2)
            } else if (distName == "cauchy" && p2 > 0) {
                params <- list(location = p1, scale = p2)
            } else if (distName == "chisq" && p1 > 0) {
                params <- list(df = p1)
            } else if (distName == "exp" && p1 > 0) {
                params <- list(rate = p1)
            } else if (distName == "f" && p1 > 0 && p2 > 0) {
                params <- list(df1 = p1, df2 = p2)
            } else if (distName == "gamma" && p1 > 0 && p2 > 0) {
                params <- list(shape = p1, rate = p2)
            } else if (distName == "lnorm" && p2 > 0) {
                params <- list(meanlog = p1, sdlog = p2)
            } else if (distName == "logis" && p2 > 0) {
                params <- list(location = p1, scale = p2)
            } else if (distName == "norm" && p2 > 0) {
                params <- list(mean = p1, sd = p2)
            } else if (distName == "t" && p1 > 0) {
                params <- list(df = p1)
            } else if (distName == "unif" && p1 < p2) {
                params <- list(min = p1, max = p2)
            } else if (distName == "weibull" && p1 > 0 && p2 > 0) {
                params <- list(shape = p1, scale = p2)
            }
            return(params)
        },
        .mleParameters = function(distName, aVar) {
            # Mod from https://github.com/aloy/qqplotr/blob/master/R/stat_pp_point.R
            distFun <- switch(distName,
                        beta = "beta",
                        cauchy = "cauchy",
                        chisq = "chi-squared",
                        exp = "exponential",
                        f = "f",
                        gamma = "gamma",
                        lnorm = "log-normal",
                        logis = "logistic",
                        norm = "normal",
                        weibull = "weibull",
                        NULL)
            initialValues <- switch(distName,
                                    beta = list(shape1 = 1, shape2 = 1),
                                    chisq = list(df = 1),
                                    f = list(df1 = 1, df2 = 2),
                                    NULL)
            params <- tryCatch(
                MASS::fitdistr(x = aVar, densfun = distFun, start = initialValues)$estimate,
                error = function (e) NULL
            )
            return(params)
        },
        .mmeParameters = function(distName, aVar) {
            m <- mean(aVar)
            s <- stats::sd(aVar)
            if (distName == "beta") {
                # The moments only fit a Beta distribution when s² < m(1-m): otherwise both shapes are ≤ 0
                if (s**2 >= m*(1-m))
                    return(NULL)
                shape1 <- m*(m*(1-m)/s**2 - 1)
                shape2 <- (1-m)*(m*(1-m)/s**2 - 1)
                params <- list(shape1 = shape1, shape2 = shape2)
            } else if (distName == "exp") {
                rate <- 1/m
                params <- list(rate = rate)
            } else if (distName == "gamma") {
                shape <- m**2/s**2
                rate <- m/s**2
                params <- list(shape = shape, rate = rate)
            } else if (distName == "lnorm") {
                meanlog <- mean(log(aVar))
                sdlog <- stats::sd(log(aVar))
                params <- list(meanlog = meanlog, sdlog = sdlog)
            } else if (distName == "logis") {
                location <- m
                scale <- sqrt(3)*s/pi
                params <- list(location = location, scale = scale)
            } else if (distName == "norm") {
                params <- list(mean = m, sd = s)
            } else if (distName == "unif") {
                params <- list(min = min(aVar), max = max(aVar))
            } else { # if (distName %in% c("cauchy","chisq", "f", "weibull","t")) {
                params <- NULL
            }
            return(params)
        },
        # Parameters estimated by the chosen method (error naming the group, if any, when it fails)
        .estimateParameters = function(distName, x, level = NULL) {
            method <- self$options$paramEstMethod
            params <- if (method == "mle") private$.mleParameters(distName, x) else private$.mmeParameters(distName, x)
            if (is.null(params) && is.null(level))
                vijErrorMessage(self, jmvcore::format(.("Unable to estimate the distribution parameters using {method} method."), method = toupper(method)))
            if (is.null(params))
                vijErrorMessage(self, jmvcore::format(.("Unable to estimate the distribution parameters of group {group} using {method} method."), group = level, method = toupper(method)))
            params
        },
        # Points, reference line and band from the selected data
        .qqPlotData = function(data, params, byGroup = FALSE) {
            if (!self$options$band)
                bandType <- NULL
            else if (self$options$type == "QQ")
                bandType <- self$options$methodQQ
            else
                bandType <- self$options$methodPP
            groupVar <- self$options$group
            private$.qqData(data[[self$options$dep]], if (is.null(groupVar)) NULL else data[[groupVar]],
                            self$options$distrib, params, byGroup, self$options$type, self$options$detrend,
                            self$options$refType == "identity", bandType)
        },
        # Points, reference line and band of each group (in .GROUP; params: one set, or one per level when byGroup), seed 42 for each group's random draws
        .qqData = function(smp, group, distName, params, byGroup, type, detrend, identity, bandType) {
            if (is.null(group))
                return(withr::with_seed(42, private$.qqGroup(smp, distName, params, type, detrend, identity, bandType)))
            parts <- list()
            for (level in levels(group)) {
                inGroup <- group == level
                if (!any(inGroup))
                    next
                part <- withr::with_seed(42, private$.qqGroup(smp[inGroup], distName, if (byGroup) params[[level]] else params,
                                                               type, detrend, identity, bandType))
                for (k in names(part))
                    if (!is.null(part[[k]]))
                        part[[k]]$.GROUP <- factor(level, levels = levels(group))
                parts[[level]] <- part
            }
            list(points = do.call(rbind, lapply(parts, `[[`, "points")),
                 line = do.call(rbind, lapply(parts, `[[`, "line")),
                 band = do.call(rbind, lapply(parts, `[[`, "band")))
        },
        # Adapted from qqplotr 0.0.7 (A. Almeida, A. Loy, H. Hofmann; GPL-3), ELL band from qqconf
        .qqGroup = function(smp, distName, params, type, detrend, identity, bandType, B = 1000, conf = 0.95) {
            smp <- sort(smp)
            n <- length(smp)
            probs <- stats::ppoints(n)
            pFunc <- private$.distFun("p", distName)
            qFunc <- private$.distFun("q", distName)
            rFunc <- private$.distFun("r", distName)
            band <- NULL
            if (type == "QQ") {
                theoretical <- do.call(qFunc, c(list(p = probs), params))
                if (identity) {
                    slope <- 1
                    intercept <- 0
                } else { # Line through the first and third quartiles
                    xq <- do.call(qFunc, c(list(p = c(0.25, 0.75)), params))
                    yq <- stats::quantile(smp, probs = c(0.25, 0.75), type = 7)
                    slope <- diff(yq) / diff(xq)
                    intercept <- yq[1] - slope * xq[1]
                }
                fitted <- slope * theoretical + intercept
                points <- data.frame(x = theoretical, y = if (detrend) smp - fitted else smp)
                xLine <- range(theoretical)
                line <- data.frame(x = xLine, y = if (detrend) 0 else slope * xLine + intercept)
                if (!is.null(bandType)) {
                    if (bandType == "pointwise") { # Normal confidence intervals
                        dens <- do.call(private$.distFun("d", distName), c(list(x = theoretical), params))
                        stdErr <- (slope / dens) * sqrt(probs * (1 - probs) / n)
                        zCrit <- stats::qnorm(1 - (1 - conf) / 2)
                        lower <- fitted - stdErr * zCrit
                        upper <- fitted + stdErr * zCrit
                    } else if (bandType == "boot") { # Parametric bootstrap
                        bs <- apply(matrix(do.call(rFunc, c(list(n = n * B), as.list(params))), n, B), 2, sort)
                        upper <- apply(bs, 1, stats::quantile, probs = (1 + conf) / 2)
                        lower <- apply(bs, 1, stats::quantile, probs = (1 - conf) / 2)
                    } else if (bandType == "ks") { # Dvoretzky-Kiefer-Wolfowitz inequality
                        epsilon <- sqrt((1 / (2 * n)) * log(2 / (1 - conf)))
                        lower <- intercept + slope * do.call(qFunc, c(list(p = pmax(probs - epsilon, 0)), params))
                        upper <- intercept + slope * do.call(qFunc, c(list(p = pmin(probs + epsilon, 1)), params))
                    } else if (bandType == "ts") { # Tail-sensitive (Normal distribution only)
                        center <- function(x) robustbase::s_Qn(x, mu.too = TRUE)[[1]]
                        scale <- function(x) robustbase::Qn(x, finite.corr = FALSE)
                        sim <- NULL
                        for (i in 1:B)
                            sim <- cbind(sim, sort(stats::rnorm(n)))
                        sim <- sweep(sweep(sim, 2, apply(sim, 2, center), FUN = "-"), 2, apply(sim, 2, scale), FUN = "/")
                        sim <- t(apply(sim, 1, stats::pnorm))
                        pValue <- matrix(NA, nrow = n, ncol = B)
                        for (i in 1:n) {
                            tmp <- stats::pbeta(sim[i, ], shape1 = i, shape2 = n + 1 - i)
                            pValue[i, ] <- apply(cbind(tmp, 1 - tmp), 1, min)
                        }
                        critical <- stats::quantile(apply(pValue, 2, min), prob = 1 - conf)
                        upper <- stats::qnorm(stats::qbeta(1 - critical, 1:n, n + 1 - (1:n))) * scale(smp) + center(smp)
                        lower <- stats::qnorm(stats::qbeta(critical, 1:n, n + 1 - (1:n))) * scale(smp) + center(smp)
                    } else { # ell: Equal Local Levels
                        ell <- qqconf::get_qq_band(n = n, alpha = 1 - conf, distribution = qFunc, dparams = params, prob_pts_method = "normal")
                        lower <- ell$lower_bound
                        upper <- ell$upper_bound
                    }
                    band <- data.frame(x = theoretical, lower = lower, upper = upper)
                    if (detrend) {
                        band$lower <- band$lower - fitted
                        band$upper <- band$upper - fitted
                    }
                }
            } else { # PP
                y <- do.call(pFunc, c(list(q = smp), params))
                points <- data.frame(x = probs, y = if (detrend) y - probs else y)
                line <- data.frame(x = c(0, 1), y = if (detrend) c(0, 0) else c(0, 1))
                if (!is.null(bandType)) {
                    x <- probs
                    if (bandType == "boot") { # Parametric bootstrap
                        bs <- matrix(do.call(rFunc, c(list(n = n * B), params)), n, B)
                        sim <- apply(bs, 2, function(s) do.call(pFunc, c(list(q = sort(s)), params)))
                        upper <- apply(sim, 1, stats::quantile, prob = (1 + conf) / 2)
                        lower <- apply(sim, 1, stats::quantile, prob = (1 - conf) / 2)
                    } else { # ell: Equal Local Levels, back to probabilities
                        ell <- qqconf::get_qq_band(n = n, alpha = 1 - conf, distribution = qFunc, dparams = params, prob_pts_method = "normal")
                        x <- do.call(pFunc, c(list(q = ell$expected_value), params))
                        lower <- do.call(pFunc, c(list(q = ell$lower_bound), params))
                        upper <- do.call(pFunc, c(list(q = ell$upper_bound), params))
                    }
                    band <- data.frame(x = x, lower = lower, upper = upper)
                    if (detrend) {
                        band$lower <- band$lower - x
                        band$upper <- band$upper - x
                    }
                }
            }
            list(points = points, line = line, band = band)
        },
        .distFun = function(prefix, distName) {
            funs <- switch(distName,
                beta    = list(d = stats::dbeta,    p = stats::pbeta,    q = stats::qbeta,    r = stats::rbeta),
                cauchy  = list(d = stats::dcauchy,  p = stats::pcauchy,  q = stats::qcauchy,  r = stats::rcauchy),
                chisq   = list(d = stats::dchisq,   p = stats::pchisq,   q = stats::qchisq,   r = stats::rchisq),
                exp     = list(d = stats::dexp,     p = stats::pexp,     q = stats::qexp,     r = stats::rexp),
                f       = list(d = stats::df,       p = stats::pf,       q = stats::qf,       r = stats::rf),
                gamma   = list(d = stats::dgamma,   p = stats::pgamma,   q = stats::qgamma,   r = stats::rgamma),
                lnorm   = list(d = stats::dlnorm,   p = stats::plnorm,   q = stats::qlnorm,   r = stats::rlnorm),
                logis   = list(d = stats::dlogis,   p = stats::plogis,   q = stats::qlogis,   r = stats::rlogis),
                norm    = list(d = stats::dnorm,    p = stats::pnorm,    q = stats::qnorm,    r = stats::rnorm),
                t       = list(d = stats::dt,       p = stats::pt,       q = stats::qt,       r = stats::rt),
                unif    = list(d = stats::dunif,    p = stats::punif,    q = stats::qunif,    r = stats::runif),
                weibull = list(d = stats::dweibull, p = stats::pweibull, q = stats::qweibull, r = stats::rweibull))
            funs[[prefix]]
        },
        .showHelpMessage = function() {
            helpMsg <- .('<p>The points, reference lines and confidence bands are computed with the methods of the <a href = "https://CRAN.R-project.org/package=qqplotr" target="_blank">qqplotr R package</a> by Alexandre Almeida, Adam Loy and Heike Hofmann, the ELL bands with the <a href = "https://CRAN.R-project.org/package=qqconf" target="_blank">qqconf R package</a>. In-depth information can be found in the documentation of these packages on CRAN site.</p>
<p><strong>Reference line:</strong> Draws either the <em>identity line</em> (y = x) or the commonly-used <em>Q-Q line</em> that intercepts two data quantiles (Q<sub>0.25</sub> and Q<sub>0.75</sub>). P-P plot only supports identity line.</p>
<p><strong>Confidence band:</strong> Draws a confidence band around the reference line. Several methods are available to compute the confidence band:</p>
<ul>
<li><strong><em>Pointwise</em></strong> constructs pointwise confidence bands based on Normal confidence intervals;</li>
<li><strong><em>Bootstrap</em></strong> creates pointwise confidence bands based on a parametric bootstrap;</li>
<li><strong><em>Kolmogorov-Smirnov</em></strong> band is based on the Kolmogorov-Smirnov test;</li>
<li><strong><em>Tail-Sensitive</em></strong> constructs a tail-sensitive confidence bands but is only valid for Normal Q-Q plots;</li>
<li><strong><em>Equal Local Levels (ELL)</em></strong> constructs simultaneous bands using the equal local levels.</li>
</ul>
<p>P-P plots only support "ELL" and "Bootstrap" methods.</p>
<p><strong>Detrended plot:</strong> The objects are <em>detrended</em> according to the reference line. This procedure may help reducing visual bias caused by the orthogonal distances from the points to the reference line.</p>
<p><strong>Parameter values:</strong> The distribution parameters can be estimated from data using:</p>
<ul>
<li><strong>Maximum Likelihood Method</strong> (using MASS package) : it should work with all distributions but "t" and "uniform";</li>
<li><strong>Method of Moments</strong> for moment based parameters : it should work with "normal", "log-normal", "Beta", "Exponential", "Gamma", "Logistic" and "Uniform" distributions;</li>
</ul>
<p>or entered by user:</p>
<ul>
<li><strong>Parameter 1:</strong> mean (Normal), meanlog (Log-normal), shape1 (Beta), location (Cauchy, Logistic), df (Chi-squared, Student), df1 (F), rate (Exponential), shape (Gamma, Weibull) and min (Uniform);</li>
<li><strong>Parameter 2:</strong> sd (normal), sdlog (Log-normal), shape2 (Beta), scale (Cauchy, Logistic), df2 (F), rate (Gamma), scale (Weibull) and max (Uniform).</li>
</ul>
<p><strong>Grouping variable:</strong> The data are standardized and the parameters estimated within each group; parameter values entered by the user are common to all groups.</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)
