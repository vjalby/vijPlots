qqplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "qqplotClass",
    inherit = qqplotBase,
    private = list(
        .init = function() {
            # Stretchable dimensions
            width <- 450
            height <- 450
            # Fixed dimensions
            fixed_height <- 50 # X-Axis legend
            fixed_width <- 75 # Y-Axis legend
            # Legend
            if (!is.null(self$options$group)) {
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
            data[[depVar]] <- jmvcore::toNumeric(data[[depVar]])

            data <- jmvcore::naOmit(data)
            if (nrow(data) == 0)
                return(FALSE)

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

            if (self$options$standardize) {
                data[[depVar]] <- (data[[depVar]] - mean(data[[depVar]]))/stats::sd(data[[depVar]])
            }

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
                if (self$options$paramEstMethod == "mle") {
                    params <- private$.mleParameters(distrib, data[[depVar]])
                } else { # paramEstMethod == "mme"
                    params <- private$.mmeParameters(distrib, data[[depVar]])
                }
                if (is.null(params)) {
                    vijErrorMessage(self, jmvcore::format(.("Unable to estimate the distribution parameters using {method} method."), method = toupper(self$options$paramEstMethod)))
                }
            } else {
                params <- private$.userParameters(distrib, self$options$param1, self$options$param2)
                if (is.null(params)) {
                    vijErrorMessage(self, .("Wrong parameter values."))
                }
            }

            #### Populate the Parameters table ####

            self$results$paramTable$getColumn('var')$setTitle(private$.distTitleName(distrib))
            for(i in seq_along(params)) {
                self$results$paramTable$addColumn(names(params[i]), type = 'number', format = 'zto')
            }
            self$results$paramTable$setRow(rowNo=1, params)
            self$results$paramTable$setCell(rowNo=1, col = 1, ifelse(self$options$transLog, paste0("LN(",self$options$dep,")"), self$options$dep))
            if (self$options$standardize)
                self$results$paramTable$setNote("1", .("Standardized values"), init = TRUE)
            if (self$options$paramMethod == "paraEstimate"){
                if (self$options$paramEstMethod == "mle")
                    self$results$paramTable$setTitle(.("Parameter Estimates (MLE)"))
                else
                    self$results$paramTable$setTitle(.("Parameter Estimates (MME)"))
            } else {
                self$results$paramTable$setTitle(.("Parameter Values"))
            }

            plotData <- list(data = data, params = params)

            image <- self$results$plot
            image$setState(plotData)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            plotData <- image$state$data
            params <- image$state$params

            depVar <- rlang::sym(self$options$dep)
            groupVar <- vijVar(self$options$group)

            distrib <- self$options$distrib

            if (self$options$detrend)
                detrend = TRUE
            else
                detrend = FALSE

            identity = (self$options$refType == "identity")

            # Do the plot
            if (is.null(groupVar))
                plot <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(sample = !!depVar))
            else
                plot <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(sample = !!depVar, color = !!groupVar, fill = !!groupVar))

            if (self$options$type == "QQ") {
                if (self$options$band) {
                    if (is.null(groupVar))
                        plot <- plot + qqplotr::geom_qq_band(distribution = distrib, dparams = params, bandType = self$options$methodQQ,
                                                         identity = identity, detrend = detrend, alpha=0.5, fill = "darkgrey")
                    else
                        plot <- plot + qqplotr::geom_qq_band(distribution = distrib, dparams = params, bandType = self$options$methodQQ,
                                                             identity = identity, detrend = detrend, alpha=0.5)
                }
                if (self$options$refLine) {
                    if (is.null(groupVar) || identity)
                        plot <- plot + qqplotr::stat_qq_line(distribution = distrib, dparams = params, identity = identity,
                                                             detrend = detrend, color = "darkgrey")
                    else
                        plot <- plot + qqplotr::stat_qq_line(distribution = distrib, dparams = params, identity = identity,
                                                             detrend = detrend)
                }
                plot <- plot + qqplotr::stat_qq_point(distribution = distrib, identity = identity, dparams = params,
                                                      detrend = detrend, key_glyph = ggplot2::draw_key_rect, show.legend = TRUE)
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
                if (self$options$band) {
                    if (is.null(groupVar))
                        plot <- plot + qqplotr::stat_pp_band(distribution = distrib, dparams = params, bandType = self$options$methodPP,
                                                             identity = identity, detrend = detrend, alpha=0.5, fill = "darkgrey")
                    else
                        plot <- plot + qqplotr::stat_pp_band(distribution = distrib, dparams = params, bandType = self$options$methodPP,
                                                             identity = identity, detrend = detrend, alpha=0.5)
                }
                if (self$options$refLine) {
                    if (detrend)
                        plot <- plot + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 0), color = "darkgrey")
                    else
                        plot <- plot + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1), color = "darkgrey")
                }
                plot <- plot + qqplotr::stat_pp_point(distribution = distrib, dparams = params,  detrend = detrend,
                                                      key_glyph = ggplot2::draw_key_rect, show.legend = TRUE)
                xLab <- .("Theoretical Probabilities")
                yLab <-  ifelse(detrend, .("Sample Probability Deviation"), .("Sample Probabilities"))
            }

            # Theme and colors
            plot <- plot + ggtheme + vijColorScale(self$options$colorPalette, "color", theme, drop = FALSE) + vijColorScale(self$options$colorPalette, "fill", theme, drop = FALSE)

            # Legend
            if (is.null(groupVar))
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
                shape1 = m*(m*(1-m)/s**2 - 1)
                shape2 = (1-m)*(m*(1-m)/s**2 - 1)
                params <- list(shape1 = shape1, shape2 = shape2)
            } else if (distName == "exp") {
                rate <- 1/m
                params <- list(rate = rate)
            } else if (distName == "gamma") {
                shape = m**2/s**2
                rate = m/s**2
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
        .showHelpMessage = function() {
            helpMsg <- .('<p>This module uses <a href = "https://CRAN.R-project.org/package=qqplotr" target="_blank">qqplotr R package<a/> by Alexandre Almeida, Adam Loy and Heike Hofmann. In-depth information can be found in the package documentation on CRAN site.</p>
<p><strong>Reference line:</strong> Draws either the <em>identity line</em> (y = x) or the commonly-used <em>Q-Q line</em> that intercepts two data quantiles (Q<sub>0.25</sub> and Q<sub>0.75</sub>). P-P plot only supports identity line.</p>
<p><strong>Confidence band:</strong> Draws a confidence band around the reference line. qqplotr package provides several methods to compute the confidence band:</p>
<ul>
<li><strong><em>Pointwise</em></strong> constructs pointwise confidence bands based on Normal confidence intervals;</li>
<li><strong><em>Bootstrap</em></strong> creates pointwise confidence bands based on a parametric bootstrap;</li>
<li><strong><em>Kolmogorov-Smirnov</em></strong> band is based on the Kolmogorov-Smirnov test;</li>
<li><strong><em>Tail-Sensitive</em></strong> constructs a tail-sensitive confidence bands but is only implemented for Normal Q-Q plots;</li>
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
<li><strong>Parameter 2:</strong> sd (normal), sdlog (Log-normal), shape2 (Beta), scale (Cauchy, Logistic), df2 (F), rate (Gamma, Weibull) and max (Uniform).</li>
</ul>
</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)
