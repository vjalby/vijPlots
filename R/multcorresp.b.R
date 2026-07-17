
# This file is a generated template, your changes will not be overwritten

multcorrespClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "multcorrespClass",
    inherit = multcorrespBase,
    private = list(
        .getVarName = function(aVar) {
            if (self$options$descAsVarName) {
                aVarName <- attr(self$data[[aVar]], "jmv-desc", TRUE)
                if (!is.null(aVarName))
                    return(aVarName)
                else
                    return(aVar)
            } else {
                return(aVar)
            }
        },
        .init = function() {
            if (is.null(self$options$vars)) {
                private$.showHelpMessage()
            }
        },
        .run = function() {
            if (is.null(self$options$vars) || length(self$options$vars) < 3  || nrow(self$data) == 0)
                return()

            # check dim values
            nDim <- self$options$dimNum
            if (self$options$xaxis > nDim || self$options$yaxis > nDim)
                errorMessage <- .("X-Axis and Y-Axis cannot be greater than the number of dimensions.")
            else if (self$options$xaxis == self$options$yaxis)
                errorMessage <- .("X-Axis and Y-Axis cannot be equal.")
            else
                errorMessage <- NULL

            if (!is.null(errorMessage)) {
                vijErrorMessage(self, errorMessage)
                return(TRUE)
            }

            activeVars <- self$options$vars
            supplVars <- self$options$supplVars
            allVars <- c(activeVars, supplVars)
            if (is.null(supplVars))
                supplIdx <- NULL
            else
                supplIdx <- (length(activeVars)+1):(length(allVars))

            # remove cases with with NA in vars
            data <- self$data[complete.cases(self$data[, allVars, drop = FALSE]), , drop = FALSE]
            data <- droplevels(data)

            if (nrow(data) == 0) {
                vijErrorMessage(self, .("Unable to compute MCA because of too many missing values."))
                return(TRUE)
            }


            # list of ordered factors (used to draw path)
            ordVars <- c()
            for(i in seq_along(allVars)) {
                if("ordered" %in% attr(data[[allVars[i]]], "class"))
                    ordVars <- c(ordVars, i)
            }

            if (!is.null(self$options$labelVar))
                rowLabels <- as.character(data[[self$options$labelVar]])
            else
                rowLabels <- NULL

            # Set variable names
            allVars <- sapply(allVars, FUN = private$.getVarName, USE.NAMES = FALSE)
            names(data) <- sapply(names(data), FUN = private$.getVarName, USE.NAMES = FALSE)

            method <- self$options$method
            methodStr <- switch(method,
                                "Indicator" = .("Indicator matrix"),
                                "Burt" = .("Burt matrix"))

            nullOrValue = function(x) {
                if(is.na(x)) NULL else x
            }

            #### Compute MCA ####

            res <- tryCatch(
                    private$.mca(data[,allVars], method = method, nd = nDim, supcol = supplIdx,
                                 rowlabels = rowLabels, rownames = rownames(data)),
                    error = function (e) NULL
                )

            if (is.null(res)) {
                vijErrorMessage(self, .("Unable to compute MCA for the selected variables."))
                return(TRUE)
            }

            if (nDim > res$nd.max) {
                errorMessage <- jmvcore::format(.("The number of dimensions cannot be greater than {max}."), max = res$nd.max)
                vijErrorMessage(self, errorMessage)
                return(TRUE)
            }

            #### Inertia Table ####

            # Populate the inertia table
            if (self$options$showSummary) {
                if (method == "Burt" && self$options$BenzecriAdj) {
                    self$results$eigenvalues$addColumn("adjB", title = .("Inertia"), type = "number", format = "zto", superTitle = .("Benzécri Correction"))
                    self$results$eigenvalues$addColumn("%B", title = .("% of Inertia"), type = "number", format = "pc", superTitle = .("Benzécri Correction"))
                    self$results$eigenvalues$addColumn("C%B", title = .("Cumulative %"), type = "number", format = "pc", superTitle = .("Benzécri Correction"))

                }
                if (method == "Burt" && self$options$GreenacreAdj) {
                    self$results$eigenvalues$addColumn("adjG", title = .("Inertia"), type = "number", format = "zto", superTitle = .("Greenacre Correction"))
                    self$results$eigenvalues$addColumn("%G", title = .("% of Inertia"), type = "number", format = "pc", superTitle = .("Greenacre Correction"))
                    self$results$eigenvalues$addColumn("C%G", title = .("Cumulative %"), type = "number", format = "pc", superTitle = .("Greenacre Correction"))

                }
                for (i in seq_len(res$nd.max)) {
                    values = list(
                        dim = i,
                        inertia = res$eig[i,1],
                        proportion = res$eig[i,2],
                        cumulative = res$eig[i,3]
                    )
                    if (method == "Burt" && self$options$BenzecriAdj) {
                        values["adjB"] <- nullOrValue(res$adjEig[i,1])
                        values["%B"] <- nullOrValue(res$adjEig[i,2])
                        values["C%B"] <- nullOrValue(res$adjEig[i,3])
                    }
                    if (method == "Burt" && self$options$GreenacreAdj) {
                        values["adjG"] <- nullOrValue(res$adjEig[i,1])
                        values["%G"] <- nullOrValue(res$adjEig[i,4])
                        values["C%G"] <- nullOrValue(res$adjEig[i,5])
                    }
                    self$results$eigenvalues$addRow(rowKey = as.character(i), values = values)
                }
                # Add total row
                values = list(
                    dim = .("Total"),
                    inertia = sum(res$eig[,1]),
                    proportion = sum(res$eig[,2]),
                    cumulative = NA
                )
                if (method == "Burt" && self$options$BenzecriAdj) {
                    values["adjB"] <- res$totalInrB
                    values["%B"] <- 1
                    values["C%B"] <- NULL
                }
                if (method == "Burt" && self$options$GreenacreAdj) {
                    values["adjG"] <- res$totalInrB
                    values["%G"] <- sum(res$adjEig[,4], na.rm=TRUE)
                    values["C%G"] <- NULL
                }
                self$results$eigenvalues$addRow(rowKey = "Total", values = values)
                self$results$eigenvalues$addFormat(rowKey = "Total", 1, jmvcore::Cell.BEGIN_END_GROUP)
            }
            self$results$eigenvalues$setNote("method", paste(.("Method:"), methodStr))
            if (method == "Burt" && self$options$GreenacreAdj)
                self$results$eigenvalues$setNote("adjusted", paste(.("Greenacre's corrected inertia ="), round(res$totalInrG,4)))

            #### Discrimination Table ####

            if (self$options$showDiscriminations) {
                for (j in seq(nDim))
                    self$results$discrim$addColumn(paste0("dim",j), title = paste("Dim",j), type = "number", format = "zto", superTitle = .("Discrimination"))
                for (i in seq_len(nrow(res$allvar$eta2))) {
                    values = list()
                    values[["var"]] <- rownames(res$allvar$eta2)[i]
                    for (j in seq(nDim))
                        values[[paste0("dim",j)]] <- res$allvar$eta2[i,j]
                    self$results$discrim$addRow(rowKey = as.character(i), values = values)
                }
            }
            if (!is.null(supplIdx))
                self$results$discrim$setNote("sup", paste("* :", .("Suppl. variables")))

            #### Category Table ####

            if (self$options$showCategories) {
                for (j in seq(nDim))
                    self$results$categories$addColumn(paste0("coord",j), title = paste("Dim",j), type = "number", format = "zto", superTitle = paste(.("Coordinates"),"†"))
                for (j in seq(nDim))
                    self$results$categories$addColumn(paste0("ctr",j), title = paste("Dim",j), type = "number", format = "zto", superTitle = .("Contributions"))
                for (j in seq(nDim))
                    self$results$categories$addColumn(paste0("co2",j), title = paste("Dim",j), type = "number", format = "zto", superTitle = .("COS2"))
                previousfactor <- res$cat$factors[1]
                for (i in seq_len(nrow(res$cat$coord))) {
                    values = list(
                        factor = res$cat$factors[i],
                        level = rownames(res$cat$coord)[i],
                        mass = nullOrValue(res$cat$mass[i]),
                        qlt = res$cat$qlt[i],
                        inertia = nullOrValue(res$cat$inertia[i])
                    )
                    for (j in 1:nDim) {
                        if (self$options$normalization %in% c("principal", "catprincipal"))
                            values[[paste0("coord",j)]] <- res$cat$coord[i,j]
                        else
                            values[[paste0("coord",j)]] <- res$cat$stdcoord[i,j]
                        values[[paste0("co2",j)]] <- res$cat$cos2[i,j]
                        values[[paste0("ctr",j)]] <- nullOrValue(res$cat$contrib[i,j])
                    }
                    self$results$categories$addRow(rowKey = as.character(i), values = values)
                    if( res$cat$factors[i] != previousfactor) {
                        self$results$categories$addFormat(rowKey = as.character(i), 1, jmvcore::Cell.BEGIN_END_GROUP)
                        previousfactor <- res$cat$factors[i]
                    }
                }
                if (self$options$normalization %in% c("principal", "catprincipal"))
                    self$results$categories$setNote("normalization",paste("† :",.("Principal coordinates")))
                else
                    self$results$categories$setNote("normalization",paste("† :",.("Standard coordinates")))
                if (!is.null(supplIdx))
                    self$results$categories$setNote("sup", paste("* :", .("Supplementary variables")))
            }

            #### Observation Table ####

            nrows <- length(res$rowlabels)
            if (nrows > 100) {
                self$results$observations$setNote("100", .("Limited to the first 100 observations"))
                nrows <- 100
            }

            if (self$options$showObservations) {
                self$results$observations$addColumn("inertia", title = .("% Inertia"), type = "number", format = "zto")
                self$results$observations$addColumn("qlt", title = "QLT", type = "number", format = "zto")
                for (j in seq(nDim))
                    self$results$observations$addColumn(paste0("coord",j), title = paste("Dim",j), type = "number", format = "zto", superTitle = paste(.("Coordinates"),"†"))
                for (j in seq(nDim))
                    self$results$observations$addColumn(paste0("ctr",j), title = paste("Dim",j), type = "number", format = "zto", superTitle = .("Contributions"))
                for (j in seq(nDim))
                    self$results$observations$addColumn(paste0("co2",j), title = paste("Dim",j), type = "number", format = "zto", superTitle = .("COS2"))
                for (i in seq_len(nrows)) {
                    values = list(
                        name = res$rowlabels[i],
                        mass = res$call$marge.row[i],
                        qlt = res$ind$qlt[i],
                        inertia = res$ind$inertia[i]
                    )
                    for (j in 1:nDim) {
                        if (self$options$normalization %in% c("principal", "obsprincipal"))
                            values[[paste0("coord",j)]] <- res$ind$coord[i,j]
                        else
                            values[[paste0("coord",j)]] <- res$ind$stdcoord[i,j]
                        values[[paste0("co2",j)]] <- res$ind$cos2[i,j]
                        values[[paste0("ctr",j)]] <- res$ind$contrib[i,j]
                    }
                    self$results$observations$addRow(rowKey = as.character(i), values = values)
                }
                if (self$options$normalization %in% c("principal", "obsprincipal"))
                    self$results$observations$setNote("normalization",paste("† :",.("Principal coordinates")))
                else
                    self$results$observations$setNote("normalization",paste("† :",.("Standard coordinates")))
            }

            #### Plots  ####

            res$ordVars <- ordVars # List of ordered factors

            if (self$options$showDiscriminationPlot) {
                discrimplot <- self$results$discrimplot
                discrimplot$setState(res)
            }

            if (self$options$showCategoryPlot) {
                categoryplot <- self$results$categoryplot
                categoryplot$setState(res)
            }
            if (self$options$showObservationPlot) {
                obsplot <- self$results$obsplot
                obsplot$setState(res)
            }
            if (self$options$showBiPlot) {
                biplot <- self$results$biplot
                biplot$setState(res)
            }

            #### Saving coordinates  ####

            if (self$options$normalization %in% c("principal", "obsprincipal"))
                private$.saveCoordinates(res$ind$coord, "principal")
            else
                private$.saveCoordinates(res$ind$stdcoord, "standard")
        },
        .mca = function(data, method, nd, supcol, rowlabels = NULL, rownames = NULL) {
            res <- FactoMineR::MCA(data, method = method, ncp = 999, quali.sup = supcol, graph = FALSE)
            res$nd.max <- nrow(res$eig)
            if (nd > res$nd.max)
                return(res)
            # res$rowlabels is used for Observation table and plot
            if (!is.null(rowlabels))
                res$rowlabels <- rowlabels
            else
                res$rowlabels <- as.character(rownames)
            # rownames = rownames(self$data-without-NA) is used for saving coordinates
            rownames(res$ind$coord) <- rownames
            # Build the list of variable names for levels (hmmm, i'm sure there's a better way to do that)
            varNames <- names(data)     # variable list
            if (!is.null(supcol))
                varNames <- varNames[-supcol]   # remove supplementary variables
            varFactors <- c()
            for (aVar in varNames) {
                #varFactors <- c(varFactors, rep(aVar, nlevels(factor(data[[aVar]]))))
                varFactors <- c(varFactors, rep(aVar, nlevels(data[[aVar]]))) # unused levels are now dropped from the begining
            }
            # Build the list of supplementary variable names for levels
            supSymbol <- "*"
            supFactors <- c()
            if (!is.null(supcol)) {
                varNames <- names(data)
                varNames <- varNames[supcol]
                for (aVar in varNames)
                    supFactors <- c(supFactors, rep(paste(aVar,supSymbol), nlevels(factor(data[[aVar]]))))
            }
            res$cat$factors <- c(varFactors, supFactors)
            # convert % to decimal
            res$eig[,2:3] <- res$eig[,2:3] / 100
            res$var$contrib <- res$var$contrib / 100
            res$ind$contrib <- res$ind$contrib / 100
            # var QLT
            res$var$qlt <- rowSums(res$var$cos2[,1:nd, drop = FALSE])
            # var Inertia
            varinertia <- res$var$contrib %*% res$eig[,1]
            res$var$inertia <- varinertia / sum(varinertia)
            # ind QLT
            res$ind$qlt <- rowSums(res$ind$cos2[,1:nd, drop = FALSE])
            # ind Inertia
            indinertia <- res$ind$contrib %*% res$eig[,1]
            res$ind$inertia <- indinertia / sum(indinertia)
            # Cat Std coordinates
            res$var$stdcoord <- sweep(res$var$coord, 2, sqrt(res$eig [,1]), FUN = "/")
            # Observation Std coordinates
            res$ind$stdcoord <- sweep(res$ind$coord, 2, sqrt(res$eig [,1]), FUN = "/")

            #### Burt fixes ####
            if (method == "Burt") {
                # var eta2
                res$var$eta2 <- sweep(res$var$eta2, 2, sqrt(res$eig[,1]), FUN = "*")
                ## MCA compute Pal Coordinates for Indicator Inertia only. So we have to rebuild std coordinates from
                ## indicator-principal coordinates and redo the computation of co2 & qlt. contrib are unchanged. Inertia computed above is ok.
                # Obs coordinates
                res$ind$stdcoord <- sweep(res$ind$coord, 2, res$eig [,1]**(1/4), FUN = "/")
                res$ind$coord <- sweep(res$ind$stdcoord, 2, sqrt(res$eig [,1]), FUN = "*")
                # Obs CO2
                res$ind$cos2 <- sweep(res$ind$coord**2, 1, rowSums(res$ind$coord**2), FUN = "/")
                res$ind$qlt <- rowSums(res$ind$cos2[,1:nd, drop = FALSE])
            }

            #### Supp Categories ####
            if (!is.null(supcol)) {
                res$quali.sup$qlt <- rowSums(res$quali.sup$cos2[,1:nd, drop = FALSE])
                res$quali.sup$stdcoord <- sweep(res$quali.sup$coord, 2, sqrt(res$eig [,1]), FUN = "/")
            }

            #### All categories ####
            if (!is.null(supcol)) {
                res$cat$coord <- rbind(res$var$coord, res$quali.sup$coord)
                res$cat$stdcoord <- rbind(res$var$stdcoord, res$quali.sup$stdcoord)
                res$cat$cos2 <- rbind(res$var$cos2, res$quali.sup$cos2)
                null <- matrix(NA, nrow(res$quali.sup$coord), ncol(res$quali.sup$coord))
                res$cat$contrib <- rbind(res$var$contrib, null)
                null <- matrix(NA, nrow(res$quali.sup$coord), ncol = 1)
                res$cat$inertia <- rbind(res$var$inertia, null)
                res$cat$qlt <- c(res$var$qlt, res$quali.sup$qlt)
                null <- rep(NA, nrow(res$quali.sup$coord))
                res$cat$mass <- c(res$call$marge.col, null)
            } else {
                res$cat$coord <- res$var$coord
                res$cat$stdcoord <- res$var$stdcoord
                res$cat$cos2 <- res$var$cos2
                res$cat$contrib <- res$var$contrib
                res$cat$inertia <- res$var$inertia
                res$cat$qlt <- res$var$qlt
                res$cat$mass <- res$call$marge.col
            }
            res$varActive <- c(ncol(data) - length(supcol), length(supcol))  # (nb of active var, nb of supp var)
            # All var
            if (!is.null(supcol)) {
                rownames(res$quali.sup$eta2) <- paste(rownames(res$quali.sup$eta2), supSymbol)
                res$allvar$eta2 <- rbind(res$var$eta2,res$quali.sup$eta2)
            } else {
                res$allvar$eta2 <- res$var$eta2
            }

            #### Benzecri / Greenacre Adjusment ####
            if (method == "Burt") {
                p <- length(res$call$quali) # Nb of variables
                m <- length(res$call$marge.col) # Nb of categories
                res$totalInertia <- sum(res$eig[,1])
                res$adjEig <- matrix(nrow = length(res$eig[,1]), ncol = 5)
                res$adjEig[,1] <- (p/(p-1))**2 * (sqrt(res$eig[,1]) - 1/p)**2
                res$adjEig[sqrt(res$eig[,1]) <= 1/p,1] <- 0
                res$totalInrB <- sum(res$adjEig[,1])
                res$totalInrG <- (p/(p-1)) * (res$totalInertia - (m-p)/p**2)
                res$adjEig[,2] <- res$adjEig[,1] / res$totalInrB
                res$adjEig[,3] <- cumsum(res$adjEig[,2])
                res$adjEig[,4] <- res$adjEig[,1] / res$totalInrG
                res$adjEig[,5] <- cumsum(res$adjEig[,4])
                res$adjEig[sqrt(res$eig[,1]) <= 1/p,] <- rep(NA,5)
            }

            # Delete unused large tables (to save memory ?)
            res$call$X <- NULL
            res$call$Xtot <- NULL
            res$var <- NULL
            return(res)
        },
        .discrimplot = function(image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            dim1 <- self$options$xaxis
            dim2 <- self$options$yaxis
            dim1name <- paste0(.("Dimension"), " ", dim1, " (", round(res$eig[dim1,2]*100,1), '\u2009%)')
            dim2name <- paste0(.("Dimension"), " ", dim2, " (", round(res$eig[dim2,2]*100,1), '\u2009%)')

            data <- res$allvar$eta2[,c(dim1, dim2)]
            colnames(data) <- c("x","y")

            plot <- ggplot(data, aes(x = x, y = y, label = rownames(data)))
            plot <- plot + geom_point()
            plot <- plot + geom_segment(aes(xend = 0, yend = 0))
            #plot <- plot + geom_text(show.legend = FALSE, hjust = 0.25, vjust = 1, nudge_y = 0.025, size = self$options$labelSize/.pt)
            plot <- plot + ggrepel::geom_text_repel(show.legend = FALSE, nudge_y = 0.03/.pt, min.segment.length = 2,
                                                    size = self$options$labelSize/.pt)
            plot <- plot + ggtheme

            # Axes
            plot <- plot + coord_fixed(clip = "off")

            # Titles & Labels
            defaults <- list(title = .("Discrimination Plot"), y = dim2name, x = dim1name)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plotType = "discrim") + vijTitleAndLabelFormat(self$options)

            return(plot)
        },
        .mainplot = function(plotType, image, ggtheme, theme) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            #### Define the dimensions ####
            dim1 <- self$options$xaxis
            dim2 <- self$options$yaxis
            dim1name <- paste0(.("Dimension"), " ", dim1, " (", round(res$eig[dim1,2]*100,1), '\u2009%)')
            dim2name <- paste0(.("Dimension"), " ", dim2, " (", round(res$eig[dim2,2]*100,1), '\u2009%)')

            #### Prepare data ####
            if (self$options$normalization == "principal") {
                catdata <- as.data.frame(res$cat$coord[,c(dim1,dim2)])
                obsdata <- as.data.frame(round(res$ind$coord[,c(dim1,dim2)],4))
            } else if (self$options$normalization == "obsprincipal") {
                catdata <- as.data.frame(res$cat$stdcoord[,c(dim1,dim2)])
                obsdata <- as.data.frame(round(res$ind$coord[,c(dim1,dim2)],4))
            } else if (self$options$normalization == "catprincipal") {
                catdata <- as.data.frame(res$cat$coord[,c(dim1,dim2)])
                obsdata <- as.data.frame(round(res$ind$stdcoord[,c(dim1,dim2)],4))
            } else {
                catdata <- as.data.frame(res$cat$stdcoord[,c(dim1,dim2)])
                obsdata <- as.data.frame(round(res$ind$stdcoord[,c(dim1,dim2)],4))
            }
            colnames(catdata) <- c("x","y")
            colnames(obsdata) <- c("x","y")

            # Height of the obs/bi plot (to nudge geom_text)
            if (plotType == "biplot")
                ggheight <- max(obsdata$y,catdata$y) - min(obsdata$y,catdata$y)
            else
                ggheight <- max(obsdata$y) - min(obsdata$y)

            # Start the plot
            plot <- ggplot()

            #### Observation Plot ####

            if (plotType != "cat") {
                # plot the points
                if (self$options$propPoint)
                    plot <- plot + geom_count(data = obsdata, aes(x = x, y = y), shape = 15, color = self$options$obsColor, show.legend = FALSE)
                else
                    plot <- plot + geom_point(data = obsdata, aes(x = x, y = y), shape = 15, color = self$options$obsColor)
                # plot the labels
                if (!is.null(self$options$labelVar)) {
                    obsdata$label <- res$rowlabels
                    if (self$options$ggrepel) {
                        plot <- plot + ggrepel::geom_text_repel(data = unique(obsdata), aes(x = x, y = y, label = label),
                                                            size = self$options$labelSize/.pt,
                                                            color = self$options$obsColor)
                    } else {
                        plot <- plot + geom_text(data = unique(obsdata), aes(x = x, y = y, label = label),
                                                 size = self$options$labelSize/.pt,
                                                 color = self$options$obsColor,
                                                 nudge_y = ggheight*0.03, hjust = 0.5, check_overlap = TRUE)
                    }
                }
            }

            #### Category Plot ####
            if (plotType != "obs") {
                catdata$level <- rownames(catdata)
                # Order color levels
                catdata$factors <- factor(res$cat$factors, levels = unique(res$cat$factors), ordered = TRUE)
                if (self$options$boldCat) {
                    catFace <- "bold"
                    supCatFace <- "bold.italic"
                    catPtSize <- 3
                } else {
                    catFace <- "plain"
                    supCatFace <- "italic"
                    catPtSize <- 2
                }
                # Plotting
                plot <- plot + geom_point(data = catdata, aes(x = x, y = y, color = factors, shape = factors), size = catPtSize)
                plot <- plot + ggrepel::geom_text_repel(data = catdata, aes(x = x, y = y, label = level, color = factors, fontface = factors),
                                                        size = self$options$labelSize/.pt, show.legend = FALSE)
                if (self$options$connectOrdinalCat)
                    plot <- plot + geom_path(data = catdata[catdata$factors %in% levels(catdata$factors)[res$ordVars],], aes(x = x, y = y, color = factors), show.legend = FALSE)
            }

            plot <- plot + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2)

            #### Theme and colors ####
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "color")

            # Shape
            varShapes <- c(rep(16, res$varActive[1]), rep(17, res$varActive[2]))
            if (plotType != "obs")
                plot <- plot + scale_shape_manual(values = varShapes)
            # FontFace
            if (plotType != "obs") {
                varFontface <- c(rep(catFace, res$varActive[1]), rep(supCatFace, res$varActive[2]))
                plot <- plot + scale_discrete_manual("fontface", values = varFontface )
            }

            # Plot frame & coord
            plot <- plot + theme(axis.line = element_line(linewidth = 0), panel.border = element_rect(color = "black", fill = NA, size = 1))
            plot <- plot + coord_fixed()

            #### Plot title ####
            title <- switch(plotType,
                                   cat = .("Category Plot"),
                                   obs = .("Observation Plot"),
                                   biplot = .("Biplot")
            )
            if (plotType == "obs") {
                if (self$options$normalization %in% c("principal", "obsprincipal"))
                    subtitle <- .("Principal coordinates")
                else
                    subtitle <- .("Standard coordinates")
            } else if (plotType == "cat") {
                if (self$options$normalization %in% c("principal", "catprincipal"))
                    subtitle <- .("Principal coordinates")
                else
                    subtitle <- .("Standard coordinates")
            } else { # biplot
                subtitle <- switch(self$options$normalization,
                                           principal = .("Principal coordinates"),
                                           obsprincipal = .("Observation principal coordinates"),
                                           catprincipal = .("Category principal coordinates"),
                                           standard = .("Standard coordinates")
                        )
            }

            # Titles & Labels
            defaults <- list(title = title, subtitle = subtitle, y = dim2name, x = dim1name, legend = .("Variables"))
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plotType = plotType) + vijTitleAndLabelFormat(self$options)

            #self$results$text$setContent(plot) # Show debug messages !

            return(plot)
        },
        .categoryplot = function(image, ggtheme, theme, ...) {
            return(private$.mainplot(plotType = 'cat', image, ggtheme, theme))
        },
        .biplot = function(image, ggtheme, theme, ...) {
            return(private$.mainplot(plotType = 'biplot', image, ggtheme, theme))
        },
        .obsplot = function(image, ggtheme, theme, ...) {
            return(private$.mainplot(plotType = 'obs', image, ggtheme, theme))
        },
        .saveCoordinates = function(coord, type) {
            if (self$options$obsCoordOV && self$results$obsCoordOV$isNotFilled()) {
                nDim <- self$options$dimNum
                keys <- 1:nDim
                measureTypes <- rep("continuous", nDim)

                titles <- paste(.("Dim"), keys)

                if (type == "principal")
                    descriptionString <- .("MCA Principal Coordinates")
                else
                    descriptionString <- .("MCA Standard Coordinates")

                descriptionString <- paste0(descriptionString, " (", self$options$method, ")")

                descriptions <- character(length(keys))
                for (i in keys) {
                    descriptions[i] = descriptionString
                }

                self$results$obsCoordOV$set(
                    keys=keys,
                    titles=titles,
                    descriptions=descriptions,
                    measureTypes=measureTypes
                )

                self$results$obsCoordOV$setRowNums(rownames(coord))

                for (i in 1:nDim)
                    self$results$obsCoordOV$setValues(index=i, coord[, i])
            }

        },
        .showHelpMessage = function() {
            helpMsg <- .('<p>This module computes <strong>Multiple Correspondence Analysis (MCA)</strong> for several categorical variables. Computations are based on <a href = "https://CRAN.R-project.org/package=FactoMineR" target="_blank">FactoMineR<a/> package by F.&nbsp;Husson, J.&nbsp;Josse, S.&nbsp;Le, J.&nbsp;Mazet.</p>
<p>Both classic methods are available:</p>
<ul>
<li><strong>Indicator matrix:</strong> CA of the indicator matrix</li>
<li><strong>Burt matrix:</strong> CA of the Burt matrix. The eigenvalues are the squares of those of the indicator matrix method. </li>
</ul>
<p>Both methods give the same <em>standard</em> coordinates (but different <em>principal</em> coordinates).</p>
<p>When selected, <strong>Benzécri and Greenacre corrections</strong> are applied to eigenvalues only (<strong>Summary</strong> table). Coordinates (and inertia) of categories and observations are computed from the original eigenvalues of the Burt matrix.</p>
<p>The <strong>Normalization</strong> options specify how the coordinates are scaled (by eigenvalues):</p>
<ul>
<li><strong>Principal:</strong> Both category and observation coordinates are scaled.</li>
<li><strong>Category principal:</strong> Only category coordinates are scaled.</li>
<li><strong>Observation principal:</strong> Only observation coordinates are scaled.</li>
<li><strong>Standard:</strong> Both category and observation coordinates are standard.</li>
</ul>
<p>A sample file is included at Open > Data Library > vijPlots > Cars</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)
