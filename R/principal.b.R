
# This file is a generated template, your changes will not be overwritten

principalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "principalClass",
    inherit = principalBase,
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

            if (!is.null(self$options$groupVar)) {
                if (self$options$legendPosition %in% c('top','bottom')) {
                    fixed_width <- 0
                    fixed_height <- 50
                } else {
                    fixed_width <- 100
                    fixed_height <- 0
                }
                # Set the image dimensions
                image <- self$results$obsPlot
                image2 <- self$results$biPlot
                if (is.null(image[['setSize2']])) { # jamovi < 2.7.16
                    image$setSize(600 + fixed_width, 600 + fixed_height)
                    image2$setSize(600 + fixed_width, 600 + fixed_height)
                } else {
                    image$setSize2(600, 600, fixed_width, fixed_height)
                    image2$setSize2(600, 600, fixed_width, fixed_height)
                }
            }
        },
        .run = function() {
            if (is.null(self$options$vars) || length(self$options$vars) < 2 || nrow(self$data) == 0)
                return()
            # check dim values
            nDim <- self$options$dimNum
            if (nDim > length(self$options$vars))
                errorMessage <- .("The number of dimensions cannot be greater than the number of variables.")
            else if (self$options$xaxis > nDim || self$options$yaxis > nDim)
                errorMessage <- .("X-Axis and Y-Axis cannot be greater than the number of dimensions.")
            else if (self$options$xaxis == self$options$yaxis)
                errorMessage <- .("X-Axis and Y-Axis cannot be equal.")
            else
                errorMessage <- NULL

            if (!is.null(errorMessage)) {
                vijErrorMessage(self, errorMessage)
                return(TRUE)
            }

            #### Prepare data ####
            data <- self$data[,c(self$options$vars, self$options$labelVar, self$options$groupVar)]
            # Be sure data is numeric (for ordinal data)
            for (aVar in self$options$vars) {
                data[[aVar]] <- jmvcore::toNumeric(data[[aVar]])
            }
            # remove cases with NA in vars
            data <- data[complete.cases(data[,self$options$vars]),]

            if (nrow(data) < 2) {
                vijErrorMessage(self, .("Not enough complete observations to perform PCA."))
                return(TRUE)
            }

            # Compute the correlation matrix
            corrMat <- cor(data[,self$options$vars])

            if (anyNA(corrMat)) {
                vijErrorMessage(self, .("Unable to compute the correlation matrix."))
                return(TRUE)
            }

            if (abs(det(corrMat)) < .Machine$double.eps) {
                vijWarningMessage(self, .("The correlation matrix is not positive definite. Computations may not be accurate."))
            }

            #### KMO & Bartlett's test ####
            if (self$options$showKMO) {
                kmo <- tryCatch(
                            psych::KMO(corrMat),
                            error = function (e) list(MSA = NA)
                        )
                bartlett <- tryCatch(
                                psych::cortest.bartlett(corrMat, n = nrow(data)),
                                error = function (e) list(cf = NA, p.value = NA)
                            )

                self$results$kmoTable$setRow(rowNo = 1,
                                             values = list(test = .("Bartlett's Test of Sphericity"),
                                                           statistic = bartlett$chisq,
                                                           df = bartlett$df, p = bartlett$p.value))
                self$results$kmoTable$setRow(rowNo = 2,
                                             values = list(test = .("Kaiser-Meyer-Olkin Measure of Sampling Adequacy (MSA)"),
                                                            statistic = kmo$MSA,
                                                            df = NULL, p = NULL))
            }

            #### PCA computation ####
            res <- tryCatch(
                        private$.pca(data[,self$options$vars], scale = self$options$stdVariables,
                               nfact = nDim, rotation = self$options$rotation),
                        error = function (e) NULL
                    )
            if (is.null(res)) {
                vijErrorMessage(self, .("Unable to compute principal components for the selected variables."))
                return(TRUE)
            }

            if (!is.null(self$options$labelVar)) {
                rownames(res$scores) <- data[[self$options$labelVar]]
                rownames(res$stdScores) <- data[[self$options$labelVar]]
            } else {
                rownames(res$scores) <- rownames(data)
                rownames(res$stdScores) <- rownames(data)
            }
            if (!is.null(self$options$groupVar))
                res$group <- data[[self$options$groupVar]]

            rotationName <- switch(self$options$rotation,
                                   none = "None",
                                   Varimax = "Varimax",
                                   quartimax = "Quartimax",
                                   equamax = "Equamax",
                                   parsimax = "Parsimax",
                                   varimin = "Varimin",
                                   entropy = "Minimum entropy",
                                   tandemI = "Comrey's Tandem 1",
                                   tandemII = "Comrey's Tandem 2",
                                   bentlerT = "Bentler T"
                            )

            if (res$rotation != self$options$rotation) {
                rotationNote <- .("No rotation used.")
                rotationMsg <- jmvcore::format(.("Unable to use {rotation} rotation."), rotation = rotationName)
                vijWarningMessage(self, rotationMsg, '.rotation')
            } else if (self$options$rotation != "none") {
                if (self$options$kaiser) {
                    rotationNote <- jmvcore::format(.("{rotation} rotation with Kaiser normalization was used."), rotation = rotationName)
                    res$rotationStr <- jmvcore::format(.("{rotation} rotation with Kaiser normalization"), rotation = rotationName) # for plot
                } else {
                    rotationNote <- jmvcore::format(.("{rotation} rotation was used."), rotation = rotationName)
                    res$rotationStr <- jmvcore::format(.("{rotation} rotation"), rotation = rotationName) # for plot
                }
            } else {
                rotationNote <- NULL
            }

            #### Summary Table ####
            if (self$options$showSummary) {
                eigen <- res$eigenvalues
                eigenSum <- sum(eigen)
                eigenCum <- cumsum(eigen)
                ssl <- res$SSL
                sslCum <- cumsum(ssl)
                for (i in 1:nDim) { # first dimensions
                    self$results$summaryTable$addRow(rowKey = i,
                                                     list(comp = i,
                                                          eigenvalue = eigen[i],
                                                          initVarProp = eigen[i]/eigenSum,
                                                          initVarCum = eigenCum[i]/eigenSum,
                                                          loadings = ssl[i],
                                                          varProp = ssl[i]/eigenSum,
                                                          varCum = sslCum[i]/eigenSum
                                                     ))
                }
                if (length(eigen) > nDim) { # is there more dimensions ?
                    for (i in (nDim+1):length(eigen)) {
                        self$results$summaryTable$addRow(rowKey = i,
                                                         list(comp = i,
                                                              eigenvalue = eigen[i],
                                                              initVarProp = eigen[i]/eigenSum,
                                                              initVarCum = eigenCum[i]/eigenSum,
                                                              loadings = NULL,
                                                              varProp = NULL,
                                                              varCum = NULL
                                                         ))
                    }
                }
                if (!is.null(rotationNote))
                    self$results$summaryTable$setNote('rot', rotationNote)
            }

            #### Loading Table ####
            if (self$options$showLoadings) {
                for(i in 1:nDim) {
                    self$results$loadingTable$addColumn(name = paste0("loading:",i), title = as.character(i), superTitle = "Component", type = "number") #, format = "zto")
                }
                self$results$loadingTable$addColumn(name = "QLT",
                                                    title = "Extraction", #ifelse(self$options$stdVariables, "Communalities", "Explained"),
                                                    type = "number")
                for(aVar in rownames(res$loadings)) {
                    values = list()
                    values[["var"]] <- private$.getVarName(aVar)
                    for(i in 1:nDim) {
                        if (self$options$stdLoadings)
                            values[[paste0("loading:",i)]] <- res$stdLoadings[aVar, i]
                        else
                            values[[paste0("loading:",i)]] <- res$loadings[aVar, i]
                    }
                    values[["QLT"]] <- res$communalities[aVar]
                    self$results$loadingTable$setRow(rowKey = aVar, values = values)
                }
                if (!is.null(rotationNote))
                    self$results$loadingTable$setNote('rot', rotationNote)
                if (self$options$stdLoadings)
                    self$results$loadingTable$setNote('norm', .("Standard coordinates"))
            }

            #### Observation Table ####
            if (self$options$showObservations) {
                if (is.null(self$options$labelVar))
                    self$results$obsTable$addColumn("obs", title = "Observation", type = "integer")
                else
                    self$results$obsTable$addColumn("obs", title = private$.getVarName(self$options$labelVar), type = "text")
                if (!is.null(self$options$groupVar))
                    self$results$obsTable$addColumn("group", title = private$.getVarName(self$options$groupVar), type = "text")
                for(i in 1:nDim) {
                    self$results$obsTable$addColumn(as.character(i), title = as.character(i), superTitle = "Component", type = "number") #, format = "zto")
                }
                self$results$obsTable$addColumn("qlt", title = "Extraction", type = "number", format = "zto")

                nrows <- nrow(res$scores)
                if (nrows > 100) {
                    self$results$obsTable$setNote("100", .("Limited to the first 100 observations"))
                    nrows <- 100
                }
                for (i in 1:nrows) {
                    values = list()
                    values["obs"] <- rownames(res$scores)[i]
                    if (!is.null(self$options$groupVar))
                        values["group"] <- as.character(res$group[i])
                    values["qlt"] <- res$qlt[i]
                    for(j in 1:nDim) {
                        if (self$options$stdScores)
                            values[as.character(j)] <- res$stdScores[i,j]
                        else
                            values[as.character(j)] <- res$scores[i,j]
                    }
                    self$results$obsTable$addRow(rowKey = i, values = values)
                }
                if (!is.null(rotationNote))
                    self$results$obsTable$setNote('rot', rotationNote)
                if (self$options$stdScores)
                    self$results$obsTable$setNote('norm', .("Standard coordinates"))
            }

            #### Plots ####

            rownames(res$loadings) <- sapply(rownames(res$loadings), FUN = private$.getVarName, USE.NAMES = FALSE)
            rownames(res$stdLoadings) <- sapply(rownames(res$loadings), FUN = private$.getVarName, USE.NAMES = FALSE)
            res$groupVarName <- private$.getVarName(self$options$groupVar)

            if (self$options$showScreePlot) {
                screeplot <- self$results$screePlot
                screeplot$setState(res$eigenvalues)
            }
            if (self$options$showVarPlot) {
                varplot <- self$results$varPlot
                varplot$setState(res)
            }
            if (self$options$showObsPlot) {
                obsplot <- self$results$obsPlot
                obsplot$setState(res)
            }
            if (self$options$showBiplot) {
                biplot <- self$results$biPlot
                biplot$setState(res)
            }

            #### Saving coordinates  ####
            if (self$options$stdScores)
                private$.saveCoordinates(res$stdScores, norm = "standard")
            else
                private$.saveCoordinates(res$scores, norm = "principal")

        },
        .screeplot = function(image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            nd <- length(res)
            plot <- ggplot(NULL,aes(x=1:nd, y=res))
            plot <- plot + geom_line(size=0.8) + geom_point(size=3, color="darkgrey")

            plot <- plot + scale_x_continuous(breaks = 1:nd)
            plot <- plot + ggtheme

            # Titles & Labels
            defaults <- list(title = .("Scree Plot"), y = .("Eigenvalues"), x = .("Component"))
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plotType = "scree") + vijTitleAndLabelFormat(self$options)
            # Reset x and y labs (which cannot be common with other plots)
            plot <- plot + labs(x = .("Component"), y = .("Eigenvalues"))

            return(plot)
        },
        .varplot = function(image, ggtheme, theme, ...) {
            return(private$.pcaplot("var", image, ggtheme, theme))
        },
        .obsplot = function(image, ggtheme, theme, ...) {
            return(private$.pcaplot("obs", image, ggtheme, theme))
        },
        .biplot = function(image, ggtheme, theme, ...) {
            return(private$.pcaplot("biplot", image, ggtheme, theme))
        },
        .pcaplot = function(plotType, image, ggtheme, theme, ...) {
            res <- image$state
            if (is.null(res))
                return(FALSE)

            # Axe Titles
            eigenSum <- sum(res$eigenvalues)
            propIn <- round(100*res$SSL/eigenSum,1)
            dim1 <- self$options$xaxis
            dim2 <- self$options$yaxis
            dim1name <- paste0(.("Component"), " ", dim1, " (", propIn[dim1], '\u2009%)')
            dim2name <- paste0(.("Component"), " ", dim2, " (", propIn[dim2], '\u2009%)')

            type <- self$options$biplotType
            if (plotType == "biplot" && type == "formPlot") {
                res$loadings <- res$stdLoadings
            } else if (plotType == "biplot" && type == "covPlot") {
                res$scores <- res$stdScores
            } else if (plotType == "var" && self$options$stdLoadings) {
                res$loadings <- res$stdLoadings
            } else if (plotType == "obs" && self$options$stdScores) {
                res$scores <- res$stdScores
            }

            plot <- ggplot()

            # Reference lines
            plot <- plot + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2)
            # Unit circle
            if (self$options$stdVariables && plotType == "var" && !self$options$stdLoadings)
                plot <- plot + ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), linewidth = 0.2, n = 720)

            #### Obs Plot ####

            if (plotType != "var") {

                if (!is.null(self$options$groupVar))
                    res$scores <- cbind(as.data.frame(res$scores), group = res$group)

                obsData <- as.data.frame(res$scores)
                obsData$Label <- rownames(obsData)

                if (self$options$labelColor == "none")
                    labelColor <- self$options$obsColor
                else
                    labelColor <- self$options$labelColor

                c1 <- names(obsData)[dim1]
                c1 <- ensym(c1)
                c2 <- names(obsData)[dim2]
                c2 <- ensym(c2)

                if (!is.null(self$options$groupVar)) {
                    plot <- plot + geom_point(data = obsData, aes(x = !!c1, y = !!c2, color = group), size = self$options$pointSize)
                } else {
                    plot <- plot + geom_point(data = obsData, aes(x = !!c1, y = !!c2), color = self$options$obsColor, size = self$options$pointSize)
                }
                if (!is.null(self$options$labelVar)) {
                    if (!is.null(self$options$groupVar) && self$options$labelColor == "none") {
                        plot <- plot + ggrepel::geom_text_repel(data = obsData, aes(x = !!c1, y = !!c2, label = Label, color = group),
                                                                check_overlap = TRUE, box.padding = 0.4, min.segment.length = 0.6,
                                                                size = self$options$obsLabelSize/.pt)
                    } else {
                        plot <- plot + ggrepel::geom_text_repel(data = obsData, aes(x = !!c1, y = !!c2, label = Label),
                                                                check_overlap = TRUE, color = labelColor, box.padding = 0.4, min.segment.length = 0.6,
                                                                size = self$options$obsLabelSize/.pt)
                    }
                }
            }

            #### Var Plot ####
            if (plotType !="obs") {
                if (self$options$biplotStretch && plotType == "biplot")
                    res$loadings <- res$loadings * self$options$biplotStretchFactor
                varData <- as.data.frame.array(res$loadings)
                varData$Label <- rownames(varData)

                if (self$options$labelColor == "none")
                    labelColor <- self$options$varColor
                else
                    labelColor <- self$options$labelColor

                c1 <- names(varData)[self$options$xaxis]
                c1 <- ensym(c1)
                c2 <- names(varData)[self$options$yaxis]
                c2 <- ensym(c2)
                if (self$options$biplotLines && plotType == "biplot")
                    plot <- plot + geom_abline(data = varData, aes(intercept = 0, slope = !!c2/!!c1), linetype = 3, color="gray")
                plot <- plot + geom_segment(data = varData, aes(x = 0, y = 0, xend = !!c1, yend = !!c2),
                                            arrow = arrow(length = unit(0.05, "inches"), type = "closed"),
                                            color = self$options$varColor, size = 0.8)
                plot <- plot + ggrepel::geom_text_repel(data = varData, aes(x = !!c1, y = !!c2, label = Label),
                                                        check_overlap = TRUE, min.segment.length = 2,
                                                        position = ggpp::position_nudge_center(x = 0.2, y = 0.01, center_x = 0, center_y = 0),
                                                        size = self$options$varLabelSize/.pt, color = labelColor, fontface="bold")
             }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "color")

            # Axe limits
            if (plotType == "var") {
                xmin = 1.02*min(varData[[dim1]], -1)
                xmax = 1.02*max(varData[[dim1]], 1)
                ymin = 1.02*min(varData[[dim2]], -1)
                ymax = 1.02*max(varData[[dim2]], 1)
            } else if (plotType == "obs") { # floor/ceiling *2 /2 => extend the plot to next 0.5 point
                xmin = 1.01*floor(min(obsData[[dim1]])*2)/2
                xmax = 1.01*ceiling(max(obsData[[dim1]])*2)/2
                ymin = 1.01*floor(min(obsData[[dim2]])*2)/2
                ymax = 1.01*ceiling(max(obsData[[dim2]])*2)/2
            } else { # biplot
                xmin = 1.01*floor(min(obsData[[dim1]], varData[[dim1]])*2)/2
                xmax = 1.01*ceiling(max(obsData[[dim1]], varData[[dim1]])*2)/2
                ymin = 1.01*floor(min(obsData[[dim2]], varData[[dim2]])*2)/2
                ymax = 1.01*ceiling(max(obsData[[dim2]], varData[[dim2]])*2)/2
            }
            plot <- plot + coord_fixed(xlim = c(xmin,xmax), ylim = c(ymin,ymax))

            # Axis ticks (be sure there's a tick at each integer)
            if (plotType != "var" && self$options$stdVariables) {
                if( xmin < -2 && xmax > 2)
                    plot <- plot + scale_x_continuous(breaks = c(-5:5))
                if( ymin < -2 && ymax > 2)
                    plot <- plot + scale_y_continuous(breaks = c(-5:5))
            }

            # Plot frame
            plot <- plot + theme(axis.line = element_line(linewidth = 0), panel.border = element_rect(color = "black", fill = NA, size = 1))

            # Plot title
            title <- switch(plotType,
                                var = .("Component Plot"),
                                obs = .("Observation Plot"),
                                biplot = ifelse(type == "covPlot", .("Covariance Biplot"), .("Form Biplot"))
                            )
            # Plot subtitle
            if (res$rotation != "none")
                subtitle <- res$rotationStr
            else
                subtitle <- NULL

            # Plot Caption
            if ((plotType == "var" && self$options$stdLoadings) || (plotType == "obs" && self$options$stdScores))
                caption <- .("Standard coordinates")
            else
                caption <- NULL

            # Plot legend
            if (!is.null(self$options$groupVar) && plotType != "var")
                legend <- res$groupVarName  #private$.varName[[self$options$groupVar]]
            else
                legend <- NULL


            # Titles & Labels
            defaults <- list(title = title, subtitle = subtitle, caption = caption, legend = legend, y = dim2name, x = dim1name)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plotType = plotType) + vijTitleAndLabelFormat(self$options)

            return(plot)
        },
        .pca = function(data, scale = TRUE, nfact = 2, rotation = "none") {
            data <- jmvcore::naOmit(data)
            res <- prcomp(data, scale. = scale)
            # Full solution
            eigenvalues <- res$sdev**2
            # Loadings (principal)
            stdLoadings <- res$rotation[,1:nfact]
            loadings <- stdLoadings %*% diag(res$sdev[1:nfact])
            # Loading QLT (Communalities)
            if (scale)
                communalities <- rowSums(loadings**2)
            else
                communalities <- rowSums(loadings**2) / rapply(data, var)
            # Principal Scores
            scores <- res$x[,1:nfact]
            # Score QLT
            zdata <- scale(data, scale = scale)
            norm2 <- rowSums(zdata**2)
            norm2pca <- rowSums(scores**2)
            qlt <- norm2pca / norm2
            # Rotation
            if (rotation %in% c("Varimax", "quartimax", "equamax", "parsimax", "entropy", "bentlerT")) {
                if (self$options$stataRotation)
                    rotatedRes <-  try(do.call(getFromNamespace(rotation,'GPArotation'),list(stdLoadings, normalize = self$options$kaiser)))
                else
                    rotatedRes <-  try(do.call(getFromNamespace(rotation,'GPArotation'),list(loadings, normalize = self$options$kaiser)))
                if (inherits(rotatedRes, as.character("try-error"))) {
                    rotation <- "none"
                } else {
                    if (self$options$stataRotation) {
                        rotatedStdLoadings <- rotatedRes$loadings
                        rotatedRes$rotmat <- rotatedRes$Th
                        rotatedSSL <- colSums((loadings %*% rotatedRes$rotmat)**2)
                        rotatedLoadings <- rotatedStdLoadings %*% diag(sqrt(rotatedSSL))
                        rotatedScores <- scores %*% rotatedRes$rotmat
                        rotatedStdScores <- rotatedScores %*% diag(1/sqrt(rotatedSSL))
                    } else {
                        rotatedLoadings <- rotatedRes$loadings
                        rotatedRes$rotmat <- rotatedRes$Th # t(solve(rotatedRes$Th))
                        rotatedSSL <- colSums(rotatedLoadings**2)
                        rotatedStdScores <- scale(scores) %*% rotatedRes$rotmat
                        rotatedScores <- rotatedStdScores %*% diag(sqrt(rotatedSSL))
                        #
                        rotatedStdLoadings <- rotatedLoadings %*% diag(1/sqrt(rotatedSSL))
                    }
                }
            }
            if (rotation == "none") { # no rotation or rotation failed
                rotatedSSL <- eigenvalues[1:nfact]
                rotatedLoadings <- loadings
                rotatedScores <- scores
                #
                rotatedStdScores <- rotatedScores %*% diag(1/sqrt(rotatedSSL))
                rotatedStdLoadings <- stdLoadings
            }
            # Reoder the dims
            dimOrder <- order(rotatedSSL,decreasing=TRUE)
            rotatedSSL <- rotatedSSL[dimOrder]
            rotatedLoadings <- rotatedLoadings[,dimOrder]
            rotatedScores <- rotatedScores[,dimOrder]
            #
            rotatedStdScores <- rotatedStdScores[,dimOrder]
            rotatedStdLoadings <- rotatedStdLoadings[,dimOrder]
            # Fix axis orientations (from psych::principal)
            sign.tot <- sign(colSums(rotatedLoadings[,]))
            sign.tot[sign.tot==0] <- 1
            rotatedLoadings <- rotatedLoadings %*% diag(sign.tot)
            rotatedScores <- rotatedScores %*% diag(sign.tot)
            #
            rotatedStdScores <- rotatedStdScores %*% diag(sign.tot)
            rotatedStdLoadings <- rotatedStdLoadings %*% diag(sign.tot)
            #
            return(list(
                eigenvalues = eigenvalues,
                SSL = rotatedSSL,
                loadings = rotatedLoadings,
                scores = rotatedScores,
                stdLoadings = rotatedStdLoadings,
                stdScores = rotatedStdScores,
                communalities = communalities,
                qlt = qlt,
                rotation = rotation
            ))
        },
        .saveCoordinates = function(coord, norm) {
            if (self$options$obsCoordOV && self$results$obsCoordOV$isNotFilled()) {
                nDim <- self$options$dimNum
                keys <- 1:nDim
                measureTypes <- rep("continuous", nDim)

                titles <- paste(.("Dim"), keys)
                descriptions <- character(length(keys))

                if (norm == "principal") {
                    if (self$options$rotation == "none")
                        descriptionString <- .("PCA Principal Coordinates")
                    else
                        descriptionString <- paste0(.("PCA Principal Coordinates"), " (", self$options$rotation, ")")
                } else {
                    if (self$options$rotation == "none")
                        descriptionString <- .("PCA Standard Coordinates")
                    else
                        descriptionString <- paste0(.("PCA Standard Coordinates"), " (", self$options$rotation, ")")
                }

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
            helpMsg <- .('<p>This module computes <strong>Principal Component Analysis (PCA)</strong> for several continuous variables. Computations are based on <tt>stats::prcomp</tt> function.</p>
<p>Although rotated components are not principal components, they are widely used. Only orthogonal rotations are available (from GPArotation package).</p>
<p>Loadings (variable coordinates) and scores (observation coordinates) are principal (scaled by the squareroot of eigenvalues).</p>
<p>Biplot follows "Biplots in Practice" (Michael Greenacre, 2010):</p>
<ul>
<li><strong>Form biplot:</strong> Scores are principal (scaled by eigenvalues) while loadings are standard.</li>
<li><strong>Covariance biplot:</strong> Loadings are principal while scores are standard. </li>
</ul>
<p><strong>Advanced options</strong>:</p>
<ul>
<li><strong>Standardize loadings:</strong> loadings are normalized with sums of squared equal to 1 (instead of eigenvalues)</li>
<li><strong>Standardize scores:</strong> scores are normalized with variances equal to 1 (instead of eigenvalues)</li>
<li><strong>Rotate eigenvectors:</strong> the rotation (varimax, etc) is applied to eigenvectors (standard loadings) instead of principal loadings (STATA way).</li>
</ul>
<p>A sample file is included at Open > Data Library > vijPlots > Iris</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)

