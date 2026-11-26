### Legacy CA/MCA (corresp, multcorresp): FactoMineR 2.14 code (GPL >= 2, Husson, Josse, Le, Mazet),
### reduced to what vijPlots calls, so that old documents give the same results without importing FactoMineR.

vijLegacySvd <- function(X, row.w, col.w, ncp) {
    trySvd <- function(M) tryCatch(suppressWarnings(svd(M, nu = ncp, nv = ncp)), error = function(e) NULL)
    ncp <- min(ncp, nrow(X) - 1, ncol(X))
    row.w <- row.w / sum(row.w)
    X <- t(t(X) * sqrt(col.w)) * sqrt(row.w)
    if (ncol(X) < nrow(X)) {
        s <- trySvd(X)
        if (is.null(s)) {
            s <- trySvd(t(X))
            if (is.null(s)) stop("svd failed")
            s[c("u", "v")] <- s[c("v", "u")]
        }
        U <- s$u
        V <- s$v
        if (ncp > 1) {
            mult <- sign(as.vector(crossprod(rep(1, nrow(V)), as.matrix(V))))
            mult[mult == 0] <- 1
            U <- t(t(U) * mult)
            V <- t(t(V) * mult)
        }
        U <- U / sqrt(row.w)
        V <- V / sqrt(col.w)
    } else {
        s <- trySvd(t(X))
        if (is.null(s)) {
            s <- trySvd(X)
            if (is.null(s)) stop("svd failed")
            s[c("u", "v")] <- s[c("v", "u")]
        }
        U <- s$v
        V <- s$u
        mult <- sign(as.vector(crossprod(rep(1, nrow(V)), as.matrix(V))))
        mult[mult == 0] <- 1
        V <- t(t(V) * mult) / sqrt(col.w)
        U <- t(t(U) * mult) / sqrt(row.w)
    }
    vs <- s$d[1:min(ncol(X), nrow(X) - 1)]
    num <- which(vs[1:ncp] < 1e-15)
    if (length(num) > 0) {
        U[, num] <- t(t(U[, num, drop = FALSE]) * vs[num])
        V[, num] <- t(t(V[, num, drop = FALSE]) * vs[num])
    }
    list(vs = vs, U = U, V = V)
}

vijLegacyCA <- function(X, ncp = 5, row.sup = NULL, col.sup = NULL) {
    if (is.table(X))
        X <- matrix(as.vector(X), nrow(X), dimnames = dimnames(X))
    if (is.null(rownames(X)))
        rownames(X) <- 1:nrow(X)
    if (is.null(colnames(X)))
        colnames(X) <- colnames(X, do.NULL = FALSE, prefix = "V")
    X <- as.data.frame(X)
    Xtot <- X
    if (!is.null(row.sup))
        X <- as.data.frame(X[-row.sup, ])
    if (!is.null(col.sup))
        X <- as.data.frame(X[, -col.sup])
    # FactoMineR drops empty active rows/columns here: rejected before by corresp, impossible in MCA
    row.w <- rep(1, nrow(X))
    total <- sum(X * row.w)
    F <- as.matrix(X) * (row.w / total)
    marge.col <- colSums(F)
    marge.row <- rowSums(F)
    ncp <- min(ncp, (nrow(X) - 1), (ncol(X) - 1))
    Tc <- t(t(F / marge.row) / marge.col) - 1
    tmp <- vijLegacySvd(Tc, row.w = marge.row, col.w = marge.col, ncp = ncp)
    eig <- tmp$vs^2
    vp <- matrix(NA, length(eig), 3)
    rownames(vp) <- paste("dim", 1:length(eig))
    colnames(vp) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
    vp[, "eigenvalue"] <- eig
    vp[, "percentage of variance"] <- (eig / sum(eig)) * 100
    vp[, "cumulative percentage of variance"] <- cumsum(vp[, "percentage of variance"])
    V <- tmp$V
    U <- tmp$U
    eig <- eig[1:ncol(U)]
    coord.col <- t(t(V) * sqrt(eig))
    coord.row <- t(t(U) * sqrt(eig))
    dist2.col <- colSums(Tc^2 * marge.row)
    contrib.col <- t(t(coord.col^2 * marge.col) / eig)
    cos2.col <- coord.col^2 / dist2.col
    colnames(coord.col) <- colnames(contrib.col) <- colnames(cos2.col) <- paste("Dim", 1:length(eig))
    rownames(coord.col) <- rownames(contrib.col) <- rownames(cos2.col) <- attributes(X)$names
    dist2.row <- rowSums(t(t(Tc^2) * marge.col))
    contrib.row <- t(t(coord.row^2 * marge.row) / eig)
    cos2.row <- coord.row^2 / dist2.row
    colnames(coord.row) <- colnames(contrib.row) <- colnames(cos2.row) <- paste("Dim", 1:length(eig))
    rownames(coord.row) <- rownames(contrib.row) <- rownames(cos2.row) <- attributes(X)$row.names
    # Unnamed, as in FactoMineR (names(x) <- attributes(matrix)$row.names, i.e. NULL)
    inertia.row <- unname(marge.row * dist2.row)
    inertia.col <- unname(marge.col * dist2.col)
    res <- list(eig = vp[1:min(nrow(X) - 1, ncol(X) - 1), , drop = FALSE],
                call = list(marge.col = marge.col, marge.row = marge.row),
                row = list(coord = coord.row[, 1:ncp], contrib = contrib.row[, 1:ncp] * 100,
                           cos2 = cos2.row[, 1:ncp], inertia = inertia.row),
                col = list(coord = as.matrix(coord.col[, 1:ncp]), contrib = as.matrix(contrib.col[, 1:ncp] * 100),
                           cos2 = as.matrix(cos2.col[, 1:ncp]), inertia = inertia.col))
    if (!is.null(row.sup)) {
        X.row.sup <- as.data.frame(Xtot[row.sup, ])
        if (!is.null(col.sup))
            X.row.sup <- as.data.frame(X.row.sup[, -col.sup])
        somme.row <- rowSums(X.row.sup)
        X.row.sup <- X.row.sup / somme.row
        coord.row.sup <- crossprod(t(as.matrix(X.row.sup)), V)
        dist2.row <- rowSums(t((t(X.row.sup) - marge.col)^2 / marge.col))
        cos2.row.sup <- coord.row.sup^2 / dist2.row
        coord.row.sup <- coord.row.sup[, 1:ncp, drop = FALSE]
        cos2.row.sup <- cos2.row.sup[, 1:ncp, drop = FALSE]
        colnames(coord.row.sup) <- colnames(cos2.row.sup) <- paste("Dim", 1:ncp)
        rownames(coord.row.sup) <- rownames(cos2.row.sup) <- rownames(X.row.sup)
        res$row.sup <- list(coord = coord.row.sup, cos2 = cos2.row.sup)
        res$call$row.sup <- row.sup
    }
    if (!is.null(col.sup)) {
        X.col.sup <- as.data.frame(Xtot[, col.sup])
        if (!is.null(row.sup))
            X.col.sup <- as.data.frame(X.col.sup[-row.sup, ])
        X.col.sup <- X.col.sup * row.w
        colnames(X.col.sup) <- colnames(Xtot)[col.sup]
        somme.col <- colSums(X.col.sup)
        X.col.sup <- t(t(X.col.sup) / somme.col)
        coord.col.sup <- crossprod(as.matrix(X.col.sup), U)
        dist2.col <- colSums((X.col.sup - marge.row)^2 / marge.row)
        coord.col.sup <- as.matrix(coord.col.sup[, 1:ncp, drop = FALSE])
        cos2.col.sup <- coord.col.sup^2 / dist2.col
        cos2.col.sup <- cos2.col.sup[, 1:ncp, drop = FALSE]
        colnames(coord.col.sup) <- colnames(cos2.col.sup) <- paste("Dim", 1:ncp)
        rownames(coord.col.sup) <- rownames(cos2.col.sup) <- colnames(X.col.sup)
        res$col.sup <- list(coord = coord.col.sup, cos2 = cos2.col.sup)
        res$call$col.sup <- col.sup
    }
    res
}

vijLegacyDisjunctive <- function(tab) {
    tab <- as.data.frame(tab)
    for (k in seq_along(tab)) tab[, k] <- as.factor(tab[, k])
    nlev <- vapply(tab, nlevels, integer(1))
    offset <- c(0L, cumsum(nlev)[-length(nlev)])
    Z <- matrix(0, nrow(tab), sum(nlev))
    for (k in seq_along(tab))
        Z[cbind(seq_len(nrow(tab)), offset[k] + as.integer(tab[, k]))] <- 1
    if (ncol(tab) == 1) {
        dimnames(Z) <- list(rownames(tab), levels(tab[, 1]))
    } else {
        listModa <- unlist(lapply(tab, levels))
        wlistModa <- which(listModa %in% c("y", "n", "Y", "N"))
        variable <- rep(attributes(tab)$names, lapply(tab, nlevels))
        listModa[wlistModa] <- paste(variable[wlistModa], listModa[wlistModa], sep = ".")
        dimnames(Z) <- list(attributes(tab)$row.names, listModa)
    }
    Z
}

vijLegacyMCA <- function(X, ncp = 5, quali.sup = NULL, method = "Indicator") {
    fct.eta2 <- function(group, Y, weights) {
        weights_norm <- weights / sum(weights)
        somme_p_group <- tapply(weights, group, sum)
        r2 <- function(y) {
            moy_glob <- sum(weights_norm * y)
            d <- y - moy_glob
            sct <- sum(weights * d * d)
            moy_g <- tapply(weights * y, group, sum) / somme_p_group
            dg <- moy_g[group] - moy_glob
            sum(weights * dg * dg) / sct
        }
        unlist(lapply(as.data.frame(Y), r2))
    }
    if (is.null(rownames(X)))
        rownames(X) <- 1:nrow(X)
    X <- as.data.frame(X)
    is.quali <- which(!unlist(lapply(X, is.numeric)))
    X[, is.quali] <- lapply(X[, is.quali, drop = FALSE], as.factor)
    X <- droplevels(X)
    niveau <- unlist(lapply(X, levels))
    if (sum(duplicated(niveau)) > 0) {
        for (j in 1:ncol(X)) {
            if (sum(niveau %in% levels(X[, j])) != nlevels(X[, j]))
                levels(X[, j]) <- paste(attributes(X)$names[j], levels(X[, j]), sep = "_")
        }
    }
    act <- if (!is.null(quali.sup)) (1:ncol(X))[-quali.sup] else 1:ncol(X)
    Z <- vijLegacyDisjunctive(X[, act, drop = FALSE])
    Ztot <- Z
    col.sup <- NULL
    if (!is.null(quali.sup)) {
        Zqs <- vijLegacyDisjunctive(X[, quali.sup])
        Ztot <- cbind.data.frame(Z, Zqs)
        col.sup <- (ncol(Z) + 1):ncol(Ztot)
    }
    Xact <- X[, act]
    row.w <- rep(1, nrow(X))
    if (tolower(method) == "burt") {
        res.mca <- vijLegacyCA(Ztot, ncp = ncol(Z) - length(act), col.sup = col.sup)
        res.mca$col$coord <- t(t(res.mca$col$coord) * sqrt(res.mca$eig[1:ncol(res.mca$col$coord), 1]))
        auxil <- rowSums(res.mca$col$coord^2)
        if (!is.null(col.sup)) {
            res.mca$col.sup$coord <- t(t(res.mca$col.sup$coord) * sqrt(res.mca$eig[1:ncol(res.mca$col.sup$coord), 1]))
            auxil2 <- rowSums(res.mca$col.sup$coord^2)
        }
    }
    res.mca <- vijLegacyCA(Ztot, ncp = min(ncp, ncol(Z) - length(act)), col.sup = col.sup)
    if (is.null(ncol(res.mca$row$coord)))
        res.mca$row$coord <- matrix(res.mca$row$coord, ncol = 1)
    ncp <- ncol(res.mca$row$coord)
    res.mca$call$quali <- (1:ncol(X))
    if (!is.null(quali.sup))
        res.mca$call$quali <- res.mca$call$quali[-quali.sup]
    res.mca$call$quali.sup <- quali.sup
    if (length(act) > 1)
        res.mca$eig <- res.mca$eig[1:min(nrow(X) - 1, sum(sapply(Xact, nlevels)) - length(act)), , drop = FALSE]
    else
        res.mca$eig <- res.mca$eig[1:(nlevels(Xact) - 1), , drop = FALSE]
    names(res.mca)[names(res.mca) == "row"] <- "ind"
    res.mca$ind <- res.mca$ind[1:3]
    names(res.mca)[names(res.mca) == "col"] <- "var"
    if (tolower(method) == "burt") {
        res.mca$var$coord <- t(t(res.mca$var$coord) * sqrt(res.mca$eig[1:ncol(res.mca$var$coord), 1]))
        res.mca$var$cos2 <- res.mca$var$coord^2 / auxil
    }
    res.mca$var <- res.mca$var[1:3]
    if (!is.null(quali.sup)) {
        names(res.mca)[names(res.mca) == "col.sup"] <- "quali.sup"
        if (tolower(method) == "burt") {
            res.mca$quali.sup$coord <- t(t(res.mca$quali.sup$coord) * sqrt(res.mca$eig[1:ncol(res.mca$quali.sup$coord), 1]))
            res.mca$quali.sup$cos2 <- res.mca$quali.sup$coord^2 / auxil2
        }
    }
    variable <- rep(attributes(Xact)$names, unlist(lapply(Xact, nlevels)))
    if (length(act) > 1) {
        CTR <- stats::aggregate(res.mca$var$contrib / 100, by = list(factor(variable)), FUN = sum)
        rownames(CTR) <- CTR[, 1]
        CTR <- t(t(CTR[, -1, drop = FALSE]) * res.mca$eig[1:ncp, 1]) * ncol(Xact)
        res.mca$var$eta2 <- CTR[attributes(Xact)$names, , drop = FALSE]
    } else {
        eta2 <- matrix(1, 1, ncp)
        colnames(eta2) <- paste("Dim", 1:ncp)
        rownames(eta2) <- colnames(X[, act, drop = FALSE])
        res.mca$var$eta2 <- eta2
    }
    if (!is.null(quali.sup)) {
        if (ncp > 1)
            eta2 <- t(sapply(as.data.frame(X[rownames(Xact), quali.sup, drop = FALSE]), fct.eta2, res.mca$ind$coord, weights = row.w))
        else
            eta2 <- as.matrix(sapply(as.data.frame(X[rownames(Xact), quali.sup, drop = FALSE]), fct.eta2, res.mca$ind$coord, weights = row.w), ncol = ncp)
        res.mca$quali.sup$eta2 <- eta2
    }
    if (tolower(method) == "burt") {
        res.mca$eig[, 1] <- res.mca$eig[, 1]^2
        res.mca$eig[, 2] <- res.mca$eig[, 1] / sum(res.mca$eig[, 1]) * 100
        res.mca$eig[, 3] <- cumsum(res.mca$eig[, 2])
    }
    res.mca
}
