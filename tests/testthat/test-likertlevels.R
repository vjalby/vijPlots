# Level order of likertplot (see CLAUDE.md, "integer variables are treated as ordinal factors").
# Each set of levels is run with integer variables and with ordinal factors labelled "1", "2"...
# (ordered, with the "values" attribute, as jamovi provides them; a factor only has the levels its
# column uses), over every combination of tidyUp, toInteger and reverseLikert.
# The order is checked only with tidyUp: without it, partial levels keep their order of first appearance.

levelCases <- list(
    "complete 1-5"            = list(q1 = 1:5, q2 = 1:5),
    "partial, low first"      = list(q1 = 1:3, q2 = 3:5),
    "partial, high first"     = list(q1 = 3:5, q2 = 1:3),
    "partial, nested"         = list(q1 = 2:4, q2 = 1:5),
    "partial, with holes"     = list(q1 = c(1L, 2L, 4L, 5L), q2 = c(1L, 3L, 5L)),
    "non-continuous"          = list(q1 = c(1L, 10L, 20L, 30L, 50L), q2 = c(1L, 10L, 20L, 30L, 50L)),
    "non-continuous, partial" = list(q1 = c(20L, 30L, 50L), q2 = c(1L, 10L, 20L)),
    "negative"                = list(q1 = -2:3, q2 = -2:3),
    "negative, partial"       = list(q1 = 0:3, q2 = -2:0),
    "no zero"                 = list(q1 = c(-2L, -1L, 1L, 2L), q2 = c(-2L, -1L, 1L, 2L)),
    "no zero, partial"        = list(q1 = 1:2, q2 = -2:-1)
)

# Every level appears at least once, then random answers
levelAnswers <- function(levels, seed) {
    withr::with_seed(seed, c(levels, sample(levels, 24 - length(levels), replace = TRUE)))
}

labelledFactor <- function(x) {
    levels <- sort(unique(x))
    f <- factor(x, levels = levels, labels = as.character(levels), ordered = TRUE)
    attr(f, "values") <- levels
    f
}

runLikertLevels <- function(data, tidyUp, toInteger, reverseLikert) {
    options <- likertplotOptions$new(liks = c("q1", "q2"), group = "g", frequencyTable = TRUE,
                                     showMannU = TRUE, showKW = TRUE,
                                     tidyUp = tidyUp, toInteger = toInteger, reverseLikert = reverseLikert)
    analysis <- likertplotClass$new(options = options, data = data)
    suppressWarnings(suppressMessages({
        analysis$init()
        analysis$run()
        plot <- analysis$.__enclos_env__$private$.plot(analysis$results$plot, ggplot2::theme_gray(), list())
    }))
    list(results = analysis$results, plot = plot)
}

for (caseName in names(levelCases)) {
    case <- levelCases[[caseName]]
    numeric1 <- levelAnswers(case$q1, 1)
    numeric2 <- levelAnswers(case$q2, 2)
    g <- factor(rep(c("x", "y"), 12))
    allLevels <- sort(unique(c(case$q1, case$q2)))

    for (type in c("integer", "factor")) {
        convert <- if (type == "integer") identity else labelledFactor
        data <- data.frame(g = g)
        data$q1 <- convert(numeric1)
        data$q2 <- convert(numeric2)

        for (tidyUp in c(FALSE, TRUE)) for (toInteger in c(FALSE, TRUE)) for (reverseLikert in c(FALSE, TRUE)) {
            label <- sprintf("%s | %s | tidyUp=%d toInteger=%d reverse=%d", caseName, type, tidyUp, toInteger, reverseLikert)
            res <- runLikertLevels(data, tidyUp, toInteger, reverseLikert)

            test_that(paste(label, "| plot"), {
                if (tidyUp) {
                    expected <- if (reverseLikert) rev(allLevels) else allLevels
                    expect_equal(levels(res$plot$data$.answer), as.character(expected))
                }
                expect_no_error(suppressWarnings(suppressMessages(ggplot2::ggplot_build(res$plot))))
            })

            frequencies <- res$results$frequencies$asDF
            levelColumns <- grep("^-?[0-9]+$", names(frequencies), value = TRUE)

            if (tidyUp) {
                test_that(paste(label, "| table order"), {
                    expect_equal(levelColumns, as.character(allLevels))
                })
            }

            test_that(paste(label, "| answers kept"), {
                counts <- as.matrix(frequencies[levelColumns])
                expect_equal(sum(counts, na.rm = TRUE), 48)
                for (level in allLevels) {
                    expect_equal(sum(frequencies[[as.character(level)]], na.rm = TRUE),
                                 sum(numeric1 == level) + sum(numeric2 == level))
                }
            })

            test_that(paste(label, "| numeric tests"), {
                mannWhitney <- suppressWarnings(c(stats::wilcox.test(numeric1 ~ g)$p.value,
                                                  stats::wilcox.test(numeric2 ~ g)$p.value))
                kruskal <- c(stats::kruskal.test(numeric1, g)$p.value, stats::kruskal.test(numeric2, g)$p.value)
                expect_equal(unname(res$results$comp$uTestTable$asDF$p.value), mannWhitney)
                expect_equal(unname(res$results$comp$kwTable$asDF$p.value), kruskal)
            })
        }
    }
}
