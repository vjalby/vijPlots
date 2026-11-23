# Renders a results' plot through jmvcore (as jamovi does) and returns the ggplot drawn
renderedPlot <- function(results) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(results$plot)
    ggplot2::last_plot()
}
