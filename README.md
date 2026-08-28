# vijPlots

A [jamovi](https://www.jamovi.org) module. Main statistical charts, all built with **ggplot2** and offering extensive customization options:

- **Continuous variables**: Histogram, Box plot, Raincloud plot, Scatter plot, Error bar chart, Lollipop plot, QQ plot
- **Categorical variables**: Bar plot, Pie chart, Mosaic plot, Likert plot
- **Time series**: Line chart, Area chart
- **Multiple Response**: Frequencies, Crosstab

**Main classic multivariate data analyses** are also available:

- Principal Component Analysis
- Correspondence Analysis
- Multiple Correspondence Analysis

The module is available in the **Plots** tab.

## Histogram

![](img/hist.jpg)

## Box Plot

![](img/box.jpg)

## Scatter Plot

![](img/scatter.jpg)

## Bar Chart (for quantitative variables)

![](img/barchart.jpg)

## Lollipop Plot

![](img/lollipop.jpg)

## Raincloud Plot

![](img/raincloud.jpg)

## QQ Plot

![](img/qqplot.jpg)

## Bar Plot (for qualitative variables)

![](img/barplot.jpg)

## Pie Chart

![](img/pie.jpg)

## Mosaic Plot

![](img/mosaic.jpg)

## Likert Plot

![](img/likert.jpg)

## Multiple Responses

![](img/mr.jpg)

## Line Charts

![](img/line.jpg)

## Area Chart

![](img/areachart.jpg)

## Principal Component Analysis

![](img/principal.jpg)

## Correspondence Analysis

![](img/correspondence.jpg)

## Multiple Correspondence Analysis

![](img/mca.jpg)

## Last changes: vijPlots 1.3.1 (2026/08/31)

* Mosaic Plot
* Facet label options (font, size, alignment, background style)
* Multiple Response Plots: the "Counted value" option now accepts any text (e.g. Y/N)
* Line/Area Chart: manual X-axis date range can now be entered as text, and fixed breaks falling outside the specified range
* Multiple Response Frequencies/Crosstabs: fixed a value containing only the separator (e.g. ";") not being treated as missing
* MCA (Burt method): Fixed variable discrimination computation
* Histogram: Stacking group densities and normal curves when Grouping:stacked option is selected
* Histogram: Options to hide bins and to show lines
* Histogram: option to show mean/median reference lines, with an optional value label
* Scatter Plot: Option to use different shapes. Option to set label text size
* Fixed stacking order in barplot/barchart/mrcrosstab
* Support for weights in barplot/piechart
* Improved error handling
* Improved ggplot2 4.0 support
* Code optimization and fixes everywhere


## References

- Larmarange J. (2025). ggstats: Extension to 'ggplot2' for Plotting Stats. R package version 0.8.0, <https://github.com/larmarange/ggstats>.
- Almeida, A., Loy, A., Hofmann, H. (2023). qqplotr: Quantile-Quantile Plot Extensions for 'ggplot2'. R package version 0.0.6, <https://github.com/aloy/qqplotr>.
- Bernaards, C., Gilbert, P., Jennrich, R. (2025), GPArotation: Gradient Projection Factor Rotation. R package version 2025.3.1, <https://cran.r-project.org/package=GPArotation>.
- Greenacre, M. (2010), Biplots in Practice, Fundación BBVA. <https://www.fbbva.es/en/publicaciones/biplots-in-practice-7/>.
- Husson, F., Josse, J., Le, S., Mazet, J. (2025). FactoMineR: Multivariate Exploratory Data Analysis and Data Mining. R package version 2.12, <https://cran.r-project.org/package=FactoMineR>
- Engler, J.B. (2026). tidyplots: Tidy Plots for Scientific Papers, R package version 0.4.0, <https://CRAN.R-project.org/package=tidyplots>
