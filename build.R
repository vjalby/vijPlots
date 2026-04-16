install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
options(jamovi_home='/Applications/_Applications/jamovi') # => Put in .Rprofile
options(jamovi_home='C:\\Program Files\\jamovi 2.7.5.0')
jmvtools::check()

getOption("jamovi_home")


jmvtools::create('vijPlots') # Module Name

# From vijMR
jmvtools::addAnalysis(name='mrfrequencies', title='MR Frequencies') # name = function/files name, title = menu item name
jmvtools::addAnalysis(name='mrcrosstabs', title='MR Crosstabs') # name = function/files name, title = menu item name
# From vijLikert
jmvtools::addAnalysis(name='likertplot', title='Likert Plot') # name = function/files name, title = menu item name
# New in vijPlots
jmvtools::addAnalysis(name='histogram', title='Histogram')
jmvtools::addAnalysis(name='boxplot', title='Box Plot')
jmvtools::addAnalysis(name='piechart', title='Pie Chart')
jmvtools::addAnalysis(name='scatterplot', title='Scatter Plot')
jmvtools::addAnalysis(name='barplot', title='Bar Plot')
jmvtools::addAnalysis(name='lollipop', title='Lollipop Plot')
jmvtools::addAnalysis(name='linechart', title='Line Chart')
jmvtools::addAnalysis(name='areachart', title='Area Chart')
jmvtools::addAnalysis(name='raincloud', title='Raincloud Plot')
jmvtools::addAnalysis(name='qqplot', title='Q-Q & P-P Plots')
jmvtools::addAnalysis(name='corresp', title='Correspondence Analysis')
jmvtools::addAnalysis(name='principal', title='Principal Component Analysis')
jmvtools::addAnalysis(name='multcorresp', title='Multiple Correspondence Analysis')
#jmvtools::addAnalysis(name='barplot2', title='Bar Plot 2')

# Install module

jmvtools::install()






## i18n (update)
jmvtools::i18nUpdate("en")
jmvtools::i18nUpdate("fr")
jmvtools::i18nUpdate("es")





## i18n (creation)
jmvtools::i18nCreate("en")
jmvtools::i18nCreate("fr")
jmvtools::i18nCreate("es")



## old i18n
# Find the correct path
node::node()
#/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/node/node-darwin/bin/node
jmvtools:::jmcPath()
# /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/jmvtools/node_modules/jamovi-compiler/index.js

# Command (terminal) to generate i18n
#/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/node/node-darwin/bin/node /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/jmvtools/node_modules/jamovi-compiler/index.js --i18n ./  --create en
#/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/node/node-darwin/bin/node /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/jmvtools/node_modules/jamovi-compiler/index.js --i18n ./  --update en

#/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/node/node-darwin/bin/node /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/jmvtools/node_modules/jamovi-compiler/index.js --i18n ./  --create fr
#/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/node/node-darwin/bin/node /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/jmvtools/node_modules/jamovi-compiler/index.js --i18n ./  --update fr
