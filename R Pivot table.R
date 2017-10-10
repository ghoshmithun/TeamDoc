install.packages("devtools")
## Install packages
library(devtools)
install_github("ramnathv/htmlwidgets") 
install_github("smartinsightsfromdata/rpivotTable")
## Load rpivotTable
library(rpivotTable)
data(mtcars)
## One line to create pivot table
rpivotTable(mtcars, rows="gear", col="cyl", aggregatorName="Average", 
            vals="mpg", rendererName="Treemap")