#--------------------------------------------------------
# Description
# Loads consumption for Amprion from 20100104 to 20130408
# Deaseasonalizing
# Estimates expectiles with tau= 0.01,0.05,0.25,0.5,0.75,0.95,0.99
#--------------------------------------------------------

libraries = c("fda.usc",
              "ggplot2",
              "vars",
              "dse",
              "CADFtest",
              "forecast",
              "gdata",
              "urca",
              "car",
              "np",
              "sm",
              "DierckxSpline",
              "mFilter",
              "stats",
              "tseries",
              "zoo",
              "quantreg",
              "orthogonalsplinebasis",
              "expectreg",
              "splines",
              "Matrix",
              "MatrixModels",
              "cobs",
              "fields",
              "xtable")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)

setwd("C:\\Users\\Franziska Schulz\\Dropbox\\Phd\\R Files\\Electricity Consumption")
