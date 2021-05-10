# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Script name:  PlotCorrelation_line.R
# 
# Author:       Caroline Nettekoven, 2021
# Contact:      nettekoven-enquiries@web.de 
# 
# Description:  Function to plot correlation between two variables.
#               Plots regression line if correlation is significant at 
#               p < .05.
# 
# Parameters: 
# 
#   data        Dataframe with the two variables to be correlated.
# 
#   results    Results of correlation.
#               Object is in the format of the output from the "rcorr"
#               function from the Hmisc package.
#  
#   X           Variable name of x-variable (must match column name in data)
#   Y           Variable name of y-variable (must match column name in data)
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# --------------------------------------------------------------------
PlotCorrelation_line_if_significant <- function(data, results, X, Y) {
  source("/Users/CN/Documents/repos/my_scripts/r/flattenCorrMatrix.R")
  library(ggthemes)
  flat <- flattenCorrMatrix(results$r, results$P)
  
  # Find relevant rows
  selectedRows <- flat[grep(paste(X), flat$row), ]
  selectedRow <- selectedRows[grep(paste(Y), selectedRows$column), ]
  
  r <- selectedRow$cor
  p <- selectedRow$p
  

  xValues <- data[ , which(colnames(data)==paste(X))]
  yValues <- data[ , which(colnames(data)==paste(Y))]
  plotdata <- data.frame(xValues, yValues)
  
  if (p<0.05) { 
  currentplot <- ggplot(plotdata, aes(x=xValues, y=yValues)) + 
    geom_point(color="black") +
    geom_smooth(method=lm, se=TRUE, color="black") +
    theme_tufte(base_size = 15, base_family = "serif") +
    geom_rangeframe() # + ggtitle(paste("p = ", round(p,4), "R = ", round(r,4)))
  } else {
    currentplot <- ggplot(plotdata, aes(x=xValues, y=yValues)) + 
      geom_point(color="black") +
      theme_tufte(base_size = 15, base_family = "serif") +
      geom_rangeframe()
  }
  
  # Add Labels
  print(currentplot + labs(y=paste(Y), x = paste(X), size=5))
  # Print results
  paste("R = ", round(r,4), "p = ", round(p,4))


  }
