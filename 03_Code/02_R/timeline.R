## Package install and load function ======================================
function_package <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}



## Package list ===========================================================
packages <- c("xlsx",                                                      # read excel files
              "reshape2",                                                  # data reshaping such as melt $ cast
              "scales",                                                    # for nice formatting options
              "imputeTS",                                                  # imputation methods for time series data
              "lubridate",                                                 # simple year, month, ... functions
              "zoo",                                                       # another time series package
              "forecast",                                                  # another time series package
              "tseries",                                                   # another time series package
              "timetk",                                                    # yet another time series package (suitable for time series machine learning)
              "multDM",                                                    # Diebold-Mariano test for equal predictive accuracy of two forecasting models
              "dynlm",                                                     # estimating time series models
              "lmtest",                                                    # test statistics for parameter estimates
              "urca",                                                      # for stationarity testing
              "portes",                                                    # univariate and multivariate portmanteau test statistics
              "car",                                                       # for testing of granger causality (funtion linearHypothesis())
              "vars",                                                      # vector autoregressive modelling (VAR)
              "tree",                                                      # machine learning: constructs classification and rgression trees
              "rpart",                                                     # machine learning: recursive partitioning for decision trees
              "rpart.plot",                                                # plotting decision trees
              "caret",                                                     # plotting decision trees
              "randomForest",                                              # machine learning: random forest
              "randomForestSRC",                                           # machine learning: random forest
              "ranger",                                                    # machine learning: random forest
              "h2o",                                                       # machine learning: random forest
              "gbm",                                                       # machine learning: gradient boosting machine
              "xgboost",                                                   # machine learning: extreme gradient boosting
              "e1071",                                                     # machine learning: support vector regression
              "kernlab",                                                   # machine learning: support vector regression
              "pls",                                                       # machine learning: principal component analysis
              "mlr",                                                       # machine learning environment
              "Matrix",                                                    # coercing to sparse matrix (required as input data for some ML algorithms (e.g. xgboost))
              "irace",                                                     # iterated F-race tuning
              "janitor",                                                   # upper case colnames
              "gridExtra",                                                 # allow several ggplots to appear in one window
              "viridis",                                                   # color palette
              "tidyverse",                                                 # open up the universe ...
              "tsibble",                                                   # ... and expand it to time series analyses
              "tidyimpute",                                                # ... and data imputation
              "tidyquant",                                                 # ... and even more functions which can be applied in a tidy fashion
              "beepr",                                                     # sound indicating that code execution has finished
              "ggthemes",                                                  # additional ggplot themes (e.g. economist theme)
              "corrplot",                                                  # correlation plot
              "ggcorrplot",                                                # correlation plot
              "tikzDevice"                                                 # for latex visualiztions
)

function_package(packages)


# Colors ------------------------------------------------------------------
bb_blue_dark <- rgb(0, 69, 125, maxColorValue = 255)
bb_blue_medium <- rgb(102, 144, 177, maxColorValue = 255)
bb_blue_light <- rgb(204, 218, 229, maxColorValue = 255)

bb_red_dark <- rgb(230, 68, 79, maxColorValue = 255)
bb_red_medium <- rgb(235, 105, 114, maxColorValue = 255)
bb_red_light <- rgb(240, 143, 149, maxColorValue = 255)

bb_green_dark <- rgb(151, 191, 13, maxColorValue = 255)
bb_green_medium <- rgb(172, 204, 61, maxColorValue = 255)
bb_green_light <- rgb(193, 216, 110, maxColorValue = 255)

ml_green_dark <- "seagreen4"
ml_green_medium <- "seagreen3"
ml_green_light <- "seagreen2"
# ml_green_dark <- "aquamarine4"
# ml_green_medium <- "aquamarine3"
# ml_green_light <- "aquamarine2"

function_gradient_blue <- colorRampPalette(c(bb_blue_light, bb_blue_dark))
function_gradient_green <- colorRampPalette(c(ml_green_light, ml_green_dark))
function_gradient_redTOgreen <- colorRampPalette(c(bb_red_dark, ml_green_dark))
function_gradient_redTOwhiteTOgreen <- colorRampPalette(c(bb_red_dark, "white", ml_green_dark))
function_gradient_redTOblueTOgreen <- colorRampPalette(c(bb_red_dark, bb_blue_dark, ml_green_dark))


## Time series cross validation ===========================================

# Define some sample data

par(mar=c(0,0,0,0))
plot(0,0,xlim=c(-1,34),ylim=c(0,1),
     xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")

arrows(x0 = 0, y0 = 1, x1 = 32, y1 = 1, 
       lwd = 1.5, length = 0.05)
points(x = 1:21, y = rep(1, 21), pch = 19, col = ml_green_dark)
points(x = 22:31, y = rep(1, 10), pch = 19, col = bb_red_dark)
brackets(x1 = 1, x2 = 21, y1 = 0.98, y2 = 0.98, h = -0.025, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = 11, y = 0.93, labels = "Training set", cex = 0.75)
brackets(x1 = 22, x2 = 31, y1 = 0.98, y2 = 0.98, h = -0.025, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = 26.5, y = 0.93, labels = "Test set", cex = 0.75)

start_y <- 0.8
gap_y <- 0.05 
n_arrow <- 5

brackets(x1 = 1, x2 = 4, y1 = start_y+0.02, y2 = start_y+0.02, h = 0.015, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = 2.5, y = start_y+0.06, labels = "Training subset", cex = 0.75)

brackets(x1 = 4.5, x2 = 5.5, y1 = start_y+0.02, y2 = start_y+0.02, h = 0.015, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = 7, y = start_y+0.06, labels = "Validation set", cex = 0.75)
arrows(x0 = 5, x1 = 7, y0 = start_y + 0.039, y1 = start_y + 0.045, 
       lwd = 0.25, length = 0.00)

for(j in 1:n_arrow)
{
  train <- 1:(20/n_arrow*j)
  val <- (20/n_arrow*j)+1
  rest <- ((20/n_arrow*j)+2):21

  arrows(x0 = 0, y0 = start_y-gap_y*(j-1), x1 = 22, y1 = start_y-gap_y*(j-1), 
         lwd = 1.5, length = 0.05)
  points(x = train, y = rep(start_y-gap_y*(j-1), length(train)), pch = 19, col = ml_green_dark)
  points(x = val, y = rep(start_y-gap_y*(j-1), 1), pch = 19, col = ml_green_medium, cex = 1.5)
  #points(x = val, y = rep(start_y-gap_y*(j-1), 1), pch = "F", col = ml_green_dark, cex = 0.5)
  if(j < n_arrow){
    points(x = rest, y = rep(start_y-gap_y*(j-1), length(rest)), pch = 19, col = ggplot2::alpha("grey60", 0.2))
  }
}
brackets(x1 = -0.2, x2 = -0.2, y1 = start_y, y2 = start_y-(gap_y*(n_arrow-1)), h = -0.5, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = -1.2, y = start_y-((gap_y*(n_arrow-1))/2), labels = "Inner loop", srt = 90, cex = 0.75)



start_y <- 0.5
gap_y <- 0.05 
n_arrow <- 10
j <- 1

for(j in 1:n_arrow)
{
  green <- 1:21
  train <- 22:(22+10/n_arrow*(j-1))
  val <- 21+(10/n_arrow*j)
  rest <- (23+(j-1)):31
  
  arrows(x0 = 0, y0 = start_y-gap_y*(j-1), x1 = 32, y1 = start_y-gap_y*(j-1), 
         lwd = 1.5, length = 0.05)
  points(x = green, y = rep(start_y-gap_y*(j-1), length(green)), pch = 19, col = ml_green_dark)
  if(j != 1){
    points(x = train, y = rep(start_y-gap_y*(j-1), length(train)), pch = 19, col = bb_red_dark)
  } 
  
  points(x = val, y = rep(start_y-gap_y*(j-1), 1), pch = 19, col = bb_red_light, cex = 1.5)
  if(j < n_arrow){
    points(x = rest, y = rep(start_y-gap_y*(j-1), length(rest)), pch = 19, col = ggplot2::alpha("grey60", 0.2))
  }

  
}
brackets(x1 = -0.2, x2 = -0.2, y1 = start_y, y2 = start_y-(gap_y*(n_arrow-1)), h = -0.5, 
         curvature = 1, ticks = 0.5, type = 1)
text(x = -1.2, y = start_y-((gap_y*(n_arrow-1))/2), labels = "Outer loop", srt = 90, cex = 0.75)

text(33.5,1,"time", cex = 0.75)


