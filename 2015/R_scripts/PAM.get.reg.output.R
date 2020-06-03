## Function will calculate the regression parameters of multiple light curves within a single data frame

## input.df = a single data frame with multiple light curves
## output.df = the name of the output data frame


PAM.regression.batch <- function(input.df) {
  require(lubridate)
if(is.Date(input.df$Date) == FALSE){  stop("Change Date column to Class == Date")}

## Break data frame into a list of elements with each element being a specific light curve
  lc.list <- split(input.df[,c(1:8, 21, 24)], f=paste(input.df$Mem, input.df$Date))
?stop
## Run regression parameter function over each element in the list
  source("/Users/kbg/R_Functions/PAM.regression.formulas.R")
  reg.output.list <- lapply(lc.list, function(x) get.Pmax(x))

## Transform list into data frames for regression parameters and residual sum squares
  reg.params.df <- do.call("rbind", reg.output.list)
  reg.params.df$Date <- as.Date(reg.params.df$Date, "%Y-%m-%d")
  reg.params.df$Mem <- factor(reg.params.df$Mem)

## Create data frame of categorical variables
  cat.var.df <- input.df[,c(1:8, 21, 24)]

## Merge categorical variables and regression output
  reg.df <- merge(cat.var.df, reg.params.df)

return(reg.df)

}

