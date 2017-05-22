my.function <- function(df) {
  # this function adds numeric values column by column, 
  # while omitting NA values, for all variables in a data.frame
  # input: df, a data.frame
  # output: the sum of all numbers in the data.frame
  # step 1: sum all columns while omitting NAs
  # step 2: sum all the sums previously calculated
  for (var in names(df)) s <- c(s,sum(na.omit(df[, var])))
  return(sum(s))
}


my.function <- function(df) {# same but by index instead of name
  for (i in 1:length(df)) s <- cat(s, sum(na.omit(df[,i])))
  return(s)
}





my.function <- function(df) {
  for (var in names(df)) s <- c(s, mean(df[, var], na.rm = TRUE))
  return(s)
}