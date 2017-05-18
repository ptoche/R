### The Anscombe Quartet Revisited
## Anscombe, Francis J. (1973). "Graphs in Statistical Analysis". American Statistician. 27 (1): 17–21.

## Directory:
if(.Platform$OS.type == "windows"){
  setwd("c:/R/math/probabilities/correlations")
} else { 
  setwd("~/R/math/probabilities/correlations")
}

## Data
df <- anscombe
View(df)

getOption("digits")  # default value is 7
## [1] 7

options(digits=2)

# Means of x
mean(df$x1)
## [1] 9
mean(df$x2)
## [1] 9
mean(df$x3)
## [1] 9
mean(df$x4)
## [1] 9

# Means of y
mean(df$y1)
## [1] 7.5
mean(df$y2)
## [1] 7.5
mean(df$y3)
## [1] 7.5
mean(df$y4)
## [1] 7.5


# Variances of x
var(df$x1)
## [1] 11
var(df$x2)
## [1] 11
var(df$x3)
## [1] 11
var(df$x4)
## [1] 11

# Variances of y
var(df$y1)
## [1] 4.1
var(df$y2)
## [1] 4.1
var(df$y3)
## [1] 4.1
var(df$y4)
## [1] 4.1

## Covariances
cov(df$x1, df$y1)
## [1] 5.5
cov(df$x2, df$y2)
## [1] 5.5
cov(df$x3, df$y3)
## [1] 5.5
cov(df$x4, df$y4)
## [1] 5.5

## Correlations
cor(df$x1, df$y1) -> cor1
## [1] 0.82
cor(df$x2, df$y2) -> cor2
## [1] 0.82
cor(df$x3, df$y3) -> cor3
## [1] 0.82
cor(df$x4, df$y4) -> cor4
## [1] 0.82

## Correlation: Check the definition
cov(df$x1, df$y1) / (sd(df$x1) * sd(df$y1))
## [1] 0.82

## Correlations: Automate the process
sapply(1:4, function(x) cor(df[, x], df[, x+4]))
## [1] 0.82 0.82 0.82 0.82

## Medians: Now the descriptive statistics do differ!
median(df$y1)
## [1] 7.6
median(df$y2)
## [1] 8.1
median(df$y3)
## [1] 7.1
median(df$y4)
## [1] 7.0


## Plots
library("ggplot2")
ggplot(data = df, aes(x = x1, y = y1)) + 
  geom_point(size = 3, pch = 21, fill = "red") + 
  theme_bw() +
  ggtitle('y1 vs x1') -> p1

ggplot(data = df, aes(x = x2, y = y2)) + 
    geom_point(size = 3, pch = 21, fill = "blue") + 
    theme_bw() +
    ggtitle('y2 vs x2') -> p2

ggplot(data = df, aes(x = x3, y = y3)) + 
    geom_point(size = 3, pch = 21, fill = "green") + 
    theme_bw() +
    ggtitle('y3 vs x3') -> p3

ggplot(data = df, aes(x = x4, y = y4)) + 
    geom_point(size = 3, pch = 21, fill = "yellow") + 
    theme_bw() + 
    ggtitle('y4 vs x4') -> p4

## OLS regression
lm1 <- lm(y1 ~ x1, data = df)
lm2 <- lm(y2 ~ x2, data = df)
lm3 <- lm(y3 ~ x3, data = df)
lm4 <- lm(y4 ~ x4, data = df)

## Add regression line to plots
## Method 1: with geom_abline() and lm$coefficients
p1 + 
  geom_abline(intercept = lm1$coefficients[1], 
              slope = lm1$coefficients[2]) +
  annotate("text", x = 12, y = 5, 
           label = paste("correlation = ", format(cor1, digits=2))) -> p1
ggsave(last_plot(), file = "correlations-anscombe-1a.pdf", 
    width = 5, height = 5)

p2 + 
  geom_abline(intercept = lm2$coefficients[1], 
              slope = lm2$coefficients[2]) +
  annotate("text", x = 12, y = 5, 
           label = paste("correlation = ", format(cor2, digits=2))) -> p2
ggsave(last_plot(), file = "correlations-anscombe-2a.pdf", 
    width = 5, height = 5)


p3 + 
  geom_abline(intercept = lm3$coefficients[1], 
              slope = lm3$coefficients[2]) +
  annotate("text", x = 12, y = 5, 
           label = paste("correlation = ", format(cor3, digits=2))) -> p3
ggsave(last_plot(), file = "correlations-anscombe-3a.pdf", 
    width = 5, height = 5)


p4 + 
  geom_abline(intercept = lm4$coefficients[1], 
              slope = lm4$coefficients[2]) +
  annotate("text", x = 12, y = 5, 
           label = paste("correlation = ", format(cor4, digits=2))) -> p4
ggsave(last_plot(), file = "correlations-anscombe-4a.pdf", 
    width = 5, height = 5)


pdf("correlations-anscombe-a.pdf")
  library('gridExtra')
  grid.arrange(p1, p2, p3, p4, 
    top = 'Anscombe Quadrant -- Correlations')
dev.off()

## Method2: with geom_smooth(method = 'lm')
p1 + geom_smooth(method = 'lm')
ggsave(last_plot(), file = "correlations-anscombe-1b.pdf", 
    width = 5, height = 5)
p2 + geom_smooth(method = 'lm')
ggsave(last_plot(), file = "correlations-anscombe-2b.pdf", 
    width = 5, height = 5)
p3 + geom_smooth(method = 'lm')
ggsave(last_plot(), file = "correlations-anscombe-3b.pdf", 
    width = 5, height = 5)
p4 + geom_smooth(method = 'lm')
ggsave(last_plot(), file = "correlations-anscombe-4b.pdf", 
    width = 5, height = 5)



## Make a function to automate the plot-making process
## Remarks: 
   ## explicit return() is good programming habit
   ## variable xn stores "x1" for n=1, 
   ## get(xn) returns the value of the named object
ggplot_n <- function(n) {
  require('ggplot2')
  fn <- c('red', 'blue', 'green', 'yellow')[n]
  xn <- paste0('x', n)
  yn <- paste0('y', n)
  lm <- lm(get(yn) ~ get(xn), data = df)
  cr <- format(cor(df[, xn], df[, yn]), digits=2)
  x1 <- ifelse(n == 4, 6, 2)
  x2 <- ifelse(n == 4, 20, 15)
  ggplot(data = df, aes(x = get(xn), y = get(yn))) + 
    geom_point(size = 3, pch = 21, fill = fn) + 
    geom_smooth(method='lm') +
#    geom_abline(intercept = lm$coefficients[1], 
#              slope = lm$coefficients[2]) +
    annotate("text", x = 12, y = 2.3, 
             label = paste("correlation =", cr)) +
    scale_x_continuous(limits = c(x1,x2), breaks = seq(0, 20, 2)) + 
    scale_y_continuous(limits = c(2,13), breaks = seq(0, 20, 2)) +
    theme_bw() +
    xlab(xn) + ylab(yn) +
    ggtitle(paste0(yn, ' vs ', xn)) -> p
  return(p)
}

ggplot_n(1)

pdf("correlations-anscombe-b.pdf", bg = "lightgray")
  library('gridExtra')
  grid.arrange(ggplot_n(1), ggplot_n(2), ggplot_n(3), ggplot_n(4), 
    top = 'Anscombe Quadrant -- Correlations')
dev.off()

# Add background color to grid - FAIL
pdf("correlations-anscombe.pdf", bg = "gray")
  library('gridExtra')
  margin <- theme(plot.margin = unit(c(1,1,1,1), 'cm'))
  grobs <- lapply(list(p1, p2, p3, p4), '+', margin)
  title <- 'Anscombe Quadrant -- Correlations'
  grid.arrange(grobs = grobs, top = title)
dev.off()
