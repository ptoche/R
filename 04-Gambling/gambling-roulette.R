### Casino Gambling Simulations in R
### Created: 18 April 2015 by Vincent D. Warmerdam
### https://blog.godatadriven.com/lazy-plot
### Edited:  27 April 2017 by Patrick Toche.

## Consider the following roulette strategy:
## 1. bet on black 
##   - if you lose, bet on black again but now double the amount
##     - if you win now, you will have not lost any money 
##     - if you lose again, bet on black again but now double the amount 
##       - if you win now, you will have not lost any money 
##       - if you lose again, bet on black again but now double the amount 
##         - if you win now, you will have not lost any money 
##         - if you lose again, bet on black again but now double the amount 
##           etc ... 
##   - if you win, profit! 
## 2. repeat 
## 
## The idea is that the probability of always getting red converges to zero and as of such you should always be able to win lost money back. This strategy is flawed!

## Set directory:
if(.Platform$OS.type == "windows"){
  setwd("c:/R/math/probabilities/gambling-roulette")
} else { 
  setwd("~/R/math/probabilities/gambling-roulette")
}


## Simulating Gambles in R

# Simulate a random path
win_or_lose <- function(size) {
    sample(c(-1, 1), size, replace = TRUE)
}

set.seed(1)
df1 <- data.frame(cs = cumsum(win_or_lose(100)), t = 1:100)

# plot
library("ggplot2")
ggplot(data = df1, aes(x = t, y = cs)) + geom_line()
# ggplot(df1, aes(t, cs)) + geom_line()  # lazy version

# Simulate another path and add it to the plot
df2 <- data.frame(cs = cumsum(win_or_lose(100)), t = 1:100)
df3 <- data.frame(cs = cumsum(win_or_lose(100)), t = 1:100)
ggplot() + geom_line(data = df1, aes(t, cs)) + 
           geom_line(data = df2, aes(t, cs)) + 
           geom_line(data = df3, aes(t, cs))

# Or add new layers to the previous layer step by step:
set.seed(1)
df1 <- data.frame(cs = cumsum(win_or_lose(100)), t = 1:100)
df3 <- data.frame(cs = cumsum(win_or_lose(100)), t = 1:100)
df3 <- data.frame(cs = cumsum(win_or_lose(100)), t = 1:100)
p <- ggplot() 
p <- p + geom_line(data = df1, aes(t, cs), color = "red") 
p <- p + geom_line(data = df2, aes(t, cs), color = "green")
p <- p + geom_line(data = df3, aes(t, cs), color = "blue")
p 

# You can do many simulations and draw them from a single function:
# if the package has not been loaded, require() gives a warning, while library() gives an error -- require() is usually preferred inside functions
plot_paths <- function(nruns){
  require("ggplot2")  
  p <- ggplot()  
  for(i in 1:nruns){
    df <- data.frame(cs = cumsum(win_or_lose(100)), t = 1:100)
    p <- p + geom_line(data = df, aes(t, cs), alpha = 0.1) + theme_bw()
  }
  p
}

plot_paths(300)

# save the plot to the current directory
ggsave(last_plot(), file = "gambling-roulette-fig1.pdf", width = 5, height = 5)

# 1. Assign colors to series if they result in a long term profit or loss. 
# 2. Make the length of the random path be an input for our simulation function.
# 3. Be explicit about the package where the function is located - more readable
plot_paths_colored <- function(nruns, len){
  p <- ggplot2::ggplot()  
  for(i in 1:nruns){
    color <- "darkgreen"
    df <- data.frame(cs = cumsum(win_or_lose(len)), t = 1:len)
    if(dplyr::select(df, cs)[len,] < 0) color = "darkorange"
    p <- p + geom_line(data = df, aes(t, cs), alpha = 0.4, color = color)
  }
  p
}

plot_paths_colored(300, 200)  # gamble_plot(nruns = 300, len = 200)

# save the plot to the current directory
ggsave(last_plot(), file = "gambling-roulette-fig2.pdf", width = 5, height = 5)


## Simulate the roulette strategy
## Coding is easier if you split your simulation into smaller functional bits. 
## Version 1: Ignore casino wins

gamble <- function(moneyin) {
  if(runif(1) < 0.5) return(moneyin) # outcome is black
  return(-moneyin) # outcome is red
}

nextm <- function(gamble, outcome) {
  if(outcome < 0) return(2*gamble) # if you lose, double the wager
  return(1)
}

simulate <- function(maxt) {
  df <- data.frame(time = as.numeric(c()), money = as.numeric(c()))
  move <- 0
  outcome <- 0
  for(i in 1:maxt) {
    move <- nextm(move, outcome)
    outcome <- gamble(move)
    df <- rbind(df, data.frame(time = i, money = outcome))
  }
  df$money = cumsum(df$money)
  df
}

plot_gambler_ruin <- function(num, maxsim) {
  p <- ggplot2::ggplot() 
  for(i in 1:num) {
    df <- simulate(maxsim)
    p <- p + geom_line(data = df, aes(time, money), alpha = 0.3)
  }
  p
}

# Simulate
set.seed(123456789)
plot_gambler_ruin(1, 100)
plot_gambler_ruin(1, 1000)
plot_gambler_ruin(1, 10000)

# Add a title and save plot
plot_gambler_ruin <- function(num, maxsim) {
  p <- ggplot2::ggplot() 
  for(i in 1:num) {
    df <- simulate(maxsim)
    p <- p + geom_line(data = df, aes(time, money), alpha = 0.3) +
             ggtitle(paste0('1 simulation, ',maxsim,' bets'))
  }
    ggplot2::ggsave(plot = p, file = paste0('gambler_ruin_',maxsim,'.pdf'), 
                              width = 5, height = 5)
  return(p)
}

set.seed(123456789)
plot_gambler_ruin(1, 100)
plot_gambler_ruin(1, 1000)
plot_gambler_ruin(1, 10000)


# The probability of getting red 12 times in a row is small, but if you play this game for a very long time, then this event is not unlikely. And when it occurs, you need to come up with 2^12 dollars of cash.

# Probability Theory: If we play the game an infinite amount of time, we face an infinite amount of risk. Thus, if you want to earn an infinite amount of money, you need an infinite amount of money. In real life, casinos set a maximum bet in all their gambling games, so you cannot apply the 'doubling' strategy indefinitely.

# How likely is it to hit a casino limit during a game? Let k be the number of sequential losses in a game and let n be the number of bets played. Then the probability of having no losses in one game is:
# Prob(no loss in 1 game) = 1 - Prob(loss in 1 game) = 1 - 1/2^k
# Extending this to n games:
# Prob(at least 1 loss in n games) = Prob(0 loss in n games) = 1-(1-1/2^k)^n
# limit(Prob, n -> infty) = 1
f <- function(k, n) 1-((2^k-1)/(2^k))^n

df <- data.frame(prob = f(9, 1:10000), n = 1:10000, limit = " 512")
df <- rbind(df, data.frame(prob = f(10, 1:10000), n = 1:10000, limit = '1024'))
df <- rbind(df, data.frame(prob = f(11, 1:10000), n = 1:10000, limit = '2048'))
df <- rbind(df, data.frame(prob = f(12, 1:10000), n = 1:10000, limit = '4096'))
p <- ggplot()
p <- p + geom_line(data = df, aes(n, prob, colour = limit))
p + ggtitle("probability of hitting budget limit after 'n' gambles")


# Version 2: Incorporate casino wins
# In the European roulette game, there are 37 slots, with a zero for the casino.
# In the American roulette game, there are 38 slots, with a zero and a double zero for the casino.
nextm <- function(gamble, outcome) {
  if(outcome < 0){
    if(gamble > 500){
      return(1)
    }
    return(2*gamble)
  }
  return(1)
}

gamble <- function(moneyin){
  if(runif(1) < 18/37) return(moneyin) # European roulette: one zero
  return(-moneyin)
}

plot_gambler_ruin(50, 2500) + ggtitle("Simulations taking casino win into account: European roulette")

ggsave(last_plot(), file = "gambler_ruin_European_2500.pdf", 
                    width = 5, height = 5)


# Make a histogram of the endgame states
gambler_endgame <- function(num, maxsim){
  df <- data.frame(time = as.numeric(c()), money = as.numeric(c()))
  for(i in 1:num) {
    if(i %% 5 == 0){
      cat(i, 'simulations have now run\n')
    }
    df <- rbind(df, simulate(maxsim)[maxsim,])
  }
  return(df)
}

df <- gambler_endgame(200, 2500) # big simulation, warning: takes a long time
p <- ggplot() 
p <- p + geom_histogram(data = df, aes(money), alpha = 0.9, binwidth = 1000)
p + ggtitle("histogram of casino outcomes after 2500 bets")

ggsave(last_plot(), file = "gambler_ruin_endgame_2500.pdf", 
                    width = 5, height = 5)

# Compute the mean
mean(df$money)
## [1] -223.04

