## Plot of standard error by sample size
## accounting for finite population correction
setwd("~/work/Teaching/Courses/ResearchPracticum/LessonPlans/Week14-ReviewOfConcepts")

library(ggplot2)
library(latex2exp)

samp_size_max <- 2000
N <- c(1e9,4000,samp_size_max)
n <- seq(1,samp_size_max,1)

Ns <- rep(N,each=samp_size_max)
f <- n/matrix(Ns,ncol=length(N))

sigma <- 1
std_error <- sqrt(sigma * (1 - f)/n)
std_error <- matrix(std_error,ncol=1)

d <- data.frame(std_error,n,N=as.factor(Ns))

g <- ggplot(data=d,aes(x=n,y=std_error,colour=N)) +
    geom_line() +
    ylim(0,sigma) +
    xlim(0,samp_size_max) +
    labs(
        y=TeX("Standard error ($\\sigma^2 = 1$)"),
        x="Sample size"
    )
g
ggsave(g,file="images/sampleSizePlot.jpeg")

