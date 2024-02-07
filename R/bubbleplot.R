# Bubble plot raw data visualisation
# years descending Y, species in columns laong X, CPUE as bubble sizes
# Like scatterplot but species a factor/series
# Simon Dedman 24/2/2017

# import data
CPUE <- read.csv("~/Dropbox/Galway/Project Sections/5. Intro & Conclusion/Viva/CPUEs for Bubble.csv")
CPUE <- read.csv("~/Dropbox/Galway/Project Sections/5. Intro & Conclusion/Viva/CPUEs for Bubble Rev.csv")

# plot data
install.packages("ggplot2")
library("ggplot2")

rays <- colnames(CPUEs)[2:13]
rays
CPUEs[,rays]

# (p + geom_point(aes(x = month, y = year, size = Value, colour = VIX),shape=16, alpha=0.80) +
#     scale_colour_gradient(limits = c(10, 60), low="red", high="black", breaks= seq(10, 60, by = 10))  +
#     scale_x_continuous(breaks = 1:12, labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
#     scale_y_continuous(trans = "reverse") +
#     theme_bw() + opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank())
# )

p <- ggplot(CPUEs, aes(x = CPUEs[,rays], y = CPUEs[,"Year"]))
p <- ggplot(CPUEs)
(p + geom_point(aes(x = CPUEs[,rays], y = CPUEs[,"Year"], size = 3, colour = "black"),shape = 1, alpha = 0.80) +
    scale_x_continuous(breaks = 1:12, labels = rays) +
    scale_y_continuous(trans = "reverse")
)


p <- ggplot(CPUEs, aes(x = CPUEs[,2], y = CPUEs[,"Year"]))
p <- ggplot(CPUEs)
(p + geom_point(aes(size = 3, colour = "black"),shape = 1, alpha = 0.80) +
    scale_y_continuous(trans = "reverse")
)
# plots C.Juve CPUEs descending year on Y, rising CPUE on X
plot(x = CPUE[,2], y = CPUE[,1], xlab = "Ray", ylab = "Year")
# plots C.Juve CPUEs ascending year on Y, rising CPUE on X
plot(x = CPUE[,2], y = CPUE[,1], xlab = "Ray", ylab = "Year", ylim = rev(range(CPUE[,1])))
# plots C.Juve point as 1s ascending year on Y, rising CPUE on X
plot(x = rep(1,length(CPUE[,2])), y = CPUE[,1], xlab = "Ray", ylab = "Year", ylim = rev(range(CPUE[,1])))
# as above, bubble size is CPUE. Looks awful! X axis centred around 1, expand it
plot(x = rep(1,length(CPUE[,2])), y = CPUE[,1], xlab = "Ray", ylab = "Year", ylim = rev(range(CPUE[,1])), cex = CPUE[,2])
# as above, with reducing factor. Better but doesn't display info particularly well. Use log scale
plot(x = rep(1,length(CPUE[,2])), y = CPUE[,1], xlab = "Ray", ylab = "Year", ylim = rev(range(CPUE[,1])), cex = (CPUE[,2])/5, xaxt = "n")
# loses all nuance, all look the same. Go back to before. Need a size chart
plot(x = rep(1,length(CPUE[,2])), y = CPUE[,1], xlab = "Ray", ylab = "Year", ylim = rev(range(CPUE[,1])), cex = log(CPUE[,2]))
# add multiple plots
library(gridExtra)
plot1 <- qplot(x = rep(1,length(CPUE[,2])), y = CPUE[,1], xlab = colnames(CPUE)[2], ylab = "Year", ylim = rev(range(CPUE[,1])), cex = (CPUE[,2])/5)
plot2 <- qplot(x = rep(1,length(CPUE[,3])), y = CPUE[,1], xlab = colnames(CPUE)[3], ylab = "Year", ylim = rev(range(CPUE[,1])), cex = (CPUE[,3])/5)
grid.arrange(plot1, plot2, ncol = 2)

par(mfrow = c(1,2))
par(mar = c(1.5, 0, 0, 0), oma = c(1.5, 1.5, 0.5, 0.5), mgp = c(0.5, 0.5, 0))
plot(x = rep(1,length(CPUE[,2])), y = CPUE[,1], xlab = colnames(CPUE)[2], ylab = "Year", ylim = rev(range(CPUE[,1])), cex = (CPUE[,2])/5, xaxt = "n")
plot(x = rep(1,length(CPUE[,3])), y = CPUE[,1], xlab = colnames(CPUE)[3], ylim = rev(range(CPUE[,1])), cex = (CPUE[,3])/5, xaxt = "n", yaxt = "n")

par(mfrow = c(1,12))
par(mar = c(1.5, 0, 0, 0),
    oma = c(1.5, 1.5, 0.5, 0.5),
    mgp = c(0.5, 0.5, 0))
plot(x = rep(1,length(CPUE[,2])),
     y = CPUE[,1],
     xlab = colnames(CPUE)[2],
     ylab = "Year",
     ylim = rev(range(CPUE[,1])),
     cex = (CPUE[,2])/6, xaxt = "n")
for (i in 3:13) {
plot(x = rep(1,length(CPUE[,i])),
     y = CPUE[,1],
     xlab = colnames(CPUE)[i],
     ylim = rev(range(CPUE[,1])),
     cex = (CPUE[,i])/6,
     xaxt = "n",
     yaxt = "n")
}

par(mfrow = c(1,10))
par(mar = c(1.5, 0, 0, 0),
    oma = c(1.5, 1.5, 0.5, 0.5),
    mgp = c(0.5, 0.5, 0))
plot(x = rep(1,length(CPUE[,2])),
     y = CPUE[,1],
     xlab = colnames(CPUE)[2],
     ylab = "Year",
     ylim = c(2015,1990),
     cex = (CPUE[,2])/14, xaxt = "n")
for (i in c(3,5,6,8,9,11,12)) {
  plot(x = rep(1,length(CPUE[,i])),
       y = CPUE[,1],
       xlab = colnames(CPUE)[i],
       ylim = rev(range(CPUE[,1])),
       cex = (CPUE[,i])/14,
       xaxt = "n",
       yaxt = "n")
}
plot(x = rep(0,length(CPUE[,2])),
     y = CPUE[,1],
     xlab = "",
     ylab = "Year",
     ylim = rev(range(CPUE[,1])),
     cex = 0, xaxt = "n")
legend("bottomright",
      legend = c(1,25,50,100,288),
      col = "black",
      pch = 1,
      pt.cex = c(1/14,25/14,50/14,100/14,288/14),
      y.intersp = 6.8,
      x.intersp = 2.8)

# add horizontal (& vertical lines) to split up by 5 year increments.
# All DOES NOT INCLUDE MATURE MALES!!
