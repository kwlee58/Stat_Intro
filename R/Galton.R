library(HistData)
library(car)
with(Galton, 
     {
       sunflowerplot(parent, child, xlim=c(62,74), ylim=c(62,74))
       reg <- lm(child ~ parent)
       abline(reg)
       lines(lowess(parent, child), col="blue", lwd=2)
       if(require(car)) {
         dataEllipse(parent,child, xlim=c(62,74), ylim=c(62,74), plot.points=FALSE)
       }
     })
library(psych)
data(galton)
describe(galton)
#show the scatter plot and the lowess fit 
pairs.panels(galton, main = "Galton's Parent child heights")  
#but this makes the regression lines look the same
pairs.panels(galton, lm = TRUE, main = "Galton's Parent child heights") 
#better is to scale them 
pairs.panels(galton,
             lm = TRUE,
             xlim = c(62, 74),
             ylim = c(62, 74),
             main = "Galton's Parent child heights") 
with(PearsonLee, 
     {
       lim <- c(55,80)
       xv <- seq(55,80, .5)
       sunflowerplot(parent,child, number=frequency, xlim=lim, ylim=lim, seg.col="gray", size=.1)
       abline(lm(child ~ parent, weights=frequency), col="blue", lwd=2)
       lines(xv, predict(loess(child ~ parent, weights=frequency), data.frame(parent=xv)), 
             col="blue", lwd=2)
       # NB: dataEllipse doesn't take frequency into account
       if(require(car)) {
         dataEllipse(parent,child, xlim=lim, ylim=lim, plot.points=FALSE)
       }
     })
library(ggplot2)
ggplot(PearsonLee, aes(x = parent, y = child, weight=frequency)) +
  geom_point(size = 1.5, position = position_jitter(width = 0.2)) +
  geom_smooth(method = lm, aes(weight = PearsonLee$frequency,
                               colour = 'Linear'), se = FALSE, size = 1.5) +
  geom_smooth(aes(weight = PearsonLee$frequency,
                  colour = 'Loess'), se = FALSE, size = 1.5) +
  facet_grid(chl ~ par) +
  scale_colour_manual(breaks = c('Linear', 'Loess'),
                      values = c('green', 'red')) +
  theme(legend.position = c(0.14, 0.885),
        legend.background = element_rect(fill = 'white'))

x <- rnorm(1000)
z <- rnorm(1000)
par(mfrow = c(3, 2))
for(r in c(0, 0.1, 0.3, 0.5, 0.7, 0.9)){
  y <- r * x + sqrt(1 - r ^ 2) * z
  plot(x, y, pch = 20, main = paste("r = ", r))
}
par(mfrow = c(1, 1))
x <- rnorm(1600)
z <- rnorm(1600)
r <- 0.5
y <- r * x + sqrt(1 - r ^ 2) * z
x <- 68 + 2 * x
y <- 69 + 2 * y
plot(x, y, 
     pch = 20, 
     xlim = c(61, 75),
     ylim = c(62, 76),
     xaxt = "n",
     yaxt = "n",
     xlab = "Father's Height", 
     ylab = "Son's Height", 
     main = "Regression to Mediocrity")
axis(side = 1, at = c(64, 68, 72), labels = c(64, 68, 72))
axis(side = 2, at = c(65, 69, 73), labels = c(65, 69, 73))
abline(v = c(63.8, 64.2, 71.8, 72.2), lty = 2, col = "black")
abline(a = 1, b = 1, lty = 1, lwd = 2, col = "red")
abline(a = 35, b = 0.5, lty = 1, lwd = 2, col = "blue")








