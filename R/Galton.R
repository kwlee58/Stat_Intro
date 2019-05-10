with(Galton, 
     {
       sunflowerplot(parent,child, xlim=c(62,74), ylim=c(62,74))
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
pairs.panels(galton,main="Galton's Parent child heights")  
#but this makes the regression lines look the same
pairs.panels(galton,lm=TRUE,main="Galton's Parent child heights") 
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

