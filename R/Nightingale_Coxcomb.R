library(HistData)
library(tidyverse)
library(magrittr)
library(grid)
library(gridExtra)
str(Nightingale)
Night <- Nightingale %>%
  as_tibble %>%
  subset(select = c(1, 8:10))
Night
Night %<>%
  gather(key = "Cause", value = "Deaths", -Date) %>%
  mutate(Month = gl(12, 1, 72, labels = month.name[c(4:12, 1:3)])) %>%
  mutate(Regime = gl(2, 12, 72, labels = c("Before", "After"), ordered = TRUE)) 
Night
str(Night)
Night$Cause %<>% 
  sub("\\.rate", "", .)
Night
cxc1 <- ggplot(data = Night %>% subset(Regime == "Before"),
               aes(x = Month, y = Deaths, fill = Cause)) +
  geom_bar(width = 1, stat = "identity", position = "stack", colour = "black") +
  scale_y_sqrt() +
  coord_polar(start = 3*pi/2)
cxc2 <- ggplot(data = Night %>% subset(Regime == "After"),
               aes(x = Month, y = Deaths, fill = Cause)) +
  geom_bar(width = 1, stat = "identity", position = "stack", colour = "black") +
  scale_y_sqrt() +
  coord_polar(start = 3*pi/2)
cxc <- ggplot(data = Night,
              aes(x = Month, y = Deaths, fill = Cause)) +
  geom_bar(width = 1, stat = "identity", position = "stack", colour = "black") +
  scale_y_sqrt() +
  facet_grid(. ~ Regime, scales = "fixed", labeller = label_both) +
  coord_polar(start = 3*pi/2)
# grid.arrange(cxc1, cxc2, 
#              ncol = 2, 
#              respect = TRUE,
#              top = "Nightingale Rose Diagram")