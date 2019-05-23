library(RColorBrewer)
library(magrittr)
col_pal <- brewer.pal(3, "Spectral")
age_dist <- matrix(c(13709, 17540, 991, 11077, 29648, 2144), nrow = 2, byrow = TRUE)
rownames(age_dist) <- c(1970, 1990)
colnames(age_dist) <- c("0-14세", "15-64세", "65세 이상")
age_dist
text_x <- age_dist %>%
  apply(MARGIN = 1, prop.table) %>%
  apply(MARGIN = 2, function(x) cumsum(x) - x / 2) %>%
  `*`(2 * pi) %>%
  cos %>%
  `*`(0.4)
text_y <- age_dist %>%
  apply(MARGIN = 1, prop.table) %>%
  apply(MARGIN = 2, function(x) cumsum(x) - x / 2) %>%
  `*`(2 * pi) %>%
  sin %>%
  `*`(0.4)
#> 다음 `par`함수의 argument들이 설정되지 않았을 때 어떤 차이가 생기는지 유의
par(family = "AppleGothic", mfrow = c(1, 2), oma = c(0, 0, 2.0, 0))
pie(age_dist[1, ], 
    radius = 0.8, 
    col = col_pal, 
    cex.sub = 1.5)
text(text_x[, 1], text_y[, 1], labels = age_dist[1, ])
title(sub = "1970", line = -2, cex.sub = 1.5)
pie(age_dist[2, ], 
    radius = 0.8, 
    col = col_pal, 
    cex.sub = 1.5)
text(text_x[, 2], text_y[, 2], labels = age_dist[2, ])
title(sub = "1990", line = -3, cex.sub = 1.5)
mtext("연령대별 인구구성", 
      side = 3, 
      outer = TRUE, 
      cex = 2.0, 
      line = -3)
par(mfrow = c(1, 1))
barplot(age_dist)
b <- barplot(t(age_dist), 
             names.arg = rownames(age_dist), 
             width = 2, 
             xlim = c(0, 10),
             space = 0.5,
             axes = FALSE)
axis(side = 2, 
     at = c(0, apply(age_dist, MARGIN = 1, FUN = cumsum)), 
     labels = format(c(0, apply(age_dist, MARGIN = 1, FUN = cumsum)), big.mark = ","), 
     las = 2)
y_text <- apply(age_dist, MARGIN = 1, FUN = function(x) cumsum(x) - x / 2)
text(rep(b, each = 3), y_text, labels = c(t(age_dist)))
mosaicplot(age_dist)
