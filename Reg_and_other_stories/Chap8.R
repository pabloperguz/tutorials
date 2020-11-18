packages <- c("ggplot2","ggsci","dplyr","insight","bayestestR",
              "rstanarm"  # rstanarm uses Stan as statistical inference engine
              # to fit regression models
)
lapply(packages, require, character.only = TRUE)

### ---- Chapter 8, fitting regression models ---- ###

### Residual sum of squares
rss <- function(x, y, a, b) {
  resid <- y - (a - b*x)
  return(sum(resid^2))
}
