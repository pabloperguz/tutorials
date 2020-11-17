packages <- c("ggplot2","ggsci",
              "rstanarm"  # rstanarm uses Stan as statistical inference engine
                          # to fit regression models
              )
lapply(packages, require, character.only = TRUE)

### ---- Chapter 6, background on regression ---- ###

