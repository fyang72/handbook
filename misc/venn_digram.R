source("http://www.bioconductor.org/biocLite.R")
biocLite("limma")
library(limma)
library(tidyverse)
library(ggforce)
set.seed((123))
mydata <- data.frame(A = rbinom(100, 1, 0.8),
                     B = rbinom(100, 1, 0.7),
                     C = rbinom(100, 1, 0.6)) %>%
  mutate_all(., as.logical)


df.venn <- data.frame(x = c(0, 0.866, -0.866),
                      y = c(1, -0.5, -0.5),
                      labels = c('A', 'B', 'C'))

ggplot(df.venn) +
  geom_circle(aes(x0 = x, y0 = y, r = 1.5, fill = labels), alpha = .3, size = 1, colour = 'grey') +
  coord_fixed() +
  theme_void() +
  theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('cornflowerblue', 'firebrick',  'gold')) +
  scale_colour_manual(values = c('cornflowerblue', 'firebrick', 'gold'), guide = FALSE) +
  labs(fill = NULL) +
  annotate("text", x = df.vdc$x, y = df.vdc$y, label = df.vdc$Counts, size = 5)
