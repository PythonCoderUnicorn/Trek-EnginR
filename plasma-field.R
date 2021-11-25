library(ggplot2)
library(viridis)

library(ggpointdensity)

dat <- bind_rows(
  tibble(x = rnorm(7000, sd = 1),
         y = rnorm(7000, sd = 10),
         group = "foo"),
  tibble(x = rnorm(3000, mean = 1, sd = .5),
         y = rnorm(3000, mean = 7, sd = 5),
         group = "bar"))

ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_pointdensity(show.legend = F) +
  scale_color_viridis(option ='G' )+ # A => H 
  ggdark::dark_mode()
