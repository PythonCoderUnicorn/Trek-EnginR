
# ======== plasma flow regulator

# install.packages('ggstream')
library(ggstream)
library(viridis)




ggplot(blockbusters, aes(year, box_office, fill = genre)) +
  geom_stream(show.legend = F)+
  scale_fill_viridis_d(option = 'F')+
  labs(title = 'Plasma Flow',
       x= 'Plasma Cycle',
       y='radiation level',
       fill='Particle Types'
       )+
  ggdark::dark_mode()













library(patchwork)

base <- ggplot(blockbusters, aes(year, box_office, fill = genre)) + 
  theme(legend.position = "none",
        plot.background = 'black')

(base +  
    geom_stream(bw = 0.5) + 
    ggdark::dark_mode()+
    ggtitle("bw = 0.5")) /
  (base +  geom_stream() + 
     ggdark::dark_mode()+
     ggtitle("Default (bw = 0.75)")) /
  (base +  geom_stream(bw = 1) + 
     ggdark::dark_mode()+
     ggtitle("bw = 1")) +
  scale_fill_viridis_d(option = 'D')+
  theme(legend.position = "none")+
  ggdark::dark_mode()


sample(c('B','J','K','M','D','O'), 4) 
sample(1:400, 6)


round(runif(8, min=123, max = 788))
rnorm(10, mean = 7, sd= 2)












# ============ more plasma


library(patchwork)
set.seed(123)
df <- map_dfr(1:30, ~{
  x <- 1:sample(1:70, 1)
  tibble(x = x + sample(1:150, 1)) %>% 
    mutate(y = sample(1:10, length(x), replace = T),
           k = .x %>% as.character())
})

p <- df %>% 
  ggplot(aes(x, y, fill = k)) +
  # theme_void() +
  ggdark::dark_mode()+
  theme(legend.position = "none", legend.box ="horizontal" )

p1 <- p + 
  geom_stream(color = "black") +
  ggtitle("None (Default)")+
  theme(legend.position = "none")+
  ggdark::dark_mode()

p1
p2 <- p + geom_stream(color = "black", sorting = "inside_out") +
  ggtitle("Inside out")+
  ggdark::dark_mode()
p2

p3 <- p +
  geom_stream(color = "black", sorting = "onset") +
  ggtitle("Onset")+
  ggdark::dark_mode()
p3

p1 / 
  p2 / 
  p3










