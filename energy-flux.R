library(showtext)
library(ggtext)


font_add_google(family = 'Oswald','Oswald')
showtext_auto()

data = data.frame(
  day = c(5,10,25,45,70,85,90,100,110,125,130,150,175),
  sales = c(100,125,250,300,350,460,510,460,430,400,370,340,330)
)

ggplot(data = data,
       aes(x= day, y= sales))+
  geom_point(col= '#91a832', size=2, alpha=0.7)+
  geom_smooth()+
  ggdark::dark_mode()+
  labs(title = "\nEnergy flux Analysis\n",
       # subtitle = "Federation Engines",
       x="Stardate.hours",
       y="Energy Flux Levels")+
  scale_x_log10()+
  theme(
    text = element_text(family = "Oswald"),
    plot.title = element_text(face = 'bold', hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_markdown(size = 12),
    axis.title.x = element_markdown(size = 12),
    axis.text.y = element_text(color = '#91a832', 
                               face = 'bold', 
                               size = 12),
    axis.text.x  = element_text(size = 10)
  )
