################################
# *** STAR TREK FUNCTIONS ***
################################

library(tidyverse)

recalibration = function(x){
  
  if(x == 8){
    print(" 8 entered")
    x1 = rnorm(1000, mean = 166, sd= 13)
    y1 = rnorm(1000, mean = 180, sd=8)
    df = data.frame(
      x= x1,
      y= y1
    )
    ggplot2::ggplot(
      df,
      aes(x= x1, y= y1)
    )+
      geom_point(size= 0.7, alpha= 0.5, color="sienna1")+
      ggdark::dark_mode()
    
  } else{
    x1 = runif(1000, min = 178, max = 899 )
    y1 = runif(1000, min = 100, max = 1900)
    df = data.frame(x = x1,
                    y = y1)
    ggplot2::ggplot(
      df,
      aes(x= x1, y= y1)
    )+
      geom_point(size= 0.7, 
                 alpha= 0.5,
                 color="sienna1")+
      ggdark::dark_mode()
  }
}
recalibration(8)
