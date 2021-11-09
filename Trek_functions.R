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







# Newton's gravity_law  
# F_g = G*(m1 * m2)/R^2
# F_g = gravity force between mass 1 and mass 2
# G = universal gravitational constant (6.67e-11 N m^2/kg^2)
# m1 = mass 1 (kg)
# m2 = mass 2 (kg)
# R^2 = distance from center of mass 1 to center of mass 2


# calculate the force of gravity between the Sun and planet Uranus

gravity_constant = 6.67e-11 #N m^2/kg^2

# m1, m2
Sun_mass_kg = 2e30 # kg
Uranus_mass_kg = 8.7e25

# R
Uranus_distance_Sun_km = 2.87e9 # 2.74e9 to 3.01e9 km

# convert km to m
R_meters = Uranus_distance_Sun_km *1000


gravity_force = function(m1,m2,R){
  # F_g = G*(m1 * m2)/R^2
  gravity_constant = 6.67e-11 #N m^2/kg^2
  force = (gravity_constant) * ((m1 * m2)/ R^2)
  glue::glue("{formatC(force,format='e', digits=2 )} N")
}

gravity_force(m1= Sun_mass_kg, m2= Uranus_mass_kg, R= R_meters)









