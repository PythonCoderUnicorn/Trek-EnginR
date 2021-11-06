#========================================
#  STAR TREK ASTROPHYSICS / PHYSICS 
#   By: Zane Dax (She/They)
#   Nov 5, 2021
#========================================


# Note: sunspot.month  is a dataset


library(tidyverse)


#============== constants

Avogadro_number = 6.02214179 * 10e23 # mole^-1   symbol: N_A
Boltzmann_constant = 1.3806504 * 10e-23 #J/K   symbol: k
Rydberg_constant_R = 1.097 * 10e7 # m−1
electron_charge_magnitude = 1.602176487 * 10e-19 # Celsius   symbol: e
permittivity_free_space = 8.854187817 * 10e-12 # C^2 / (N * m^2)  
Planck_constant = 6.62606896 * 10e-34 # J * s  the  h symbol
mass_electron = 9.10938215 * 10e-31 # kg
mass_neutron = 1.674927211 * 10e-27 # kg
mass_proton = 1.672621637 * 10e-27 # kg
mass_hydrogen_electron =  1.67353322 * 10e-27 # kg
mass_helium_2electrons = 6.6466032 * 10e-27 # kg

speed_of_light_vaccuum = 2.99792458 * 10e8 # m/s  symbol: c
light_speed_kms = 299792 # km/s

universal_grav_constant = 6.674 * 10e-11 # N * n^2 /kg^2  symbol: G
universal_gas_constant = 8.314472 # J/(mol * K)  symbol: R

# formula function
permeability_free_space = function(t, m, a){
  vacuum = (4 * pi * 10e-7)
  vacuum * t * (m / a)
}


accelerate_by_earth_gravity = 9.80 # m/s^2 = 32.2 ft/s^2
atmos_pressure_sea = 1.013 * 10e5 # Pa  = 14.70 Lb/inch^2
density_air = 1.29 # kg/m^3   at 0 Celsius 1 atm pressure
sound_speed_air = 343 # m/s  at 20 Celsius 


# -- water 
water_density = 1.000 * 10e3 # kg/m^3  at 4 Celsius
water_latent_heat_fusion = 3.35 * 10e5 # J/Kg
water_latent_heat_vaporization = 2.26  * 10e6 # J/Kg
water_heat_capacity = 4186 # J/(kg * Celsius)

# -- earth
earth_mass = 5.98 * 10e24 # kg
earth_radius = 6.38 * 10e6 # m
earth_avg_dist_sun = 1.50 * 10e11 # m

# -- moon
moon_mass = 7.35 * 10e22 # kg
moon_avg_radius = 1.74 * 10e6 # m
moon_avg_dist_earth = 3.85 * 10e8 # m

# -- sun
sun_mass = 1.99 * 10e30 # kg
sun_avg_radius = 6.96 * 10e8 # m



#  conversions

length_inch_cm = function(inch){
  cm = inch * 2.54
  glue::glue("{cm} cm")
}
length_inch_cm(3)

length_ft_m = function(ft){
  m = ft / 3.281
  glue::glue("{m} m")
}
length_ft_m(4)

length_mi_km = function(mi){
  km = mi * 1.609
  glue::glue("{km} km")
}
length_mi_km(4)

ft_m = 0.3048 # m
mile_ft = 5280 # ft
mile_km = 1.609 # km
angstrom = 10e-10 #m


# -- mass
slug_kg = 14.59 # kg
atomic_mass_unit = 1.6605 * 10e-27 # kg
kg_Lb = 2.205 # Lb

# -- time
earth_day = 24 #hours
earth_day_min = 1.44 * 10e3 # min
earth_day_seconds = 8.64 * 10e4 # seconds

# -- speed
mph_kmH = 1.609 # km/h
mph_mSec = 0.4470 # m/s

# -- force
Lb_N = 4.448 # N
N_dynes = 10e5
N_dynes_Lb = 0.2248 # Lb

# work & energy
Joule_ft.Lb = 10^7 # ergs
kcal_Joule = 4186 # J
Btu_Joule = 1055# J
kWh = 3.600 * 10e6 # J
eV_Joule = 1.602 * 10e19 # J


# --power 
hp_ft.Lb = 550
hp_Watts = 745.7

# -- pressure
Pa_Lb_inch = 1.450 * 10e-4 # Lb/inch^2
Lb_inch_Pa = 6.895 * 10e3 # Pa
atm_Pa = 1.013 * 10e5 # Pa
atm_Lb.inch = 14.70 # Lb/in^2

#-- volume 
Liter_m3 = 10^-3

# -- angle
radian = 57.30
degree_radian = 0.01745


# scientific scale
Tera = 10^12
Giga = 1e9
Mega = 1e6
Kilo = 1e3
Hecto = 1e2
Deka = 1e1
Deci = 10e-1
Centi = 10e-2
Milli = 10e-3
Micro = 10e-6
Nano = 10e-9
Pico = 10e-12
Femto = 10e-15


# ---- formulas

area_of_circle = function(r){ pi * r^2}
area_of_circle(r= 4)

circumf_circle = function(r){ 2 * pi * r}
circumf_circle(r=6)

surf_area_sphere = function(r){4 * pi * r^2}
surf_area_sphere(7)

volume_sphere = function(r){ 4/3 * pi * r^3}
volume_sphere(4)

pythagorean_theorem = function(h0, h_a){ h0^2 + h_a^2}

# cos(x) | sin(x) | tan(x)
# acos(x) | asin(x) | atan(x) | atan2(y, x)
# cospi(x) | sinpi(x) | tanpi(x)

# sine_angle = function(x){ sin(x)}

#-- quadratic formula 
# ax^2 + bx + c = 0 
# x= (-b + sqrt(b^2 - 4*a*c) / 2*a)



# 65 mph => m/s
mph = 65
meter_ft = 3.281
mile_ft #5280
hr_seconds = 3600

speed_ft_sec = function(mph){
  speed_ft_sec = (mph * mile_ft /hr_seconds)
  glue::glue("{speed_ft_sec} ft per second")
}
speed_ft_sec(65)

speed_m_sec = function(ft_sec){
  speed_m_sec = ft_sec / meter_ft
  glue::glue("{speed_m_sec} meters per second")
}
speed_m_sec(ft_sec = 95.3)


# parsecs to light years
parsec_light_year = function(parsec){ 
  ly = parsec * 3.26/1
  glue::glue("{ly} light years")
}   
parsec_light_year(1.29)



