
rm(list = ls())

# For data wrangling and spatial analysis
library(tidyverse)
library(sf)
library(tigris)
library(lubridate)

# For better visuals
library(viridis)
library(ggplot2)
library(ggtext)
library(ggpubr)
library(shadowtext)
library(ggspatial)


source("functions.R")
# 
# get_perm_set1(percent = 0.5, rush = "am")
# get_perm_set2(percent = 0.5, rush = "am")
# get_perm_set3(percent = 0.5, rush = "am")
# get_perm_set4(percent = 0.5, rush = "am")
# get_perm_set5(percent = 0.5, rush = "am")
# get_perm_set6(percent = 0.5, rush = "am")
# get_perm_set7(percent = 0.5, rush = "am")
# get_perm_set8(percent = 0.5, rush = "am")
# 
# get_perm_set1(percent = 0.5, rush = "pm")
# get_perm_set2(percent = 0.5, rush = "pm")
# get_perm_set3(percent = 0.5, rush = "pm")
# get_perm_set4(percent = 0.5, rush = "pm")
# get_perm_set5(percent = 0.5, rush = "pm")
# get_perm_set6(percent = 0.5, rush = "pm")
# get_perm_set7(percent = 0.5, rush = "pm")
# get_perm_set8(percent = 0.5, rush = "pm")
# 
# get_perm_set1(percent = 0.55, rush = "am")
# get_perm_set2(percent = 0.55, rush = "am")
# get_perm_set3(percent = 0.55, rush = "am")
# get_perm_set4(percent = 0.55, rush = "am")
# get_perm_set5(percent = 0.55, rush = "am")
# get_perm_set6(percent = 0.55, rush = "am")
# get_perm_set7(percent = 0.55, rush = "am")
# get_perm_set8(percent = 0.55, rush = "am")
# 
# get_perm_set1(percent = 0.6, rush = "am")
# get_perm_set2(percent = 0.6, rush = "am")
# get_perm_set3(percent = 0.6, rush = "am")
# get_perm_set4(percent = 0.6, rush = "am")
# get_perm_set5(percent = 0.6, rush = "am")
# get_perm_set6(percent = 0.6, rush = "am")
# get_perm_set7(percent = 0.6, rush = "am")
# get_perm_set8(percent = 0.6, rush = "am")
# 
# get_perm_set1(percent = 0.55, rush = "pm")
# get_perm_set2(percent = 0.55, rush = "pm")
# get_perm_set3(percent = 0.55, rush = "pm")
# get_perm_set4(percent = 0.55, rush = "pm")
# get_perm_set5(percent = 0.55, rush = "pm")
# get_perm_set6(percent = 0.55, rush = "pm")
# get_perm_set7(percent = 0.55, rush = "pm")
# get_perm_set8(percent = 0.55, rush = "pm")
# 
# get_perm_set1(percent = 0.6, rush = "pm")
# get_perm_set2(percent = 0.6, rush = "pm")
# get_perm_set3(percent = 0.6, rush = "pm")
# get_perm_set4(percent = 0.6, rush = "pm")
# get_perm_set5(percent = 0.6, rush = "pm")
# get_perm_set6(percent = 0.6, rush = "pm")
# get_perm_set7(percent = 0.6, rush = "pm")
# get_perm_set8(percent = 0.6, rush = "pm")

# get_perm_set1(percent = 0.67, rush = "am")
# get_perm_set2(percent = 0.67, rush = "am")
# get_perm_set3(percent = 0.67, rush = "am")
# get_perm_set4(percent = 0.67, rush = "am")
# get_perm_set5(percent = 0.67, rush = "am")
# get_perm_set6(percent = 0.67, rush = "am")
# get_perm_set7(percent = 0.67, rush = "am")
# get_perm_set8(percent = 0.67, rush = "am")
# 
# get_perm_set1(percent = 0.67, rush = "pm")
# get_perm_set2(percent = 0.67, rush = "pm")
# get_perm_set3(percent = 0.67, rush = "pm")
# get_perm_set4(percent = 0.67, rush = "pm")
# get_perm_set5(percent = 0.67, rush = "pm")
# get_perm_set6(percent = 0.67, rush = "pm")
# get_perm_set7(percent = 0.67, rush = "pm")
# get_perm_set8(percent = 0.67, rush = "pm")




