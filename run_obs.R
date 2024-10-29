
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

get_obs_set(percent = 0.5, rush = "am")
get_obs_set(percent = 0.55, rush = "am")
get_obs_set(percent = 0.60, rush = "am")
get_obs_set(percent = 0.65, rush = "am")
get_obs_set(percent = 0.67, rush = "am")
get_obs_set(percent = 0.5, rush = "pm")
get_obs_set(percent = 0.55, rush = "pm")
get_obs_set(percent = 0.60, rush = "pm")
get_obs_set(percent = 0.65, rush = "pm")
get_obs_set(percent = 0.67, rush = "pm")

# Cleanup
rm(list = ls())
gc()
