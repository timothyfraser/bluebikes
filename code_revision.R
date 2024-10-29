# title: "<b>BlueBikes Team Guide</b>"
# subtitle: "Paper: Cycling Cities:<br>Measuring Transportation Equity in Bikeshare Networks"
# date: "`r Sys.Date()`"
# author: "Timothy Fraser, Katherine Van Woert, Sophia Olivieri, Jonathan Baron, Katelyn Buckley, & Pamela Lalli"


# Welcome! This site presents all materials for replicating our team's project analyzing the Boston BlueBikes network. 
# 
# First step: Unzip the `our_data/bluebikes.zip` file into `our_data/bluebikes.sqlite`.

unzip("our_data/bluebikes.zip")


# This repo is our repository for sharing all coding, 
# data, and new techniques for our research project.
# To see our code for building the dataset,
# please see ```project_builder_code.Rmd``` in our RStudioCloud project. To see our code for analyzing the dataset, see below (also labelled ```guide.Rmd``` in the project)!


# PACKAGES #################################

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

# PERMUTATIONS #######################################
# The following code depends on you having run this.
# Prior permutations are already available in the Github Repo, making this not necessarily,
# unless you are interested in re-running them.

# rm(list = ls())
# source("functions.R")
# source("run_sims.R")
# source("run_obs.R")


# MAIN TEXT ##################################################

## Table 1 ############################################################

rm(list = ls())
# Load observed statistics
myobs = read_rds("our_data/obs.rds") %>%
  expand_grid(controls = 1:8) %>%
  bind_rows(
    read_rds("viz/obs_8_50_pm.rds") %>% mutate(controls = 9),
    read_rds("viz/obs_8_67_am.rds") %>% mutate(controls = 10),
    read_rds("viz/obs_8_67_pm.rds") %>% mutate(controls = 11)) %>%
  group_by(controls) %>%
  summarize_at(vars(-c("year", "total")), list(~sum(.) / sum(total) )) %>%
  pivot_longer(cols = -c(controls), names_to = "variable", values_to = "obs") %>%
  separate(col = "variable", into = c("variable", "level"), sep = "_") %>%
  pivot_wider(id_cols = c(controls,variable), names_from = level, values_from = obs) %>%
  mutate(index = 2/3*( abs(ll - .25)  + abs(lh - .25) + abs(hl - .25) + abs(hh - .25))) %>%
  select(controls, variable, obs = index)


# Load perms
myperms <- bind_rows(
  read_rds("our_data/perms_1.rds"),
  read_rds("our_data/perms_2.rds"),
  read_rds("our_data/perms_3.rds"),
  read_rds("our_data/perms_4.rds"),
  read_rds("our_data/perms_5.rds"),
  read_rds("our_data/perms_6.rds"),
  read_rds("our_data/perms_7.rds"),
  read_rds("our_data/perms_8.rds"),
  read_rds("viz/perms_8_50_pm.rds"),
  read_rds("viz/perms_8_67_am.rds"),
  read_rds("viz/perms_8_67_pm.rds"),
  .id = "controls") %>%
  mutate(controls = as.integer(controls)) %>%
  group_by(controls, replicate) %>%
  summarize_at(vars(-c("year", "total")), list(~sum(.) / sum(total) )) %>%
  pivot_longer(cols = -c(controls, replicate), names_to = "variable", values_to = "perm") %>%
  separate(col = "variable", into = c("variable", "level"), sep = "_") %>%
  pivot_wider(id_cols = c(replicate, controls, variable),
              names_from = level, values_from = perm) %>%
  mutate(index = 2/3*( abs(ll - .25)  + abs(lh - .25) + abs(hl - .25) + abs(hh - .25))) %>%
  select(replicate, controls, variable, perm = index)
#0 = complete mixing 1 = maximum divisions





myperms %>%
  group_by(controls) %>%
  summarize(critical = quantile(perm, probs = 0.95),
            lower = quantile(perm, probs = 0.025),
            upper = quantile(perm, probs = 0.975))

mycompare <- myobs %>%
  left_join(by = c("controls", "variable"),
            y = myperms %>% select(controls, variable, perm, replicate),
            multiple = "all") %>%
  # separate(col = "variable", into = c("variable", "level"), sep = "_") %>%
  mutate(variable = variable %>% recode_factor(
    "income" = "<b>Income</b><br>High: >50% 60K+",
    "nonwhite" = "<b>Race</b><br>High: >50% Nonwhite",
    "dense" = "<b>Density</b><br>High: >Median",
    "edu" = "<b>Education</b><br>High: >Median % Some College"),
    controls = controls %>% recode_factor(
      "1" = "1\nYear",
      "2" = "2\nWith\nTraffic",
      "3" = "3\nWith\nDistance",
      "4" = "4\nWith\nDensity",
      "5" = "5\nWith\nEducation",
      "6" = "6\nWith\nAge",
      "7" = "7\nWith\nIncome",
      "8" = "8\nWith\nRace",
      "9" = "9\nWith\nRace\n50%, PM",
      "10" = "10\nWith\nRace\n67%, AM",
      "11" = "10\nWith\nRace\n67%, PM")) %>%
  filter(str_detect(variable, "Income|Race"))


mytext <- mycompare %>%
  group_by(controls, variable) %>%
  # Standardize the test statistics around 0.
  mutate(
    se = sd(perm, na.rm = TRUE),
    mu = mean(perm, na.rm = TRUE),
    perm_scaled = (perm - mu) / se,
    obs_scaled = (obs - mu) / se) %>%
  summarize(
    obs = unique(obs),
    obs_scaled = unique(obs_scaled),
    med = median(perm),
    lower = quantile(perm, probs = 0.025),
    upper = quantile(perm, probs = 0.975),
    # You can either calculate it using scaled measures
    p = sum(abs(perm_scaled) >= abs(obs_scaled)) / n(),
    # Or calculate it directly, then multiply by 2 for two-tailed test
    p_value = case_when(
      obs > med ~ sum(perm >= obs) / n(),
      obs < med ~ sum(perm <= obs) / n())*2,
    # One-tailed test
    p_one = sum( perm >= obs) / n(),
    
    p_label = paste("p = ", p_one, sep = ""),
    # Calculate residual statistic
    res = obs - med) %>%
  # Update such that NAs become p = 1 (since this just means that
  # the permuted and observed values were the same)
  mutate(p_label = case_when(
    p_label == "p = NA" ~ "p = 1",
    p_label == "p = 0" ~ "p < 0.001",
    TRUE ~ p_label),
    obs_scaled = if_else(is.na(obs_scaled), 0, obs_scaled)) 

remove(myperms, myobs)

# Lets output this to csv
mytext %>%
  select(controls, variable, obs, p_label) %>%
  mutate(type = str_extract(variable, "Income|Race")) %>%
  mutate(obs = scales::number(obs, accuracy = 0.001)) %>%
  pivot_wider(id_cols = c(controls), names_from = type, values_from = c(obs, p_label)) %>%
  slice(1:8) %>%
  write_csv("viz/table_1.csv")

## Figures 2-5 ##################################################
rm(list = ls())

# Load functions
source("functions.R")

get_visuals(percent = 0.5, rush = "am")

file.copy(from = "viz/map_50_am.png", to = "viz/fig_2_map.png")
file.copy(from = "viz/heatgrid_50_am.png", to = "viz/fig_3_heatgrid.png")
file.copy(from = "viz/bars_50_am.png", to = "viz/fig_4_bars.png")
file.copy(from = "viz/lines_50_am.png", to = "viz/fig_5_lines.png")
file.copy(from = "viz/lines_multiples_50_am.png", to = "viz/fig_6_lines_multiples.png")


# APPENDIX #######################################################

## Table A1 ##########################################################
# Descriptive Statistics of Neighborhoods around Bluebikes Stations

rm(list = ls())

# Load functions
source("functions.R")

nodes = get_nodes(percent = 0.5)

nodes %>% get_descriptives() %>% write_csv("viz/tab_A1.csv")


## Table A2 ####################################################
# Descriptive Statistics of Total Annual Trips per Bluebikes Station (2011-2021)

# Some stations had as many as 82,756 trips per year;
rm(list = ls())

# Load functions
source("functions.R")

nodes = get_nodes(percent = 0.5)

# Get range of trips for AM
edges = get_edges(nodes = nodes, rush = "am", .weight = 100)
tab1 = get_node_trips_range(edges, rush = "am")

# Get range of trips for PM
edges = get_edges(nodes = nodes, rush = "pm", .weight = 100) 
tab2 = get_node_trips_range(edges, rush = "pm")

# Annual total trips in the sampled ranged from 100 to 82,756 trips per year,
# with an interquartile range of 517 to 23425.

bind_rows(tab1,tab2, .id = "rush") %>%
  mutate(rush = factor(rush, levels = c("am", "pm"))) %>%
  write_csv("viz/tab_A2.csv")


## Table A3 #######################################
rm(list = ls())
source("functions.R")

get_compare_beta(filename = "viz/table_A3.csv")



## Table A4 ###################################################
# Table A4: Share of Bluebikes Neighborhoods above Classification Thresholds

# Also, what percentage of % nonwhite and % Lower Income are above these thresholds?
rm(list = ls())

# Load functions
source("functions.R")

nodes = get_nodes(percent = 0.5)
get_percents(nodes, percent = c(0.5, 0.55, 0.6, 0.67),
             var = c("pop_nonwhite", "pop_lower_income")) %>%
  write_csv("viz/tab_A4.csv")




## Figure A1 ######################################################
rm(list = ls())
# Load functions
source("functions.R")
# Get histograms
gg = nodes %>% get_histograms(percent = 0.5)
ggsave(gg, filename = "viz/fig_A1_histograms.png", dpi = 500, width = 8, height = 8)


## Figure A2 #######################################################
# Figure A2: Sensitivity Testing for All Model, by Classification Threshold and Rush Hour Type


# Repeat permutation tests on these conditions.

rm(list = ls())
source("functions.R")

p1 = get_ptest(percent = 0.5, rush = "am")
p2 = get_ptest(percent = 0.5, rush = "pm")

p3 = get_ptest(percent = 0.55, rush = "am")
p4 = get_ptest(percent = 0.55, rush = "pm")

p5 = get_ptest(percent = 0.60, rush = "am")
p6 = get_ptest(percent = 0.60, rush = "pm")

p7 = get_ptest(percent = 0.67, rush = "am")
p8 = get_ptest(percent = 0.67, rush = "pm")

# Bundle results together into a jumbo table.
bind_rows(p1,p2,p3,p4,p5,p6,p7,p8, .id = "group") %>%
  mutate(group = as.numeric(group)) %>%
  write_csv("viz/ptest.csv")

# Load in ptest data
ptest = read_csv("viz/ptest.csv") %>%
  mutate(controls = controls %>% recode_factor(
    "1" = "1\nYear",
    "2" = "2\nWith\nTraffic",
    "3" = "3\nWith\nDistance",
    "4" = "4\nWith\nDensity",
    "5" = "5\nWith\nEducation",
    "6" = "6\nWith\nAge",
    "7" = "7\nWith\nIncome",
    "8" = "8\nWith\nRace"
  )) %>%
  mutate(variable = variable %>% recode_factor(
    "nonwhite" = "<b>Race</b><br>High:<br>>X% Nonwhite",
    "income" = "<b>Income</b><br>High:<br>>X% $60K+"
  )) %>%
  mutate(rush = rush %>% recode_factor("am" = "AM Rush Hour", "pm" = "PM Rush Hour"))
# Create a series of line-charts

#ptest %>% head()

gg = ggplot() +
  geom_point(data = ptest, mapping = aes(x = percent, y = p_one, color = rush), size = 3) +
  geom_line(data = ptest, mapping = aes(x = percent, y = p_one, color = rush, group = rush)) +
  geom_hline(yintercept = 0.10, linetype = "solid", color = "#373737", linewidth = 1.5, alpha = 0.5) +
  facet_grid(cols = vars(controls),
             rows = vars(variable)) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, color = "#373737"),
        panel.spacing = unit(0.5, "cm"),
        legend.position = "bottom",
        strip.text.y = ggtext::element_markdown(angle = 0)) +
  scale_x_continuous(labels = scales::label_percent(suffix = ""))  +
  scale_color_manual(values =   c("#92CCC7","#250F5A"))  +
  labs(x = "Classification Threshold (X%)",
       y = "p-value for Similarity Index",
       color = "Time",
       subtitle = "Until what % Classification Threshold is Similarity Index Significantly Extreme?")

ggsave(gg, filename = "viz/sensitivity_lines.png", dpi = 500, width = 12, height = 8)
browseURL("viz/sensitivity_lines.png")



ptest$percent_label = scales::percent(ptest$percent)
ptest$controls_id = as.numeric(ptest$controls)



gg = ggplot() +
  geom_rect(
    data = tibble(
      xmin = -Inf, xmax = 6,
      ymin = -Inf, ymax = 0.10),
    mapping = aes(xmin = xmin, xmax = xmax, 
                  ymin = ymin, ymax = ymax,
                  fill = "Target Area")) +
  geom_point(data = ptest,
             mapping = aes(x = controls_id, y = p_one, color = rush), size = 3) +
  geom_line(data = ptest,
            mapping = aes(x = controls_id, y = p_one, color = rush, group = rush)) +
  geom_hline(yintercept = 0.10, linetype = "solid", color = "#373737", linewidth = 1.5, alpha = 0.5) +
  facet_grid(rows = vars(percent_label),
             cols = vars(variable)) +
  scale_fill_manual(values = "#37373733") +
  scale_x_continuous(
    breaks = c(1,2,3,4,5,6,7,8),
    labels = c("1\nYear",
               "2\nWith\nTraffic",
               "3\nWith\nDistance",
               "4\nWith\nDensity",
               "5\nWith\nEducation",
               "6\nWith\nAge",
               "7\nWith\nIncome",
               "8\nWith\nRace")
  ) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, color = "#373737"),
        panel.spacing = unit(0.5, "cm"),
        legend.position = "right",
        strip.text.y = ggtext::element_markdown(angle = 0, color = "white"),
        strip.text.x = ggtext::element_markdown(angle = 0, color = "white"),
        strip.background = element_rect(fill = "black")
  ) +
  #scale_x_continuous(labels = scales::label_percent(suffix = ""))  +
  scale_color_manual(values =   c("#92CCC7","#250F5A"))  +
  labs(x = "Classification Threshold (X%)",
       y = "p-value for Similarity Index",
       color = "Time",
       subtitle = "Until what % Classification Threshold is Similarity Index Significantly Extreme?",
       fill = "Significance\nfor Main Tests")

ggsave(gg, filename = "viz/sensitivity_lines2.png", dpi = 500, width = 14, height = 9)
browseURL("viz/fig_A2_sensitivity_lines2.png")



## Figure A3 #######################################################
# Figure A3: Sensitivity Testing for Change in Neighborhood Similarity Over Time
rm(list = ls())
source("functions.R")

get_compare_visual(filename = "viz/fig_A3_compare.png")


## Extra Figures ######################################################
nodes = get_nodes(percent = 0.5)

edges = bind_rows(
  get_edges(nodes = nodes, rush = "am", .weight = 100) %>% mutate(rush = "am"),
  get_edges(nodes = nodes, rush = "pm", .weight = 100) %>% mutate(rush = "pm")
)


tidyr::expand_grid(
  percent = c(0.5, 0.55, 0.6, 0.67),
  rush = c("am", "pm")
) %>%
  mutate(id = 1:n()) %>%
  split(.$id) %>%
  purrr::walk(.f = ~get_visuals(percent = .$percent, rush = .$rush))

get_visuals(percent = 0.5, rush = "am")
get_visuals(percent = 0.5, rush = "pm")

## Figure A4-A6 ######################################
# Map PM

file.copy(from = "viz/map_50_pm.png", to = "viz/fig_A4_map.png")
file.copy(from = "viz/bars_50_pm.png", to = "viz/fig_A5_bars.png")
file.copy(from = "viz/heatgrid_50_pm.png", to = "viz/fig_A6_heatgrid.png")
file.copy(from = "viz/lines_50_pm.png", to = "viz/fig_A7_lines.png")


# Extra ##################################################

# obs = edges %>% get_stat(weight = "weight")

# Create a label we'll use often
# label = paste0(scales::percent(percent, suffix = ""), "_", rush)




# 
# get_perm_set1(percent = 0.5, rush = "pm")
# get_perm_set2(percent = 0.5, rush = "pm")
# get_perm_set3(percent = 0.5, rush = "pm")
# get_perm_set4(percent = 0.5, rush = "pm")
# get_perm_set5(percent = 0.5, rush = "pm")
# get_perm_set6(percent = 0.5, rush = "pm")
# get_perm_set7(percent = 0.5, rush = "pm")
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
# 
# 
# 
# system.time({
#   get_perm_set8(percent = 0.5, rush = "pm")
# })
# gc()
# system.time({
#   get_perm_set8(percent = 0.67, rush = "pm")
# })
# system.time({
#   get_perm_set8(percent = 0.67, rush = "am")
# })
# system.time({
#   get_perm_set8(percent = 0.5, rush = "am")
# })
# 
# 
# system.time({
#   get_obs_set8(percent = 0.5, rush = "pm")
# })
# gc()
# system.time({
#   get_obs_set8(percent = 0.67, rush = "pm")
# })
# system.time({
#   get_obs_set8(percent = 0.67, rush = "am")
# })
# system.time({
#   get_obs_set8(percent = 0.5, rush = "am")
# })




## extra ########################################


## table (by level) #######################################################
# 
# # Load observed statistics
# myobs = read_rds("our_data/obs.rds") %>%
#   expand_grid(controls = 1:8) %>%
#   bind_rows(
#     read_rds("viz/obs_8_50_pm.rds") %>% mutate(controls = 9),
#     read_rds("viz/obs_8_67_am.rds") %>% mutate(controls = 10),
#     read_rds("viz/obs_8_67_pm.rds") %>% mutate(controls = 11)) %>%
#   group_by(controls) %>%
#   summarize_at(vars(-c("year", "total")), list(~sum(.) / sum(total) )) %>%
#   pivot_longer(cols = -c(controls), names_to = "variable", values_to = "obs")
# 
# 
# # Load perms
# myperms <- bind_rows(
#   read_rds("our_data/perms_1.rds"),
#   read_rds("our_data/perms_2.rds"),
#   read_rds("our_data/perms_3.rds"),
#   read_rds("our_data/perms_4.rds"),
#   read_rds("our_data/perms_5.rds"),
#   read_rds("our_data/perms_6.rds"),
#   read_rds("our_data/perms_7.rds"),
#   read_rds("our_data/perms_8.rds"),
#   read_rds("viz/perms_8_50_pm.rds"),
#   read_rds("viz/perms_8_67_am.rds"),
#   read_rds("viz/perms_8_67_pm.rds"),
#   .id = "controls") %>%
#   mutate(controls = as.integer(controls)) %>%
#   group_by(controls, replicate) %>%
#   summarize_at(vars(-c("year", "total")), list(~sum(.) / sum(total) )) %>%
#   pivot_longer(cols = -c(controls, replicate), names_to = "variable", values_to = "perm")
# 
# mycompare <- myobs %>%
#   left_join(by = c("controls", "variable"),
#             y = myperms %>% select( controls, variable, perm, replicate),
#             multiple = "all") %>%
#   separate(col = "variable", into = c("variable", "level"), sep = "_") %>%
#   mutate(variable = variable %>% recode_factor(
#     "nonwhite" = "<b>Race</b><br>(High = >50% Nonwhite)",
#     "dense" = "<b>Density</b><br>(High = >Median)",
#     "income" = "<b>Income</b><br>(High = >50% 60K+)",
#     "edu" = "<b>Education</b><br>(High = >Median % Some College)"),
#     level = level %>% recode_factor(
#       "ll" = "Low->Low",
#       "lh" = "Low->High",
#       "hl" = "High->Low",
#       "hh" = "High->High"),
#     controls = controls %>% recode_factor(
#       "1" = "1\nYear",
#       "2" = "2\nWith\nTraffic",
#       "3" = "3\nWith\nDistance",
#       "4" = "4\nWith\nDensity",
#       "5" = "5\nWith\nEducation",
#       "6" = "6\nWith\nAge",
#       "7" = "7\nWith\nIncome",
#       "8" = "8\nWith\nRace",
#       "9" = "9\nWith\nRace\n50%, PM",
#       "10" = "10\nWith\nRace\n67%, AM",
#       "11" = "10\nWith\nRace\n67%, PM"
#     )) %>%
#   # Filter to income or race, as our main independent variables
#   filter(str_detect(variable, "Income|Race"))
# 
# 
# mycompare %>% View()
# 
# mytext %>% View()
# mytext <- mycompare %>%
#   group_by(controls, variable, level) %>%
#   # Standardize the test statistics around 0.
#   mutate(se = sd(perm, na.rm = TRUE),
#          mu = mean(perm, na.rm = TRUE),
#          perm_scaled = (perm - mu) / se,
#          obs_scaled = (obs - mu) / se) %>%
#   summarize(
#     obs = unique(obs),
#     obs_scaled = unique(obs_scaled),
#     med = median(perm),
#     lower = quantile(perm, probs = 0.025),
#     upper = quantile(perm, probs = 0.975),
#     # You can either calculate it using scaled measures
#     p = sum(abs(perm_scaled) >= abs(obs_scaled)) / n(),
#     # Or calculate it directly, then multiply by 2 for two-tailed test
#     p_value = case_when(
#       obs > med ~ sum(perm >= obs) / n(),
#       obs < med ~ sum(perm <= obs) / n())*2,
#     # Run a one-tailed test, since we're testing for homophily, not both
#     # How often do permuted values exceed this level of homophily?
#     p_one = sum(perm >= obs) / n(),
#     p_label = paste("p = ", p_one, sep = ""),
#     # Calculate residual statistic
#     res = obs - med) %>%
#   # Update such that NAs become p = 1 (since this just means that
#   # the permuted and observed values were the same)
#   mutate(p_label = case_when(p_label == "p = NA" ~ "p = 1",
#                              p_label == "p = 0" ~ "p < 0.001",
#                              TRUE ~ p_label),
#          obs_scaled = if_else(is.na(obs_scaled), 0, obs_scaled))
# 
# mytext %>% tail()
# # You can test it out here.
# #mycompare %>%
# #  filter(str_detect(variable, "Education") ) %>%
# #  filter(str_detect(controls, "Education") )%>%
# #  head()
# 
# remove(myperms, myobs)
# 



