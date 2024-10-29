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

# THRESHOLDING ###############################################


#' @name get_nodes
#' @description
#' Write a function to procure a nodes dataset whose variables have been recoded
#' such that the threshold for splitting categories is `percent`,
get_nodes = function(percent = 0.50){
  
  library(dplyr)
  library(readr)
  library(sf)
  
  output = read_rds("our_data/stationbg_dataset.rds") %>%
    # Build a threshold by pop density tericles
    mutate(density = ntile(pop_density_2020_smooth10, 2)) %>%
    # Create halves by age
    mutate(age = ntile(pop_over_65_2019_smooth10, 2)) %>%
    # Create halves by education
    mutate(edu = ntile(pop_some_college_smooth10, 2)) %>%
    # Calculate total (smoothed) share of residents making UNDER the US median household income
    mutate(pop_lower_income = pop_0_40000_2019_smooth10 + pop_40001_60000_2019_smooth10) %>%
    # Now build a threshold describing income 
    mutate(income = ntile(pop_lower_income, 2)) %>%
    # Calculate total (smoothed) nonwhite population per block group
    mutate(pop_nonwhite = 1 - pop_white_2020_smooth10) %>%
    # Create quartiles by race
    mutate(nonwhite = ntile(pop_nonwhite, 2)) %>%
    bind_cols(st_coordinates(.$geometry) %>% as_tibble() %>% select(x = 1, y = 2)) %>%
    # convert to tibble, 
    as_tibble() %>%
    
    # Create our homophily variables for testing
    mutate(
      # Reverse code income, so that Low = More lower income families and High = more upper income families
      bin_income = if_else(pop_lower_income > percent, true = "low", false = "high"),
      bin_nonwhite = if_else(pop_nonwhite > percent, true =  "high", false = "low"),
      bin_edu = if_else(pop_some_college_smooth10 > quantile(pop_some_college_smooth10, probs = percent, na.rm = TRUE),
                        true = "high", false = "low"),
      bin_dense = if_else(pop_density_2020_smooth10 > quantile(pop_density_2020_smooth10, probs = percent, na.rm = TRUE),
                          true = "high", false = "low")) 
  
  return(output)
  
}



get_edges = function(nodes, rush = "am", .weight = 100){
  
  # Load Packages
  library(dplyr)
  library(readr)
  library(RSQLite)
  library(DBI)
  library(sf)
  library(tidygraph)
  library(stringr)
  
  # Tell R to hire a 'SQL translator' object, which we'll name 'mydat',
  # sourced from our bluebikes data
  db <- dbConnect(RSQLite::SQLite(), "our_data/bluebikes.sqlite")
  
  edges_raw = db %>%
    tbl("tally_rush_edges") %>%
    # zoom into just morning
    filter(rush == !!rush) %>%
    # drop unnecessary variables, to keep this dataset as small as possible
    select(-rush) %>%
    collect()
  # Disconnect
  dbDisconnect(db)
  
  # Report  
  ndim = dim(edges_raw)
  cat("\nedges_raw: ", ndim[1], " x ", ndim[2], "\n")
  
  
  # Threshold
  edges_annual = edges_raw %>%
    # Filter to valid stations
    filter(start_code %in% nodes$code &
             end_code %in% nodes$code) %>%
    #filter(!is.na(start_code) & !is.na(end_code)) %>%
    group_by(start_code, end_code, year = str_sub(day, 1,4)) %>%
    summarize(weight = sum(count, na.rm = TRUE)) %>%
    ungroup() %>%
    # THRESHOLD BY USAGE
    # Design-based control for low usage levels
    # Threshold: filter to just pairs where at least 100 bluebikes trips occurred ridden in an entire year
    filter(weight >= .weight) %>%
    # BLOCK BY USAGE
    # classify edgeweights into quintiles
    mutate(traffic = ntile(weight, 5)) %>%
    # BLOCK BY DISTANCE
    left_join(by = c("start_code" = "code"), y = nodes %>% select(code, x1 = x, y1 = y)) %>%
    left_join(by = c("end_code" = "code"), y = nodes %>% select(code, x2 = x, y2 = y)) %>%
    mutate(geometry = sprintf("LINESTRING(%s %s, %s %s)", x1, y1, x2, y2)) %>%
    st_as_sf(wkt = "geometry", crs = 4326) %>%
    select(-x1, -x2, -y1, -y2) %>%
    mutate(dist = as.numeric(st_length(geometry)) / 1000) %>%
    mutate(distcat = ntile(dist, 5)) %>%
    as_tibble() %>%
    # Join in income terciles
    left_join(by = c("start_code" = "code"), y = nodes %>% 
                select(code, 
                       start_density = density, start_edu = edu, start_age = age,
                       start_income = income, start_nonwhite = nonwhite,
                       start_pop_density = pop_density_2020_smooth10)) %>%
    left_join(by = c("end_code" = "code"), y = nodes %>% 
                select(code, end_density = density, end_edu = edu, end_age = age,
                       end_income = income, end_nonwhite = nonwhite,
                       end_pop_density = pop_density_2020_smooth10)) %>%
    # Let's also create a population density normalizing weight for our edges,
    # representing the product between the start and end population density.
    # Also, divide by 1000, because the numbers get really big otherwise
    mutate(weight_pop_density = start_pop_density/1000 * end_pop_density/1000)
  
  # Report  
  ndim = dim(edges_annual)
  cat("\nedges_annual: ", ndim[1], " x ", ndim[2], "\n")
  
  library(tidyverse)
  library(sf)
  library(tidygraph)
  
  edges_annual_prepped = edges_annual %>%
    # Join in key traits of interest
    left_join(
      by = c("start_code" = "code"), 
      y = nodes %>% 
        select(code, start_bin_income = bin_income, 
               start_bin_nonwhite = bin_nonwhite, 
               start_bin_edu = bin_edu, start_bin_dense = bin_dense)) %>%
    left_join(
      by = c("end_code" = "code"), 
      y = nodes %>% 
        select(code, end_bin_income = bin_income, 
               end_bin_nonwhite = bin_nonwhite, 
               end_bin_edu = bin_edu, end_bin_dense = bin_dense)) %>%
    mutate(bin_nonwhite = paste(start_bin_nonwhite, end_bin_nonwhite, sep = "-"),
           bin_income = paste(start_bin_income, end_bin_income, sep = "-"),
           bin_edu = paste(start_bin_edu, end_bin_edu, sep = "-"),
           bin_dense = paste(start_bin_dense, end_bin_dense, sep = "-"))
  
  # Report  
  ndim = dim(edges_annual_prepped)
  cat("\nedges_annual_prepped: ", ndim[1], " x ", ndim[2], "\n")
  
  
  
  return(edges_annual_prepped)
  
}



get_histograms = function(nodes, percent = 0.50){
  
  hists = nodes %>%
    select(geoid, pop_nonwhite, pop_lower_income, pop_some_college_smooth10, pop_density_2020_smooth10) %>%
    pivot_longer(cols = -c(geoid), names_to = "var", values_to = "x") %>%
    mutate(var_label = var %>% dplyr::recode_factor(
      "pop_nonwhite" = "% Nonwhite",
      "pop_lower_income" = "% Lower Income (<$60K)",
      "pop_some_college_smooth10" = "% Some College Education",
      "pop_density_2020_smooth10" = "Population Density"
    ))
  
  
  lines = hists %>%
    group_by(var, var_label) %>%
    summarize(threshold = quantile(x, probs = percent, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(threshold = if_else(var %in% c("pop_nonwhite", "pop_lower_income"), percent, threshold))
  
  
  
  myblues <- c("#2A0B49",
               "#250F5A",
               "#1A136A",
               "#18247A",
               "#1d428a",
               "#3A6D9B",
               "#5793AC",
               "#74B5BC",
               "#92CCC7",
               "#B1DBD0",
               "#D0EADE")
  
  ggplot() +
    geom_histogram(data = hists, mapping = aes(x = x, group = var, fill = var),
                   color = "white", linewidth = 0.5) +
    geom_vline(data = lines, mapping = aes(xintercept = threshold, group = var, color = "Threshold"),
               linewidth = 1.5, linetype = "dashed") +
    facet_wrap(~var_label, scales = "free_x", ncol = 2) +
    guides(fill = "none") +
    scale_fill_manual(values = myblues[c(1,4,6,8)],
                      breaks = c("pop_nonwhite", "pop_lower_income", 
                                 "pop_some_college_smooth10", "pop_density_2020_smooth10")) +
    scale_color_manual(values = "black") +
    scale_y_continuous(expand = expansion(0,0)) +
    labs(x = "Distribution of Census Block Group Traits per Station",
         y = "Frequency (Count)",
         color = NULL,
         caption = "Threshold shows cutoff for binning stations into high vs. low categories.") +
    theme_classic(base_size = 14) +
    theme(legend.position = "bottom",
          panel.border = element_rect(fill = NA, color = "#373737"), 
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(color = "white"),
          panel.spacing = unit(0.5, "cm"))
  
}

get_descriptives = function(nodes){
  
  nodes %>%
    select(geoid, pop_nonwhite, pop_lower_income, pop_some_college_smooth10, pop_density_2020_smooth10) %>%
    pivot_longer(cols = -c(geoid), names_to = "var", values_to = "x") %>%
    mutate(var_label = var %>% dplyr::recode_factor(
      "pop_nonwhite" = "% Nonwhite",
      "pop_lower_income" = "% Lower Income (<$60K)",
      "pop_some_college_smooth10" = "% Some College Education",
      "pop_density_2020_smooth10" = "Population Density"
    )) %>%
    group_by(var, var_label) %>%
    summarize(
      mean = mean(x),
      sd = sd(x),
      median = median(x),
      iqr = quantile(x, probs = c(0.75, 0.25)) %>% diff(),
      min = min(x),
      max = max(x),
    ) %>%
    ungroup() %>%
    mutate(across(mean:max, .fns = ~scales::number(.x, accuracy = 0.001))) %>%
    arrange(var_label)
  
  
}

get_percents = function(nodes, percent = c(0.5, .55, .60, .67), 
                        var = c("pop_nonwhite", "pop_lower_income")){
  tidyr::expand_grid(
    percent = percent,
    var = var
  ) %>%
    mutate(id = 1:n()) %>%
    split(.$id) %>%
    purrr::map_dfr(
      .f = ~tibble(
        var = .$var, percent = .$percent, 
        over = sum(nodes[.$var] > .$percent) / sum(!is.na(nodes[.$var])))) %>%
    pivot_wider(id_cols = c(percent), names_from = var, values_from = over)
  
}

get_node_trips = function(edges){
  bind_rows(
    edges %>%
      group_by(code = start_code, year) %>%
      summarize(weight = sum(weight)),
    edges %>%
      group_by(code = end_code, year) %>%
      summarize(weight = sum(weight))
  ) %>%
    group_by(year, code) %>%
    summarize(weight = sum(weight))
}



get_node_trips_range = function(edges, rush= "am"){
  # Looking at morning rush hour...
  edges %>%
    filter(rush == !!rush) %>%
    get_node_trips() %>%
    ungroup() %>%
    summarize(
      n = unique(code) %>% length(),
      mean = mean(weight),
      sd = sd(weight),
      median = quantile(weight, probs = 0.5),
      
      lower = quantile(weight, probs = 0.25),
      upper = quantile(weight, probs = 0.975),
      min = quantile(weight, probs = 0),
      max = quantile(weight, probs = 1))
}


# Repeat our sampling function
get_sample = function(x){
  if(length(x) == 1){
    return(x)
  }else{
    sample(x, size = length(x), replace = FALSE) %>%
      return()}
}

# Let's write a quick wrapper function to permute the data
get_perm = function(net, nreps){
  print(nreps)
  
  net %>%
    group_by(block) %>%
    mutate(perm = get_sample(weight) %>% unlist()) %>%
    ungroup() %>%
    return()
}

# Let's write a second function to calculate summary statistics from the data
get_stat = function(data, weight = "weight"){
  #data = edges; weight = 'weight'
  data %>%
    # Normalize by the product of source * destination pop density per km2
    mutate(weight = !!sym(weight) / weight_pop_density) %>%
    # Get homophily by year (since we can just aggregate the final stats later)
    group_by(year) %>%
    summarize(
      # Race
      nonwhite_ll = sum(weight[bin_nonwhite == "low-low"], na.rm = TRUE),
      nonwhite_lh = sum(weight[bin_nonwhite == "low-high"], na.rm = TRUE),
      nonwhite_hl = sum(weight[bin_nonwhite == "high-low"], na.rm = TRUE),
      nonwhite_hh = sum(weight[bin_nonwhite == "high-high"], na.rm = TRUE),
      # Income
      income_ll = sum(weight[bin_income == "low-low"], na.rm = TRUE),
      income_lh = sum(weight[bin_income == "low-high"], na.rm = TRUE),
      income_hl = sum(weight[bin_income == "high-low"], na.rm = TRUE),
      income_hh = sum(weight[bin_income == "high-high"], na.rm = TRUE),
      # Education
      edu_ll = sum(weight[bin_edu == "low-low"], na.rm = TRUE),
      edu_lh = sum(weight[bin_edu == "low-high"], na.rm = TRUE),
      edu_hl = sum(weight[bin_edu == "high-low"], na.rm = TRUE),
      edu_hh = sum(weight[bin_edu == "high-high"], na.rm = TRUE),
      # Dense
      dense_ll = sum(weight[bin_dense == "low-low"], na.rm = TRUE),
      dense_lh = sum(weight[bin_dense == "low-high"], na.rm = TRUE),
      dense_hl = sum(weight[bin_dense == "high-low"], na.rm = TRUE),
      dense_hh = sum(weight[bin_dense == "high-high"], na.rm = TRUE)) %>%
    # Get total
    mutate(total = nonwhite_ll + nonwhite_lh + nonwhite_hl + nonwhite_hh) %>%
    return()
}


# Run analysis
get_visuals = function(percent = 0.5, rush = "am"){
  # Create a label we'll use often
  label = paste0(scales::percent(percent, suffix = ""), "_", rush)
  
  # Generate nodes and edges
  nodes = get_nodes(percent = percent) 
  edges = get_edges(nodes = nodes, rush = rush, .weight = 100)
  
  # Get edgewise visuals
  get_heatgrid(
    edges = edges, 
    nodes = nodes,
    filename = paste0("viz/heatgrid_", label, ".png"))
  
  get_map(
    edges = edges,
    percent = percent,
    filename = paste0("viz/map_", label, ".png")
  )
  
  # Generate homophily/mixing statistics
  obs = edges %>% get_stat(weight = "weight")
  
  # Get homophily visuals
  get_bars(obs = obs, percent = percent, filename = paste0("viz/bars_", label, ".png"))
  
  get_lines(obs = obs, filename = paste0("viz/lines_", label, ".png"))
  
  get_lines_multiples(obs = obs, filename = paste0("viz/lines_multiples_", label, ".png"))
  
  # Message completion
  cat("\n---done: ", percent, " x ", rush, "\n")
}

get_compare_visual = function(filename){
  get_obs_one = function(percent, rush){
    # Generate nodes and edges
    nodes = get_nodes(percent = percent) 
    edges = get_edges(nodes = nodes, rush = rush, .weight = 100)
    # Generate homophily/mixing statistics
    obs = edges %>% get_stat(weight = "weight")
    return(obs)
  }
  
  grid = tidyr::expand_grid(
    percent = c(0.5, 0.67),
    rush = c("am", "pm")
  ) %>%
    mutate(id = 1:n())
  
  stat = grid %>%
    split(.$id) %>%
    purrr::map_dfr(.f = ~get_obs_one(percent = .$percent, rush = .$rush), .id = "id") %>%
    mutate(id = as.integer(id)) %>%
    left_join(by = "id", y = grid)
  
  
  stat = stat %>% 
    mutate(rush = rush %>% recode_factor(
      "am" = "AM Rush Hour",
      "pm" = "PM Rush Hour"
    ))
  
  myobs <- stat %>%
    mutate_at(vars(contains("nonwhite"), contains("income"), 
                   contains("dense"), contains("edu")), 
              list(~. / total )) %>%
    select(id, percent, rush, year, contains("nonwhite"), contains("income"), 
           contains("dense"), contains("edu")) %>%
    pivot_longer(cols = -c(id, percent, rush, year), names_to = "variable", values_to = "obs") %>%
    separate(col = "variable", into = c("variable", "level"), sep = "_") 
  
  mystat <- myobs %>%
    pivot_wider(id_cols = c(id, percent, rush, year, variable), names_from = level, values_from = obs) %>%
    mutate(index = 2/3*( abs(ll - .25)  + abs(lh - .25) + abs(hl - .25) + abs(hh - .25))) %>%
    select(id, percent, rush, variable, year, obs = index) %>%
    mutate(label = round(obs, 2),
           label = str_pad(label, width = 4, side = "right", pad = "0"),
           label = str_sub(label, 2, 4),
           label = if_else(obs == 1, "1.0", label))
  
  myblues <- c("#2A0B49",
               "#250F5A",
               "#1A136A",
               "#18247A",
               "#1d428a",
               "#3A6D9B",
               "#5793AC",
               "#74B5BC",
               "#92CCC7",
               "#B1DBD0",
               "#D0EADE")
  
  
  ribbons = mystat %>%
    group_by(rush, variable, year) %>%
    summarize(ymin = min(obs, na.rm = TRUE),
              ymax = max(obs, na.rm = TRUE)) %>%
    ungroup()
  
  gg = ggplot() +
    geom_ribbon(
      data = ribbons, mapping = aes(
        x = year, ymin = ymin, ymax = ymax, 
        group = variable, fill = variable),
      alpha = 0.250) +
    geom_line(
      data = mystat %>%
        filter(percent == 0.5),
      mapping = aes(x = year, y = obs, 
                    color = variable, group = variable,
                    linewidth = factor(percent))
    ) +
    geom_line(
      data = mystat %>%
        filter(percent == 0.67),
      mapping = aes(x = year, y = obs, 
                    color = variable, group = variable,
                    linewidth = factor(percent))
    ) +
    facet_wrap(~rush) +
    theme_classic(base_size = 14) +
    scale_color_manual(values = myblues[c(1,4,6,8)],
                       breaks = c("nonwhite", "income", "edu", "dense"),
                       labels = c("Nonwhite", "Income", "Education", "Pop. Density")) +
    scale_fill_manual(values = myblues[c(1,4,6,8)],
                      breaks = c("nonwhite", "income", "edu", "dense"),
                      labels = c("Nonwhite", "Income", "Education", "Pop. Density")) +
    scale_linewidth_manual(
      values = c(1, 0.25),
      breaks = c(0.5, 0.67),
      labels = c("50% Threshold", "67% Threshold")) +
    labs(title = "Change in Neighborhood Similarity Over Time",
         x = "Year",
         y = "Similarity Index\n(0 = All Mixed; 1 = All Same)",
         fill = "Type of Similarity",
         linewidth = "Classification\nThreshold") +
    guides(color = "none") +
    theme(panel.border = element_rect(fill = NA, color = "#373737"),
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(color = "white"),
          plot.title = element_text(hjust = 0.5))
  
  ggsave(gg, filename = filename, dpi = 500, width = 12, height = 7)
  
}


get_compare_beta = function(filename){
  
  get_obs_one = function(percent, rush){
    # Generate nodes and edges
    nodes = get_nodes(percent = percent) 
    edges = get_edges(nodes = nodes, rush = rush, .weight = 100)
    # Generate homophily/mixing statistics
    obs = edges %>% get_stat(weight = "weight")
    return(obs)
  }
  
  grid = tidyr::expand_grid(
    percent = c(0.5, 0.67),
    rush = c("am", "pm")
  ) %>%
    mutate(id = 1:n())
  
  stat = grid %>%
    split(.$id) %>%
    purrr::map_dfr(.f = ~get_obs_one(percent = .$percent, rush = .$rush), .id = "id") %>%
    mutate(id = as.integer(id)) %>%
    left_join(by = "id", y = grid)
  
  stat = stat %>% 
    mutate(rush = rush %>% recode_factor(
      "am" = "AM Rush Hour",
      "pm" = "PM Rush Hour"
    ))
  
  myobs <- stat %>%
    mutate_at(vars(contains("nonwhite"), contains("income"), 
                   contains("dense"), contains("edu")), 
              list(~. / total )) %>%
    select(id, percent, rush, year, contains("nonwhite"), contains("income"), 
           contains("dense"), contains("edu")) %>%
    pivot_longer(cols = -c(id, percent, rush, year), names_to = "variable", values_to = "obs") %>%
    separate(col = "variable", into = c("variable", "level"), sep = "_") 
  
  mystat <- myobs %>%
    pivot_wider(id_cols = c(id, percent, rush, year, variable), names_from = level, values_from = obs) %>%
    mutate(index = 2/3*( abs(ll - .25)  + abs(lh - .25) + abs(hl - .25) + abs(hh - .25))) %>%
    select(id, percent, rush, variable, year, obs = index) %>%
    mutate(label = round(obs, 2),
           label = str_pad(label, width = 4, side = "right", pad = "0"),
           label = str_sub(label, 2, 4),
           label = if_else(obs == 1, "1.0", label))
  
  
  # Calculate change over time
  mybetas = mystat %>%
    group_by(id, percent, rush, variable) %>%
    reframe({
      m = lm(obs ~ as.numeric(year) )
      broom::tidy(m) %>%
        filter(term == "as.numeric(year)") %>%
        mutate(rsq = broom::glance(m)$r.squared)
    } ) %>%
    # Format  
    mutate(estimate = scales::number(estimate, style_negative = "minus", style_positive = "plus", accuracy = 0.001),
           se = scales::number(std.error, accuracy = 0.001), 
           p = scales::number(p.value, accuracy = 0.001),
           rsq = scales::number(rsq, accuracy = 0.01)) %>%
    select(id, percent, rush, variable, estimate, se, p, rsq)
  
  mybetas %>%
    # pivot_wider(id_cols = c(variable),
    #             names_from = c(rush, percent), values_from = c(estimate, se, p, rsq)) %>%
    write_csv(filename)
  
  # mybetas %>%
  #   pivot_wider(id_cols = c(id, rush, variable),
  #               names_from = percent, values_from = c(estimate, se, p, rsq))
  
}




get_obs_set = function(percent = 0.5, rush = "am", weight = 100){
  
  library(dplyr)
  library(purrr)
  library(readr)
  
  filename = paste0("viz/obs_0_", scales::percent(percent, suffix = ""), "_", rush, ".rds")
  
  # Generate nodes and edges
  nodes = get_nodes(percent = percent) 
  edges = get_edges(nodes = nodes, rush = rush, .weight = weight)
  
  edges = edges %>%
    # mutate(block = paste(year, traffic, 
    #                      distcat,
    #                      start_density, end_density,
    #                      start_edu, end_edu,
    #                      start_age, end_age,
    #                      start_income, end_income,
    #                      start_nonwhite, end_nonwhite, 
    #                      sep = "-") %>% factor() %>% as.numeric())  %>%
    select(start_code, end_code, year, # block, 
           weight, bin_nonwhite, bin_income, bin_edu, bin_dense, weight_pop_density)  %>%
    ungroup()
  
  edges %>% get_stat(weight = "weight") %>%
    saveRDS(filename)
  
  return(filename)
  
}

get_perm_set1 = function(percent = 0.5, rush = "am", weight = 100){
  
  library(dplyr)
  library(purrr)
  library(readr)
  
  filename = paste0("viz/perms_1_", scales::percent(percent, suffix = ""), "_", rush, ".rds")
  
  # Generate nodes and edges
  nodes = get_nodes(percent = percent) 
  edges = get_edges(nodes = nodes, rush = rush, .weight = weight)
  
  
  edges = edges %>%
    mutate(block = paste(year, sep = "-") %>% factor() %>% as.numeric())  %>%
    select(start_code, end_code, year, block, weight, 
           bin_nonwhite, bin_income, bin_edu, bin_dense, weight_pop_density)  %>%
    ungroup()
  
  1:1000 %>%
    map_dfr(
      .f = ~get_perm(net = edges, nreps = .) %>% get_stat(weight = "perm"), 
      .id = "replicate") %>%
    saveRDS(filename)
  
}

get_perm_set2 = function(percent = 0.5, rush = "am", weight = 100){
  
  library(dplyr)
  library(purrr)
  library(readr)
  
  filename = paste0("viz/perms_2_", scales::percent(percent, suffix = ""), "_", rush, ".rds")
  
  # Generate nodes and edges
  nodes = get_nodes(percent = percent) 
  edges = get_edges(nodes = nodes, rush = rush, .weight = weight)
  
  
  edges = edges %>%
    mutate(block = paste(year, traffic, 
                         sep = "-") %>% factor() %>% as.numeric())  %>%
    select(start_code, end_code, year, block, weight, 
           bin_nonwhite, bin_income, bin_edu, bin_dense, weight_pop_density)  %>%
    ungroup()
  
  1:1000 %>%
    map_dfr(
      .f = ~get_perm(net = edges, nreps = .) %>% get_stat(weight = "perm"), 
      .id = "replicate") %>%
    saveRDS(filename)
  
}


get_perm_set3 = function(percent = 0.5, rush = "am", weight = 100){
  
  library(dplyr)
  library(purrr)
  library(readr)
  
  filename = paste0("viz/perms_3_", scales::percent(percent, suffix = ""), "_", rush, ".rds")
  
  # Generate nodes and edges
  nodes = get_nodes(percent = percent) 
  edges = get_edges(nodes = nodes, rush = rush, .weight = weight)
  
  
  edges = edges %>%
    mutate(block = paste(year, traffic, 
                         distcat,
                         sep = "-") %>% factor() %>% as.numeric())  %>%   
    select(start_code, end_code, year, block, weight, 
           bin_nonwhite, bin_income, bin_edu, bin_dense, weight_pop_density)  %>%
    ungroup()
  
  1:1000 %>%
    map_dfr(
      .f = ~get_perm(net = edges, nreps = .) %>% get_stat(weight = "perm"), 
      .id = "replicate") %>%
    saveRDS(filename)
  
}

get_perm_set4 = function(percent = 0.5, rush = "am", weight = 100){
  
  library(dplyr)
  library(purrr)
  library(readr)
  
  filename = paste0("viz/perms_4_", scales::percent(percent, suffix = ""), "_", rush, ".rds")
  
  # Generate nodes and edges
  nodes = get_nodes(percent = percent) 
  edges = get_edges(nodes = nodes, rush = rush, .weight = weight)
  
  
  edges = edges %>%
    mutate(block = paste(year, traffic, 
                         distcat,
                         start_density, end_density,
                         sep = "-") %>% factor() %>% as.numeric())  %>%
    select(start_code, end_code, year, block, weight, 
           bin_nonwhite, bin_income, bin_edu, bin_dense, weight_pop_density)  %>%
    ungroup()
  
  1:1000 %>%
    map_dfr(
      .f = ~get_perm(net = edges, nreps = .) %>% get_stat(weight = "perm"), 
      .id = "replicate") %>%
    saveRDS(filename)
  
}

get_perm_set5 = function(percent = 0.5, rush = "am", weight = 100){
  
  library(dplyr)
  library(purrr)
  library(readr)
  
  filename = paste0("viz/perms_5_", scales::percent(percent, suffix = ""), "_", rush, ".rds")
  
  # Generate nodes and edges
  nodes = get_nodes(percent = percent) 
  edges = get_edges(nodes = nodes, rush = rush, .weight = weight)
  
  
  edges = edges %>%
    mutate(block = paste(year, traffic, 
                         distcat,
                         start_density, end_density,
                         start_edu, end_edu,
                         sep = "-") %>% factor() %>% as.numeric())  %>%
    select(start_code, end_code, year, block, weight, 
           bin_nonwhite, bin_income, bin_edu, bin_dense, weight_pop_density)  %>%
    ungroup()
  
  1:1000 %>%
    map_dfr(
      .f = ~get_perm(net = edges, nreps = .) %>% get_stat(weight = "perm"), 
      .id = "replicate") %>%
    saveRDS(filename)
  
}


get_perm_set6 = function(percent = 0.5, rush = "am", weight = 100){
  
  library(dplyr)
  library(purrr)
  library(readr)
  
  filename = paste0("viz/perms_6_", scales::percent(percent, suffix = ""), "_", rush, ".rds")
  
  # Generate nodes and edges
  nodes = get_nodes(percent = percent) 
  edges = get_edges(nodes = nodes, rush = rush, .weight = weight)
  
  
  edges = edges %>%
    mutate(block = paste(year, traffic, 
                         distcat,
                         start_density, end_density,
                         start_edu, end_edu,
                         start_age, end_age,
                         sep = "-") %>% factor() %>% as.numeric())  %>%
    select(start_code, end_code, year, block, weight, 
           bin_nonwhite, bin_income, bin_edu, bin_dense, weight_pop_density)  %>%
    ungroup()
  
  1:1000 %>%
    map_dfr(
      .f = ~get_perm(net = edges, nreps = .) %>% get_stat(weight = "perm"), 
      .id = "replicate") %>%
    saveRDS(filename)
  
}


get_perm_set7 = function(percent = 0.5, rush = "am", weight = 100){
  
  library(dplyr)
  library(purrr)
  library(readr)
  
  filename = paste0("viz/perms_7_", scales::percent(percent, suffix = ""), "_", rush, ".rds")
  
  # Generate nodes and edges
  nodes = get_nodes(percent = percent) 
  edges = get_edges(nodes = nodes, rush = rush, .weight = weight)
  
  
  edges = edges %>%
    mutate(block = paste(year, traffic, 
                         distcat,
                         start_density, end_density,
                         start_edu, end_edu,
                         start_age, end_age,
                         start_income, end_income,
                         sep = "-") %>% factor() %>% as.numeric())  %>%
    select(start_code, end_code, year, block, weight, 
           bin_nonwhite, bin_income, bin_edu, bin_dense, weight_pop_density)  %>%
    ungroup()
  
  1:1000 %>%
    map_dfr(
      .f = ~get_perm(net = edges, nreps = .) %>% get_stat(weight = "perm"), 
      .id = "replicate") %>%
    saveRDS(filename)
  
}
get_perm_set8 = function(percent = 0.5, rush = "am", weight = 100){
  
  library(dplyr)
  library(purrr)
  library(readr)
  
  filename = paste0("viz/perms_8_", scales::percent(percent, suffix = ""), "_", rush, ".rds")
  
  # Generate nodes and edges
  nodes = get_nodes(percent = percent) 
  edges = get_edges(nodes = nodes, rush = rush, .weight = weight)
  
  edges = edges %>%
    mutate(block = paste(year, traffic, 
                         distcat,
                         start_density, end_density,
                         start_edu, end_edu,
                         start_age, end_age,
                         start_income, end_income,
                         start_nonwhite, end_nonwhite, 
                         sep = "-") %>% factor() %>% as.numeric())  %>%
    select(start_code, end_code, year, block, weight, 
           bin_nonwhite, bin_income, bin_edu, bin_dense, weight_pop_density)  %>%
    ungroup()
  
  
  1:1000 %>%
    map_dfr(
      .f = ~get_perm(net = edges, nreps = .) %>% get_stat(weight = "perm"), 
      .id = "replicate") %>%
    saveRDS(filename)
  
  return(filename)
  
}


get_ptest = function(percent = 0.5, rush = "am"){
  
  # Testing values 
  # percent = 0.5; rush = "am"
  
  label_percent = scales::percent(percent, suffix = "")
  f = tibble(file = dir("viz", pattern = "perms_", full.names = TRUE)) %>%
    filter(str_detect(file, label_percent) & str_detect(file, rush))
  
  # Get permuted data
  myperms = f %>%
    mutate(controls = 1:n() )%>%
    split(.$controls) %>%
    map_dfr(.f = ~read_rds(.$file), .id = "controls") %>%
    mutate(controls = as.numeric(controls)) %>%
    group_by(controls, replicate) %>%
    summarize_at(vars(-c("year", "total")), list(~sum(.) / sum(total) )) %>%
    pivot_longer(cols = -c(controls, replicate), names_to = "variable", values_to = "perm") %>%
    separate(col = "variable", into = c("variable", "level"), sep = "_") %>%
    pivot_wider(id_cols = c(replicate, controls, variable),
                names_from = level, values_from = perm) %>%
    mutate(index = 2/3*( abs(ll - .25)  + abs(lh - .25) + abs(hl - .25) + abs(hh - .25))) %>%
    select(replicate, controls, variable, perm = index)
  
  
  
  # Get number of controls to iterate by
  n_controls = nrow(f)
  
  # Get paths to observed data
  o = tibble(file = dir("viz",pattern = "obs_0_", full.names = TRUE)) %>%
    filter(str_detect(file, label_percent) & str_detect(file, rush))
  
  # Get observed data, iterated by number of controls
  myobs = o$file %>% read_rds() %>%
    expand_grid(controls = 1:n_controls) %>%
    group_by(controls) %>%
    summarize_at(vars(-c("year", "total")), list(~sum(.) / sum(total) )) %>%
    pivot_longer(cols = -c(controls), names_to = "variable", values_to = "obs") %>%
    separate(col = "variable", into = c("variable", "level"), sep = "_") %>%
    pivot_wider(id_cols = c(controls,variable), names_from = level, values_from = obs) %>%
    mutate(index = 2/3*( abs(ll - .25)  + abs(lh - .25) + abs(hl - .25) + abs(hh - .25))) %>%
    select(controls, variable, obs = index)
  
  
  mycompare <- myobs %>%
    left_join(by = c("controls", "variable"),
              y = myperms %>% select(controls, variable, perm, replicate),
              multiple = "all") %>%
    # separate(col = "variable", into = c("variable", "level"), sep = "_") %>%
    # mutate(variable = variable %>% recode_factor(
    #   "income" = "<b>Income</b><br>High: >50% 60K+",
    #   "nonwhite" = "<b>Race</b><br>High: >50% Nonwhite",
    #   "dense" = "<b>Density</b><br>High: >Median",
    #   "edu" = "<b>Education</b><br>High: >Median % Some College")
    # ) %>%
    filter(str_detect(variable, "income|nonwhite"))
  
  remove(myperms, myobs)
  
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
  
  
  output = mytext %>% 
    mutate(percent = percent, rush = rush)
  
  return(output)
  
}
## Figure C (map) #####################################################



get_map = function(edges, percent = 0.50, filename = "our_data/network_race.png"){
  
  library(dplyr)
  library(readr)
  library(tidygraph)
  library(sf)
  library(ggnewscale)
  library(ggtext)
  
  # net <- read_rds("our_data/edges_annual_prepped.rds") %>%
  #   st_as_sf(crs = 4326) %>%
  #   mutate(weight = weight / weight_pop_density)
  
  net <- edges %>%
    st_as_sf(crs = 4326) %>%
    mutate(weight = weight / weight_pop_density)
  
  label_percent = scales::percent(percent)
  subtitle_race = paste0("<b>Ridership by Race</b><br>High: Block Groups where >", label_percent, " Nonwhite Residents")
  subtitle_wealth = paste0("<b>Ridership by Income</b><br>High: Block Groups where >", label_percent, " 60K+ Earning Households")
  
  
  # Get a 15 km buffer around the center of Suffolk county
  mybuffer <- tigris::counties(state = "MA",  cb = TRUE, year = 2019) %>%
    st_as_sf(crs = 4326) %>%
    st_transform(crs = 4326) %>%
    select(geoid = GEOID, name = NAME, geometry)  %>%
    filter(name == "Suffolk") %>%
    summarize(geometry = st_centroid(geometry)) %>%
    st_buffer(dist = 15000)
  
  mycounties <- tigris::counties(state = "MA", cb = TRUE, year = 2019) %>%
    st_as_sf(crs = 4326)
  
  myboston <- tigris::county_subdivisions(state = "MA", county = "Suffolk", cb = TRUE, year = 2019) %>%
    st_as_sf(crs = 4326) %>%
    filter(NAME == "Boston") %>%
    st_transform(crs = 4326)
  
  
  nodes <- read_rds("our_data/nodes.rds") %>%
    st_as_sf(crs = 4326) %>%
    # Join in total out-degree (people leaving each source)
    left_join(by = "code", y = net %>% 
                as_tibble() %>%
                group_by(code = start_code) %>%
                summarize(outdegree = sum(weight, na.rm = TRUE))) %>%
    # Join in total out-degree (people leaving each source)
    left_join(by = "code", y = net %>% 
                as_tibble() %>%
                group_by(code = end_code) %>%
                summarize(indegree = sum(weight, na.rm = TRUE))) %>%
    # Now filter to just nodes that were either sources or destinations in the threshold network
    # (meaning that a flow of at least 100 folks flowed between that place during some year)
    filter(indegree > 0 | outdegree > 0)
  
  mybox <- nodes %>% st_bbox()
  
  bg <- read_rds("our_data/bgdataset.rds") %>%
    st_as_sf(crs = 4326)
  
  
  background <- bg %>% summarize(geometry = st_union(geometry))
  
  library(ggnewscale)
  
  
  myblues <- c("#2A0B49",
               "#250F5A",
               "#1A136A",
               "#18247A",
               "#1d428a",
               "#3A6D9B",
               "#5793AC",
               "#74B5BC",
               "#92CCC7",
               "#B1DBD0",
               "#D0EADE")
  
  
  g1 <- ggplot() +
    # Get background layer
    #geom_sf(data = background, fill = "steelblue", color = "steelblue", size = 3, alpha = 0.2) +
    geom_sf(data = myboston, fill = "white", color = "#373737", size = 3) +
    # Get block group population density
    geom_sf(data = bg %>% filter(area > 0), mapping = aes(fill = pop_density_2020_smooth10), color = NA) +
    scale_fill_gradient(na.value = "black", low = "black", high = "white", trans = "log",
                        breaks = bg$pop_density_2020_smooth10 %>% 
                          quantile(probs = c(0.01, .25, .50, .75, 0.99), na.rm = TRUE) %>% 
                          unname() %>% round(0),
                        name = "Population Density (per km<sup>2</sup>)<br>by Block Group",
                        guide = guide_legend(order = 4)) +
    #  geom_sf(data = background, color = "darkgrey", size = 1, fill = NA) +
    geom_sf(data = myboston, color = "#373737", size = 1, fill = NA) +
    #  geom_sf(data = mycounties, color = "black", size = 1, fill = NA) +
    # Add outline layer then realy layer
    geom_sf(data = net %>% filter(bin_nonwhite == "low-low"), 
            mapping = aes(size = weight + 2), color = "white")  +
    geom_sf(data = net %>% filter(bin_nonwhite == "low-low"), 
            mapping = aes(size = weight, color = bin_nonwhite))  +
    # Add outline layer then realy layer
    geom_sf(data = net %>% filter(bin_nonwhite == "low-high"), 
            mapping = aes(size = weight + 2), color = "white")  +
    geom_sf(data = net %>% filter(bin_nonwhite == "low-high"), 
            mapping = aes(size = weight, color = bin_nonwhite))  +
    # Add outline layer then realy layer
    geom_sf(data = net %>% filter(bin_nonwhite == "high-low"), 
            mapping = aes(size = weight + 2), color = "white")  +
    geom_sf(data = net %>% filter(bin_nonwhite == "high-low"), 
            mapping = aes(size = weight, color = bin_nonwhite))  +
    # Add outline layer then realy layer
    geom_sf(data = net %>% filter(bin_nonwhite == "high-high"), 
            mapping = aes(size = weight + 2), color = "white")  +
    geom_sf(data = net %>% filter(bin_nonwhite == "high-high"), 
            mapping = aes(size = weight, color = bin_nonwhite))  +
    scale_size_continuous(
      range = c(0.1, 2), trans = "log",
      breaks = net$weight %>% 
        quantile(probs = c(0.1, .3, .6, 0.99), na.rm = TRUE) %>% 
        unname() %>% round(1),
      name = "Ridership Rate",
      guide = guide_legend(order = 2)) +
    scale_color_manual(
      values = myblues[c(1,4,6,8)], 
      breaks = c("high-high", "high-low", "low-high", "low-low"),
      labels = c("High &#9654; High",
                 "High &#9654; Low",
                 "Low &#9654; High",
                 "Low &#9654; Low"),
      name = "Demographics<br>of Start/Destination",
      guide = guide_legend(order = 1, override.aes = list(size = 3))) +
    
    # Add Node Positions
    ggnewscale::new_scale("size") +
    geom_sf(data = nodes, mapping = aes(size = outdegree), 
            shape = 21, color = "white", fill = "#696880", stroke = 0.5) +
    scale_size_continuous(
      range = c(0.1, 5), trans = "log", 
      breaks = nodes$outdegree %>% 
        quantile(probs = c(0.1, .3, .6, 0.99), na.rm = TRUE) %>% 
        unname() %>% round(1),
      name = "Cumulative Ridership Rate<br>Leaving Starting Station<br>(Weighted Outdegree)",
      guide = guide_legend(order = 3)) +
    coord_sf(xlim = c(mybox["xmin"], mybox["xmax"]),
             ylim = c(mybox["ymin"], mybox["ymax"])) +
    theme_void(base_size = 14) +
    theme(legend.position = "right",
          legend.title = ggtext::element_markdown(size = 12, hjust = 0),
          legend.text = ggtext::element_markdown(size = 11, hjust = 0)) +
    theme(plot.subtitle = ggtext::element_markdown(size = 14, hjust = 0.5),
          panel.border = element_rect(fill = NA, color = "#373737", size = 1)) +
    labs(subtitle = subtitle_race) +
    ggspatial::annotation_north_arrow(width = unit(0.5, "cm"), height = unit(0.5, "cm")) +
    ggspatial::annotation_scale(pad_x = unit(1, "cm"))
  
  
  
  
  
  g2 <- ggplot() +
    # Get background layer
    geom_sf(data = myboston, fill = "white", color = "#373737", size = 3) +
    # Get block group population density
    geom_sf(data = bg %>% filter(area > 0), mapping = aes(fill = pop_density_2020_smooth10), color = NA) +
    scale_fill_gradient(na.value = "black", low = "black", high = "white", trans = "log",
                        breaks = bg$pop_density_2020_smooth10 %>% 
                          quantile(probs = c(0.01, .25, .50, .75, 0.99), na.rm = TRUE) %>% 
                          unname() %>% round(0),
                        name = "Population Density (per km<sup>2</sup>)<br>by Block Group",
                        guide = guide_legend(order = 4)) +
    #  geom_sf(data = background, color = "darkgrey", size = 1, fill = NA) +
    geom_sf(data = myboston, color = "#373737", size = 1, fill = NA) +
    #  geom_sf(data = mycounties, color = "black", size = 1, fill = NA) +
    # Add outline layer then realy layer
    geom_sf(data = net %>% filter(bin_nonwhite == "low-low"), 
            mapping = aes(size = weight + 2), color = "white")  +
    geom_sf(data = net %>% filter(bin_nonwhite == "low-low"), 
            mapping = aes(size = weight, color = bin_income))  +
    # Add outline layer then realy layer
    geom_sf(data = net %>% filter(bin_nonwhite == "low-high"), 
            mapping = aes(size = weight + 2), color = "white")  +
    geom_sf(data = net %>% filter(bin_nonwhite == "low-high"), 
            mapping = aes(size = weight, color = bin_income))  +
    # Add outline layer then realy layer
    geom_sf(data = net %>% filter(bin_nonwhite == "high-low"), 
            mapping = aes(size = weight + 2), color = "white")  +
    geom_sf(data = net %>% filter(bin_nonwhite == "high-low"), 
            mapping = aes(size = weight, color = bin_income))  +
    # Add outline layer then realy layer
    geom_sf(data = net %>% filter(bin_nonwhite == "high-high"), 
            mapping = aes(size = weight + 2), color = "white")  +
    geom_sf(data = net %>% filter(bin_nonwhite == "high-high"), 
            mapping = aes(size = weight, color = bin_income))  +
    scale_size_continuous(range = c(0.1, 2), trans = "log",
                          breaks = net$weight %>% 
                            quantile(probs = c(0.1, .3, .6, 0.99), na.rm = TRUE) %>% 
                            unname() %>% round(1),
                          name = "Ridership Rate",
                          guide = guide_legend(order = 2)) +
    scale_color_manual(values = myblues[c(1,4,6,8)], 
                       breaks = c("high-high", "high-low", "low-high", "low-low"),
                       labels = c("High &#9654; High",
                                  "High &#9654; Low",
                                  "Low &#9654; High",
                                  "Low &#9654; Low"),
                       name = "Demographics<br>of Start/Destination",
                       guide = guide_legend(order = 1, override.aes = list(size = 3))) +
    
    # Add Node Positions
    ggnewscale::new_scale("size") +
    geom_sf(data = nodes, mapping = aes(size = outdegree), 
            shape = 21, color = "white", fill = "#696880", stroke = 0.5) +
    scale_size_continuous(range = c(0.1, 5), trans = "log", 
                          breaks = nodes$outdegree %>% 
                            quantile(probs = c(0.1, .3, .6, 0.99), na.rm = TRUE) %>% 
                            unname() %>% round(1),
                          name = "Cumulative Ridership Rate<br>Leaving Starting Station<br>(Weighted Outdegree)",
                          guide = guide_legend(order = 3)) +
    coord_sf(xlim = c(mybox["xmin"], mybox["xmax"]),
             ylim = c(mybox["ymin"], mybox["ymax"])) +
    theme_void(base_size = 14) +
    theme(legend.position = "right",
          legend.title = ggtext::element_markdown(size = 12, hjust = 0),
          legend.text = ggtext::element_markdown(size = 11, hjust = 0)) +
    theme(plot.subtitle = ggtext::element_markdown(size = 14, hjust = 0.5),
          panel.border = element_rect(fill = NA, color = "#373737", size = 1)) +
    labs(subtitle = subtitle_wealth)
  
  combo <- ggpubr::ggarrange(g1,g2, nrow = 1, legend = "right", common.legend = TRUE)
  
  #ggsave(g1, filename = "our_data/network_race.png", dpi = 500, width = 8, height = 6.5)
  ggsave(combo, filename = filename, dpi = 500, width = 13.5, height = 6.5)
  
  return(filename) 
}



## Figure D (heatgrid) ###########################################################

get_heatgrid = function(nodes, edges, filename = "our_data/tiles_wealth.png"){
  
  
  myblues <- c("#2A0B49",
               "#250F5A",
               "#1A136A",
               "#18247A",
               "#1d428a",
               "#3A6D9B",
               "#5793AC",
               "#74B5BC",
               "#92CCC7",
               "#B1DBD0",
               "#D0EADE")
  
  # Compare against original network
  mystations <- nodes %>%
    # Get percentage of residents making OVER 60K a year
    mutate(pop_upper_income = 1 - pop_lower_income)
  
  # Get results binned into 20 categories
  bins <- 20
  
  wealth <- edges %>%
    select(start_code, end_code, year, weight, weight_pop_density) %>%
    # Join in wealth
    left_join(
      by = c("start_code" = "code"), 
      y = mystations %>% select(code, start_wealthy = pop_upper_income)) %>%
    left_join(
      by = c("end_code" = "code"),
      y = mystations %>% select(code, end_wealthy = pop_upper_income))  %>%
    # Calculate population density normed bins
    mutate(weight = weight * weight_pop_density) %>%
    # Now get count within 20 bins
    group_by(start = floor(start_wealthy*bins), 
             end = floor(end_wealthy*bins)) %>%
    summarize(count = sum(weight, na.rm = TRUE)) %>%
    # Now join these tallies into the following grid
    right_join(by = c("start", "end"), 
               y = expand_grid(start = 0:bins, end = 0:bins)) %>%
    mutate(count = if_else(is.na(count), 0, as.numeric(count)))
  
  
  race <- edges %>%
    select(start_code, end_code, year, weight, weight_pop_density) %>%
    # Join in wealth
    left_join(
      by = c("start_code" = "code"),
      y = mystations %>% select(code, start_nonwhite = pop_nonwhite)) %>%
    left_join(
      by = c("end_code" = "code"), 
      y = mystations %>% select(code, end_nonwhite = pop_nonwhite))  %>%
    # Calculate population density normed bins
    mutate(weight = weight * weight_pop_density) %>%
    # Now get count within 20 bins
    group_by(start = floor(start_nonwhite*bins), 
             end = floor(end_nonwhite*bins)) %>%
    summarize(count = sum(weight, na.rm = TRUE)) %>%
    # Now join these tallies into the following grid
    right_join(by = c("start", "end"), 
               y = expand_grid(start = 0:bins, end = 0:bins)) %>%
    mutate(count = if_else(is.na(count), 0, as.numeric(count)))
  
  net <- bind_rows(wealth %>% mutate(type = "Wealth (% Household Income >60K)"),
                   race %>% mutate(type = "Race (% Nonwhite)"))
  
  g4 <- net %>%
    ggplot(aes(x = start, y = end, fill = count / 1000)) +
    geom_tile(color = "grey", size = 0.05) +
    geom_tile(data = . %>% filter(count > 0), color = "#373737", size = 0.1) +
    geom_vline(mapping = aes(xintercept = 10), alpha = 0.5, size = 2) +
    geom_hline(mapping = aes(yintercept = 10), alpha = 0.5, size = 2) +
    scale_fill_gradient(
      low = myblues[4], high = "white",
      # Zeros become NA on a log scale, but we don't want to lose them,
      # Since they were actually there,
      # So we'll color them with the bottom of the scale
      na.value = myblues[2],
      limits = NULL, trans = "log",
      breaks = c(0.01, 0.1, 1, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000),
      # Now, multiply all by 1000, since we divided earlier, to get back to original
      labels = c(0.01, 0.1, 1, 10, 30, 100, 300, "1,000", "3,000", "10,000", "30,000", "100,000"),
      guide = guide_colorbar(barheight = 13, ticks.colour = "#373737", frame.colour = "#373737", ticks.linewidth = 2)) +
    scale_x_continuous(breaks = (0:bins), labels = ((0:bins)/bins)*100, 
                       expand = expansion(add = c(0,0))) +
    scale_y_continuous(breaks = (0:bins), labels = ((0:bins)/bins)*100, 
                       expand = expansion(add = c(0,0))) +
    labs(
      title = "Is Ridership linked to Race and Wealth?",
      x = "Source Neighborhood \n % of Residents/Households",
      y = "Destination Neighborhood \n % of Residents/Households",
      subtitle = "White, wealthy neighborhoods are more often a beginning and/or a destination.",
      caption = "Data from 2011-2021 (routes with 100 or more users per year)\nMedian US household income is ~$67,000 USD. Dark blue shows 0 rides.",
      fill = "<b>Ridership Rate</b>
    <br>per station pair
    <br>per 1000 residents
    <br>per km<sup>2</sup>"
    ) +
    theme_classic(base_size = 14) +
    theme(plot.caption = element_text(hjust = 0),
          plot.title = element_text(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          legend.title = ggtext::element_markdown(size = 12, hjust = 0),
          axis.ticks = element_blank()) +
    coord_fixed(ratio = 1) +
    facet_wrap(~type)
  
  
  ggsave(g4, filename = filename, dpi = 500, height = 7, width = 12.5)
  
  return(filename)
}

# get_heatgrid(edges = read_rds("our_data/edges_annual_prepped.rds"), 
#              nodes = read_rds("our_data/nodes.rds"),
#              filename = "out_data/tiles_wealth.png")


## Figure E (bars) ###################################################################


get_bars = function(obs, percent = 0.5, filename = "our_data/bars.png"){
  
  myobs <- obs %>%
    summarize_at(vars(-c("year", "total")), list(~sum(.) / sum(total) )) %>%
    pivot_longer(cols = -c(), names_to = "variable", values_to = "obs") %>%
    separate(col = "variable", into = c("variable", "level"), sep = "_") %>%
    mutate(varlabel = variable %>% recode_factor(
      "nonwhite" = paste0("<b>Race</b><br>High: >", scales::percent(percent, suffix = ""), "% Nonwhite<br>Residents"),
      "income" = paste0("<b>Income</b><br>High: >", scales::percent(percent, suffix = ""), "% 60K+<br>Earning Households"),
      "dense" = paste0("<b>Density</b><br>High: >", scales::percent(percent, suffix = "th %"), "<br>Population Density"),
      "edu" = paste0("<b>Education</b><br>High: >", scales::percent(percent, suffix = "th %"), "<br>% Some College")
    ),
    level = level %>% recode_factor(
      "hh" = "High<br>&#9660;<br>High",
      "hl" = "High<br>&#9660;<br>Low",
      "lh" = "Low<br>&#9660;<br>High",
      "ll" = "Low<br>&#9660;<br>Low"))
  
  
  myblues <- c("#2A0B49",
               "#250F5A",
               "#1A136A",
               "#18247A",
               "#1d428a",
               "#3A6D9B",
               "#5793AC",
               "#74B5BC",
               "#92CCC7",
               "#B1DBD0",
               "#D0EADE")
  g5 <- myobs %>%
    ggplot(mapping = aes(x = level, y = obs, fill = level, label = paste(round(obs, 3)*100, "%", sep = "" ))) +
    geom_hline(yintercept = 0.25, linetype = "dashed", color = "#373737", alpha = 0.5, size = 1) +
    geom_col(color = "#373737", size = 0.2) +
    geom_text(nudge_y = 0.04, color = "#373737") +
    facet_grid(~varlabel) +
    theme_classic(base_size = 14) +
    theme(axis.text.x = ggtext::element_markdown(size = 11, hjust = 0.5),
          strip.text = ggtext::element_markdown(size = 12, hjust = 0.5),
          axis.title.x = ggtext::element_markdown(size = 12, hjust = 0.5),
          axis.title.y = ggtext::element_markdown(size = 12, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          strip.background = element_blank(),
          axis.line = element_line(color = "#373737", size = 0.5),
          axis.ticks = element_blank(),
          panel.border = element_rect(fill = NA, color = "#373737", size = 0.5),
          panel.spacing = unit(0.5, "cm")) +
    scale_fill_manual(values = myblues[c(1,4,6,8)], guide = "none") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1), 
                       labels = c("0%", "25%", "50%", "75%", "100%"), expand = expansion(add = c(0,0))) +
    labs(y = "<b>Percentage of Ridership</b><br><i>(Ridership Normed by Population Density)</i>",
         x = "<b>Traits of Station Pairs by Level of Variables</b><br><i>Type of Source Station &#x2192; Type of Destination Station</i>",
         subtitle = "Neighborhood Mixing Overall (2011-2021)")
  
  ggsave(g5, filename = filename, dpi = 500, width = 9, height = 5)
  
}

## Figure F (lines) ############################################################
get_lines = function(obs, filename = "viz/lines.png"){
  myobs <- obs %>%
    mutate_at(vars(contains("nonwhite"), contains("income"), 
                   contains("dense"), contains("edu")), 
              list(~. / total )) %>%
    select(year, contains("nonwhite"), contains("income"), 
           contains("dense"), contains("edu")) %>%
    pivot_longer(cols = -c(year), names_to = "variable", values_to = "obs") %>%
    separate(col = "variable", into = c("variable", "level"), sep = "_") 
  
  mystat <- myobs %>%
    pivot_wider(id_cols = c(year, variable), names_from = level, values_from = obs) %>%
    mutate(index = 2/3*( abs(ll - .25)  + abs(lh - .25) + abs(hl - .25) + abs(hh - .25))) %>%
    select(variable, year, obs = index) %>%
    mutate(label = round(obs, 2),
           label = str_pad(label, width = 4, side = "right", pad = "0"),
           label = str_sub(label, 2, 4),
           label = if_else(obs == 1, "1.0", label))
  
  myblues <- c("#2A0B49",
               "#250F5A",
               "#1A136A",
               "#18247A",
               "#1d428a",
               "#3A6D9B",
               "#5793AC",
               "#74B5BC",
               "#92CCC7",
               "#B1DBD0",
               "#D0EADE")
  
  gg = mystat %>%
    ggplot(mapping = aes(x = year, y = obs, group = variable, 
                         color = variable, label = label)) +
    geom_line() +
    geom_point(data = . %>% filter(variable == "nonwhite"), size = 10) +
    geom_text(data = . %>% filter(variable == "nonwhite"), color = "white") +
    
    geom_point(data = . %>% filter(variable == "income"), size = 10) +
    geom_text(data = . %>% filter(variable == "income"), color = "white") +
    
    geom_point(data = . %>% filter(variable == "edu"), size = 10) +
    geom_text(data = . %>% filter(variable == "edu"), color = "white") +
    
    geom_point(data = . %>% filter(variable == "dense"), size = 10) +
    geom_text(data = . %>% filter(variable == "dense"), color = "white") +
    theme_classic(base_size = 14) +
    scale_color_manual(values = myblues[c(1,4,6,8)],
                       breaks = c("nonwhite", "income", "edu", "dense"),
                       labels = c("Nonwhite", "Income", "Education", "Pop. Density")) +
    labs(title = "Change in Neighborhood Similarity Over Time",
         x = "Year",
         y = "Similarity Index\n(0 = All Mixed; 1 = All Same)",
         color = "Type of Similarity") 
  
  ggsave(gg, filename = filename, dpi = 500, width = 9, height = 7)
  
}


## Figure G (lines) ###########################################################

get_lines_multiples = function(obs, filename = "viz/lines_multiples.png", gg = FALSE){
  
  
  myblues <- c("#2A0B49",
               "#250F5A",
               "#1A136A",
               "#18247A",
               "#1d428a",
               "#3A6D9B",
               "#5793AC",
               "#74B5BC",
               "#92CCC7",
               "#B1DBD0",
               "#D0EADE")
  
  myobs <- obs %>%
    mutate_at(vars(contains("nonwhite"), contains("income"), 
                   contains("dense"), contains("edu")), 
              list(~. / total )) %>%
    select(year, contains("nonwhite"), contains("income"), 
           contains("dense"), contains("edu")) %>%
    pivot_longer(cols = -c(year), names_to = "variable", values_to = "obs") %>%
    separate(col = "variable", into = c("variable", "level"), sep = "_") 
  
  g3 <- bind_rows(
    myobs %>%
      filter(variable == "nonwhite" & level == "hh"),
    myobs %>%
      filter(variable == "income" & level == "lh"),
    myobs %>%
      filter(variable == "edu" & level == "ll"),
    myobs %>%
      filter(variable == "dense" & level == "hh")
  ) %>%
    mutate(varlabel = variable %>% recode_factor(
      "nonwhite" = "<b>% Nonwhite</b><br>(High->High)",
      "dense" = "<b>Population Dense</b><br>(High->High)",
      "edu" = "<b>Education</b><br>(Low->Low)",
      "income" = "<b>Income</b><br>(Low->High)")) %>%
    mutate(label = round(obs, 2),
           label = str_pad(label, width = 4, side = "right", pad = "0"),
           label = str_sub(label, 2, 4),
           label = case_when(obs == 1 ~ "1.0", 
                             obs == 0 ~ "0", 
                             TRUE ~ label)) %>%
    
    ggplot(mapping = aes(x = as.numeric(year), y = obs, group = level, 
                         color = level, label = label)) +
    geom_line() +
    geom_point(data = . %>% filter(variable == "nonwhite"), size = 10) +
    geom_text(data = . %>% filter(variable == "nonwhite"), color = "white") +
    
    geom_point(data = . %>% filter(variable == "income"), size = 10) +
    geom_text(data = . %>% filter(variable == "income"), color = "white") +
    
    geom_point(data = . %>% filter(variable == "edu"), size = 10) +
    geom_text(data = . %>% filter(variable == "edu"), color = "white") +
    
    geom_point(data = . %>% filter(variable == "dense"), size = 10) +
    geom_text(data = . %>% filter(variable == "dense"), color = "white") +
    theme_classic(base_size = 14) +
    scale_color_manual(values = myblues[c(1,4,6,8)]
                       #                     breaks = c("nonwhite", "income", "edu", "dense"),
                       #                     labels = c("Nonwhite", "Income", "Education", "Pop. Density")
    ) +
    scale_x_continuous(breaks = seq(from = 2011, to = 2021, by = 2), expand = c(0.05, 0.05)) +
    scale_y_continuous(expand = c(.025, .025)) +
    labs(title = "Notable Changes in Neighborhood Similarity Over Time",
         x = "Year",
         y = "Percentage of Rides\n(population density-normed ridership)",
         color = "Type of Similarity") +
    facet_wrap(~varlabel, scales = "free_y") +
    theme(strip.text = ggtext::element_markdown(size = 12, hjust = 0.5),
          strip.background = element_blank(),
          legend.position = "none",
          axis.line = element_blank(),
          panel.border = element_rect(fill = NA, color = "#373737"))
  
  if(gg == TRUE){ return(g3) }
  ggsave(g3, filename = filename, dpi = 500, width = 9, height = 6)
  
}



