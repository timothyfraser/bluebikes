# code.R

# code for generating illustration of network block permutations

# SETUP #################################

# set working directory
setwd(paste0(rstudioapi::getActiveProject(), "/illustration"))

rm(list = ls())

library(dplyr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(ggraph)
library(tidygraph)
library(tidyr)
library(readr)
library(purrr)
library(ggtext)
library(shadowtext)

# DATA ##########################




# mynodes = tribble(
#   ~id, ~group, ~pop_density,
#   "A",  TRUE,  10000,
#   "B",  TRUE,  15000,
#   "C",  FALSE, 20000,
#   "D",  FALSE, 10000
# )
# 
# 
# myedges = tribble(
#   ~from, ~to, ~weight,
#   "A",    "B",  100,
#   "A",    "C",  200,
#   "B",    "C",  300,
#   "C",    "D",  200,
#   "D",    "A",  100,
#   "D",    "B",  300,
#   "C",    "B",  200,
#   "C",    "A",  300
# )

# FUNCTIONS #####################


simulate_nodes = function(n){
  
  tibble(
    id = toupper(1:n),
    group = as.logical(rbinom(n = n, size = 1, prob = 0.5)),
    pop_density = rnorm(n = n, mean = 10000, sd= 2500)
  )
  
  
}


simulate_edges = function(nodes, percent_zero = 0.75, min = 100, max = 300){
  midpoint = (max - min) / 2 + min
  # Assign one of these weights
  expand_grid(
    from = nodes$id,
    to = nodes$id
  ) %>%
    # Skip loops
    filter(from != to) %>%
    mutate(weight = runif(n = n(), min = min, max = max) %>% {./100} %>% round(digits = 1) * 100) %>%
    mutate(weight = weight * rbinom(n = n(), size = 1, prob = 1 - percent_zero)) %>%
    filter(weight > 0)  %>%
    # Last, make a blocking variable for traffic level
    mutate(block = if_else(weight > midpoint, "A", "B"))
  
}


get_perm = function(nodes, edges, blocks = "block"){
  
  data = edges %>%
    left_join(by = c("from" = "id"), y = nodes %>% select(id, from_group = group, from_pop_density = pop_density)) %>%
    left_join(by = c("to" = "id"), y = nodes %>% select(id, to_group = group, to_pop_density = pop_density)) %>%
    mutate(weight = weight / (from_pop_density * to_pop_density) * 1e6) %>%
    select(-from_pop_density, -to_pop_density) %>%
    mutate(level = case_when(
      from_group == TRUE & to_group == TRUE ~ "hh",
      from_group == FALSE & to_group == TRUE ~ "lh",
      from_group == TRUE & to_group == FALSE ~ "hl",
      from_group == FALSE & to_group == FALSE ~ "ll"
    ),
    level = factor(level, levels = c("hh", "hl", "lh", "ll")))
  
  # If any blocking variables are provided, permute by block
  if(!is.null(blocks)){
    permuted = data %>%
      group_by(across(any_of(blocks))) %>%
      mutate(weight = sample(weight, size = n(), replace = FALSE))  %>%
      ungroup() 
  }else if(is.null(blocks)){
    permuted = data %>%
      mutate(weight = sample(weight, size = n(), replace = FALSE))
  }
    # group_by(level) %>%
    # ungroup()
  output = list(nodes = nodes, edges = permuted)
  return(output)
}

get_obs = function(nodes, edges){
  
  obs = edges %>%
    left_join(by = c("from" = "id"), y = nodes %>% select(id, from_group = group, from_pop_density = pop_density)) %>%
    left_join(by = c("to" = "id"), y = nodes %>% select(id, to_group = group, to_pop_density = pop_density)) %>%
    mutate(weight = weight / (from_pop_density * to_pop_density) * 1e6) %>%
    select(-from_pop_density, -to_pop_density) %>%
    mutate(level = case_when(
      from_group == TRUE & to_group == TRUE ~ "hh",
      from_group == FALSE & to_group == TRUE ~ "lh",
      from_group == TRUE & to_group == FALSE ~ "hl",
      from_group == FALSE & to_group == FALSE ~ "ll"
    ),
    level = factor(level, levels = c("hh", "hl", "lh", "ll")))
  
  output = list(nodes = nodes, edges = obs)
  return(output)
}
# bundle = get_perm(nodes = nodes, edges = edges)
# bundle

get_metrics = function(edges){
  # edges = bundle$edges
  metrics = edges %>%
    group_by(level, .drop = FALSE) %>%
    summarize(weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate(total = sum(weight, na.rm = TRUE),
           weight = weight / total) %>%
    select(level, weight) %>%
    tidyr::pivot_wider(names_from = level, values_from = weight) %>%
    mutate(index = 2/3*( abs(ll - .25)  + abs(lh - .25) + abs(hl - .25) + abs(hh - .25)))
  return(metrics)
}

get_stat = function(bundle){
  
  nodes = bundle$nodes
  edges = bundle$edges
  
  # Make graph
  g = tbl_graph(
    nodes = nodes, edges= edges, directed = TRUE, node_key = "id"
  ) %>%
    # unweighted indegree/outdegree
    mutate(indegree = tidygraph::centrality_degree(mode = "in"),
           outdegree = tidygraph::centrality_degree(mode = "out"),
           degree = indegree + outdegree)
  
  
  # Get coordinates
  coords = g %>% ggraph::ggraph(layout = "circle") %>% with(data) %>% select(id, group, indegree, outdegree, degree, x, y) %>%
    # Flip the x-axis to be more visually appealing
    mutate(x = x*-1)
  
  net = edges %>% 
    left_join(by = c("from" = "id"), y = coords %>% select(id, from_x = x, from_y = y)) %>%
    left_join(by = c("to" = "id"), y = coords %>% select(id, to_x = x, to_y = y))
  
  output = list(
    nodes = coords,
    edges = net,
    n_edges = nrow(edges),
    n_nodes = nrow(nodes)
  )
  
  output$metrics = get_metrics(edges = edges)
  
  output$density = output$n_edges / (output$n_nodes * (output$n_nodes - 1))
  
  
  
  return(output)  
}

# get_stat(bundle)

get_network = function(stat, curvature = 0.3, alpha = 0.5, sizerange = c(5,20), linewidthrange = c(0,5), arrowsize = 0.5){
  gg = ggplot() +
    geom_point(
      data = stat$nodes,
      mapping = aes(x = x, y = y, size = degree, fill = group),
      alpha = 0.9, shape = 21, color = "#373737"
    ) +
    # Add white
    geom_curve(
      data = stat$edges, 
      mapping = aes(x = from_x, y = from_y, xend = to_x, yend = to_y, 
                    linewidth = weight + 1),
      alpha = alpha,
      color = "white",
      curvature = curvature, arrow = grid::arrow(type = "closed")) +
    
    geom_curve(
      data = stat$edges, 
      mapping = aes(x = from_x, y = from_y, xend = to_x, yend = to_y, 
                    linewidth = weight),
      alpha = alpha,
      color = "#373737", 
      curvature = curvature,arrow.fill = "#373737", arrow = grid::arrow(length = unit(arrowsize, "cm"), type = "closed"))  +
    shadowtext::geom_shadowtext(
      data = stat$nodes,
      mapping = aes(x = x, y = y, label = id, color = group),
      size = 10,nudge_y = 0.25,
      bg.color = "white",  bg.r = 0.1
    ) +
    shadowtext::geom_shadowtext(
      data = stat$nodes,
      mapping = aes(x = x, y = y, label = id, color = group),
      size = 10,nudge_y = 0.25,
      bg.color = "#373737",  bg.r = 0.025
    )
  
  
  
  colors = c("lightgrey", "black")
  
  gg = gg + 
    theme_void(base_size = 14) +
    scale_linewidth(range = linewidthrange, guide = "none") +
    scale_size(range = sizerange, guide = "none") +
    scale_fill_manual(values = colors, guide = "none") +
    scale_color_manual(values = colors, guide = "none") +
    scale_x_continuous(limits = c(-1.25, 1.25)) +
    scale_y_continuous(limits = c(-1.25, 1.25)) 
  
  return(gg)
}

get_bars = function(stat){
  
  triangle = "<br>&darr;<br>"
  data = stat$metrics %>%
    tidyr::pivot_longer(cols = c(hh, hl, ll, lh), names_to = "type", values_to = "value") %>%
    mutate(type = type %>% dplyr::recode_factor(
      "hh" = paste0("High ", triangle, " High"),
      "hl" = paste0("High ", triangle, " Low"),
      "lh" = paste0("Low ", triangle, " High"),
      "ll" = paste0("Low ", triangle, " Low")
    )) %>%
    mutate(label = scales::percent(value))
  
  num = tibble(
    x = 2.5, y = 0.95,
    index = stat$metrics$index,
    label = paste("Similarity Index: ", round(index, digits = 2) )
  )
  
  colors = c("#290b49", "#1a2476", "#3d6b9c", "#74b5bb")
  
  gg = ggplot() +
    geom_col(
      data = data,
      mapping = aes(x = type, y = value, fill = type)
    ) +
    geom_hline(yintercept = 0.25, linetype = "dashed", color = "darkgrey") +
    shadowtext::geom_shadowtext(
      data = data,
      mapping = aes(x = type, y = value, label = label, color = type),
      bg.color = "white", bg.r = 0.1, nudge_y = 0.05, fontface = "bold"
    ) +
    shadowtext::geom_shadowtext(
      data = num,
      mapping = aes(x = x, y = y, label = label),
      bg.color = "white", color = "#373737", bg.r = 0.1, fontface = 'bold'
    ) +
    scale_y_continuous(limits = c(0, 1), 
                       expand = expansion(c(0,0)), 
                       labels = scales::label_percent()) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme_bw(base_size = 14) +
    theme(panel.grid = element_blank(),
          axis.text.x = ggtext::element_markdown(),
          axis.title.x = ggtext::element_markdown(size = 11),
          axis.title.y = ggtext::element_markdown(size = 11),
          axis.ticks = element_blank(),
          panel.border = element_rect(fill = NA, color = "#373737"),
          axis.line = element_blank()) +
    guides(fill = "none", color = "none") +
    labs(x = "Source Type &rarr; Destination Type",
         y = "Share of Ridership") 
  return(gg)
}

get_many_perms = function(nodes, edges, n = 25, blocks = "block"){
  # Get histogram
  #system.time({
  perms = 1:n %>%
    purrr::map_dfr(
      .f = ~get_perm(nodes = nodes, edges = edges, blocks = blocks)$edges %>% get_metrics(), .id = "rep"
    ) %>%
    mutate(rep = as.numeric(rep))
  # })
  obs = get_obs(nodes = nodes, edges = edges)$edges %>% get_metrics() %>% mutate(rep = 0)
  
  result = bind_rows(obs, perms)  
  
  return(result)
}


get_dist = function(many_perms, binwidth = 0.025, label = "", limits = NULL){
  
  # Testing values
  # binwidth = 0.025
  
  # show percentage of permutations more extreme than that one...
  obs = many_perms %>% filter(rep == 0)
  perms = many_perms %>% filter(rep != 0)
  
  
  height = perms %>%
    mutate(group = ggplot2::cut_interval(x = index, length = binwidth)) %>%
    group_by(group) %>%
    summarize(count = n(), .groups = "drop") %>%
    ungroup() %>%
    arrange(desc(count)) %>%
    slice(1) %>%
    with(count) * 0.9
  
  num = tibble(
    value = obs$index,
    x = value - 0.02,
    y = height,
    label = paste0("Observed:\n", round(value, digits = 2))
  )
  
  prob = many_perms %>%
    summarize(
      med = median(index[rep != 0]),
      obs = index[rep == 0],
      # one-tailed test
      p_value = sum(index >= obs) / n(),
      # p_value = case_when(
      #   obs > med ~ sum(index >= obs) / n(),
      #   obs <= med ~ sum(index <= obs) / n()
      # ) * 2,
      p_value_label = case_when(
        p_value < 0.001 ~ "p < 0.001",
        p_value >= 0.001 ~ paste0('p = ', round(p_value, digits = 3))
      ),
      
      xmin = case_when(
        obs < med ~ obs,
        TRUE ~ -Inf
      ),
      xmax = case_when(
        obs < med ~ Inf,
        TRUE ~ obs
      ),
      ymin = 0, 
      ymax = Inf,
      x = obs + 0.01,
      y = height,
      label = paste0("% more extreme: \n", p_value_label )
    )
  
  
  # Find the box more extreme
  # box = tibble(
  #   xmin = obs$index,
  #   xmax = Inf,
  #   ymin = 0, ymax = Inf
  # )
  
  gg = ggplot() +
    # geom_rect(
    #   data = prob,
    #   mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #   fill = "#74b5bb75",
    #   color = NA
    # ) +
    geom_histogram(
      data = many_perms, mapping = aes(x = index),
      color = "white",
      fill = "#3d6b9c",
      binwidth = binwidth,
    ) +
    
    geom_vline(
      data = obs, 
      mapping = aes(xintercept = index),
      color = "black",
      linetype = "dashed", linewidth = 1.25
    ) +
    
    geom_shadowtext(
      data = num,
      mapping = aes(x = x, y = y, label = label),
      bg.r = 0.2, bg.color = "white", color = "#373737",
      hjust = 1
    ) +
    
    
    geom_shadowtext(
      data = prob,
      mapping = aes(x = x, y = y, label = label),
      bg.r = 0.2, bg.color = "white", color = "#373737",
      hjust = 0
    ) +
    
    scale_x_continuous(limits = limits, 
                       expand = expansion(c(0,0))) +
    scale_y_continuous(expand = expansion(c(0,0.01))) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = ggtext::element_markdown(),
      axis.title.x = ggtext::element_markdown(),
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.ticks = element_blank(),
      panel.border = element_rect(fill = NA, color = "#373737"),
      axis.line = element_blank()
    ) +
    labs(x = "Similarity Index<br><sup>[0 = heterogenous, 1 = homogenous]</sup>",
         y = "Frequency in Permuted Networks",
         title = paste0("Null Distribution ", label, "\n(n = ",  nrow(perms), " networks)"))
  return(gg)
}








