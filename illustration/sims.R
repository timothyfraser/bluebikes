# sims.R
rm( list = ls())
# set working directory
setwd(paste0(rstudioapi::getActiveProject(), "/illustration"))
source("functions.R")

# SIMS ######################

mynodes = simulate_nodes(n = 5)
myedges = simulate_edges(nodes = mynodes, percent_zero = 0.25, min = 100, max = 300)
get_perm(nodes = mynodes, edges = myedges)

get_obs(mynodes, myedges)
get_obs(mynodes, myedges) %>% get_stat() %>%  get_network()
get_obs(nodes = mynodes, edges = myedges)$edges %>% get_metrics()
get_perm(nodes = mynodes, edges = myedges, blocks = "block")$edges %>% get_metrics()

mynodes %>% write_csv("mynodes.csv")
myedges %>% write_csv("myedges.csv")



mynodes = read_csv("mynodes.csv")
myedges = read_csv("myedges.csv")

many_perms = get_many_perms(nodes = mynodes, edges = myedges, n = 1000)
many_perms %>% saveRDS("mymany_perms.rds")


many_perms_unblocked = get_many_perms(nodes = mynodes, edges = myedges, n = 1000, blocks = NULL)
many_perms_unblocked %>% saveRDS("mymany_perms_unblocked.rds")

rm(list = ls())
