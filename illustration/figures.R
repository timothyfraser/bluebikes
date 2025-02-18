# figures.R

# set working directory
rm(list = ls())
setwd(paste0(rstudioapi::getActiveProject(), "/illustration"))
source("functions.R")

# FIGURES ######################

mynodes = read_csv("mynodes.csv")
myedges = read_csv("myedges.csv")
many_perms = read_rds("mymany_perms.rds")
many_perms_unblocked = read_rds("mymany_perms_unblocked.rds")
read_rds("mystat.rds")
# get_perm(nodes = mynodes, edges = myedges)
# get_obs(nodes = mynodes, edges = myedges)


# 3 rows
# Get observed
stat0 = get_obs(nodes = mynodes, edges = myedges) %>% get_stat()
stat0 %>% saveRDS("mystat.rds")

net0 = stat0 %>% get_network()
bars0 = stat0 %>% get_bars()

# Permute without block permutations
stat1 = get_perm(nodes = mynodes, edges = myedges, blocks = NULL) %>% get_stat()
net1 = stat1 %>% get_network()
bars1 = stat1 %>% get_bars()

# Permute with block permutations
stat2 = get_perm(nodes = mynodes, edges = myedges, blocks = "block") %>% get_stat()
net2 = stat2 %>% get_network()
bars2 = stat2 %>% get_bars()


plot1 = ggpubr::ggarrange(
  plotlist = list(net0, bars0,
                  net1, bars1,
                  net2, bars2),
  labels = c("D\nObserved", "G",
             "E\nPermuted", "H",
             "F\nBlock\nPermuted", "I"), hjust = 0,
  ncol = 2, nrow = 3
)
#
# ggsave(plot = plot1, filename = "graphs_and_bars.png", dpi = 300, width = 8, height = 12)
# 
# browseURL("graphs_and_bars.png")

dist1 = get_dist(many_perms = many_perms, binwidth = 0.025, label = "[Unblocked]", limits = c(0, 0.5))
dist2 = get_dist(many_perms = many_perms_unblocked, binwidth = 0.025, label = "[Blocked]", limits = c(0, 0.5))

# gg = ggpubr::ggarrange(
#   plotlist = list(plot1, plot2),
#   nrow = 1, ncol = 2, widths = c(2,1),
#   labels = c("", "J"), hjust = 0
# )
# 
# ggsave(plot = gg, filename = "illustration.png", dpi = 300, width = 12, height = 12)

plot2 = ggpubr::ggarrange(
  plotlist = list(NULL, dist1, dist2),
  nrow = 3, ncol = 1, heights = c(1,1,1),
  labels = c("J", "K", "L")
)

gg = ggpubr::ggarrange(
  plotlist = list(plot1, plot2),
  nrow = 1, ncol = 2, widths = c(2,1)
)

ggsave(plot = gg, filename = "illustration.png", dpi = 300, width = 12, height = 12)


browseURL("illustration.png")
# many_perms = get_many_perms(nodes = nodes, edges = edges)



 

rm(list = ls())
