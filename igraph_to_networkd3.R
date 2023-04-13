library(igraph)
library(networkD3)
setwd("~/Desktop/FYP_Project/current_data_for_work")
full_edge_list <- read.csv(file = 'NOD_EDGE_LIST_FINAL.csv')
edge_list_121 <- read.csv(file = 'edge_list_deg_80+.csv')
# entire_graph <- graph_from_data_frame(full_edge_list, directed = FALSE, vertices = NULL)
graph121 <- graph_from_data_frame(edge_list_121, directed = FALSE, vertices = NULL)

dg <- decompose.graph(graph121) # returns a list of connected sub-graphs
# our full data contains 788 connected graphs, so here we just plot the first 10
for (i in 1:14) {
plot(dg[[i]]) # plot e.g. the 1st one
}

sample_g <- dg[[1]]


# Find group membership
wt <- cluster_walktrap(sample_g, steps = 1)
members <- membership(wt)

# Convert igraph to list for networkD3
sj_list <- igraph_to_networkD3(sample_g, group = members)
# Plot as a forceDirected Network
forceNetwork(Links = sj_list$links, Nodes = sj_list$nodes, Source = 'source',
             Target = 'target', NodeID = 'name', Group = 'group',
             zoom = TRUE, 
             opacity = 1,
             linkDistance = 200)