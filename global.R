# Load requested data by the user
setwd("~/Desktop/FYP_Project/current_data_for_work")
full_data_set <- read.csv(file = 'combined_data.csv')
promt_sub_g <- "You can choose which connected sub-graph to visualize. For the current subset of data there are "
link_str <- "https://clericus.ie/person/"

general_info_p <- paste(readLines("text.txt"), collapse = "\n")

edge_list_5p <- read.csv(file = 'edge_list_deg_85+.csv')
edge_list_10p <- read.csv(file = 'edge_list_deg_81+.csv')
edge_list_15p <- read.csv(file = 'edge_list_deg_78+.csv')
edge_list_20p <- read.csv(file = 'edge_list_deg_73+.csv')
edge_list_25p <- read.csv(file = 'edge_list_deg_70+.csv')

# data for the histogram and plot configuration 
full_edge_list <- read.csv(file = 'NOD_EDGE_LIST_FINAL.csv')
full_graph <- graph_from_data_frame(full_edge_list, directed = FALSE, vertices = NULL)
nodal_degree <- degree(full_graph)
degrees_as_df <- as.data.frame(nodal_degree)
nodal_d_df <- as.data.frame(table(degrees_as_df$nodal_degree))

histogram_plot <- ggplot(nodal_d_df, aes(Var1, Freq)) +
  xlab("Nodal degree") + ylab("Frequency") +
  geom_col(fill = "#8675A9") +
  scale_x_discrete(breaks = seq(0, 221, by = 5),
                   labels = seq(0, 221, by = 5)) +
  coord_cartesian(ylim = c(0, 600)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

# End of the histogram code

load_requested_data_based_on_percent <- function(selected_percent) {

  if (selected_percent == 5) {
    edge_list <- edge_list_5p
  } else if (selected_percent == 10) {
    edge_list <- edge_list_10p
  } else if (selected_percent == 15) {
    edge_list <- edge_list_15p
  } else if (selected_percent == 20) {
    edge_list <- edge_list_20p
  } else if (selected_percent == 25) {
    edge_list <- edge_list_25p
  }
  
  list_of_unique_nodes <- unique(c(edge_list$A, edge_list$B))
  selected_nodes_df <<- full_data_set[full_data_set$node.ID %in% list_of_unique_nodes,]
  
  # Preparing a graph object for the selected file (which will be held on the server and called based on user input)
  graph_object <- graph_from_data_frame(edge_list, directed = FALSE, vertices = NULL)
  degrees_curr_g <- degree(graph_object)
  decomposed_g <- decompose.graph(graph_object)
  return(decomposed_g)
} 


create_sj_object <- function(sub_gr_number) {
  current_subgraph <- decomposed_g[[sub_gr_number]] # Selects a subgraph chosen by the user
  members <- membership(cluster_walktrap(current_subgraph, steps = 1))
  my_sj_list <- igraph_to_networkD3(current_subgraph, group = members)
  return(my_sj_list)
}

create_sj_indiv <- function(selected_node_id) {

  egocentric_edge_l = full_edge_list[which(full_edge_list[,1] == selected_node_id),]
  
  g_o <- graph_from_data_frame(egocentric_edge_l, directed = FALSE, vertices = NULL)
  m <- membership(cluster_walktrap(g_o, steps = 1))
  my_sj_list <- igraph_to_networkD3(g_o, group = m)
 
  return(my_sj_list)
}










