# Load requested data by the user
setwd("~/Desktop/FYP_Project/Shiny/FYP_app/application_data")

full_data_set <- read.csv(file = 'combined_data.csv')
full_edge_list <- read.csv(file = 'upd_full_edge_list.csv')
edge_list_5p <- read.csv(file = 'edge_list_deg_85+.csv')
edge_list_10p <- read.csv(file = 'edge_list_deg_81+.csv')
edge_list_15p <- read.csv(file = 'edge_list_deg_78+.csv')
edge_list_20p <- read.csv(file = 'edge_list_deg_73+.csv')
edge_list_25p <- read.csv(file = 'edge_list_deg_70+.csv')
updt_full_data <- read.csv(file = 'clean_updt_full_data.csv')
# updt_full_edge_list <- read.csv(file = 'upd_full_edge_list.csv')

# Defining strings 
PROMT_SUB_G_P1 <- "You can choose which connected sub-graph to visualize. For the 
current subset of data there are "
PROMT_SUB_G_P2 <- " subgraphs. Please note that they are ordered by the amount of nodes in the decreasing
order. "
HELP_STR <- "Note: to see the node information click on any node on the right."
LINK_STR <- "https://clericus.ie/person/"
NO_DATA_STR <- "There is not enough data in the current time frame selection to 
represent a network. Please select another timeline."
HISTOGRAM_STR <- "Histogram below represents the distribution of nodal degrees for the entire data, 
which accounts for 40,000 unique entries."


FIXED_INTERVAL <- 10
# Loading text files for the app
general_info_p <- paste(readLines("general_p_text.txt"), collapse = "\n")
time_info_p <- paste(readLines("time_text.txt"), collapse = "\n")
# define js function for opening urls in new tab/window
js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
" 

decomposed_g <- decompose.graph(graph_from_data_frame(edge_list_10p, directed = FALSE, vertices = NULL))


# data for the histogram and plot configuration 
full_graph <- graph_from_data_frame(full_edge_list, directed = FALSE, vertices = NULL)
nodal_degree <- degree(full_graph)
degrees_as_df <- as.data.frame(nodal_degree)
nodal_d_df <- as.data.frame(table(degrees_as_df$nodal_degree))

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
  
  # can potentially improve two code lines below
  list_of_unique_nodes <- unique(c(edge_list$A, edge_list$B))
  selected_nodes_df <<- full_data_set[full_data_set$node.ID %in% list_of_unique_nodes,]
  # Preparing a graph object for the selected file (which will be held on the server and called based on user input)
  graph_object <- graph_from_data_frame(edge_list, directed = FALSE, vertices = NULL)
  decomposed_g <- decompose.graph(graph_object)
  # sort it by the amount of nodes. 
  node_counts <- sapply(decomposed_g, vcount)
  decomposed_g_ordered <- decomposed_g[order(unlist(node_counts), decreasing = TRUE)]
  return(decomposed_g_ordered)
} 

create_sj_object <- function(sub_gr_number) {
  current_subgraph <<- decomposed_g[[sub_gr_number]] # Selects a subgraph chosen by the user
  members <- membership(cluster_walktrap(current_subgraph, steps = 1))
  my_sj_list <- igraph_to_networkD3(current_subgraph, group = members)
  return(my_sj_list)
}

# create_sj_indiv <- function(selected_node_id) {
# 
#   egocentric_edge_l = full_edge_list[which(full_edge_list[,1] == selected_node_id),]
#   # create a data frame for individual network representation 
#   indv_unq_n <- unique(c(egocentric_edge_l$A, egocentric_edge_l$B))
#   selected_indv_n_df <<- full_data_set[full_data_set$node.ID %in% indv_unq_n,]
#   
#   g_o <- graph_from_data_frame(egocentric_edge_l, directed = FALSE, vertices = NULL)
#   m <- membership(cluster_walktrap(g_o, steps = 1))
#   indv_sj_list <- igraph_to_networkD3(g_o, group = m)
#   return(indv_sj_list)
# }

time_frame_decomposed_creation <- function(st_y, end_y) {
  
  selected_tf_df <<- subset(updt_full_data, event.start.date >= st_y & event.start.date <= end_y)
  # create a uniqe list of required nodes
  uniqe_nodes_tf <- unique(selected_tf_df$node.ID)
  # sort full edge_list so we have only required nodes
  tf_edge_list <- full_edge_list[full_edge_list$A %in% uniqe_nodes_tf & full_edge_list$B %in% uniqe_nodes_tf, ]
  # construct a decomposed graph time frame object

  # Preparing a graph object for the selected file (which will be held on the server and called based on user input)
  time_f_graph_object <- graph_from_data_frame(tf_edge_list, directed = FALSE, vertices = NULL)
  
  if (length(time_f_graph_object) > 1) {
    tf_decomposed_g <- decompose.graph(time_f_graph_object)
    # sort it by the amount of nodes. 
    tf_node_counts <- sapply(tf_decomposed_g, vcount)
    tf_decomposed_g_ordered <- tf_decomposed_g[order(unlist(tf_node_counts), decreasing = TRUE)]
    return(tf_decomposed_g_ordered)
  } else {
    empty_graph <- decompose.graph(graph.empty())
    return(empty_graph)
  }
  
} 


create_tf_sj_object <- function(tf_sub_gr_number) {
  
  print(tf_sub_gr_number)
  
  # if (is.null(tf_sub_gr_number)) {
  #   tf_sub_gr_number = 1
  # }
  # cat("INSIDE THE CREATE_TF_FUNC:", tf_sub_gr_number, "\n")
  current_tf_subgraph <- time_frame_decomposed_g[[tf_sub_gr_number]] # Selects a subgraph chosen by the user
  tf_members <- membership(cluster_walktrap(current_tf_subgraph, steps = 1))
  tf_sj_list <- igraph_to_networkD3(current_tf_subgraph, group = tf_members)
  return(tf_sj_list)
}

nodeOnClickInfo_func <- function(information_object) {
  
  str_id <- paste("Node ID: ", information_object$node.ID[1])
  str_name <- paste("Name: ", information_object$node.name[1])
  str_type <- paste("Participated in: ", nrow(information_object), " event(s).")
  
  return(HTML(paste(str_id, str_name, str_type, sep = '<br/>')))
}


present_network_stat_func <- function(current_sub_g_n, decom_object) {
  # print(current_sub_g_n)
  current_c_subgraph <- decom_object[[current_sub_g_n]]
  
  density_v <- sprintf("%.2f", graph.density(current_c_subgraph))
  clust_coef <- sprintf("%.2f", transitivity(current_c_subgraph))
  mean_dist <- sprintf("%.2f", mean_distance(current_c_subgraph))
  
  # print(length(current_c_subgraph))
  dens_str <- paste("Density: ", density_v)
  
  if (clust_coef == "NaN") {
    cluster_coef_str <- "Not enough nodes to calculate clustering coefficient."
  } else {
    cluster_coef_str <- paste("Clustering coefficient: ", clust_coef)
  }

  mean_d_str <- paste("Mean distance: ", mean_dist)
  diameter_str <- paste("Diameter: ", diameter(current_c_subgraph))
  
  return(HTML(paste(dens_str, cluster_coef_str, mean_d_str, diameter_str, sep = '<br/>')))

}








egocentricNetwork_func <- function(selected_node_id, given_script) {
  

  egocentric_edge_l = full_edge_list[which(full_edge_list[,1] == selected_node_id),]
  # create a data frame for individual network representation
  indv_unq_n <- unique(c(egocentric_edge_l$A, egocentric_edge_l$B))
  selected_indv_n_df <<- full_data_set[full_data_set$node.ID %in% indv_unq_n,]
  # Another edge_list consturction
  # ego_n_list <- egocentric_edge_l$B
  # other_e_nodes_edge_l <- full_edge_list[full_edge_list$A %in% egocentric_edge_l$B 
  #                                        & full_edge_list$B %in% egocentric_edge_l$B,]
  # 
  # ego_centric_edge_list <- rbind(egocentric_edge_l,other_e_nodes_edge_l)
  
  
  
  g_o <- graph_from_data_frame(egocentric_edge_l, directed = FALSE, vertices = NULL)
  m <- membership(cluster_walktrap(g_o, steps = 1))
  indv_sj_list <- igraph_to_networkD3(g_o, group = m)
  
  current_script <- given_script
  
  forceNetwork(Links = indv_sj_list$links, Nodes = indv_sj_list$nodes, Source = "source",
               Target = "target", NodeID = "name", Group = "group",
               opacity = 1, zoom = T, fontSize = 0, linkDistance = 150, clickAction = current_script
  )
  
  
  
}


