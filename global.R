# Load requested data by the user


# load_my_data <- function() {
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

decomposed_g <- decompose.graph(graph_from_data_frame(edge_list_10p, directed = FALSE, vertices = NULL))

FIXED_INTERVAL <- 10
# define js function for opening urls in new tab/window
js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
" 

# Loading text files for the app

instructions_html <- readLines("html_code.html")

time_info_p <- paste(readLines("time_text.txt"), collapse = "\n")

# ================================TAG_LIST==========================================
tags_list <- tagList(
  # tag responsible for navbar customization
  tags$style(HTML("
      .navbar .navbar-nav { 
                           color: #FFFFFF; 
                           font-size: 22px; 
                           background-color: #8675A9 ; } 
      .navbar.navbar-default.navbar-static-top{ color: #8675A9; 
                                      font-size: 38px; 
                                      background-color: #8675A9;}
      .navbar-default .navbar-brand { color: #000000; 
                                      font-size: 30px; 
                                      background-color: #8675A9 ;}
      
      /* Change text color of panels when hovered */
      .navbar .navbar-nav > li > a:hover {
        color: #FFFFFF; 
      }
        
      /* Change text color of panels when not hovered */
      .navbar-default .navbar-nav > li > a {
        color: black;
      }
    
    ")),
  
  tags$style(HTML('
      p {font-family: "Helvetica Neue", Helvetica, Arial, sans-serif; font-size: 20px;}
    ')),
  # tag responsible for all buttons customization
  tags$style(HTML("
      .custom_button {
        width: 220px;
        height: 45px;
        background-color: #8675A9;
        color: #FFFFFF;
        border-color: #000000;
        border-radius: 5px;
        padding: 5px 10px;
        font-size: 18px;
        }
       ")),
  # two tags for margins
  tags$style(HTML(".small_margin_class {margin-bottom: 5px;}")),
  tags$style(HTML(".med_margin_b_class {margin-bottom: 20px;}")),
  tags$style(HTML(".med_margin_class {margin-top: 20px;}")),
  tags$style(HTML(".big_margin_class {margin-top: 40px;}")),
  # custom text tag
  tags$style(
    HTML(".text_custom_class {
           color: black;
           font-size: 24px;
           font-style: normal;
         }")
  ),
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, 
                      .js-irs-0 .irs-bar {background: #8675A9; border: #8675A9}"))
)
# ================================FUNCTIONS==========================================
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

  current_tf_subgraph <- time_frame_decomposed_g[[tf_sub_gr_number]]
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

  current_c_subgraph <- decom_object[[current_sub_g_n]]
  
  density_v <- sprintf("%.2f", graph.density(current_c_subgraph))
  clust_coef <- sprintf("%.2f", transitivity(current_c_subgraph))
  mean_dist <- sprintf("%.2f", mean_distance(current_c_subgraph))
  
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
  # first I create the needed edge list, which is "ego_centric_edge_list"
  egocentric_edge_l = full_edge_list[which(full_edge_list[,1] == selected_node_id),]
  # create a data frame for individual network representation
  indv_unq_n <- unique(c(egocentric_edge_l$A, egocentric_edge_l$B))
  selected_indv_n_df <<- full_data_set[full_data_set$node.ID %in% indv_unq_n,]
  other_e_nodes_edge_l <- full_edge_list[full_edge_list$A %in% egocentric_edge_l$B 
                                         & full_edge_list$B %in% egocentric_edge_l$B,]
  
  ego_centric_edge_list <- rbind(egocentric_edge_l,other_e_nodes_edge_l)
  # then I use this ego_centric_edge_list to create an igraph object and
  # transform it to a suitable indv_sj_list for the forceNetwork. 
  # I cant center it, but there is a solution in one link, which I saved in notion.

  g_o <- graph_from_data_frame(ego_centric_edge_list, directed = FALSE, vertices = NULL)
  m <- membership(cluster_walktrap(g_o, steps = 1))
  indv_sj_list <- igraph_to_networkD3(g_o, group = m)
  indv_sj_list$nodes$group[1] <- 100 # sets the ego node to a unique color  
  
  current_script <- given_script
  
  forceNetwork(Links = indv_sj_list$links, Nodes = indv_sj_list$nodes, Source = "source",
               Target = "target", NodeID = "name", Group = "group",
               opacity = 1, zoom = T, fontSize = 0, linkDistance = 150, clickAction = current_script
  )
  
} 
  
display_text_func <- function() { 
  return(HTML(paste("<blockquote>", text, "</blockquote>", collapse = "\n")))
  }
  


