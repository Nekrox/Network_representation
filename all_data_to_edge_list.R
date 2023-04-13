# setting a directory
library(plyr)
#library(igraph)
library(dplyr)
library(readr)  
setwd("~/Desktop/FYP_Project/data-exports")
# exporting data
#data <- read.csv(file = 'export-45.csv')

combined_data <- list.files(path = "~/Desktop/FYP_Project/data-exports",    
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                           
  bind_rows                                                      

colnames(combined_data)[1] ="node.ID"
colnames(combined_data)[2] ="node.name"
colnames(combined_data)[3] ="event.ID"

write.csv(combined_data , "/Users/markshteingardt/Desktop/FYP_Project/combined_data.csv", row.names=FALSE)






# creating a subset which only contains the ID
dt <- combined_data[ , c("node.ID", "event.ID")] 
# checking which rows to delete
occurance_table = count(dt[2]) #  DOES NOT WORK FOR SOME REASON IF DPLYR PACKAGE IS ON
event_ids_to_delete <- c() 

# ===========================DATA_PREP====================================
# Creating a list with events id which were only visited by one priest 
for (i in 1:nrow(occurance_table)) {
  if (occurance_table[i,2] == 1) {
    event_ids_to_delete = append(event_ids_to_delete, occurance_table[i,1])
  }
}

# deleting such events from the df
for (i in 1:length(event_ids_to_delete)) {
  if (event_ids_to_delete[i] %in% dt$event.ID) {
    dt <- dt[dt$event.ID != event_ids_to_delete[i],]
  }
}
# Sort the new data by event.ID
dt <- dt[order(dt$event.ID),]

# CREATE AN EDGE LIST FROM DT, WHERE DT is node.ID and event.ID without isolates
columns = c("A","B") 
edge_df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(edge_df) = columns

# ===========================EDGE_LIST_CONSTRUCTION_ALGORITHM====================================

count = 1
node_array = list()
next_event = FALSE
finished = FALSE
times_to_execute = 0


while (finished == FALSE) {
  # filling an array with the first "bunch" of nodes which is related
  node_array = append(node_array, dt$node.ID[count])
  while (next_event == FALSE) {
    
    if (dt$event.ID[count] == dt$event.ID[count+1]) {
      node_array = append(node_array, dt$node.ID[count+1])
    } else {
      next_event = TRUE
    }
    
    if (count < nrow(dt)-1) {
      count = count + 1
    } else {
      finished = TRUE
      break
    }
   
  } 
  
  # now I need to use the node array to add edges to df
  times_to_execute = length(node_array) - 1
  for (k in 1:times_to_execute) {

    for (i in (k+1):length(node_array)) {
      edge_df[nrow(edge_df) + 1,] = c(node_array[k], node_array[i])
    }
  }
  
  node_array = list()
  next_event = FALSE
  
  print(paste("Progress is", 100*(count/nrow(dt))))

}

# omit duplicates and save the results to a seperate csv file
#edge_df = na.omit(edge_df)
edge_df = distinct(edge_df)
# convert node.ID to name
index_a = 0
index_b = 0

for (i in 1:nrow(edge_list)) {
  # replace node_id with priest name in col A
  index_a = match(edge_list$A[i], data$node.ID)
  edge_list$A[i] = data$node.name[index_a]
  index_a = 0
  # replace node_id with priest name in col b
  index_b = match(edge_list$B[i], data$node.ID)
  edge_list$B[i] = data$node.name[index_b]
  index_b = 0
}

write.csv(edge_list , "/Users/markshteingardt/Desktop/FYP_Project/full_dt_node_names.csv", row.names=FALSE)






