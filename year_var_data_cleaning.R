
setwd("~/Desktop/FYP_Project/Shiny/FYP_app/application_data")
updt_full_data <- read.csv(file = 'updt_combined_TOBEDELETED.csv')
# created a filtered version of the df without NA values (data contained 516 NA values)
filtered_updt_data <- updt_full_data[!is.na(updt_full_data$event.start.date), ]
# create a list of years, based on the event.start.date in the filtered data.
# Note that small percent of the data was entered incorrectly. 
years <- sapply(1:length(filtered_updt_data$event.start.date),
          function(x) substr( filtered_updt_data$event.start.date[x], 
                           nchar(filtered_updt_data$event.start.date[x])-3, 
                           nchar(filtered_updt_data$event.start.date[x])) )
# since there are a few istances of corrupted data, values are converted from the 
# string to integer value. In that case all non-correct values turned into NA. 
filtered_updt_data$event.start.date <- as.integer(years)
# While examining the new filtered data, it was noticed (by sorting in osebding order) 
# that one value is 993 when it should be 993
row_n <- which(filtered_updt_data$event.start.date == 993)
filtered_updt_data$event.start.date[row_n] <- 1993
 # Lastly, we need to remove instanses where the is only one recored per year. 
# Note that previously preist data was subseted using less efficient code
occurance_table <- table(filtered_updt_data$event.start.date)
filtered_updt_data <- filtered_updt_data[filtered_updt_data$event.start.date %in% 
                                           names(occurance_table)[occurance_table > 1], ]
# That gives us a clean subset of data where Min year is 1593 and Max 1993 
filtered_updt_data$event.start.date <- as.integer(as.numeric(filtered_updt_data$event.start.date))
#



write.csv(filtered_updt_data , "/Users/markshteingardt/Desktop/FYP_Project/clean_updt_full_data.csv", row.names=FALSE)
