docs_in_each_system <- data.frame(matrix(ncol = 15, nrow = 15))

#check number of documents each topic, each system
for (i in 1:15) {
  current_table <- table_list[[i]]  # Get the current system
  # print(paste("system: ", i))
  
  # define totalAveragePrecision, used in calc MAP
  totalAveragePrecision <- 0
  
  # Iterate over topic '401' to '450'
  for (j in 401:450) {
    # Filter rows for the current value
    rows <- current_table[current_table[, 1] == j, ]
    
    # calculate the number of rows
    num_rows <- nrow(rows)
    
    # store the number of rows in the dataframe docs_in_each_system
    docs_in_each_system[j-400, i] <- num_rows
  }
}