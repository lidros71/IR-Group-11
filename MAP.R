# Create an empty table with 15 columns and 1 rows
p100 <- data.frame(matrix(ncol = 15, nrow = 51))
mean_average_precision <- data.frame(matrix(ncol = 15, nrow = 1))

# Take all the average_precision of the systems
for (i in 1:15) {
  current_table <- table_list[[i]]  # Get the current system
  
  # define totalAveragePrecision, used in calc MAP
  totalAveragePrecision <- 0
  
  # Iterate over topic '401' to '450'
  for (j in 401:450) {
    # Filter rows for the current value
    rows <- current_table[current_table[, 1] == j, ]
    
    # Select the top 80 docs of the topics in the system
    top_rows <- rows[1:80, ]
    
    relDocCount <- 0 #to count the number of relevant doc in a topic, in current system iteration
    totalPrecision <- 0 #used to calc average precision
    
    # Iterate through each row of top document
    for (k in 1:nrow(top_rows)) {
      # k = number of document iterated for the current topic in current system
      row <- top_rows[k, ]
      
      # Get the ID of doc
      doc_ID <- row[[3]]
      
      # checking whether doc is relevant or not
      if (any(clean_qrels$C1 == j & clean_qrels$C3 == doc_ID)) {
        
        relDocCount <- relDocCount + 1
        precision <- relDocCount / k
        totalPrecision <- totalPrecision + precision
      }
    }
    
    # STORING IN THE TABLE
    if(relDocCount != 0){
      averagePrecision <- totalPrecision / relDocCount
      # print(paste("averageprecision: ", averagePrecision))
      # print(paste("p10 at row:", j-400, ", column:", i))
      p100[j-400,i] <- averagePrecision
      
      # FOR LINE 51 SOON
      totalAveragePrecision <- totalAveragePrecision + averagePrecision
      
    } else {
      averagePrecision <- 0
      # print(paste("p10 at row:", j-400, ", column:", i))
      p100[j-400,i] <- averagePrecision
    }
  }
  
  # put MAP at row 51
  p100[51, i] <- totalAveragePrecision/50
}