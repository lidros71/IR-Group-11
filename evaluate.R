evaluate <- function(operation, depth, result_name, average_result_name) {
  
  # Create an empty table with 15 columns and 51 rows
  if (depth > 85) {
    result_table <- data.frame(matrix(ncol = 15, nrow = 50))
    colnames(result_table) <- paste0("system", 1:15)
  } else {
    result_table <- data.frame(matrix(ncol = 15, nrow = 51))
    colnames(result_table) <- paste0("system", 1:15)
  }
  
  # Create an empty table to store average
  average_result_table <- data.frame(matrix(ncol = 15, nrow = 1))
  
  # IF OPERATION MAP
  if (operation == 'MAP') {
    print(paste("MAP"))
    
    for (i in 1:15) {
      current_table <- table_list[[i]]  # Get the current system
      # print(paste("system: ", i))
      
      totalAveragePrecision <- 0
      
      #to be subtracted by j to put at the right row
      a = 400
      
      # Iterate over topic '401' to '450'
      for (j in 401:450) {
        
        # print(paste("system: ", i))
        # print(paste("topic: ", j-400))
        
        if (depth > 85 && j == 403) {
          a = a + 1
          # print(paste("skipped: ", j))
          next  # Skip this iteration and move to the next one
        }
        
        # Filter rows for the current value
        rows <- current_table[current_table[, 1] == j, ]
        
        # Select the top 10 docs of the topics in the system
        top_rows <- rows[1:depth, ]
        
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
            # print(paste("relDocCount: ", relDocCount))
            # print(paste("k: ", k))
          }
        }
        
        # STORING IN THE TABLE
        if(relDocCount != 0){
          averagePrecision <- totalPrecision / relDocCount
          # print(paste("averageprecision: ", averagePrecision))
          # print(paste("p10 at row:", j-400, ", column:", i))
          result_table[(j-a),i] <- averagePrecision
          
          # FOR LINE 51 SOON
          totalAveragePrecision <- totalAveragePrecision + averagePrecision
          
        } else {
          averagePrecision <- 0
          # print(paste("p10 at row:", j-400, ", column:", i))
          result_table[(j-a),i] <- averagePrecision
        }
      }
      
      #store result
      # print(paste("total topic: ", (nrow(result_table)-1)))
      average_result_table[1, i] <- totalAveragePrecision / (nrow(result_table)-1)
      result_table[(nrow(result_table)),i] <- average_result_table[1, i]
    }
  } 
  # IF OPERATION P10
  else {
    for (i in 1:15) {
      current_table <- table_list[[i]]  # Get the current system
      
      
      totalPrecision <- 0 # UNTUK KIRA AVERAGE PRECISION OF THE SYSTEM
      
      #to be subtracted by j to put at the right row
      a = 400
      
      # Iterate over topic '401' to '450'
      for (j in 401:450) {
        # print(paste("system: ", i))
        # print(paste("topic: ", j-400))
        
        if (depth > 85 && j == 403) {
          a = a + 1
          # print(paste("skipped: ", j))
          next  # Skip this iteration and move to the next one
        }
        
        # Filter rows for the current value
        rows <- current_table[current_table[, 1] == j, ]
        
        # Select the top depth docs of the topics in the system
        top_rows <- rows[1:depth, ]
        
        relDocCount <-0 #to count the number of relevant doc in a topic, in current system iteration
        precision <- 0
        
        # Iterate through each row of top document
        for (k in 1:nrow(top_rows)) {
          # k = number of document iterated for the current topic in current system
          row <- top_rows[k, ]
          
          # Get the ID of doc
          doc_ID <- row[[3]]
          
          # checking whether doc is relevant or not
          if (any(clean_qrels$C1 == j & clean_qrels$C3 == doc_ID)) {
            
            relDocCount <- relDocCount + 1
            # print(paste("relDocCount: ", relDocCount))
            # print(paste("k: ", k))
            
          }
          
          precision <- relDocCount / k
        }
        
        #STORING IN THE TABLE
        result_table[(j-a),i] <- precision
        
        #FOR LINE 51
        totalPrecision = totalPrecision + precision
      }
      
      #put the average at line 51 for each system
      # print(paste("total topic: ", (nrow(result_table)-1)))
      average_result_table[1, i] <- totalPrecision / (nrow(result_table)-1)
      result_table[(nrow(result_table)),i] <- average_result_table[1, i]
    }
  }
  
  # Rename the result_table dataframe with the provided name
  if (!missing(result_name) && is.character(result_name)) {
    names(result_table) <- result_name
  }
  
  # Save the resulting dataframe with the provided name in the global environment
  if (!missing(result_name) && is.character(result_name)) {
    assign(result_name, result_table, envir = .GlobalEnv)
  }
  
  # Rename the average_result_table dataframe with the provided name
  if (!missing(average_result_name) && is.character(average_result_name)) {
    names(average_result_table) <- average_result_name
  }
  
  # Save the average_result_table dataframe with the provided name in the global environment
  if (!missing(average_result_name) && is.character(average_result_name)) {
    assign(average_result_name, average_result_table, envir = .GlobalEnv)
  }
}