# Set the path to the text file
file_path <- "qrels.trec8.adhoc"

# Read the text file into R
mydata <- readLines(file_path)

# Split the text into columns
mydata <- strsplit(mydata, " ")

# Convert the list to a matrix
mydata <- matrix(unlist(mydata), ncol = 4, byrow = TRUE)

# Convert the matrix to a data frame
mydata <- as.data.frame(mydata)

# Name the columns of the data frame
colnames(mydata) <- c("C1", "C2", "C3", "C4")

clean_qrels <- subset(mydata, C4 != 0)

# write.csv(clean_qrels, file = "relevant_docs.csv")

#read the 15 input file
input_1 <- read.table("input.surfahi1")
input_2 <- read.table("input.surfahi2")
input_3 <- read.table("input.surffal2")
input_4 <- read.table("input.tno8d3")
input_5 <- read.table("input.tno8d4")
input_6 <- read.table("input.tno8t2")
input_7 <- read.table("input.UB99SW")
input_8 <- read.table("input.UB99T")
input_9 <- read.table("input.umd99a1")
input_10 <- read.table("input.unc8al32")
input_11 <- read.table("input.unc8al42")
input_12 <- read.table("input.unc8al52")
input_13 <- read.table("input.UniNET8Lg")
input_14 <- read.table("input.UniNET8St")
input_15 <- read.table("input.UT800")

# Assuming you have variables named input_1, input_2, ..., input_15

# Create a list of tables
table_list <- list(input_1, input_2, input_3, input_4, input_5, input_6, input_7, input_8, input_9, input_10, input_11, input_12, input_13, input_14, input_15)

# Create an empty table with 15 columns and 50 rows
p10 <- data.frame(matrix(ncol = 15, nrow = 51))

# Set column names
colnames(p10) <- paste0("system", 1:15)

total_average_precision <- 0

# Iterate over each systems
# length(table_list)
for (i in 1:15) {
  current_table <- table_list[[i]]  # Get the current system
  # print(paste("system: ", i))
  
  totalPrecision <- 0
  
  # Iterate over topic '401' to '450'
  for (j in 401:450) {
    # Filter rows for the current value
    rows <- current_table[current_table[, 1] == j, ]
    
    # Select the top 10 docs of the topics in the system
    top_rows <- rows[1:10, ]
    
    # print(paste("topic: ", j))
    
    relDocCount <-0 #to count the number of relevant doc in a topic, in current system iteration
    precision <- 0 #used to calc average precision
    
    # Iterate through each row of top document
    for (k in 1:nrow(top_rows)) {
      # k = number of document iterated for the current topic in current system
      row <- top_rows[k, ]
    
      # Get the ID of doc
      doc_ID <- row[[3]]
      #print(doc_ID)
      #print(paste("k: ", k))
      
      # checking whether doc is relevant or not
      if (any(clean_qrels$C1 == j & clean_qrels$C3 == doc_ID)) {
        #print(doc_ID)
        #print("found")
        
        relDocCount <- relDocCount + 1
        precision <- relDocCount / k
        #print(precision)
      }
    }
    p10[j-400,i] <- precision
    totalPrecision = totalPrecision + precision
  }
  
  #put the average at line 51 for each system
  p10[51,i] <- totalPrecision/50
}

print("Done")
