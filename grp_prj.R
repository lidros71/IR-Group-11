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
p10 <- data.frame(matrix(ncol = 15, nrow = 50))

# Set column names
colnames(p10) <- paste0("system", 1:15)

# Iterate over each table
for (i in 1:length(table_list)) {
  current_table <- table_list[[i]]  # Get the current table
  print("system")
  print(i)
  
  # Iterate over values '401' to '450'
  for (j in 401:402) {
    # Filter rows for the current value
    rows <- current_table[current_table[, 1] == j, ]
    
    # Select the top 10 rows
    top_rows <- rows[1:10, ]
    
    print("topic")
    print(j)
    # Iterate through each row of top_rows
    for (k in 1:nrow(top_rows)) {
      row <- top_rows[k, ]
    
    # Get the ID of doc
      doc_ID <- row[[3]]
      #print(doc_ID)
        
      # checking whether doc is relevant or not
      if (any(clean_qrels$C1 == j & clean_qrels$C3 == doc_ID)) {
        print(doc_ID)
        print("found")
      }
    }
  }
}