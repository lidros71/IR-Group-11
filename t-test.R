# Initialize variables
#Getting number of row and column
precision10_pvalues <- matrix(nrow = length(p10), ncol = length(p10))
map_pvalues <- matrix(nrow = length(p100), ncol = length(p100))
row.names(precision10_pvalues) <- paste0("system ", 1:15)
row.names(map_pvalues) <- paste0("system ", 1:15)


# Perform significance testing using paired t-tests for Precision@10 scores
for (i in 1:length(p10)) {
  for (j in 1:length(p10)) {
    if (i != j) {
      scores1 <- p10[, i]
      scores2 <- p10[, j]
      result <- t.test(scores1, scores2, paired = TRUE)
      precision10_pvalues[i, j] <- result$p.value
    }
  }
}

# Perform significance testing using paired t-tests for MAP scores
for (i in 1:length(p100)) {
  for (j in 1:length(p100)) {
    if (i != j) {
      scores1 <- p100[, i]
      scores2 <- p100[, j]
      result <- t.test(scores1, scores2, paired = TRUE)
      map_pvalues[i, j] <- result$p.value
    }
  }
}

# Display the results
cat('\n----------Significance Testing Results----------\n')
cat('Precision@10 p-values:\n')
for (i in 1:(length(p10) - 1)) {
  for (j in (i + 1):length(p10)) {
    scores1 <- p10[, i]
    scores2 <- p10[, j]
    result <- t.test(scores1, scores2, paired = TRUE)
    p_value <- result$p.value
    cat(paste(row.names(precision10_pvalues)[i], "vs", row.names(precision10_pvalues)[j], ": p-value =", sprintf("%.4f", p_value), ",", ifelse(p_value < 0.05, "Significant", "Not significant"), "\n"))
  }
}

cat('\nMAP p-values:\n')
for (i in 1:(length(p100) - 1)) {
  for (j in (i + 1):length(p100)) {
    scores1 <- p100[, i]
    scores2 <- p100[, j]
    result <- t.test(scores1, scores2, paired = TRUE)
    p_value <- result$p.value
    cat(paste(row.names(map_pvalues)[i], "vs", row.names(map_pvalues)[j], ": p-value =", sprintf("%.4f", p_value), ",", ifelse(p_value < 0.05, "Significant", "Not significant"), "\n"))
  }
}