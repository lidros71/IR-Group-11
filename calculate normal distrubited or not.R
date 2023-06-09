# Create an empty logical matrix
normality_shapiro <- matrix(logical(), nrow = 1, ncol = ncol(p100))

# Perform Shapiro-Wilk test and store the results
# FALSE means it is not-normally distributed, TRUE for normal distribution
for (i in 1:ncol(p100)) {
  result <- shapiro.test(p100[, i])
  
  if (result$p.value < 0.05) {
    normality_shapiro[1, i] <- FALSE
  } else {
    normality_shapiro[1, i] <- TRUE
  }
  # normality_shapiro[2, i] <- result$p.value
}