p10_avg <- as.numeric(p10[51, ])
p100_avg <- as.numeric(p100[51,])

correlation <- cor(p10_avg, p100_avg)
print(paste("correlation coefficient: ", correlation))

# Assuming your vectors are named vector1 and vector2
correlation_test <- cor.test(p10_avg, p100_avg)

# Extract the p-value from the correlation test result
p_value <- correlation_test$p.value