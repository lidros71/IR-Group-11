p10_avg <- as.numeric(p10[51, ])
p100_avg <- as.numeric(p100[51,])

correlation <- cor(p10_avg, p100_avg)
print(paste("correlation coefficient: ", correlation))