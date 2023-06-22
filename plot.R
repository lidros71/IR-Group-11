# Assuming you have two vectors or columns named `P_10` and `MAP1000` in your dataframe
# Create a scatter plot
plot(P_10, MAP1000, main = "Scatter Plot of P@10 vs MAP1000",
     xlab = "P@10", ylab = "MAP1000")

# Add a trendline or regression line
abline(lm(MAP1000 ~ P_10), col = "red")

# Add correlation coefficient as text
cor_coef <- cor(P_10, MAP1000)
text(x = max(P_10), y = max(MAP1000), labels = paste0("Correlation: ", round(cor_coef, 2)),
     pos = 3)
