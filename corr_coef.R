calc_cor_coef <- function(value1, value2){
  
  #print(paste(value1, "&", value2))
  
  value1 <- as.numeric(value1[1,])
  value2 <- as.numeric(value2[1,])
  # `P@1`
  # MAP1000
  
  correlation <- cor(value1, value2)
  
  print(paste("correlation coefficient: ", correlation))
  
  correlation_test <- cor.test(value1, value2)
  
  # Extract the p-value from the correlation test result
  p_value <- correlation_test$p.value
  
  print(paste("p-value: ", p_value))
  
}