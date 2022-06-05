#### Functions

# Conditional Entropy
# Input: a contingency table
# Returns:
# 1) rescaled conditional entropy 
# 2) non rescaled conditional entropy 
# 3) marginal entropy, 
# 4) row-wise conditional entropy, 
# for example 
# x <- table(adolescents_data_f$Q1, adolescents_data_f$Q2)

conditional_entropy <- function(x) {
  # H(Y|X=x)
  h_i <- numeric(nrow(x))
  for (i in 1:nrow(x)) {
    h_i[i] <- -sum(
      ifelse(is.nan(x[i,]/sum(x[i,])*log(x[i,]/sum(x[i,]))), 0, 
             x[i,]/sum(x[i,])*log(x[i,]/sum(x[i,])))
    )
  }
  p_i <- rowSums(x)/sum(x) 
  # H(Y|X)
  ce <- sum(h_i*p_i)
  # H(Y)
  marginal_entropy <- -sum(colSums(x)/sum(x)*log(colSums(x)/sum(x)))
  # H(Y|X)/H(Y)
  ce_rescaled <- ce/(marginal_entropy)
  return(list(ce_rescaled, ce, marginal_entropy, h_i))
}

###########
# MCE
###
# takes two entropies and calculates their mean

mce <- function(ce1, ce2) {
  mce_value <- (ce1+ce2)/2
  return(mce_value)
}