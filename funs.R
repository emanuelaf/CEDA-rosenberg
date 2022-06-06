#### Functions

# Conditional Entropy
# Input: a contingency table with response on the columns and independent variable on the rows
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
# Mutual Conditional Entropy
###########
# takes two entropies and calculates their mean

mce <- function(ce1, ce2) {
  mce_value <- (ce1+ce2)/2
  return(mce_value)
}

################
# Confidence intervals for conditional entropy measures
# Input: 
# 1) x: a contingency table with response on the columns and independent variable on the rows
# 2) M: number of iterations. Default is 1000
# entropy_measure: the entropy measure to be calculated, only accepts "ce_rescaled",
# 3) "ce","marginal_entropy","rowwise_ce". Default is ce_rescaled.
# 4) conf_level: confidence level. Default is 95%.
# Returns:
# confidence interval 


ci <- function(x, M=1000, entropy_measure="ce_rescaled", conf_level = 0.95) {
  if (!(entropy_measure %in% c("ce_rescaled","ce","marginal_entropy","rowwise_ce")))
  stop("A measure of entropy must be specified")
  n <- sum(x)
  mimicked_matrix <- matrix(NA, ncol = ncol(x), nrow =nrow(x))
  list_mimicked_matrix <- replicate(M, mimicked_matrix, simplify=F)
  ce_distribution <- numeric(M)
  ce_distribution_rows <- matrix(NA, ncol = M, nrow =nrow(x))
  for (r in 1:M) {
    for (i in 1:nrow(x)) {
      v_prob = x[i,]/sum(x[i,])
      n = sum(x[i,])
      list_mimicked_matrix[[r]][i,] <- rmultinom(1, n, prob = v_prob)
    }
    ce_distribution[r] <- 
      (entropy_measure=="ce_rescaled")*conditional_entropy(list_mimicked_matrix[[r]])[[1]]+
      (entropy_measure=="ce")*conditional_entropy(list_mimicked_matrix[[r]])[[2]]+
      (entropy_measure=="marginal_entropy")*conditional_entropy(list_mimicked_matrix[[r]])[[3]]
    ce_distribution_rows[,r]  <- (entropy_measure=="rowwise_ce")*conditional_entropy(list_mimicked_matrix[[r]])[[4]]
  }
  if (entropy_measure != "rowwise_ce") {
    ci_l <- quantile(ce_distribution, prob = (1-conf_level)/2)
    ci_u <- quantile(ce_distribution, prob = conf_level+(1-conf_level)/2)
    return(c(ci_l, ci_u))}
  else {
    ci_l <- apply(ce_distribution_rows,1, quantile, prob = (1-conf_level)/2)
    ci_u <- apply(ce_distribution_rows,1, quantile, prob = conf_level+(1-conf_level)/2)
    return(return(cbind(ci_l, ci_u)))
  }
}

ci(x1, entropy_measure="rowwise_ce")

#######################################
#### Builds ordering for displaying 
#### entropy given two predictors 
require(ggplot2)
require(rlang)

# takes two vectors and a parameter to explain
# the type of reordering (if for visualizing interactions or not):
# 1) independent variable 1
# 2) independent variable 2
# 3) interaction: logical, T if re-ordering for displaying interaction is seeked
# returns:
# 1) a character vector containing the re-ordered levels

sort_interaction <- function(x1, x2, interaction = F) {
  if (interaction == T) {
    x1x2 <- paste(x1, x2, sep = "_")
    levels_for_interaction <- levels(factor(x1x2))
    return(levels_for_interaction)
    } else {
    total <- x1 + x2
    ordered_x1_x2 <- data.frame(total = total,
                              x1x2 = factor(x1x2),
                              x1 = x1, x2 = x2)
    partial_order <- unique(ordered_x1_x2)[order(unique(ordered_x1_x2)$total,
                              unique(ordered_x1_x2)$x1,
                              unique(ordered_x1_x2)$x2,
                              decreasing = F),]$x1x2
    ordered_x1_x2$x1x2 <- factor(ordered_x1_x2$x1x2,
                               levels = partial_order)
    levels_no_interaction <- levels(ordered_x1_x2$x1x2)
    return(levels_no_interaction)
  }
}

sort_interaction(x1, x2, T)

#interaction_plots <- function(df, x, y, levels, ...) {
#  return(ggplot(data = df)+
#           geom_point(
#             aes(x=factor({{ x }}, levels = levels), 
#                 y = {{ y }}, ...)))
# }

#interaction_plots(df_conditional_entropies_order2_rowwise, 
#                  x = category, y = value, 
#                  col = sign,
#                  sort_interaction(x1, x2, T)) 
