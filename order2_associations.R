### Order 2
source('funs.R')
source('rosenberg-data-manipulation.R')
#source('order1.R')

# order 2 conditional entropy Q1|Q2,Q3, Q1|Q2,Q4 etc
# Males
n_m = nrow(adolescents_data_m_us)
adolescents_data_m_us_order2 <- data.frame(n = numeric(n_m))
questions <- c('Q1','Q2', 'Q3', 'Q4', 'Q5', 'Q6', 'Q7', 'Q8', 'Q9', 'Q10')
response <- 'Q1'
response_number <- which(questions == response)
independent <- questions[!(questions %in% response)]
independent_number <- which(questions %in% independent)

for (i in 1:8) {
  for (j in (i+1):9) {
    var_tmp <- paste(adolescents_data_m_us[[independent_number[i]]],
                     adolescents_data_m_us[[independent_number[j]]], 
                     sep ='_')
    adolescents_data_m_us_order2 <- data.frame(adolescents_data_m_us_order2, var_tmp)
    colnames(adolescents_data_m_us_order2)[colnames(adolescents_data_m_us_order2) == 'var_tmp'] <- 
      paste(paste0('Q', independent_number[i]), paste0('Q', independent_number[j]), sep = "_")
  }
}

adolescents_data_m_us_order2 <- adolescents_data_m_us_order2[,-1]

# Row-wise conditional entropies
conditional_entropies_order2_rowwise <-matrix(ncol = 16, nrow = ncol(adolescents_data_m_us_order2))
for (q in 1:ncol(adolescents_data_m_us_order2)) {
  adolescents_data_m_us_order2[[q]] <- factor(adolescents_data_m_us_order2[[q]],
                                              levels = apply(expand.grid(1:4, 1:4), 1, paste, collapse="_"))
  x <- table(adolescents_data_m_us_order2[[q]],adolescents_data_m_us[[response_number]],deparse.level = 2)
  conditional_entropies_order2_rowwise[q,] <- conditional_entropy(x)[[4]]/conditional_entropy(x)[[3]]
}

df_conditional_entropies_order2_rowwise <-
  data.frame(value = conditional_entropies_order2_rowwise[1,], 
             category = apply(expand.grid(1:4, 1:4), 1, paste, collapse="_"),
             variable = colnames(adolescents_data_m_us_order2)[1])

for (q in 2:36) {
  df_conditional_entropies_order2_rowwise <- 
    rbind(df_conditional_entropies_order2_rowwise,
      data.frame(value = conditional_entropies_order2_rowwise[q,], 
      category = c('1_1', '1_2', '1_3', '1_4', '2_1', '2_2', '2_3', '2_4',
                '3_1', '3_2', '3_3', '3_4', '4_1', '4_2', '4_3', '4_4'),
      variable = colnames(adolescents_data_m_us_order2)[q]))
}

var1_sign = ifelse(stringr::str_detect(df_conditional_entropies_order2_rowwise$variable, 
                                       '[2467]'),'pos', 'neg')
var1_sign = ifelse(stringr::str_detect(df_conditional_entropies_order2_rowwise$variable, 
                                       '^Q1_'),'pos', var1_sign)
var2_sign = ifelse(stringr::str_detect(df_conditional_entropies_order2_rowwise$variable, 
                                       '[35890]'),'neg', 'pos')

df_conditional_entropies_order2_rowwise$sign <- factor(paste(var1_sign, var2_sign))

df_conditional_entropies_order2_rowwise$category <- factor(
  df_conditional_entropies_order2_rowwise$category,
  levels = sort_interaction(adolescents_data_m_us$Q1, adolescents_data_m_us$Q2, interaction = F))

require(ggplot2)
ggplot(data = df_conditional_entropies_order2_rowwise)+
  geom_point(aes(x=category, y = value, col = sign))+
  facet_wrap(vars(factor(variable)), ncol = 4)+
  labs(y=paste0('H(Q', response_number, '|Qq=i)'), 
       x = paste('Category'), title = 'Male adolescents') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

# Interactions

df_conditional_entropies_order2_rowwise$category <- factor(
  df_conditional_entropies_order2_rowwise$category,
  levels = sort_interaction(adolescents_data_m_us$Q1, adolescents_data_m_us$Q2, interaction = T))

require(ggplot2)
p1 <- ggplot(data = df_conditional_entropies_order2_rowwise[df_conditional_entropies_order2_rowwise$sign == 'pos neg',])+
  geom_point(aes(x=category, y = value, col = sign))+
  scale_color_manual(values=c('#00BA38'))+
  facet_wrap(vars(factor(variable)), ncol = 4)+
  labs(y=paste0('H(Q', response_number, '|Qq=i)'), 
       x = paste('Category'), title = 'Male adolescents') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

p2 <- ggplot(data = df_conditional_entropies_order2_rowwise[df_conditional_entropies_order2_rowwise$sign != 'pos neg',])+
  geom_point(aes(x=category, y = value, col = sign))+
  facet_wrap(vars(factor(variable)), ncol = 4)+
  labs(y=paste0('H(Q', response_number, '|Qq=i)'), 
       x = paste('Category'), title = '') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

# save image as 700x1000
gridExtra::grid.arrange(p1, p2, nrow=2)

########

conditional_entropies_order2 <- data.frame(m_CE = numeric(n_order2_factors), f_CE = numeric(n_order2_factors))

for (q in 1:ncol(adolescents_data_m_us_order2)) {
  x <- table(adolescents_data_m_us_order2[[q]],adolescents_data_m_us$Q1,deparse.level = 2)
  conditional_entropies_order2$m_CE[q] <- conditional_entropy(x)[[1]]
}

for (q in 1:ncol(adolescents_data_f_us_order2)) {
  x <- table(adolescents_data_f_us_order2[[q]],adolescents_data_f_us$Q1, deparse.level = 2)
  conditional_entropies_order2$f_CE[q] <- conditional_entropy(x)[[1]]
}

conditional_entropies_order2

# C.I. males
mimicked_matrix_order2 <- matrix(NA, ncol = 4, nrow = 16)
list_mimicked_matrix_m_order2 <- replicate(1000, mimicked_matrix_order2, simplify=F)
conditional_entropies_distribution_m_order2 <- matrix(ncol=1000, nrow=36)

for (q in 1:ncol(adolescents_data_m_us_order2)) {
  # using as character to avoid blank rows inserted for the purpose of calculating
  # rowise conditional entropies
  x <- table(as.character(adolescents_data_m_us_order2[[q]]), adolescents_data_m_us[[1]], 
             deparse.level = 2)
  for (r in 1:1000) {
    for (i in 1:nrow(x)) {
      v_prob = x[i,]/sum(x[i,])
      n = sum(x[i,])
      list_mimicked_matrix_m_order2[[r]][i,] <- rmultinom(1, n, prob = v_prob)
    }
    conditional_entropies_distribution_m_order2[q, r] <- conditional_entropy(list_mimicked_matrix_m_order2[[r]])[[1]]
  }
}

ci_m_0.025_order2 <- apply(conditional_entropies_distribution_m_order2, 1, quantile, 0.025)
ci_m_0.975_order2 <- apply(conditional_entropies_distribution_m_order2, 1, quantile, 0.975)

# C.I. females
mimicked_matrix_order2 <- matrix(NA, ncol = 4, nrow = 16)
list_mimicked_matrix_f_order2 <- replicate(1000, mimicked_matrix_order2, simplify=F)
conditional_entropies_distribution_f_order2 <- matrix(ncol=1000, nrow=36)

for (q in 1:ncol(adolescents_data_f_us_order2)) {
  x <- table(as.character(adolescents_data_f_us_order2[[q]]), 
             adolescents_data_f_us[[1]],deparse.level = 2)
  for (r in 1:1000) {
    for (i in 1:nrow(x)) {
      v_prob = x[i,]/sum(x[i,])
      n = sum(x[i,])
      list_mimicked_matrix_f_order2[[r]][i,] <- rmultinom(1, n, prob = v_prob)
    }
    conditional_entropies_distribution_f_order2[q, r] <- conditional_entropy(list_mimicked_matrix_f_order2[[r]])[[1]]
  }
}

ci_f_0.025_order2 <- apply(conditional_entropies_distribution_f_order2, 1, quantile, 0.025)
ci_f_0.975_order2 <- apply(conditional_entropies_distribution_f_order2, 1, quantile, 0.975)

conditional_entropies_order2_with_ci <- cbind(conditional_entropies_order2, 
                                              ci_m_0.025 = ci_m_0.025_order2, 
                                              ci_m_0.975 = ci_m_0.975_order2,
                                              ci_f_0.025 = ci_f_0.025_order2, 
                                              ci_f_0.975 = ci_f_0.975_order2)

#### Build variable Q_1 x Q_2
# Males
q1_q2_m <- paste(adolescents_data_m_us[[1]],
                 adolescents_data_m_us[[2]], 
                 sep ='_')

conditional_entropies_1_2_given_1 <- data.frame(var = numeric(8),
                                                ce_m = numeric(8),
                                                ce_f = numeric(8))

conditional_entropies_1_2 <- data.frame(var = numeric(18),
                                        ce_m = numeric(18),
                                        ce_f = numeric(18))

for (q in 3:10) {
  x <- table(adolescents_data_m_us[[q]], q1_q2_m)
  conditional_entropies_1_2_given_1[q-2,1] <- colnames(adolescents_data_m_us)[q]
  conditional_entropies_1_2_given_1[q-2,2] <- round(conditional_entropy(x)[[1]], 3)
}

for (q in 9:36) {
  x <- table(adolescents_data_m_us_order2[[q]], q1_q2_m)
  conditional_entropies_1_2[q-8,1] <- colnames(adolescents_data_m_us_order2)[q]
  conditional_entropies_1_2[q-8,2] <- round(conditional_entropy(x)[[1]], 3)
}

# Females
q1_q2_f <- paste(adolescents_data_f_us[[1]],
               adolescents_data_f_us[[2]], 
               sep ='_')

for (q in 3:10) {
  x <- table(adolescents_data_f_us[[q]], q1_q2_f)
  conditional_entropies_1_2_given_1[q-2,3] <- round(conditional_entropy(x)[[1]], 3)
}

for (q in 9:36) {
  x <- table(adolescents_data_f_us_order2[[q]], q1_q2_f)
  conditional_entropies_1_2[q-8,3] <- round(conditional_entropy(x)[[1]], 3)
}
