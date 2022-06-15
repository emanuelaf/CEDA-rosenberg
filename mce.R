# MCE calculation and figures
# Age group: Late adolescents only.
# Response variable: Q1
# Independent variables: All the others, Q1|Q2, Q1|Q3, etc 
# With C.I.
# Females and Males separately

source('funs.R')
source('rosenberg-data-manipulation.R')

# Mutual Conditional Entropy, males and females separately
# Males
mce_matrix_m <- matrix(ncol=10, nrow=10)

for (q1 in 1:10) {
  #  for (q2 in (q1+1):10) {
  for (q2 in 1:10) {
    x1 <- table(adolescents_data_m_us[[q1]],
                adolescents_data_m_us[[q2]],
                deparse.level = 2)
    ce1 <- conditional_entropy(x1)[[1]]
    x2 <- table(adolescents_data_m_us[[q2]],
                adolescents_data_m_us[[q1]],
                deparse.level = 2)
    ce2 <- conditional_entropy(x2)[[1]]
    mce_matrix_m[q1, q2] <- mce(ce1, ce2)
  }
}

# Females
mce_matrix_f <- matrix(ncol=10, nrow=10)

for (q1 in 1:10) {
  #  for (q2 in (q1+1):10) {
  for (q2 in 1:10) {
    x1 <- table(adolescents_data_f_us[[q1]],
                adolescents_data_f_us[[q2]],
                deparse.level = 2)
    ce1 <- conditional_entropy(x1)[[1]]
    x2 <- table(adolescents_data_f_us[[q2]],
                adolescents_data_f_us[[q1]],
                deparse.level = 2)
    ce2 <- conditional_entropy(x2)[[1]]
    mce_matrix_f[q1, q2] <- mce(ce1, ce2)
  }
}

# MCE figures

library(qgraph)
qgraph(1-mce_matrix_m, shape='circle', posCol='darkblue', 
       negCol='darkred', layout='groups', threshold = 0.2)
qgraph(1-mce_matrix_f, shape='circle', posCol='darkblue', 
       negCol='darkred', layout='groups', threshold = 0.2)
heatmap_col <- colorRampPalette(c("red", "white"))(20)
heatmap(mce_matrix_m, col=heatmap_col, symm=TRUE)
heatmap(mce_matrix_f, col=heatmap_col, symm=TRUE)
