rm(list=ls())
source('funs.R')
source('rosenberg-data-manipulation.R')

rosemberg_data_m_us <- 
  rosemberg_data[rosemberg_data$gender == 1 & rosemberg_data$country == 'US',]

rosemberg_data_f_us <- 
  rosemberg_data[rosemberg_data$gender == 2 & rosemberg_data$country == 'US',]

# Conditional entropy by row and by age group
# with confidence intervals
# Q1|....

questions <- c('Q1','Q2', 'Q3', 'Q4', 'Q5', 'Q6', 'Q7', 'Q8', 'Q9', 'Q10')
response <- 'Q9'
response_number <- which(questions == response)
independent <- questions[!(questions %in% response)]
independent_number <- which(questions %in% independent)
age_inf <- c(15, 19, 23, 31, 41, 51)
age_sup <- c(18, 22, 30, 40, 50, 60)
runs <- 1000

# Males
rowwise_ce_m <- data.frame(CE=numeric(), sex=character(), 
                           category=character(),
                           variable=character(), 
                           age_group=numeric())

for (i in 1:length(age_inf)) {
  # filter data by age group
  matrix_h_i_CE_m <- matrix(ncol=4)
  conf_int <- matrix(ncol = 2)
  rosemberg_data_m_us_tmp <- 
    rosemberg_data_m_us[rosemberg_data_m_us$age >= age_inf[i] & 
                          rosemberg_data_m_us$age <= age_sup[i],]
  
  # focus on covariate Qi=q
  for (q in independent_number) {
    # males
    x <- table(rosemberg_data_m_us_tmp[[q]], 
                rosemberg_data_m_us_tmp[[response_number]])
    matrix_h_i_CE_m <- rbind(matrix_h_i_CE_m,
                             conditional_entropy(x)[[4]]/conditional_entropy(x)[[3]])
    conf_int <- rbind(conf_int, 
                      ci(x, M=runs, entropy_measure="rowwise_ce", conf_level = 0.95))
  }
  matrix_h_i_CE_m <- matrix_h_i_CE_m[-1,]
  conf_int <- conf_int[-1,]

  rowwise_ce_m_tmp <- 
    data.frame(CE = stack(data.frame(t(matrix_h_i_CE_m[,1:ncol(matrix_h_i_CE_m)])))[,-2],
               conf_int,
               category = rep(1:4, 9),
               sex = rep('M', 36),
               variable = rep(independent,each=4),
               age_group = i)
  
  rowwise_ce_m <- rbind(rowwise_ce_m,
                        rowwise_ce_m_tmp)
}

# Females
rowwise_ce_f <- data.frame(CE=numeric(), sex=character(), 
                           category=character(),
                           variable=character(), 
                           age_group=numeric())

for (i in 1:length(age_inf)) {
  # filter data by age group
  matrix_h_i_CE_f <- matrix(ncol=4)
  conf_int <- matrix(ncol = 2)
  rosemberg_data_f_us_tmp <- 
    rosemberg_data_f_us[rosemberg_data_f_us$age >= age_inf[i] & 
                          rosemberg_data_f_us$age <= age_sup[i],]
  
  # focus on covariate Qi=q
  for (q in independent_number) {
    # males
    x <- table(rosemberg_data_f_us_tmp[[q]], 
               rosemberg_data_f_us_tmp[[response_number]])
    matrix_h_i_CE_f <- rbind(matrix_h_i_CE_f,
                             conditional_entropy(x)[[4]]/conditional_entropy(x)[[3]])
    conf_int <- rbind(conf_int, 
                      ci(x, M=runs, entropy_measure="rowwise_ce", conf_level = 0.95))
  }
  matrix_h_i_CE_f <- matrix_h_i_CE_f[-1,]
  conf_int <- conf_int[-1,]
  
  rowwise_ce_f_tmp <- 
    data.frame(CE = stack(data.frame(t(matrix_h_i_CE_f[,1:ncol(matrix_h_i_CE_f)])))[,-2],
               conf_int,
               category = rep(1:4, 9),
               sex = rep('F', 36),
               variable = rep(independent,each=4),
               age_group = i)
  
  rowwise_ce_f <- rbind(rowwise_ce_f,
                        rowwise_ce_f_tmp)
}

rowwise_ce_final <- rbind(rowwise_ce_f, rowwise_ce_m)
rowwise_ce_final <- dplyr::arrange(rowwise_ce_final, sex, variable, category, age_group)

require(ggplot2)

age_labels <- c('Age 15-18', 'Age 19-22', 'Age 23-30', 'Age 31-40', 'Age 41-50', 'Age 51-60')
plot_list <- list()
rowwise_ce_final$age_group <- factor(rowwise_ce_final$age_group, 
                                     levels = 1:6,
                                     labels = age_labels)

# One variable given one other, by age and sex.
for (q in independent_number) {
  p <- ggplot(data = rowwise_ce_final[rowwise_ce_final$variable == questions[q],])+
  geom_point(aes(x=category, y = CE, group = age_group, col=sex))+
  geom_errorbar(aes(x = category, ymin=ci_l, ymax=ci_u, col = sex), 
                width=.1) +
  facet_wrap(vars(age_group), ncol = 6)+
  labs(y=paste0('H(', response, '|Qi=q)'), 
       x = paste0(questions[q], ': Category'))+
    scale_color_discrete(name = "Sex", h = c(30, 140)) +
    theme_bw()
  png(paste0("rowwise_CE", response, "_given", q, ".png"),width = 1000, height = 200)
  plot(p)
  dev.off()
}

require(ggplot2)
ggplot(data = 
         rowwise_ce_final[rowwise_ce_final$age_group == 'Age 15-18',])+
    geom_point(aes(x=category, y = CE, group = age_group, col=sex))+
  geom_errorbar(aes(x = category, ymin=ci_l, ymax=ci_u, col = sex), 
                width=.1) +
  facet_wrap(vars(factor(variable, levels = independent)), ncol = 3)+
    labs(#y=paste0('H(', response, '|Qq=i)'), 
         y = paste0('H(', response, '|Qq=i)'),
         x = paste('Category')) +
    scale_color_discrete(name = "Sex", h = c(30, 140)) +
  theme_bw()


