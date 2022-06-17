rosemberg_data <- readr::read_delim("rosenberg-data.csv", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)
rosemberg_data <- rosemberg_data[rowSums(is.na(rosemberg_data)) == 0,]
rosemberg_data <- rosemberg_data[rosemberg_data$country == 'US',]
rosemberg_data <- rosemberg_data[(rowSums(rosemberg_data[,1:10] == 0) == 0),]
nrow(rosemberg_data)
adolescents <- rosemberg_data[rosemberg_data$age >= 15 & rosemberg_data$age <= 18,]
nrow(adolescents)
table(adolescents$gender)

adolescents_data_m_us <- adolescents[adolescents$gender == 1,]
adolescents_data_f_us <- adolescents[adolescents$gender == 2,]
nrow(adolescents_data_m_us)
nrow(adolescents_data_f_us)
