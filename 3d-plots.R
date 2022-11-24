# library
library(plotly)
df_for_3d_plot_tmp <- df_conditional_entropies_order2_rowwise %>%
  filter(variable == 'Q9_Q10')

df_for_3d_plot <- data.frame(do.call(rbind, stringr::str_split(df_for_3d_plot_tmp$category, '_')),
                             do.call(rbind, stringr::str_split(df_for_3d_plot_tmp$variable, "_")),
                             df_for_3d_plot_tmp$value)
colnames(df_for_3d_plot) <- c(df_for_3d_plot[1,3],
                                     df_for_3d_plot[1,4],
                                     'v1', 'v2', 'CE')

fig <- plot_ly(df_for_3d_plot, x = ~Q9, y = ~Q10, z = ~CE) 
fig <- fig %>%
  add_trace(marker = list(color = "rgba(255, 0, 0, 0.6)"))
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Q9'),
                                   yaxis = list(title = 'Q10'),
                                   zaxis = list(title = 'Conditional Entropy')))

fig
#####
df_for_3d_plot_tmp <- df_conditional_entropies_order2_rowwise %>%
  filter(variable == 'Q7_Q9')

df_for_3d_plot <- data.frame(do.call(rbind, stringr::str_split(df_for_3d_plot_tmp$category, '_')),
                             do.call(rbind, stringr::str_split(df_for_3d_plot_tmp$variable, "_")),
                             df_for_3d_plot_tmp$value)
colnames(df_for_3d_plot) <- c(df_for_3d_plot[1,3],
                              df_for_3d_plot[1,4],
                              'v1', 'v2', 'CE')

fig <- plot_ly(df_for_3d_plot, x = ~Q7, y = ~Q9, z = ~CE)
fig <- fig %>%
  add_trace(marker = list(color = "rgba(0, 0.4, 0.2, 0.6)"))
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Q7'),
                                   yaxis = list(title = 'Q9'),
                                   zaxis = list(title = 'Conditional Entropy')))

fig
#####
df_for_3d_plot_tmp <- df_conditional_entropies_order2_rowwise %>%
  filter(variable == 'Q4_Q7')

df_for_3d_plot <- data.frame(do.call(rbind, stringr::str_split(df_for_3d_plot_tmp$category, '_')),
                             do.call(rbind, stringr::str_split(df_for_3d_plot_tmp$variable, "_")),
                             df_for_3d_plot_tmp$value)
colnames(df_for_3d_plot) <- c(df_for_3d_plot[1,3],
                              df_for_3d_plot[1,4],
                              'v1', 'v2', 'CE')

fig <- plot_ly(df_for_3d_plot, x = ~Q4, y = ~Q7, z = ~CE)
fig <- fig %>%
  add_trace(marker = list(color = "rgba(0, 0.2, 0.8, 0.6)"))
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Q4'),
                                   yaxis = list(title = 'Q7'),
                                   zaxis = list(title = 'Conditional Entropy')))

fig
