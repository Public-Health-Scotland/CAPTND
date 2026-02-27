######################################################
## Function to plot count for each error report tab ##
######################################################

#Author: Luke Taylor
#Date: 2025-02-25

plot_error_report_data <- function(df, x_name, plot_title){

df %>%
  ggplot(aes(x = {{x_name}}, y = count, colour = dataset_type, group = dataset_type)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 0.8, na.rm = TRUE) +
  facet_wrap(~ hb_name, scales = "free_y") +
  scale_colour_manual(name = "Dataset type", values = c("#005EB8", "#76B900")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = plot_title,
       x = "Count", y = "Data month") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}
