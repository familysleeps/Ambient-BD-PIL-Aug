library(tidyverse)
library(lubridate)
library(viridis)
library(broom)

# Simulate data for 365 days of the year
set.seed(123)
n <- 365  # Number of days in a year

# Simulating data for 6 variables (glucose, lipids, BMI, blood pressure, ketones, LDL)
days <- tibble(
  day_of_year = 1:n,
  date = ymd("2023-01-01") + days(0:(n-1)),
  glucose = 90 + 10 * sin(2 * pi * (1:n - 172) / 365) + rnorm(n, 0, 5),  # Simulated with sinusoidal pattern
  lipids = 200 + 15 * cos(2 * pi * (1:n - 172) / 365) + rnorm(n, 0, 10),
  bmi = 25 + 2 * sin(2 * pi * (1:n - 172) / 365) + rnorm(n, 0, 1),
  blood_pressure = 120 + 10 * cos(2 * pi * (1:n - 172) / 365) + rnorm(n, 0, 5),
  ketones = 0.5 + 0.1 * sin(2 * pi * (1:n - 172) / 365) + rnorm(n, 0, 0.05),
  ldl = 100 + 10 * cos(2 * pi * (1:n - 172) / 365) + rnorm(n, 0, 7)
)

# Add the month column (factor) to the dataframe
days <- days %>%
  mutate(month = factor(month(date, label = TRUE, abbr = TRUE), levels = month.abb))

# Convert data to long format for plotting
days_long <- days %>%
  pivot_longer(cols = glucose:ldl, names_to = "variable", values_to = "value")

# Aggregate by month to get the mean value for each variable
monthly_means <- days_long %>%
  group_by(variable, month) %>%
  summarise(monthly_mean = mean(value, na.rm = TRUE), .groups = 'drop')


    # Get monthly averages of predictions for plotting
    new_data %>%
      group_by(month) %>%
      summarise(fitted = mean(predicted), .groups = "drop")
  })



# Now, create the plot with boxplots and cosinor curves
ggplot(days_long_clean, aes(x = month, y = value)) +
  geom_boxplot(aes(fill = variable, color = variable), alpha = 0.5, outlier.shape = NA, size = 0.6) +
  
  # Add the cosinor curves in grey
 # geom_line(data = cosinor_fits, 
    #        aes(x = month, y = fitted, group = variable),
           # color = "grey30", size = 1.2) +
  facet_wrap(~ variable, nrow = 2, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = viridis(6)) +
  scale_color_manual(values = viridis(6)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),  # Make facet titles (variable names) size 12 and bold
    axis.title.x = element_text(size = 12, face = "bold"),  # Increase x-axis label size and make it bold
    
    panel.grid.major.y = element_blank(),   # Remove horizontal major gridlines
    panel.grid.minor.y = element_blank(),    # Remove horizontal minor gridlines
    # panel.grid = element_blank(), # Remove all grid lines
     axis.text.x = element_text(angle = 0, hjust = 0.5)
    
  ) +
  labs(x = "", y = "Values")

# Save the plot to an SVG file with specified dimensions# Save the plot to an SVG file with specified dimensions
ggsave(
  "cosinor_boxplot_grid_square.svg",        # File name
  plot = last_plot(),                       # Save the last plot
  device = "svg",                           # Save as SVG format
  width = 10,                             # Total width for A4
  height = 5.8,                            # Total height to fit 2 rows
  units = "in",                             # Use inches for size
  dpi = 300                                 # High resolution (300 DPI)
)
