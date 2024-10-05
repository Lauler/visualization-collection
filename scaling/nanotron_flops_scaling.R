library(readr)
library(dplyr)
library(ggplot2)

# Read the markdown
md_object <- "| Nodes | TFLOPs | Tokens/s  per GPU | Tokens/s |
|------:|-------:|------------------:|---------:|
| 2     |    185 |              4020 |    32200 |
| 4     |    180 |              3900 |    62400 |
| 8     |    173 |              3760 |   120000 |
| 16    |    171 |              3710 |   238000 |
| 32    |    166 |              3610 |   462000 |
| 60    |    149 |              3230 |   775000 |"


df <- readr::read_delim(md_object, delim = "|") %>% 
  # Select column 2 to 5
  select(2:5) %>%
  # Remove row 1
  slice(-1) %>%
  # Remove whitespace
  mutate_all(trimws) %>%
  # Rename columns
  setNames(c("Nodes", "TFLOPs", "Tokens/s per GPU", "Tokens/s")) %>%
  # Convert to numeric
  mutate_all(as.numeric)

df <- df %>%
  mutate(total_flops = TFLOPs * Nodes * 4)


ggplot(df, aes(x = Nodes, y = total_flops)) +
  geom_point() +
  geom_line() +
  geom_abline(intercept = 0, slope = 740, linetype = "dashed") +
  # display ticks on the x axis scale at every measured Nodes value
  scale_x_continuous(breaks = df$Nodes) +
  labs(title = "Tokens/s vs Nodes",
       x = "Nodes",
       y = "Total TFLOPs") +
  theme_minimal(base_size = 12) +
  # Remove vertical grid lines
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())




p <- ggplot(df, aes(x = Nodes, y = `Tokens/s`)) +
  geom_point(aes(fill="Actual")) +
  geom_line() +
  geom_abline(aes(intercept = 0, slope = 16100, colour="Ideal based \n on scaling \n at 2 nodes"), linetype = "dashed", show.legend = TRUE) +
  # display ticks on the x axis scale at every measured Nodes value
  scale_x_continuous(breaks = df$Nodes) +
  labs(title = "Total tokens/s processed by node count (Nanotron: Llama2 7B)",
       x = "Nodes",
       y = "Tokens/s",
       colour = "",
       fill="") +
  theme_minimal(base_size = 17) +
  # make the y labels not in scientific notation
  scale_y_continuous(labels = scales::comma) +
  # Make geom_point legend a line that is not dashed
  guides(fill = guide_legend(override.aes = list(linetype = 1))) +
  # Change color of the dashed line
  scale_color_manual(values = c("grey20", "black")) +
  # Remove vertical grid lines
  # Add black outline around the entire plot background
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(color = "black", size = 0.5))


# Save the plot
ggsave("scaling/plots/token_scaling.png", p, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
