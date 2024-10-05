library(ggplot2)
library(dplyr)
library(tidyr)

num_updates = 37500
warmup = 20000
total_num_update = 950000
lr = 0.000375
end_learning_rate = 0.00002
power = 3.0

pct_remaining <- 1 - (num_updates - warmup) / (total_num_update - warmup)
lr_range = lr - end_learning_rate
lr = lr_range * pct_remaining ** power + end_learning_rate
lr

lr_schedule <- function(num_updates, warmup, total_num_update, lr, end_learning_rate, power) {
  # total_num_update is when LR reaches end_learning_rate, and is not the same as max_update
  if (warmup > 0 & num_updates < warmup){
    warmup_factor = num_updates / warmup
    lr = lr * warmup_factor
    return(lr)
  } else if (num_updates > total_num_update){
    return(end_learning_rate)
  } else {
    pct_remaining <- 1 - (num_updates - warmup) / (total_num_update - warmup)
    lr_range = lr - end_learning_rate
    lr = lr_range * pct_remaining ** power + end_learning_rate
    return(lr)
  }
}

# Generate range of num_updates
num_updates <- seq(0, 1000000, by = 1000)
learning_rates <- sapply(num_updates, function(x) lr_schedule(x, warmup=20000, total_num_update=950000, lr=0.0003, end_learning_rate=0.00002, power=3.0))
learning_rates_orig = sapply(num_updates, function(x) lr_schedule(x, warmup=32000, total_num_update=1000000, lr=0.00059137, end_learning_rate=0.0, power=1.0))

df <- data.frame(num_updates, learning_rates, learning_rates_orig)
# pivot_longer
df <- df %>% pivot_longer(cols = c(learning_rates, learning_rates_orig), names_to = "lr_type", values_to = "learning_rates")
# Rename the lr_types
df$lr_type <- ifelse(df$lr_type == "learning_rates", "Conformer new", "Original")

ggplot(df, aes(x=num_updates, y=learning_rates, color=lr_type)) +
  geom_line() +
  labs(title = "Polynomial Decay Learning Rate Schedule Conformer",
       x = "Number of Updates",
       y = "Learning Rate",
       subtitle = "Conformer new: Warmup: 20000, Total nr updates: 950000, LR: 0.000375, End LR: 0.00002, Power: 3.0 \nOriginal: Warmup: 32000, Total nr updates: 1000000, LR: 0.0005, End LR: 0.0, Power: 1.0",
       color = "") +
  theme_minimal()


#### Wav2vec2
num_updates <- seq(0, 680000, by = 1000)

learning_rates <- sapply(num_updates, function(x) lr_schedule(x, warmup=20000, total_num_update=650000, lr=0.0001, end_learning_rate=0.000005, power=2.5))
learning_rates_orig <- sapply(num_updates, function(x) lr_schedule(x, warmup=54000, total_num_update=650000, lr=0.00027, end_learning_rate=0.00001, power=2.0))

df <- data.frame(num_updates, learning_rates, learning_rates_orig)

# pivot_longer
df <- df %>% pivot_longer(cols = c(learning_rates, learning_rates_orig), names_to = "lr_type", values_to = "learning_rates")
# Rename the lr_types
df$lr_type <- ifelse(df$lr_type == "learning_rates", "Wav2vec2 new", "Original")

ggplot(df, aes(x=num_updates, y=learning_rates, color=lr_type)) +
  geom_line() +
  labs(title = "Polynomial Decay Learning Rate Schedule Wav2vec2",
       x = "Number of Updates",
       y = "Learning Rate",
       subtitle = "Wav2vec2 new: Warmup: 48000, Total nr updates: 650000, LR: 0.00031, End LR: 0.0, Power: 1.8 \nOriginal: Warmup: 48000, Total nr updates: 600000, LR: 0.005, End LR: 0.0, Power: 1.0",
       color = "") +
  theme_minimal()

