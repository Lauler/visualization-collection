library(dplyr)
library(arrow)
library(ggplot2)

my_theme <- theme(panel.grid.major.y = element_line(colour="grey60", size=0.3, linetype=2),
                  panel.grid.major.x = element_line(colour="grey60", size=0.1, linetype=1),
                  panel.grid.minor.x = element_blank(),
                  text=element_text(family="Palatino"),
                  plot.subtitle=element_text(size=5.5))

# Read the data parquet file
df <- arrow::read_parquet("data/rixvox-alignments.parquet")

# Group by speaker_id and sum the duration_segment for each speaker
df_party <- df %>%
  group_by(speaker_id) %>%
  summarise(total_duration = sum(duration_segment),
            speaker = first(name),
            party = first(party),
            role = first(role)) %>%
  arrange(desc(total_duration)) %>%
  filter(!is.na(speaker))

p <- df_party %>%
  head(21) %>%
  # Rename Liberalerna to Folkpartiet
  mutate(party = ifelse(party == "Liberalerna", "Folkpartiet", party)) %>%
  ggplot(aes(x = reorder(speaker, total_duration), y = total_duration, fill = party)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal(base_size=8) +
  scale_y_continuous(labels = scales::unit_format(unit = "h", scale = 1/3600)) +
  # Color the parties according to the official colors
  scale_fill_manual(values = c("Socialdemokraterna" = "#e02e3d", "Moderaterna" = "#7dbee1", 
                               "Centerpartiet" = "#31a532", "Vänsterpartiet" = "#911414", "Folkpartiet" = "#1e69aa")) +
  labs(title = "Speakers in parliament sorted by total duration of speech (1966-2002)",
       x = "Speaker",
       y = "Total duration (h)",
       fill = "Party")

ggsave("plots/speech_duration_all.png", p, width=1900, height=1000, units="px", bg="white")

# Same plot but with speakers that have never been a minister
# no "minister" in the string column role
p <- df_party %>%
  filter(!grepl("minister", role, ignore.case = TRUE)) %>%
  head(21) %>%
  mutate(party = ifelse(party == "Liberalerna", "Folkpartiet", party)) %>%
  ggplot(aes(x = reorder(speaker, total_duration), y = total_duration, fill = party)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal(base_size=8) +
  scale_y_continuous(labels = scales::unit_format(unit = "h", scale = 1/3600)) +
  # Color the parties according to the official colors
  scale_fill_manual(values = c("Socialdemokraterna" = "#e02e3d", "Moderaterna" = "#7dbee1", 
                               "Centerpartiet" = "#31a532", "Vänsterpartiet" = "#911414", "Folkpartiet" = "#1e69aa")) +
  labs(title = "Non-minister speakers in parliament sorted by total duration of speech (1966-2002)",
       x = "Speaker",
       y = "Total duration (h)",
       fill = "Party")

ggsave("plots/speech_duration_mop.png", p, width=1900, height=1000, units="px", bg="white")


# Calculate the speaking rate in words per minute for each speaker
df_rate <- df %>%
  mutate(n_words = stringr::str_count(text, "\\w+")) %>%
  group_by(speaker_id) %>%
  summarise(total_duration = sum(duration_segment),
            total_words = sum(n_words),
            name = first(name),
            party = first(party)) %>%
  mutate(words_per_minute = total_words / total_duration * 60) %>%
  filter(!is.na(speaker_id))

p <- df_rate %>%
  arrange(desc(words_per_minute)) %>%
  #minimum total duration of 30 minutes
  filter(total_duration > 2400) %>%
  # If Liberalierna is in party, change to Folkpartiet
  mutate(party = ifelse(grepl("Liberalerna", party), "Folkpartiet", party)) %>%
  head(21) %>%
  ggplot(aes(x = reorder(name, words_per_minute), y = words_per_minute, fill = party)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal(base_size=8) +
    scale_fill_manual(values = c("Socialdemokraterna" = "#e02e3d", "Moderaterna" = "#7dbee1", 
                                 "Centerpartiet" = "#31a532", "Vänsterpartiet" = "#911414", 
                                 "Folkpartiet" = "#1e69aa", "Ny demokrati" = "#ffc346",
                                 "Miljöpartiet" = "#82c882")) +
    labs(title = "Fastest speaking rate in parliament (1966-2002)",
         x = "Speaker",
         y = "Words per minute",
         fill = "Party"
    )

ggsave("plots/fastest_rate_speaker.png", p, width=1900, height=1000, units="px", bg="white")

# Slowest speaking rate in parliament
p <- df_rate %>%
  arrange(words_per_minute) %>%
  #minimum total duration of 40 minutes
  filter(total_duration > 3600) %>%
  # If Liberalierna is in party, change to Folkpartiet
  # If Högerpartiet is in party, change to Moderaterna
  mutate(party = ifelse(grepl("Liberalerna", party), "Folkpartiet", party),
         party = ifelse(grepl("Högerpartiet", party), "Moderaterna", party)) %>%
  head(21) %>%
  # Reverse the order from previous plot
  ggplot(aes(x = reorder(name, -words_per_minute), y = words_per_minute, fill = party)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal(base_size=8) +
  scale_fill_manual(values = c("Socialdemokraterna" = "#e02e3d", "Moderaterna" = "#7dbee1", 
                               "Centerpartiet" = "#31a532", "Vänsterpartiet" = "#911414", 
                               "Folkpartiet" = "#1e69aa", "Ny demokrati" = "#ffc346",
                               "Miljöpartiet" = "#82c882")) +
  labs(title = "Slowest speaking rate in parliament (1966-2002)",
       x = "Speaker",
       y = "Words per minute",
       fill = "Party"
  )

ggsave("plots/slowest_rate_speaker.png", p, width=1900, height=1000, units="px", bg="white")


df_rate %>%
  arrange(words_per_minute) %>%
  filter(total_duration > 3600) %>%
  print(n = 21)
  
# Speaking rate and bleu_score by year
df_rate_year <- df %>%
  rowwise() %>%
  # Dates is a list, we want to extract the first element
  mutate(date = as.Date(dates[[1]])) %>%
  # Remove rowwise
  ungroup() %>%
  mutate(year = lubridate::year(date),
   n_words = stringr::str_count(text, "\\w+")) %>%
  group_by(year) %>%
  summarise(words_per_minute = sum(n_words) / sum(duration_segment) * 60,
            bleu_score = mean(bleu_score, na.rm = TRUE)) 


p <- ggplot(df_rate_year, aes(x = year, y = words_per_minute)) +
  geom_line() +
  geom_point() +
  theme_minimal(base_size=8) +
  labs(title = "Speaking rate in parliament by year (1966-2002)",
       x = "Year",
       y = "Words per minute")

ggsave("plots/speaking_rate_year.png", p, width=1900, height=1000, units="px", bg="white")

# Same as above but grouped by year and party  
df_party_year <- df %>%
  mutate(party = ifelse(grepl("Liberalerna", party), "Folkpartiet", party),
         party = ifelse(grepl("Högerpartiet", party), "Moderaterna", party),
         party = ifelse(party == "Bondeförbundet", "Centerpartiet", party),
         party = ifelse(grepl("Centerpartiet, Bondeförbundet", party), "Centerpartiet", party)) %>%
  rowwise() %>%
  mutate(date = as.Date(dates[[1]])) %>%
  ungroup() %>%
  mutate(year = lubridate::year(date),
         n_words = stringr::str_count(text, "\\w+")) %>%
  group_by(year, party) %>%
  summarise(words_per_minute = sum(n_words) / sum(duration_segment) * 60,
            bleu_score = mean(bleu_score, na.rm = TRUE),
            party = first(party)) 
  

p <- df_party_year %>%
  # Filter all party that have a , in the name
  filter(!grepl(",", party)) %>%
  filter(!is.na(party)) %>%
  # Order party in legend by average speaking rate over all years
  ggplot(aes(x = year, y = words_per_minute, 
             color = forcats::fct_reorder(party, words_per_minute, mean, .desc=TRUE), 
             shape = forcats::fct_reorder(party, words_per_minute, mean, .desc=TRUE))) +
    geom_line() +
    # Add a stroke to the points
    # geom_point(size=2.1, color="black") + 
    geom_point(size=1.8) +
    theme_minimal(base_size=8) +
    scale_color_manual(values = c("Socialdemokraterna" = "#e02e3d", "Moderaterna" = "#7dbee1", 
                                 "Centerpartiet" = "#31a532", "Vänsterpartiet" = "#911414", 
                                 "Folkpartiet" = "#1e69aa", "Ny demokrati" = "#ffc346",
                                 "Miljöpartiet" = "#82c882", "Medborgerlig samling (1964–1968)" = "#808080",
                                 "Kristdemokraterna" = "#331d79")) +
  scale_shape_manual(values = c("Socialdemokraterna" = 15, "Moderaterna" = 16, 
                                "Centerpartiet" = 16, "Vänsterpartiet" = 15, 
                                "Folkpartiet" = 16, "Ny demokrati" = 17,
                                "Miljöpartiet" = 15, "Medborgerlig samling (1964–1968)" = 25,
                                "Kristdemokraterna" = 16)) +
    labs(title = "Speaking rate in parliament by year and party (1966-2002)",
         x = "Year",
         y = "Words per minute",
         color = "Party",
         shape = "Party") +
  my_theme

ggsave("plots/speaking_rate_party_year.png", p, width=1900, height=1000, units="px", bg="white")




# Speaking rate by gender
df_rate_gender <- df %>%
  rowwise() %>%
  mutate(date = as.Date(dates[[1]])) %>%
  ungroup() %>%
  mutate(year = lubridate::year(date),
         n_words = stringr::str_count(text, "\\w+")) %>%
  group_by(year, gender) %>%
  summarise(words_per_minute = sum(n_words) / sum(duration_segment) * 60,
            bleu_score = mean(bleu_score, na.rm = TRUE))


p <- df_rate_gender %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(x = year, y = words_per_minute, color = gender)) +
  geom_line() +
  geom_point() +
  theme_minimal(base_size=8) +
  scale_color_manual(values = c("man" = "steelblue", "woman" = "firebrick", "NA" = "grey")) +
  labs(title = "Speaking rate by gender and year (1966-2002)",
       x = "Year",
       y = "Words per minute",
       color = "Gender") +
  my_theme

ggsave("plots/word_rate_gender.png", plot=p, width=1900, height=1000, units="px", bg="white")

# bleu_score by gender
p <- df_rate_gender %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(x = year, y = bleu_score, color = gender)) +
  geom_line() +
  geom_point() +
  theme_minimal(base_size=8) +
  scale_color_manual(values = c("man" = "steelblue", "woman" = "firebrick", "NA" = "grey")) +
  labs(title = 'Bleu score ("intelligibility") by gender and year (1966-2002)',
       x = "Year",
       y = "Bleu score",
       color = "Gender") +
  my_theme

ggsave("plots/bleu_score_gender.png", plot=p, width=1900, height=1000, units="px", bg="white")

# Bleu score aggregated by year and month
df_bleu_month <- df %>% 
  rowwise() %>%
  mutate(date = as.Date(dates[[1]])) %>%
  ungroup() %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE, abbr = TRUE)) %>%
  group_by(year, month) %>%
  summarise(bleu_score = mean(bleu_score, na.rm = TRUE),
            date = first(date))

p <- df_bleu_month %>% 
  ggplot(aes(x = date, y = bleu_score)) +
  geom_line(color = "firebrick3") +
  geom_point(color = "firebrick3", size=0.6) +
  # geom_point(color = "black", size=0.8, stroke=0.2, shape=21) +
  theme_minimal(base_size=8) +
  labs(title = "Average monthly BLEU score in the Riksdag's speeches (1966-2002)",
       x = "Date",
       y = "Bleu score") +
  theme(panel.grid.major.y = element_line(colour="grey60", size=0.3, linetype=2),
        panel.grid.major.x = element_line(colour="grey60", size=0.1, linetype=1),
        panel.grid.minor.x = element_blank(),
        text=element_text(family="Palatino"),
        plot.subtitle=element_text(size=5.5))

ggsave("plots/monthly_bleu.png", 
       plot = p, width=1900, height=1000, units="px", dpi=300, bg="white")
