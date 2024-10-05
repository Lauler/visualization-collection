library(dplyr)

df <- read.table(text = "year 	hours_matched 	nr_speeches_total 	nr_matched_speeches 	match_fraction
1966 	152.81 	4425 	1829 	0.41
1967 	597.33 	9009 	7213 	0.80
1968 	532.13 	8391 	6831 	0.81
1969 	569.99 	8762 	7365 	0.84
1970 	595.86 	8660 	7192 	0.83
1971 	506.13 	7567 	6844 	0.90
1972 	540.36 	8036 	7350 	0.91
1973 	525.45 	7635 	6898 	0.90
1974 	424.68 	6888 	6037 	0.88
1975 	468.00 	7602 	6776 	0.89
1976 	491.58 	7831 	7033 	0.90
1977 	443.38 	7620 	6881 	0.90
1978 	543.16 	9287 	8492 	0.91
1979 	520.66 	9470 	8582 	0.91
1980 	544.95 	9867 	8978 	0.91
1981 	469.89 	8659 	7848 	0.91
1982 	500.76 	8873 	7975 	0.90
1983 	532.66 	10451 	9192 	0.88
1984 	523.33 	10175 	9109 	0.90
1985 	502.23 	9902 	8799 	0.89
1986 	465.03 	9838 	8286 	0.84
1987 	453.88 	10347 	8110 	0.78
1988 	537.01 	10248 	9367 	0.91
1989 	559.53 	11680 	10617 	0.91
1990 	555.07 	11758 	10686 	0.91
1991 	522.12 	11377 	10516 	0.92
1992 	542.95 	14271 	13337 	0.93
1993 	551.71 	14049 	13087 	0.93
1994 	485.01 	11538 	10856 	0.94
1995 	479.64 	11619 	11329 	0.98
1996 	464.36 	11757 	11455 	0.97
1997 	500.22 	12273 	11510 	0.94
1998 	468.92 	11677 	11102 	0.95
1999 	517.12 	13269 	12743 	0.96
2000 	529.82 	13636 	12758 	0.94
2001 	546.92 	13579 	12974 	0.96
2002 	140.08 	4907 	3398 	0.69", header = TRUE, sep = "\t")

df <- tidyr::as_tibble(df)
df$non_matched_speeches <- df$nr_speeches_total - df$nr_matched_speeches

df <- df %>%
  tidyr::pivot_longer(cols = c(nr_speeches_total, non_matched_speeches),
                      names_to = "match_status",
                      values_to = "speeches")

# Bar plot (relative frequency out of 1)
p <- df %>%
  ggplot(aes(x = year, y = speeches, fill=match_status)) +
  geom_bar(stat = "identity", position = "fill", color="black", size=0.1) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_discrete(labels = c("Unmatched", "Matched"),
                      type = c("firebrick2", "steelblue")) +
  labs(title = "Relative matched and non-matched speeches",
       x = "Year",
       y = "Speeches (%)",
       fill = "Status") +
  theme_minimal(base_size=10) +
  my_theme

ggsave("plots/relative_matched_speeches.png",
       plot = p, width=1900, height=1000, units="px", dpi=300, bg="white")


# Regular frequency
p <- df %>%
  ggplot(aes(x = year, y = speeches, fill=match_status)) +
  geom_bar(stat = "identity", position = "stack", color="black", size=0.1) +
  scale_fill_discrete(labels = c("Unmatched", "Matched"),
                      type = c("firebrick2", "steelblue")) +
  labs(title = "Matched and non-matched speeches per year",
       x = "Year",
       y = "Number of speeches",
       fill = "Status") +
  theme_minimal(base_size=10) +
  my_theme


ggsave("plots/absolute_matched_speeches.png",
       plot = p, width=1900, height=1000, units="px", dpi=300, bg="white")

