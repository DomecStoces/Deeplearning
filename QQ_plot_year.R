library(dplyr)
library(ggplot2)

df<-data_ophonus_a2_c
df1<-data_picipennis_a2
df <- data_ophonus_a2_c %>%
  mutate(
    Year    = as.numeric(Year),
    FA3_log = log1p(FA3)
  )

# Split data by year
df_2023 <- df %>%
  filter(Year == 2023, !is.na(FA3_log)) %>%
  pull(FA3_log)
df_2024 <- df %>%
  filter(Year == 2024, !is.na(FA3_log)) %>%
  pull(FA3_log)

# Build data frame for QQ-plot
qq_df <- qqplot(df_2023, df_2024, plot.it = FALSE)

qq_df <- data.frame(
  x = qq_df$x,
  y = qq_df$y
)

# Plot
qq<-ggplot(qq_df, aes(x = x, y = y)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "2023 quantiles", y = "2024 quantiles") +
  theme_minimal(base_size = 14)
ks.test(df_2023, df_2024)

tiff('QQ_OC.tiff',units="in",width=6,height=4,bg="white",res=600)
qq
dev.off()
