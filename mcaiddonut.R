library(ggplot2)
library(dplyr)

data <- data.frame(
  label = c("Private Insurance", "Medicare", "Other",
            "Medicaid", "OOP"),
  value = c(1464.6, 1029.8, 984.7, 871.7, 505.7)
)

total <- sum(data$value)
data <- data %>%
  mutate(
    pct        = value / total * 100,
    pct_lab    = paste0(round(pct, 1), "%"),
    label      = factor(label, levels = rev(label)),  # reverse to match ggplot stacking
    label_wrap = gsub(" ", "\n", label),
    cumval     = cumsum(value),
    midval     = cumval - value / 2
  )

colors <- c(
  "Private Insurance" = "#72a3be",
  "Medicare"                 = "#a8c8d8",
  "Other"                    = "#9eacb4",
  "Medicaid"                 = "#c0522a",
  "OOP"                      = "#aacad9"
)

ggplot(data, aes(x = 2, y = value, fill = label)) +
  geom_col(width = 1, color = "white", linewidth = 0.8) +
  geom_text(
    aes(x = 2, y = midval, label = paste0(label_wrap, "\n")),
    color = "white", size = 3.5, fontface = "bold", lineheight = 0.9
  ) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = colors) +
  annotate("text", x = 0.5, y = 0,
           label = "$5 Trillion\nTotal NHE",
           size = 7, fontface = "bold", color = "#c0522a",
           hjust = 0.5, vjust = 0.5) +
  labs(
    #title    = "Medicaid Accounts for Nearly 1 in 5 Dollars\nof U.S. National Health Expenditures",
    #subtitle = "Source of funds, 2023 (total = $4,866.5 billion)",
    caption  = "Source: CMS National Health Expenditure Accounts, Table 4 (2023)",
    fill     = NULL
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40",
                                 margin = margin(b = 10)),
    plot.caption  = element_text(size = 9, color = "gray50", hjust = 2,
                                 margin = margin(t = 10)),
    legend.position = "none",
    plot.margin   = margin(10, 10, 10, 10)
  )

ggsave("medicaid_nhe_2023.png", width = 8, height = 5.5, dpi = 300,
       bg = "transparent")
