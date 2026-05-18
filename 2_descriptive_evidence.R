rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, kableExtra, ggrepel)

panel         <- readRDS("data/panel_clean.rds")
debt          <- readRDS("data/debt_2021.rds")
ecb_monthly   <- readRDS("data/ecb_rate_monthly.rds")

dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)

treatment_date <- as.Date("2022-07-01")

country_names <- c(
  IT="Italy", EL="Greece", ES="Spain", PT="Portugal", FR="France",
  DE="Germany", NL="Netherlands", AT="Austria", FI="Finland", IE="Ireland"
)

theme_paper <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 8),
        legend.position = "bottom")

cols <- c("Treated (High-Debt)" = "#D73027", "Control (Low-Debt)" = "#4575B4")

sumstats_geos <- c("IT","EL","ES","PT","FR","DE","NL","AT","FI","IE")

sumstats_var_order <- c(
  "Debt/GDP 2021 (\\%)",
  "Log GDP per Capita",
  "Population Density",
  "Tourism Share of GVA (\\%)",
  "Russian Gas Share 2021 (\\%)",
  "ECB Policy Rate (\\%)",
  "Unemployment Rate (\\%)"
)

sumstats <- panel %>%
  filter(geo %in% sumstats_geos) %>%
  select(unemp_rate, debt_gdp_2021, log_gdppc, pop_density,
         tourism_share, russian_gas_share, rate) %>%
  pivot_longer(everything(), names_to = "variable") %>%
  group_by(variable) %>%
  summarise(
    N      = sum(!is.na(value)),
    Min    = round(min(value,              na.rm = TRUE), 2),
    Q1     = round(quantile(value, 0.25,   na.rm = TRUE), 2),
    Mean   = round(mean(value,             na.rm = TRUE), 2),
    Median = round(median(value,           na.rm = TRUE), 2),
    Q3     = round(quantile(value, 0.75,   na.rm = TRUE), 2),
    Max    = round(max(value,              na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(variable = recode(variable,
    unemp_rate        = "Unemployment Rate (\\%)",
    debt_gdp_2021     = "Debt/GDP 2021 (\\%)",
    log_gdppc         = "Log GDP per Capita",
    pop_density       = "Population Density",
    tourism_share     = "Tourism Share of GVA (\\%)",
    russian_gas_share = "Russian Gas Share 2021 (\\%)",
    rate              = "ECB Policy Rate (\\%)"
  )) %>%
  mutate(variable = factor(variable, levels = sumstats_var_order)) %>%
  arrange(variable)

sumstats %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      col.names = c("Variable", "N", "Min", "Q1", "Mean", "Median", "Q3", "Max"),
      caption = "Summary Statistics --- 10 Eurozone Countries, 2018--2024 (Monthly) \\label{tab:sumstats}") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(general = paste(
    "Sources: Eurostat, ECB Statistical Data Warehouse.",
    "Tourism share = NACE sector I (accommodation \\& food services) as \\%",
    "of total gross value added at basic prices, averaged 2018--2021",
    "(Eurostat \\texttt{nama\\_10\\_a64}).",
    "Russian gas share = share of Russian natural gas in total national",
    "natural-gas consumption in 2021 (Bruegel/IEA); time-invariant",
    "exposure measure used in the panel OLS interaction with Post\\_Feb2022."
  ), general_title = "", threeparttable = TRUE) %>%
  save_kable("tables/summary_stats.tex")

sumstats_txt <- readLines("tables/summary_stats.tex", warn = FALSE)
sumstats_txt <- gsub("\\\\textbackslash\\{\\}\\\\_", "\\_", paste(sumstats_txt, collapse = "\n"))
writeLines(sumstats_txt, "tables/summary_stats.tex")

cat("Saved: tables/summary_stats.tex\n")

trends <- panel %>%
  group_by(date, group_label) %>%
  summarise(avg_unemp = mean(unemp_rate, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(trends, aes(x = date, y = avg_unemp,
                          color = group_label, linetype = group_label)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = as.numeric(treatment_date),
             linetype = "dashed", color = "black", linewidth = 0.7) +
  annotate("text", x = treatment_date + 45,
           y = max(trends$avg_unemp, na.rm = TRUE) * 0.97,
           label = "First ECB hike\n(Jul 2022)", hjust = 0, size = 3.2) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("Treated (High-Debt)" = "solid",
                                    "Control (Low-Debt)"  = "dashed")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Parallel Trends: Average Unemployment by Debt Group",
       subtitle = "10 Eurozone countries (BE and LU excluded for extreme trade exposure)",
       x = NULL, y = "Average Unemployment Rate (%)", color = NULL, linetype = NULL,
       caption = "Unweighted group averages. Treated: IT, EL, ES, PT, FR. Control: DE, NL, AT, FI, IE.\nSource: Eurostat une_rt_m.") +
  theme_paper

ggsave("figures/plot_parallel_trends.pdf", p1, width = 9, height = 5)
cat("Saved: figures/plot_parallel_trends.pdf\n")

p2 <- ggplot(ecb_monthly, aes(x = date, y = rate)) +
  geom_step(color = "#2C3E50", linewidth = 0.9) +
  geom_vline(xintercept = as.numeric(treatment_date),
             linetype = "dashed", color = "#D73027", linewidth = 0.7) +
  annotate("text", x = treatment_date + 45, y = 4.3,
           label = "First hike\n(Jul 2022)", hjust = 0, size = 3.2, color = "#D73027") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-0.1, 5), breaks = 0:5) +
  labs(title = "ECB Main Refinancing Rate, 2018--2024",
       x = NULL, y = "ECB Main Refinancing Rate (%)",
       caption = "Source: European Central Bank Statistical Data Warehouse.") +
  theme_paper

ggsave("figures/plot_ecb_rate.pdf", p2, width = 9, height = 4)
cat("Saved: figures/plot_ecb_rate.pdf\n")

panel_named <- panel %>%
  mutate(country_name_f = factor(country_name,
           levels = country_names[c("IT","EL","ES","PT","FR",
                                     "DE","NL","AT","FI","IE")]))

p3 <- ggplot(panel_named, aes(x = date, y = unemp_rate, color = group_label)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(treatment_date),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  facet_wrap(~country_name_f, ncol = 4, scales = "free_y") +
  scale_color_manual(values = cols) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Country-Level Unemployment Trends",
       subtitle = "10 Eurozone countries (BE and LU excluded for extreme trade exposure)",
       x = NULL, y = "Unemployment Rate (%)", color = NULL,
       caption = "Dashed line = July 2022. Treated: IT, EL, ES, PT, FR. Control: DE, NL, AT, FI, IE.\nSource: Eurostat une_rt_m.") +
  theme_paper +
  theme(strip.background = element_rect(fill = "grey92"),
        axis.text.x = element_text(size = 8))

ggsave("figures/plot_country_trends.pdf", p3, width = 12, height = 8)
cat("Saved: figures/plot_country_trends.pdf\n")

debt_plot <- debt %>%
  mutate(
    country_name = country_names[geo],
    group_label  = ifelse(geo %in% c("IT","EL","ES","PT","FR"),
                          "Treated (High-Debt)", "Control (Low-Debt)"),
    country_name = fct_reorder(country_name, debt_gdp_2021)
  ) %>%
  filter(geo %in% c("IT","EL","ES","PT","FR","DE","NL","AT","FI","IE"))

p4 <- ggplot(debt_plot, aes(x = country_name, y = debt_gdp_2021, fill = group_label)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dotted", color = "#D73027", linewidth = 0.7) +
  annotate("text", x = 1.5, y = 63, label = "Maastricht 60%", size = 3.2, hjust = 0) +
  annotate("text", x = 1.5, y = 93, label = "Treatment threshold 90%",
           size = 3.2, hjust = 0, color = "#D73027") +
  scale_fill_manual(values = cols) +
  coord_flip() +
  labs(title = "Government Debt-to-GDP Ratio (2021)",
       subtitle = "10 Eurozone countries (BE and LU excluded for extreme trade exposure)",
       x = NULL, y = "Debt/GDP (%)", fill = NULL,
       caption = "Treated: IT, EL, ES, PT, FR. Control: DE, NL, AT, FI, IE.\nSource: Eurostat gov_10dd_edpt1.") +
  theme_paper

ggsave("figures/plot_debt_bar.pdf", p4, width = 8, height = 5)
cat("Saved: figures/plot_debt_bar.pdf\n")

cat("\nDone: 2_descriptive_evidence.R\n")
