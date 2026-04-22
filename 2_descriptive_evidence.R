# ══════════════════════════════════════════════════════════════════════════════
# 2_descriptive_evidence.R
# Produces: tables/summary_stats.tex
#           figures/plot_parallel_trends.pdf
#           figures/plot_ecb_rate.pdf
#           figures/plot_country_trends.pdf
#           figures/plot_debt_bar.pdf
#           figures/plot_trade_exposure.pdf
#           figures/plot_sutva_scatter.pdf
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, kableExtra, ggrepel)

panel         <- readRDS("data/panel_clean.rds")
debt          <- readRDS("data/debt_2021.rds")
ecb_monthly   <- readRDS("data/ecb_rate_monthly.rds")
trade_exp     <- readRDS("data/trade_exposure.rds")

dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)

treatment_date <- as.Date("2022-07-01")

country_names <- c(
  IT="Italy", GR="Greece", ES="Spain", PT="Portugal", FR="France", BE="Belgium",
  DE="Germany", NL="Netherlands", AT="Austria", FI="Finland", IE="Ireland", LU="Luxembourg"
)

# ── Shared theme ──────────────────────────────────────────────────────────────
theme_paper <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 8),
        legend.position = "bottom")

cols <- c("Treated (High-Debt)" = "#D73027", "Control (Low-Debt)" = "#4575B4")

# ══════════════════════════════════════════════════════════════════════════════
# 1. Summary statistics table
# ══════════════════════════════════════════════════════════════════════════════
sumstats <- panel %>%
  select(unemp_rate, debt_gdp_2021, log_gdppc, pop_density, rate) %>%
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
    unemp_rate    = "Unemployment Rate (\\%)",
    debt_gdp_2021 = "Debt/GDP 2021 (\\%)",
    log_gdppc     = "Log GDP per Capita",
    pop_density   = "Population Density",
    rate          = "ECB Policy Rate (\\%)"
  ))

sumstats %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      col.names = c("Variable", "N", "Min", "Q1", "Mean", "Median", "Q3", "Max"),
      caption = "Summary Statistics --- 12 Eurozone Countries, 2018--2024 (Monthly) \\label{tab:sumstats}") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(general = "Sources: Eurostat une\\_rt\\_m, gov\\_10dd\\_edpt1, nama\\_10\\_pc, demo\\_r\\_d3dens, ECB SDW.",
           general_title = "", threeparttable = TRUE) %>%
  save_kable("tables/summary_stats.tex")

cat("Saved: tables/summary_stats.tex\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. Figure 1: Parallel trends (group averages)
# ══════════════════════════════════════════════════════════════════════════════
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
  labs(x = NULL, y = "Average Unemployment Rate (%)", color = NULL, linetype = NULL,
       caption = "Unweighted group averages. Source: Eurostat une_rt_m.") +
  theme_paper

ggsave("figures/plot_parallel_trends.pdf", p1, width = 9, height = 5, device = cairo_pdf)
cat("Saved: figures/plot_parallel_trends.pdf\n")

# ══════════════════════════════════════════════════════════════════════════════
# 3. Figure 2: ECB policy rate timeline
# ══════════════════════════════════════════════════════════════════════════════
p2 <- ggplot(ecb_monthly, aes(x = date, y = rate)) +
  geom_step(color = "#2C3E50", linewidth = 0.9) +
  geom_vline(xintercept = as.numeric(treatment_date),
             linetype = "dashed", color = "#D73027", linewidth = 0.7) +
  annotate("text", x = treatment_date + 45, y = 4.3,
           label = "First hike\n(Jul 2022)", hjust = 0, size = 3.2, color = "#D73027") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-0.1, 5), breaks = 0:5) +
  labs(x = NULL, y = "ECB Main Refinancing Rate (%)",
       caption = "Source: European Central Bank Statistical Data Warehouse.") +
  theme_paper

ggsave("figures/plot_ecb_rate.pdf", p2, width = 9, height = 4, device = cairo_pdf)
cat("Saved: figures/plot_ecb_rate.pdf\n")

# ══════════════════════════════════════════════════════════════════════════════
# 4. Figure 3: Country-level trends (faceted)
# ══════════════════════════════════════════════════════════════════════════════
panel_named <- panel %>%
  mutate(country_name_f = factor(country_name,
           levels = country_names[c("IT","GR","ES","PT","FR","BE",
                                     "DE","NL","AT","FI","IE","LU")]))

p3 <- ggplot(panel_named, aes(x = date, y = unemp_rate, color = group_label)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(treatment_date),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  facet_wrap(~country_name_f, ncol = 4, scales = "free_y") +
  scale_color_manual(values = cols) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = NULL, y = "Unemployment Rate (%)", color = NULL,
       caption = "Dashed line = July 2022. Source: Eurostat une_rt_m.") +
  theme_paper +
  theme(strip.background = element_rect(fill = "grey92"),
        axis.text.x = element_text(size = 8))

ggsave("figures/plot_country_trends.pdf", p3, width = 12, height = 8, device = cairo_pdf)
cat("Saved: figures/plot_country_trends.pdf\n")

# ══════════════════════════════════════════════════════════════════════════════
# 5. Figure 4: Debt bar chart (pre-treatment, 2021)
# ══════════════════════════════════════════════════════════════════════════════
debt_plot <- debt %>%
  mutate(
    country_name = country_names[geo],
    group_label  = ifelse(geo %in% c("IT","GR","ES","PT","FR","BE"),
                          "Treated (High-Debt)", "Control (Low-Debt)"),
    country_name = fct_reorder(country_name, debt_gdp_2021)
  )

p4 <- ggplot(debt_plot, aes(x = country_name, y = debt_gdp_2021, fill = group_label)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "black", linewidth = 0.7) +
  geom_hline(yintercept = 90, linetype = "dotted", color = "#D73027", linewidth = 0.7) +
  annotate("text", x = 1.5, y = 63, label = "Maastricht 60%", size = 3.2, hjust = 0) +
  annotate("text", x = 1.5, y = 93, label = "Treatment threshold 90%",
           size = 3.2, hjust = 0, color = "#D73027") +
  scale_fill_manual(values = cols) +
  coord_flip() +
  labs(x = NULL, y = "Debt/GDP (%)", fill = NULL,
       caption = "Source: Eurostat gov_10dd_edpt1.") +
  theme_paper

ggsave("figures/plot_debt_bar.pdf", p4, width = 8, height = 5, device = cairo_pdf)
cat("Saved: figures/plot_debt_bar.pdf\n")

# ══════════════════════════════════════════════════════════════════════════════
# 6. Figure 5: SUTVA — Trade exposure bar chart
# ══════════════════════════════════════════════════════════════════════════════
trade_named <- trade_exp %>%
  mutate(country_name = fct_reorder(country_names[country], export_share))

p5 <- ggplot(trade_named, aes(x = country_name, y = export_share * 100)) +
  geom_col(fill = "#4575B4", width = 0.6) +
  geom_text(aes(label = paste0(round(export_share * 100, 1), "%")),
            hjust = -0.2, size = 3.5) +
  coord_flip(ylim = c(0, 50)) +
  labs(x = NULL, y = "Export Share to Treated Countries (%)",
       caption = "Share of goods exports destined for IT, GR, ES, PT, FR, BE (2021).\nSource: Eurostat / Comext.") +
  theme_paper

ggsave("figures/plot_trade_exposure.pdf", p5, width = 7, height = 4, device = cairo_pdf)
cat("Saved: figures/plot_trade_exposure.pdf\n")

# ══════════════════════════════════════════════════════════════════════════════
# 7. Figure 6: SUTVA — Trade exposure vs unemployment change (scatter)
# ══════════════════════════════════════════════════════════════════════════════
ctrl_change <- panel %>%
  filter(high_debt == 0) %>%
  group_by(geo, country_name) %>%
  summarise(
    unemp_pre  = mean(unemp_rate[post == 0], na.rm = TRUE),
    unemp_post = mean(unemp_rate[post == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(delta_unemp = unemp_post - unemp_pre) %>%
  left_join(trade_exp %>% rename(geo = country), by = "geo")

p6 <- ggplot(ctrl_change, aes(x = export_share * 100, y = delta_unemp)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
  geom_smooth(method = "lm", se = TRUE, color = "#2C3E50", fill = "#2C3E50",
              alpha = 0.15, linewidth = 0.8) +
  geom_point(color = "#4575B4", size = 3) +
  geom_text_repel(aes(label = country_name), size = 3.2, max.overlaps = 10) +
  labs(x = "Export Share to Treated Countries (%)",
       y = expression(Delta ~ "Unemployment (post minus pre, pp)"),
       caption = "Control group only (N=6). OLS fit with 95% CI.\nPositive slope would indicate spillover contamination.") +
  theme_paper

ggsave("figures/plot_sutva_scatter.pdf", p6, width = 7, height = 5, device = cairo_pdf)
cat("Saved: figures/plot_sutva_scatter.pdf\n")

cat("\nDone: 2_descriptive_evidence.R\n")
