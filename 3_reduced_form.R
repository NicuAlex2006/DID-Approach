# ══════════════════════════════════════════════════════════════════════════════
# 3_reduced_form.R
# Produces: tables/ols_table.tex, tables/did_table.tex, tables/sutva_table.tex
#           figures/plot_actual_vs_predicted.pdf
#           figures/plot_event_study.pdf
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra, ggrepel, fwildclusterboot)

# Force modelsummary to produce standard LaTeX tabular via kableExtra
options(modelsummary_factory_latex = "kableExtra")

panel      <- readRDS("data/panel_clean.rds")
trade_exp  <- readRDS("data/trade_exposure.rds")

dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)

treatment_date <- as.Date("2022-07-01")
treated <- c("IT","GR","ES","PT","FR","BE")
control <- c("DE","NL","AT","FI","IE","LU")

country_names <- c(
  IT="Italy", GR="Greece", ES="Spain", PT="Portugal", FR="France", BE="Belgium",
  DE="Germany", NL="Netherlands", AT="Austria", FI="Finland", IE="Ireland", LU="Luxembourg"
)

theme_paper <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 8),
        legend.position = "bottom")

cols_group <- c("Treated (High-Debt)" = "#D73027", "Control (Low-Debt)" = "#4575B4")

# ══════════════════════════════════════════════════════════════════════════════
# 1. OLS cross-section (pre-treatment averages)
# ══════════════════════════════════════════════════════════════════════════════
pre_data <- panel %>%
  filter(post == 0) %>%
  group_by(geo) %>%
  summarise(
    unemp_pre    = mean(unemp_rate,  na.rm = TRUE),
    debt_gdp     = first(debt_gdp_2021),
    log_gdppc    = mean(log_gdppc,   na.rm = TRUE),
    pop_density  = mean(pop_density, na.rm = TRUE),
    high_debt    = first(high_debt),
    .groups = "drop"
  ) %>%
  mutate(
    country_name = country_names[geo],
    debt_group   = ifelse(high_debt == 1, "Treated (High-Debt)", "Control (Low-Debt)")
  )

ols1 <- lm(unemp_pre ~ debt_gdp,                             data = pre_data)
ols2 <- lm(unemp_pre ~ debt_gdp + log_gdppc,                 data = pre_data)
ols3 <- lm(unemp_pre ~ debt_gdp + log_gdppc + pop_density,   data = pre_data)

modelsummary(
  list("(1)" = ols1, "(2)" = ols2, "(3)" = ols3),
  output     = "tables/ols_table.tex",
  stars      = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map    = c("nobs", "r.squared", "adj.r.squared"),
  coef_rename = c(
    debt_gdp     = "Debt/GDP (2021, \\%)",
    log_gdppc    = "Log GDP per Capita",
    pop_density  = "Population Density",
    "(Intercept)" = "Constant"
  ),
  title = "OLS Cross-Section: Debt and Pre-Treatment Unemployment \\label{tab:ols}",
  notes = "Heteroskedasticity-robust SE in parentheses. $N=12$ countries."
)
cat("Saved: tables/ols_table.tex\n")

# Actual vs. predicted plot
pre_long <- pre_data %>%
  mutate(fit1 = fitted(ols1), fit2 = fitted(ols2), fit3 = fitted(ols3)) %>%
  pivot_longer(c(fit1, fit2, fit3), names_to = "spec", values_to = "fitted") %>%
  mutate(spec = recode(spec,
    fit1 = "(1) Debt/GDP only",
    fit2 = "(2) + Log GDP p.c.",
    fit3 = "(3) + Pop. Density"
  ))

p_avp <- ggplot(pre_long, aes(x = fitted, y = unemp_pre)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(color = debt_group), size = 2.5) +
  geom_text_repel(aes(label = country_name, color = debt_group),
                  size = 2.5, max.overlaps = 15, show.legend = FALSE) +
  facet_wrap(~spec) +
  scale_color_manual(values = cols_group, name = NULL) +
  labs(x = "Predicted Unemployment (%)", y = "Actual Unemployment (%)",
       caption = "Source: Eurostat.") +
  theme_paper

ggsave("figures/plot_actual_vs_predicted.pdf", p_avp, width = 12, height = 4.5, device = cairo_pdf)
cat("Saved: figures/plot_actual_vs_predicted.pdf\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. TWFE Difference-in-Differences
# ══════════════════════════════════════════════════════════════════════════════
did1 <- feols(unemp_rate ~ did_term,
              data = panel, cluster = ~geo)

did2 <- feols(unemp_rate ~ did_term | country_fe + time_fe,
              data = panel, cluster = ~geo)

did3 <- feols(unemp_rate ~ did_term + log_gdppc + pop_density | country_fe + time_fe,
              data = panel, cluster = ~geo)

did4 <- feols(unemp_rate ~ did_term + log_gdppc + pop_density + rate | country_fe + time_fe,
              data = panel, cluster = ~geo)

# Wild cluster bootstrap p-values (more reliable with 12 clusters)
cat("\nRunning wild cluster bootstrap for did2...\n")
boot2 <- boottest(did2, clustid = "geo", param = "did_term",
                  B = 9999, type = "rademacher", impose_null = TRUE)
cat("  Wild bootstrap p-value (did2):", round(boot2$p_val, 4), "\n")

cat("Running wild cluster bootstrap for did3...\n")
boot3 <- boottest(did3, clustid = "geo", param = "did_term",
                  B = 9999, type = "rademacher", impose_null = TRUE)
cat("  Wild bootstrap p-value (did3):", round(boot3$p_val, 4), "\n")

modelsummary(
  list("(1) No FE" = did1, "(2) TWFE" = did2,
       "(3) + Controls" = did3, "(4) + ECB Rate" = did4),
  output     = "tables/did_table.tex",
  stars      = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map    = c("nobs", "r.squared", "adj.r.squared",
                 "FE: country_fe", "FE: time_fe"),
  coef_rename = c(
    did_term     = "High-Debt $\\times$ Post",
    log_gdppc    = "Log GDP per Capita",
    pop_density  = "Population Density",
    rate         = "ECB Policy Rate (\\%)"
  ),
  title = "Difference-in-Differences: ECB Rate Hikes and Unemployment \\label{tab:did}",
  notes = paste0(
    "Standard errors clustered at the country level in parentheses. ",
    "Wild cluster bootstrap $p$-values (Rademacher, $B=9{,}999$): ",
    "Col.~(2) $p=", round(boot2$p_val, 3), "$; ",
    "Col.~(3) $p=", round(boot3$p_val, 3), "$. ",
    "Treatment = IT, GR, ES, PT, FR, BE. Control = DE, NL, AT, FI, IE, LU. ",
    "Post = July 2022 onward."
  )
)
cat("Saved: tables/did_table.tex\n")

# ══════════════════════════════════════════════════════════════════════════════
# 3. SUTVA / Spillover Robustness
# ══════════════════════════════════════════════════════════════════════════════

# Test 1: Drop Germany and Luxembourg (highest trade exposure)
panel_drop <- panel %>% filter(!geo %in% c("DE", "LU"))
did_drop <- feols(unemp_rate ~ did_term + log_gdppc + pop_density | country_fe + time_fe,
                  data = panel_drop, cluster = ~geo)

# Test 2: Within control group — does trade exposure predict delta unemployment?
ctrl_data <- panel %>%
  filter(high_debt == 0) %>%
  group_by(geo) %>%
  summarise(
    delta_unemp = mean(unemp_rate[post == 1], na.rm = TRUE) -
                  mean(unemp_rate[post == 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(trade_exp %>% rename(geo = country), by = "geo") %>%
  mutate(country_name = country_names[geo])

ols_sutva <- lm(delta_unemp ~ export_share, data = ctrl_data)

# Test 3: Continuous treatment intensity (debt/GDP * Post)
did_cont <- feols(
  unemp_rate ~ I(debt_gdp_2021/100 * post) + log_gdppc + pop_density | country_fe + time_fe,
  data = panel, cluster = ~geo
)

modelsummary(
  list("(1) Drop DE+LU" = did_drop,
       "(2) Control OLS" = ols_sutva,
       "(3) Continuous" = did_cont),
  output     = "tables/sutva_table.tex",
  stars      = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map    = c("nobs", "r.squared"),
  coef_rename = c(
    did_term                        = "High-Debt $\\times$ Post",
    export_share                    = "Export Share to Treated",
    "I(debt_gdp_2021/100 * post)"   = "Debt/GDP $\\times$ Post"
  ),
  title = "SUTVA and Spillover Robustness Checks \\label{tab:sutva}",
  notes = paste0(
    "Col.~(1): baseline DiD dropping Germany and Luxembourg (most trade-exposed controls). ",
    "Col.~(2): OLS within the control group --- does export share to treated countries predict ",
    "$\\Delta$ unemployment? Col.~(3): continuous treatment intensity (debt/GDP $\\times$ Post). ",
    "All panel specifications include country and year-month FE; SEs clustered at country level."
  )
)
cat("Saved: tables/sutva_table.tex\n")

# ══════════════════════════════════════════════════════════════════════════════
# 4. Event Study / Dynamic DiD
# ══════════════════════════════════════════════════════════════════════════════
panel_es <- panel %>%
  mutate(
    event_time   = pmax(pmin(months_rel, 24), -36),
    event_time_f = relevel(factor(event_time), ref = "-1")
  )

es_model <- feols(
  unemp_rate ~ i(event_time_f, high_debt, ref = "-1") | country_fe + time_fe,
  data = panel_es, cluster = ~geo
)

es_coefs <- broom::tidy(es_model, conf.int = TRUE) %>%
  filter(str_detect(term, "event_time_f")) %>%
  mutate(
    event_time = as.integer(str_extract(term, "-?\\d+")),
    pre_post   = ifelse(event_time < 0, "Pre-treatment", "Post-treatment")
  )

# Add reference period (k = -1)
es_plot <- bind_rows(
  es_coefs,
  tibble(estimate = 0, conf.low = 0, conf.high = 0,
         event_time = -1L, pre_post = "Pre-treatment")
) %>% arrange(event_time)

p_es <- ggplot(es_plot, aes(x = event_time, y = estimate,
                             ymin = conf.low, ymax = conf.high,
                             color = pre_post, fill = pre_post)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", linewidth = 0.7) +
  geom_ribbon(alpha = 0.15, color = NA) +
  geom_point(size = 1.8) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c("Pre-treatment" = "#4575B4", "Post-treatment" = "#D73027")) +
  scale_fill_manual(values  = c("Pre-treatment" = "#4575B4", "Post-treatment" = "#D73027")) +
  scale_x_continuous(breaks = seq(-36, 24, by = 6)) +
  annotate("text", x = 0.5, y = max(es_plot$conf.high, na.rm = TRUE) * 0.92,
           label = "Treatment\n(Jul 2022)", hjust = 0, size = 3, color = "black") +
  labs(x = "Months Relative to First ECB Rate Hike (July 2022)",
       y = "Differential Unemployment (pp)",
       color = NULL, fill = NULL,
       caption = "95% CI based on country-clustered SE. Reference: June 2022 (k = -1).") +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/plot_event_study.pdf", p_es, width = 10, height = 5.5, device = cairo_pdf)
cat("Saved: figures/plot_event_study.pdf\n")

# ── Pre-trends Wald test ──────────────────────────────────────────────────────
cat("\n── Pre-Trends Wald Test ──────────────────────────────────\n")
pre_times <- es_plot %>% filter(pre_post == "Pre-treatment", event_time != -1) %>% pull(event_time)
pre_terms <- paste0("event_time_f", pre_times, ":high_debt")
pre_in_model <- intersect(pre_terms, names(coef(es_model)))

if (length(pre_in_model) > 0) {
  cat("Testing joint significance of", length(pre_in_model), "pre-treatment coefficients:\n")
  print(wald(es_model, pre_in_model))
  cat("=> If p > 0.05: parallel trends assumption NOT rejected.\n")
} else {
  cat("No pre-treatment terms found in model.\n")
}

cat("\nDone: 3_reduced_form.R\n")
