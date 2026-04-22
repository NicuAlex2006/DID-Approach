# ══════════════════════════════════════════════════════════════════════════════
# 3_reduced_form.R
# Produces: tables/ols_table.tex, tables/did_table.tex
#           figures/plot_actual_vs_predicted.pdf
#           figures/plot_event_study.pdf
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra, ggrepel)

# Try to load fwildclusterboot (optional — needed for wild bootstrap p-values)
has_boot <- require("fwildclusterboot", quietly = TRUE)
if (!has_boot) {
  cat("Note: fwildclusterboot not available. Install with install.packages('fwildclusterboot').\n")
  cat("      Skipping wild cluster bootstrap p-values.\n")
}

# Force modelsummary to produce standard LaTeX tabular via kableExtra
# and disable \num{} wrapping (requires siunitx package in LaTeX)
options(modelsummary_factory_latex = "kableExtra")
options(modelsummary_format_numeric_latex = "plain")

# Helper: post-process a .tex file to clean up modelsummary escaping issues
fix_tex <- function(path, label_tag = NULL) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # 1. Strip \num{...} → just the content inside
  while (grepl("\\\\num\\{", txt)) {
    txt <- gsub("\\\\num\\{([^}]*)\\}", "\\1", txt)
  }

  # 2. Fix \textbackslash{}label\{...\} → \label{...}
  txt <- gsub("\\\\textbackslash\\{\\}label\\\\\\{([^}]*)\\\\\\}", "\\\\label{\\1}", txt)

  # 3. Fix \$\textbackslash{}times\$ → $\times$
  txt <- gsub("\\\\\\$\\\\textbackslash\\{\\}times\\\\\\$", "$\\\\times$", txt)
  # Also handle variant without escaped $
  txt <- gsub("\\$\\\\textbackslash\\{\\}times\\$", "$\\\\times$", txt)

  # 4. Fix R2 labels
  txt <- gsub("Num\\.Obs\\.", "Observations", txt)
  txt <- gsub("R2 Adj\\.", "Adj. $R^2$", txt)
  txt <- gsub("(^|\n)(R2)( &)", "\\1$R^2$\\3", txt)
  txt <- gsub("& R2 &", "& $R^2$ &", txt)
  # Standalone "R2" at start of a line in a table
  txt <- gsub("\nR2 &", "\n$R^2$ &", txt)

  # 5. Fix FE labels: country\_fe → Country, time\_fe → Year-Month
  txt <- gsub("FE: country\\\\_fe", "Country FE", txt)
  txt <- gsub("FE: time\\\\_fe", "Year-Month FE", txt)
  txt <- gsub("country\\\\_fe", "Country", txt)
  txt <- gsub("time\\\\_fe", "Year-Month", txt)

  # 6. Inject \label if needed
  if (!is.null(label_tag) && !grepl("\\\\label\\{", txt)) {
    txt <- sub("(\\\\caption\\{[^}]*)(\\})",
               paste0("\\1 \\\\label{", label_tag, "}\\2"), txt)
  }

  writeLines(txt, path)
  cat("  Post-processed:", path, "\n")
}

panel      <- readRDS("data/panel_clean.rds")

dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)

treatment_date <- as.Date("2022-07-01")
treated <- c("IT","EL","ES","PT","FR","BE")
control <- c("DE","NL","AT","FI","IE","LU")

country_names <- c(
  IT="Italy", EL="Greece", ES="Spain", PT="Portugal", FR="France", BE="Belgium",
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
  title = "OLS Cross-Section: Debt and Pre-Treatment Unemployment",
  notes = "Heteroskedasticity-robust SE in parentheses. $N=12$ countries."
)
fix_tex("tables/ols_table.tex", label_tag = "tab:ols")
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

ggsave("figures/plot_actual_vs_predicted.pdf", p_avp, width = 12, height = 4.5)
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
boot_note <- ""
if (has_boot) {
  cat("\nRunning wild cluster bootstrap for did2...\n")
  boot2 <- boottest(did2, clustid = "geo", param = "did_term",
                    B = 9999, type = "rademacher", impose_null = TRUE)
  cat("  Wild bootstrap p-value (did2):", round(boot2$p_val, 4), "\n")

  cat("Running wild cluster bootstrap for did3...\n")
  boot3 <- boottest(did3, clustid = "geo", param = "did_term",
                    B = 9999, type = "rademacher", impose_null = TRUE)
  cat("  Wild bootstrap p-value (did3):", round(boot3$p_val, 4), "\n")

  boot_note <- paste0(
    " Wild cluster bootstrap $p$-values (Rademacher, $B=9{,}999$): ",
    "Col.~(2) $p=", round(boot2$p_val, 3), "$; ",
    "Col.~(3) $p=", round(boot3$p_val, 3), "$."
  )
} else {
  cat("\nSkipping wild cluster bootstrap (package not installed).\n")
}

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
  title = "Difference-in-Differences: ECB Rate Hikes and Unemployment",
  notes = paste0(
    "Standard errors clustered at the country level in parentheses.",
    boot_note,
    " Treatment = IT, EL, ES, PT, FR, BE. Control = DE, NL, AT, FI, IE, LU. ",
    "Post = July 2022 onward."
  )
)
fix_tex("tables/did_table.tex", label_tag = "tab:did")
cat("Saved: tables/did_table.tex\n")

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

ggsave("figures/plot_event_study.pdf", p_es, width = 10, height = 5.5)
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
