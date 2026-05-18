rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra, ggrepel)

options(modelsummary_factory_latex = "kableExtra")
options(modelsummary_format_numeric_latex = "plain")

fix_tex <- function(path, label_tag = NULL) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  while (grepl("\\\\num\\{", txt)) {
    txt <- gsub("\\\\num\\{([^}]*)\\}", "\\1", txt)
  }

  txt <- gsub("\\\\textbackslash\\{\\}label\\\\\\{([^}]*)\\\\\\}", "\\\\label{\\1}", txt)

  txt <- gsub("\\\\\\$\\\\textbackslash\\{\\}times\\\\\\$", "$\\\\times$", txt)
  txt <- gsub("\\$\\\\textbackslash\\{\\}times\\$", "$\\\\times$", txt)

  txt <- gsub("Num\\.Obs\\.", "Observations", txt)
  txt <- gsub("R2 Adj\\.", "Adj. $R^2$", txt)
  txt <- gsub("(^|\n)(R2)( &)", "\\1$R^2$\\3", txt)
  txt <- gsub("& R2 &", "& $R^2$ &", txt)
  txt <- gsub("\nR2 &", "\n$R^2$ &", txt)

  txt <- gsub("FE: country\\\\_fe", "Country FE", txt)
  txt <- gsub("FE: time\\\\_fe", "Year-Month FE", txt)
  txt <- gsub("country\\\\_fe", "Country", txt)
  txt <- gsub("time\\\\_fe", "Year-Month", txt)

  if (!is.null(label_tag) && !grepl("\\\\label\\{", txt)) {
    txt <- sub("(\\\\caption\\{[^}]*)(\\})",
               paste0("\\1 \\\\label{", label_tag, "}\\2"), txt)
  }

  writeLines(txt, path)
}

panel          <- readRDS("data/panel_clean.rds")
trade_exposure <- readRDS("data/trade_exposure.rds")

dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)

treatment_date <- as.Date("2022-07-01")
treated <- c("IT","EL","ES","PT","FR")
control <- c("DE","NL","AT","FI","IE")

panel <- panel %>% filter(geo %in% c(treated, control))

country_names <- c(
  IT="Italy", EL="Greece", ES="Spain", PT="Portugal", FR="France",
  DE="Germany", NL="Netherlands", AT="Austria", FI="Finland", IE="Ireland"
)

theme_paper <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 8),
        legend.position = "bottom")

cols_group <- c("Treated (High-Debt)" = "#D73027", "Control (Low-Debt)" = "#4575B4")

ols_data <- panel %>%
  rename(debt_gdp = debt_gdp_2021)

ols1 <- feols(unemp_rate ~ debt_gdp,
              data = ols_data, cluster = ~geo)
ols2 <- feols(unemp_rate ~ debt_gdp + log_gdppc,
              data = ols_data, cluster = ~geo)
ols3 <- feols(unemp_rate ~ debt_gdp + log_gdppc + pop_density,
              data = ols_data, cluster = ~geo)
ols4 <- feols(unemp_rate ~ debt_gdp + log_gdppc + pop_density + tourism_share,
              data = ols_data, cluster = ~geo)
ols5 <- feols(unemp_rate ~ debt_gdp + log_gdppc + pop_density + tourism_share +
                            gas_post_feb22,
              data = ols_data, cluster = ~geo)

modelsummary(
  list("(1) Debt/GDP"       = ols1,
       "(2) + GDP p.c."     = ols2,
       "(3) + Pop. Density" = ols3,
       "(4) + Tourism"      = ols4,
       "(5) + Gas $\\times$ Post Feb 22" = ols5),
  output     = "tables/ols_table.tex",
  stars      = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map    = c("nobs", "r.squared", "adj.r.squared"),
  coef_rename = c(
    debt_gdp        = "Debt/GDP (2021, \\%)",
    log_gdppc       = "Log GDP per Capita",
    pop_density     = "Population Density (inhab./km$^2$)",
    tourism_share   = "Tourism Share of GVA (\\%)",
    gas_post_feb22  = "Russian Gas Share $\\times$ Post Feb 2022",
    "(Intercept)"   = "Constant"
  ),
  title = "Panel Pooled OLS: Unemployment, Structural Controls, and the Energy Shock",
  notes = paste0(
    "Standard errors clustered at the country level in parentheses. ",
    "Sample: 10 Eurozone countries x 84 months (Jan 2018--Dec 2024). ",
    "Russian gas share = share of Russian natural gas in total national ",
    "gas consumption in 2021 (Bruegel / IEA), time-invariant. ",
    "Post Feb 2022 = 1 from Feb 2022 onward (the month of Russia's invasion ",
    "of Ukraine and the start of the European energy-price shock)."
  )
)
fix_tex("tables/ols_table.tex", label_tag = "tab:ols")
cat("Saved: tables/ols_table.tex\n")

ols_data_fit <- ols_data %>%
  mutate(fit1 = fitted(ols1), fit2 = fitted(ols2),
         fit3 = fitted(ols3), fit4 = fitted(ols4),
         fit5 = fitted(ols5))

country_avp <- ols_data_fit %>%
  group_by(geo, country_name, group_label) %>%
  summarise(
    unemp_actual = mean(unemp_rate, na.rm = TRUE),
    fit1 = mean(fit1, na.rm = TRUE), fit2 = mean(fit2, na.rm = TRUE),
    fit3 = mean(fit3, na.rm = TRUE), fit4 = mean(fit4, na.rm = TRUE),
    fit5 = mean(fit5, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(fit1, fit2, fit3, fit4, fit5),
               names_to = "spec", values_to = "fitted") %>%
  mutate(spec = recode(spec,
    fit1 = "(1) Debt/GDP only",
    fit2 = "(2) + Log GDP p.c.",
    fit3 = "(3) + Pop. Density",
    fit4 = "(4) + Tourism",
    fit5 = "(5) + Gas x Post Feb 22"
  ))

p_avp <- ggplot(country_avp, aes(x = fitted, y = unemp_actual)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(color = group_label), size = 2.5) +
  geom_text_repel(aes(label = country_name, color = group_label),
                  size = 2.5, max.overlaps = 15, show.legend = FALSE) +
  facet_wrap(~spec, nrow = 1) +
  scale_color_manual(values = cols_group, name = NULL) +
  labs(title = "Panel OLS: Country-Mean Actual vs. Predicted Unemployment",
       subtitle = "10 Eurozone countries, monthly 2018-2024 (predictions averaged within country)",
       x = "Predicted Unemployment (%)", y = "Actual Unemployment (%)",
       caption = paste0("Each point is one country's average over all 84 months. ",
                        "Treated: IT, EL, ES, PT, FR. Control: DE, NL, AT, FI, IE.\n",
                        "Source: Eurostat; Russian-gas share: Bruegel / IEA.")) +
  theme_paper

ggsave("figures/plot_actual_vs_predicted_5spec.pdf", p_avp, width = 16, height = 4.5)
cat("Saved: figures/plot_actual_vs_predicted_5spec.pdf\n")

did1 <- feols(unemp_rate ~ did_term,
              data = panel, cluster = ~geo)

did2 <- feols(unemp_rate ~ did_term | country_fe + time_fe,
              data = panel, cluster = ~geo)

did3 <- feols(unemp_rate ~ did_term + log_gdppc + pop_density | country_fe + time_fe,
              data = panel, cluster = ~geo)

modelsummary(
  list("(1) No FE" = did1, "(2) TWFE" = did2,
       "(3) + Controls" = did3),
  output     = "tables/did_table.tex",
  stars      = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map    = c("nobs", "r.squared", "adj.r.squared",
                 "FE: country_fe", "FE: time_fe"),
  coef_rename = c(
    did_term     = "High-Debt $\\times$ Post",
    log_gdppc    = "Log GDP per Capita",
    pop_density  = "Population Density"
  ),
  title = "Difference-in-Differences: ECB Rate Hikes and Unemployment",
  notes = paste0(
    "Standard errors clustered at the country level in parentheses. ",
    "Treatment = IT, EL, ES, PT, FR. Control = DE, NL, AT, FI, IE. ",
    "Post = July 2022 onward."
  )
)
fix_tex("tables/did_table.tex", label_tag = "tab:did")
cat("Saved: tables/did_table.tex\n")

te_plot <- trade_exposure %>%
  mutate(
    export_share_pct = export_share * 100,
    country_label    = fct_reorder(country, export_share_pct)
  )

p_te <- ggplot(te_plot, aes(x = country_label, y = export_share_pct)) +
  geom_col(fill = "#4575B4", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", export_share_pct)),
            hjust = -0.1, size = 3.4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL,
       y = "Export Share to Treated Countries (%)",
       caption = paste0("Share of goods exports destined for IT, GR, ES, PT, FR, BE (2021).\n",
                        "Source: Eurostat / Comext.")) +
  theme_paper +
  theme(legend.position = "none")

ggsave("figures/plot_trade_exposure.pdf", p_te, width = 8, height = 4.5)
cat("Saved: figures/plot_trade_exposure.pdf\n")

cat("\nDone: 3_reduced_form.R\n")
