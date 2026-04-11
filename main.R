rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  modelsummary,
  fixest,
  readxl,
  ggrepel,
  patchwork
)

dat_panel <- read_excel("state_panel_alcohol.xlsx", sheet = "panel", skip = 3, n_max = 24) %>%
  rename_with(function(x) gsub("\n.*", "", x))

dat_ols <- read_excel("state_panel_alcohol.xlsx", sheet = "ols", skip = 3, n_max = 50) %>%
  rename_with(function(x) gsub("\n.*", "", x))

dat_panel <- dat_panel %>%
  mutate(
    did        = treated * post,
    log_income = log(income_pc),
    state      = as.factor(state),
    year       = as.integer(year)
  )

dat_ols <- dat_ols %>%
  mutate(log_income = log(income_pc))

vars_table <- dat_panel %>%
  select(
    `Fatality rate (per 100k)` = fatality_rate,
    `Treated (Maryland=1)`     = treated,
    `Post-2011`                = post,
    `Unemployment rate (%)`    = unemployment,
    `Per capita income ($)`    = income_pc,
    `Population density`       = pop_density
  )

datasummary_skim(
  vars_table,
  output = "summary_stats.tex",
  title  = "Summary Statistics - Maryland and Virginia, 2005-2016"
)

p_trends <- ggplot(dat_panel,
                   aes(x = year, y = fatality_rate,
                       color = state, linetype = state)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2011, linetype = "dashed",
             color = "black", linewidth = 0.7) +
  annotate("text", x = 2011.2, y = max(dat_panel$fatality_rate) * 0.98,
           label = "Tax increase\n(July 2011)", hjust = 0, size = 3.2) +
  scale_color_manual(values = c("Maryland" = "firebrick",
                                "Virginia"  = "steelblue")) +
  scale_x_continuous(breaks = 2005:2016) +
  labs(
    title    = "Alcohol-Related Traffic Fatality Rates: Maryland vs Virginia",
    subtitle = "2005-2016",
    x        = "Year",
    y        = "Fatalities per 100,000 inhabitants",
    color    = NULL, linetype = NULL
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plot_parallel_trends.pdf", p_trends, width = 8, height = 5)

ols1 <- feols(fatality_rate ~ beer_tax,
              data = dat_ols, vcov = "HC1")

ols2 <- feols(fatality_rate ~ beer_tax + pct_religious,
              data = dat_ols, vcov = "HC1")

ols3 <- feols(fatality_rate ~ beer_tax + pct_religious + pop_density,
              data = dat_ols, vcov = "HC1")

ols4 <- feols(fatality_rate ~ beer_tax + pct_religious + pop_density +
                unemployment + log_income,
              data = dat_ols, vcov = "HC1")

modelsummary(
  list("(1)" = ols1, "(2)" = ols2, "(3)" = ols3, "(4)" = ols4),
  coef_map = c(
    "beer_tax"      = "Beer Tax (\\$/gallon)",
    "pct_religious" = "\\% Highly Religious",
    "pop_density"   = "Population Density",
    "unemployment"  = "Unemployment Rate",
    "log_income"    = "Log Per Capita Income",
    "(Intercept)"   = "Constant"
  ),
  gof_map = list(
    list(raw = "nobs",          clean = "Observations", fmt = 0),
    list(raw = "r.squared",     clean = "$R^2$",        fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. $R^2$",   fmt = 3)
  ),
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  title  = "OLS Regressions: Beer Tax and Traffic Fatalities (50 States, 2008)",
  notes  = "Heteroskedasticity-robust standard errors (HC1) in parentheses.",
  output = "ols_table.tex"
)

region_map <- c(
  "Connecticut"    = "Northeast", "Maine"          = "Northeast",
  "Massachusetts"  = "Northeast", "New Hampshire"  = "Northeast",
  "Rhode Island"   = "Northeast", "Vermont"        = "Northeast",
  "New Jersey"     = "Northeast", "New York"       = "Northeast",
  "Pennsylvania"   = "Northeast",
  "Illinois"       = "Midwest",   "Indiana"        = "Midwest",
  "Michigan"       = "Midwest",   "Ohio"           = "Midwest",
  "Wisconsin"      = "Midwest",   "Iowa"           = "Midwest",
  "Kansas"         = "Midwest",   "Minnesota"      = "Midwest",
  "Missouri"       = "Midwest",   "Nebraska"       = "Midwest",
  "North Dakota"   = "Midwest",   "South Dakota"   = "Midwest",
  "Delaware"       = "South",     "Florida"        = "South",
  "Georgia"        = "South",     "Maryland"       = "South",
  "North Carolina" = "South",     "South Carolina" = "South",
  "Virginia"       = "South",     "West Virginia"  = "South",
  "Alabama"        = "South",     "Kentucky"       = "South",
  "Mississippi"    = "South",     "Tennessee"      = "South",
  "Arkansas"       = "South",     "Louisiana"      = "South",
  "Oklahoma"       = "South",     "Texas"          = "South",
  "Arizona"        = "West",      "Colorado"       = "West",
  "Idaho"          = "West",      "Montana"        = "West",
  "Nevada"         = "West",      "New Mexico"     = "West",
  "Utah"           = "West",      "Wyoming"        = "West",
  "Alaska"         = "West",      "California"     = "West",
  "Hawaii"         = "West",      "Oregon"         = "West",
  "Washington"     = "West"
)

make_avp <- function(model, label, data) {
  r2_val   <- round(as.numeric(r2(model)["r2"]), 3)
  rmse_val <- round(sqrt(mean(residuals(model)^2, na.rm = TRUE)), 3)

  plot_data <- data %>%
    mutate(
      predicted = predict(model),
      actual    = fatality_rate,
      region    = region_map[as.character(state)],
      residual  = actual - predicted
    )

  ggplot(plot_data, aes(x = predicted, y = actual)) +
    geom_segment(aes(xend = predicted, yend = predicted),
                 color = "grey70", linewidth = 0.35, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "black", linewidth = 0.6) +
    geom_point(aes(color = region), size = 2.8, alpha = 0.9) +
    geom_text_repel(aes(label = state, color = region),
                    size          = 2.2,
                    max.overlaps  = 18,
                    segment.size  = 0.3,
                    segment.alpha = 0.45,
                    show.legend   = FALSE) +
    annotate("text",
             x = -Inf, y = Inf, hjust = -0.08, vjust = 1.5,
             label    = paste0("R\u00b2 = ", r2_val, "\nRMSE = ", rmse_val),
             size     = 2.9,
             fontface = "italic",
             color    = "grey25") +
    scale_color_manual(
      values = c("Northeast" = "#E41A1C", "Midwest" = "#377EB8",
                 "South"     = "#4DAF4A", "West"    = "#FF7F00"),
      name   = "Census Region"
    ) +
    labs(
      title = label,
      x     = "Predicted fatality rate (per 100k)",
      y     = "Actual fatality rate (per 100k)"
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position  = "bottom",
      legend.key.size  = unit(0.4, "cm"),
      legend.title     = element_text(size = 8, face = "bold"),
      legend.text      = element_text(size = 8),
      plot.title       = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank()
    )
}

avp1 <- make_avp(ols1, "(1) Tax only",          dat_ols)
avp2 <- make_avp(ols2, "(2) + Religiosity",     dat_ols)
avp3 <- make_avp(ols3, "(3) + Density",         dat_ols)
avp4 <- make_avp(ols4, "(4) + Unemp. + Income", dat_ols)

combined_avp <- (avp1 + avp2) / (avp3 + avp4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("plot_actual_vs_predicted.pdf", combined_avp, width = 12, height = 10)

did1 <- feols(
  fatality_rate ~ did + treated + post,
  data = dat_panel,
  vcov = "HC1"
)

did2 <- feols(
  fatality_rate ~ did | state + year,
  data = dat_panel,
  vcov = "HC1"
)

did3 <- feols(
  fatality_rate ~ did + unemployment + log_income | state + year,
  data = dat_panel,
  vcov = "HC1"
)

modelsummary(
  list(
    "Basic DiD"           = did1,
    "DiD + FE"            = did2,
    "DiD + FE + Controls" = did3
  ),
  coef_map = c(
    "did"          = "Treated $\\times$ Post (DiD)",
    "treated"      = "Treated (Maryland)",
    "post"         = "Post-2011",
    "unemployment" = "Unemployment Rate",
    "log_income"   = "Log Per Capita Income"
  ),
  gof_map = list(
    list(raw = "nobs",          clean = "Observations", fmt = 0),
    list(raw = "r.squared",     clean = "$R^2$",        fmt = 3),
    list(raw = "adj.r.squared", clean = "Adj. $R^2$",   fmt = 3),
    list(raw = "FE: state",     clean = "State FE",     fmt = 0),
    list(raw = "FE: year",      clean = "Year FE",      fmt = 0)
  ),
  stars  = c("*" = .1, "**" = .05, "***" = .01),
  title  = "Difference-in-Differences: Maryland's 2011 Tax Increase and Traffic Fatalities",
  notes  = "Heteroskedasticity-robust standard errors (HC1) in parentheses.",
  output = "did_table.tex"
)
