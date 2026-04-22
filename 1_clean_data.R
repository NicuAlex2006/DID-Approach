# ══════════════════════════════════════════════════════════════════════════════
# 1_clean_data.R
# ECB Rate Hikes and Eurozone Unemployment — DiD Approach
# Downloads and cleans all data from Eurostat. Run this first.
#
# Outputs: data/panel_clean.rds, data/debt_2021.rds,
#          data/trade_exposure.rds, data/ecb_rate_monthly.rds,
#          data/panel_clean.csv
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, eurostat, lubridate)

dir.create("data", showWarnings = FALSE)

# ── Country lists ─────────────────────────────────────────────────────────────
treated <- c("IT", "GR", "ES", "PT", "FR", "BE")   # high-debt
control <- c("DE", "NL", "AT", "FI", "IE", "LU")   # low-debt
countries <- c(treated, control)

treatment_date <- as.Date("2022-07-01")

country_names <- c(
  IT = "Italy",    GR = "Greece",      ES = "Spain",
  PT = "Portugal", FR = "France",      BE = "Belgium",
  DE = "Germany",  NL = "Netherlands", AT = "Austria",
  FI = "Finland",  IE = "Ireland",     LU = "Luxembourg"
)

# ── 1. Unemployment (une_rt_m) ────────────────────────────────────────────────
cat("Downloading unemployment data...\n")
unemp_raw <- get_eurostat("une_rt_m", time_format = "date")

unemp <- unemp_raw %>%
  filter(
    geo   %in% countries,
    s_adj == "SA",
    age   == "TOTAL",
    sex   == "T",
    unit  == "PC_ACT"
  ) %>%
  rename(date = TIME_PERIOD) %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(year >= 2018, year <= 2024) %>%
  select(geo, date, year, month, unemp_rate = values)

cat("  Unemployment obs:", nrow(unemp), "\n")

# ── 2. Government debt (gov_10dd_edpt1) ───────────────────────────────────────
cat("Downloading debt data...\n")
debt_raw <- get_eurostat("gov_10dd_edpt1", time_format = "date")

debt <- debt_raw %>%
  filter(
    geo     %in% countries,
    na_item == "GD",
    sector  == "S13",
    unit    == "PC_GDP"
  ) %>%
  rename(date = TIME_PERIOD) %>%
  mutate(year = year(date)) %>%
  filter(year == 2021) %>%
  select(geo, debt_gdp_2021 = values)

cat("  Debt obs:", nrow(debt), "\n")

# ── 3. GDP per capita (nama_10_pc) ────────────────────────────────────────────
cat("Downloading GDP per capita data...\n")
gdppc_raw <- get_eurostat("nama_10_pc", time_format = "date")

gdppc <- gdppc_raw %>%
  filter(
    geo     %in% countries,
    na_item == "B1GQ",
    unit    == "CP_EUR_HAB"
  ) %>%
  rename(date = TIME_PERIOD) %>%
  mutate(year = year(date)) %>%
  filter(year >= 2018, year <= 2024) %>%
  select(geo, year, gdppc = values) %>%
  mutate(log_gdppc = log(gdppc))

cat("  GDP per capita obs:", nrow(gdppc), "\n")

# ── 4. Population density (demo_r_d3dens) ────────────────────────────────────
cat("Downloading population density data...\n")
dens_raw <- get_eurostat("demo_r_d3dens", time_format = "date")

pop_dens <- dens_raw %>%
  filter(geo %in% countries) %>%
  rename(date = TIME_PERIOD) %>%
  mutate(year = year(date)) %>%
  filter(year >= 2018, year <= 2024) %>%
  select(geo, year, pop_density = values)

cat("  Pop density obs:", nrow(pop_dens), "\n")

# ── 5. ECB policy rate ───────────────────────────────────────────────────────
# ECB main refinancing rate key dates (from ECB press releases)
# Using findInterval() to expand to monthly grid correctly:
#   rate change dates (e.g. 2022-07-27) don't fall on month-starts,
#   so a left_join+fill approach would miss them. findInterval maps each
#   month-start to the most recent rate change preceding or on that date.

ecb_rate_changes <- tibble(
  date = as.Date(c(
    "2016-03-16",  "2022-07-27",  "2022-09-14",  "2022-10-27",
    "2022-12-21",  "2023-02-08",  "2023-05-10",  "2023-06-21",
    "2023-08-02",  "2023-09-20",  "2024-06-12",  "2024-09-18",
    "2024-10-23"
  )),
  rate = c(0.00, 0.50, 1.25, 2.00, 2.50, 3.00, 3.75, 4.00,
           4.25, 4.50, 4.25, 3.65, 3.40)
)

ecb_monthly <- tibble(
  date = seq(as.Date("2018-01-01"), as.Date("2024-12-01"), by = "month")
) %>%
  mutate(
    idx  = findInterval(as.numeric(date), as.numeric(ecb_rate_changes$date)),
    rate = ifelse(idx == 0, 0.00, ecb_rate_changes$rate[idx])
  ) %>%
  select(date, rate)

cat("  ECB rate grid: ", nrow(ecb_monthly), " months\n")
cat("  Rate in Jul 2022:", ecb_monthly$rate[ecb_monthly$date == "2022-07-01"], "\n")
cat("  Rate in Oct 2022:", ecb_monthly$rate[ecb_monthly$date == "2022-10-01"], "\n")
cat("  Rate in Sep 2023:", ecb_monthly$rate[ecb_monthly$date == "2023-09-01"], "\n")

# ── 6. Intra-EU trade data (for SUTVA diagnostics) ───────────────────────────
# We compute each control country's goods export exposure to the treated group.
# Strategy: try Eurostat bilateral trade tables; fall back to gravity proxy.

cat("Downloading bilateral trade data for SUTVA diagnostics...\n")

try_eurostat <- function(table_code) {
  tryCatch(
    get_eurostat(table_code, time_format = "date"),
    error = function(e) {
      cat("  Table", table_code, "failed:", e$message, "\n")
      NULL
    }
  )
}

# Try ext_lt_intratrd first (intra-EU bilateral goods trade)
trade_raw <- try_eurostat("ext_lt_intratrd")
if (is.null(trade_raw)) {
  cat("  Trying tet00047...\n")
  trade_raw <- try_eurostat("tet00047")
}
if (is.null(trade_raw)) {
  cat("  Trying ext_lt_maineu...\n")
  trade_raw <- try_eurostat("ext_lt_maineu")
}

trade_exposure <- NULL

if (!is.null(trade_raw)) {
  cat("  Downloaded table with", nrow(trade_raw), "rows\n")
  cat("  Columns:", paste(names(trade_raw), collapse = ", "), "\n")

  cols <- names(trade_raw)
  partner_col  <- intersect(cols, c("partner", "partners", "geo_partner"))[1]
  reporter_col <- intersect(cols, c("declarant", "reporter", "geo"))[1]
  time_col     <- intersect(cols, c("TIME_PERIOD", "time", "date"))[1]
  flow_col     <- intersect(cols, c("flow", "stk_flow", "indic_et", "trade_type"))[1]

  cat("  Detected -- reporter:", reporter_col, "| partner:", partner_col,
      "| time:", time_col, "| flow:", flow_col, "\n")

  if (!is.na(partner_col) && !is.na(reporter_col)) {
    # Find export indicator
    if (!is.na(flow_col)) {
      flow_vals <- unique(trade_raw[[flow_col]])
      exp_code  <- intersect(flow_vals, c("EXP", "X", "EXPORT", "2", "export"))
      if (length(exp_code) == 0) exp_code <- flow_vals[1]
      exp_code <- exp_code[1]
      cat("  Using export flow code:", exp_code, "\n")
    }

    trade_filt <- trade_raw %>%
      rename(reporter = !!sym(reporter_col),
             partner  = !!sym(partner_col),
             time_var = !!sym(time_col))

    if (!is.na(flow_col)) {
      trade_filt <- trade_filt %>% filter(!!sym(flow_col) == exp_code)
    }

    trade_filt <- trade_filt %>%
      filter(reporter %in% control, year(time_var) == 2021)

    cat("  Filtered rows (control exporters, 2021):", nrow(trade_filt), "\n")

    if (nrow(trade_filt) > 0) {
      exp_to_treated <- trade_filt %>%
        filter(partner %in% treated) %>%
        group_by(reporter) %>%
        summarise(exp_treated = sum(values, na.rm = TRUE), .groups = "drop")

      exp_total <- trade_filt %>%
        group_by(reporter) %>%
        summarise(exp_total = sum(values, na.rm = TRUE), .groups = "drop")

      trade_exposure <- exp_to_treated %>%
        left_join(exp_total, by = "reporter") %>%
        mutate(
          export_share = exp_treated / exp_total,
          country      = reporter
        ) %>%
        select(country, export_share) %>%
        filter(!is.na(export_share), export_share > 0)

      cat("  Computed trade exposure for", nrow(trade_exposure), "countries\n")
      print(trade_exposure)
    }
  }
}

# Fallback: gravity-based proxy from national accounts
if (is.null(trade_exposure) || nrow(trade_exposure) == 0) {
  cat("  Bilateral data unavailable. Computing gravity proxy from nama_10_gdp...\n")

  gdp_comp <- try_eurostat("nama_10_gdp")

  if (!is.null(gdp_comp)) {
    exports_data <- gdp_comp %>%
      filter(
        geo     %in% control,
        na_item %in% c("P6", "B1GQ"),
        unit    == "CP_MEUR",
        year(TIME_PERIOD) == 2021
      ) %>%
      select(geo, na_item, values) %>%
      pivot_wider(names_from = na_item, values_from = values) %>%
      mutate(export_openness = P6 / B1GQ)

    # Treated countries ≈ 52% of Eurozone GDP (gravity approximation)
    trade_exposure <- exports_data %>%
      mutate(
        export_share = export_openness * 0.52,
        country      = geo
      ) %>%
      select(country, export_share)

    cat("  Gravity-based trade proxy for", nrow(trade_exposure), "countries:\n")
    print(trade_exposure)
  }
}

if (is.null(trade_exposure) || nrow(trade_exposure) == 0) {
  stop("Could not obtain trade exposure data from any Eurostat source.")
}

# ── 7. Build panel ────────────────────────────────────────────────────────────
panel <- unemp %>%
  left_join(debt,       by = c("geo" = "geo")) %>%
  left_join(gdppc,      by = c("geo", "year")) %>%
  left_join(pop_dens,   by = c("geo", "year")) %>%
  left_join(ecb_monthly, by = "date") %>%
  mutate(
    high_debt   = as.integer(geo %in% treated),
    post        = as.integer(date >= treatment_date),
    did_term    = high_debt * post,
    months_rel  = as.integer(round(
      as.numeric(difftime(date, treatment_date, units = "days")) / 30.44
    )),
    group_label = ifelse(high_debt == 1, "Treated (High-Debt)", "Control (Low-Debt)"),
    country_name = country_names[geo],
    country_fe  = as.factor(geo),
    time_fe     = as.factor(date)
  )

# Carry forward annual variables within country
panel <- panel %>%
  group_by(geo) %>%
  fill(debt_gdp_2021, gdppc, log_gdppc, pop_density, .direction = "downup") %>%
  ungroup()

# Merge trade exposure for control countries
panel <- panel %>%
  left_join(trade_exposure %>% rename(geo = country), by = "geo") %>%
  mutate(export_share = replace_na(export_share, 0))

cat("\nPanel summary:\n")
cat("  Countries:", n_distinct(panel$geo), "\n")
cat("  Months:", n_distinct(panel$date), "\n")
cat("  Observations:", nrow(panel), "\n")
cat("  Treated:", paste(treated, collapse = ", "), "\n")
cat("  Control:", paste(control, collapse = ", "), "\n")

# ── 8. Save ───────────────────────────────────────────────────────────────────
saveRDS(panel,          "data/panel_clean.rds")
saveRDS(debt,           "data/debt_2021.rds")
saveRDS(trade_exposure, "data/trade_exposure.rds")
saveRDS(ecb_monthly,    "data/ecb_rate_monthly.rds")
write_csv(panel,        "data/panel_clean.csv")

cat("\nSaved: data/panel_clean.rds, data/panel_clean.csv\n")
cat("Done: 1_clean_data.R\n")
