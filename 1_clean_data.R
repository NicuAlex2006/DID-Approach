rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, eurostat, lubridate, rvest)

dir.create("data", showWarnings = FALSE)

core_treated <- c("IT", "EL", "ES", "PT", "FR")
core_control <- c("DE", "NL", "AT", "FI", "IE")
core <- c(core_treated, core_control)

ext_extra <- c("BE", "LU",
               "CY", "EE", "HR", "LT", "LV", "MT",
               "SI", "SK")
countries <- c(core, ext_extra)

treated <- core_treated
control <- core_control

treatment_date <- as.Date("2022-07-01")

country_names <- c(
  IT = "Italy",    EL = "Greece",      ES = "Spain",
  PT = "Portugal", FR = "France",
  DE = "Germany",  NL = "Netherlands", AT = "Austria",
  FI = "Finland",  IE = "Ireland",
  BE = "Belgium",  LU = "Luxembourg",  CY = "Cyprus",
  EE = "Estonia",  HR = "Croatia",     LT = "Lithuania",
  LV = "Latvia",   MT = "Malta",       SI = "Slovenia",
  SK = "Slovakia"
)

ez_join_year <- c(
  IT = 1999, EL = 2001, ES = 1999, PT = 1999, FR = 1999,
  DE = 1999, NL = 1999, AT = 1999, FI = 1999, IE = 1999,
  BE = 1999, LU = 1999, CY = 2008, EE = 2011, HR = 2023,
  LT = 2015, LV = 2014, MT = 2008, SI = 2007, SK = 2009
)

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

cat("Downloading debt data...\n")
debt_raw <- get_eurostat("gov_10dd_edpt1", time_format = "date")

debt_long <- debt_raw %>%
  filter(
    geo     %in% countries,
    na_item == "GD",
    sector  == "S13",
    unit    == "PC_GDP"
  ) %>%
  rename(date = TIME_PERIOD) %>%
  mutate(year = year(date)) %>%
  filter(year %in% c(2019, 2021)) %>%
  select(geo, year, debt_gdp = values) %>%
  pivot_wider(names_from = year, values_from = debt_gdp,
              names_prefix = "debt_gdp_")

debt <- debt_long %>%
  mutate(delta_debt_19_21 = debt_gdp_2021 - debt_gdp_2019) %>%
  select(geo, debt_gdp_2021, debt_gdp_2019, delta_debt_19_21)

cat("  Debt obs:", nrow(debt), "\n")

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

cat("Downloading population density data...\n")
dens_raw <- get_eurostat("demo_r_d3dens", time_format = "date")

pop_dens <- dens_raw %>%
  filter(geo %in% countries) %>%
  rename(date = TIME_PERIOD) %>%
  mutate(year = year(date)) %>%
  filter(year >= 2018, year <= 2024) %>%
  select(geo, year, pop_density = values)

cat("  Pop density obs:", nrow(pop_dens), "\n")

cat("Downloading sectoral GVA (nama_10_a64) for tourism share...\n")
gva_raw <- tryCatch(
  get_eurostat("nama_10_a64", time_format = "date"),
  error = function(e) { cat("  nama_10_a64 failed:", e$message, "\n"); NULL }
)

if (!is.null(gva_raw)) {
  tour_share <- gva_raw %>%
    filter(
      geo     %in% countries,
      na_item == "B1G",
      unit    == "CP_MEUR",
      nace_r2 %in% c("I", "TOTAL")
    ) %>%
    rename(date = TIME_PERIOD) %>%
    mutate(year = year(date)) %>%
    filter(year >= 2018, year <= 2021) %>%
    select(geo, year, nace_r2, values) %>%
    pivot_wider(names_from = nace_r2, values_from = values) %>%
    mutate(tourism_share = 100 * I / TOTAL) %>%
    group_by(geo) %>%
    summarise(tourism_share = mean(tourism_share, na.rm = TRUE),
              .groups = "drop")
  cat("  Tourism share obs:", nrow(tour_share), "\n")
} else {
  tour_share <- tibble(geo = countries, tourism_share = NA_real_)
}

cat("Downloading population by age group (demo_pjangroup)...\n")
demo_raw <- tryCatch(
  get_eurostat("demo_pjangroup", time_format = "date"),
  error = function(e) { cat("  demo_pjangroup failed:", e$message, "\n"); NULL }
)

if (!is.null(demo_raw)) {
  avail_ages <- unique(demo_raw$age)

  if (all(c("Y15-19", "Y20-24") %in% avail_ages)) {
    youth_ages <- c("Y15-19", "Y20-24")
  } else if ("Y15-24" %in% avail_ages) {
    youth_ages <- "Y15-24"
  } else {
    youth_ages <- character(0)
  }

  if (length(youth_ages) > 0 && "TOTAL" %in% avail_ages) {
    youth_share <- demo_raw %>%
      filter(
        geo %in% countries,
        sex == "T",
        age %in% c(youth_ages, "TOTAL")
      ) %>%
      rename(date = TIME_PERIOD) %>%
      mutate(
        year = year(date),
        bucket = ifelse(age == "TOTAL", "total", "youth")
      ) %>%
      filter(year >= 2018, year <= 2021) %>%
      group_by(geo, year, bucket) %>%
      summarise(pop = sum(values, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = bucket, values_from = pop) %>%
      mutate(youth_share = 100 * youth / total) %>%
      group_by(geo) %>%
      summarise(youth_share = mean(youth_share, na.rm = TRUE),
                .groups = "drop")
    cat("  Youth share obs:", nrow(youth_share), "\n")
  } else {
    youth_share <- tibble(geo = countries, youth_share = NA_real_)
  }
} else {
  youth_share <- tibble(geo = countries, youth_share = NA_real_)
}

gas_baseline <- core

en_to_iso <- c(
  "Austria"      = "AT", "Germany"      = "DE", "Finland"  = "FI",
  "France"       = "FR", "Greece"       = "EL", "Ireland"  = "IE",
  "Italy"        = "IT", "Netherlands"  = "NL", "Portugal" = "PT",
  "Spain"        = "ES",
  "Belgium"      = "BE", "Luxembourg"   = "LU", "Czech Republic" = "CZ",
  "Czechia"      = "CZ", "Slovakia"     = "SK", "Slovenia" = "SI",
  "Hungary"      = "HU", "Poland"       = "PL", "Estonia"  = "EE",
  "Latvia"       = "LV", "Lithuania"    = "LT", "Bulgaria" = "BG",
  "Croatia"      = "HR", "Romania"      = "RO", "Denmark"  = "DK",
  "Sweden"       = "SE", "Cyprus"       = "CY", "Malta"    = "MT"
)

cat("Downloading Russian-gas imports (nrg_ti_gas)...\n")
gas_imp_raw <- tryCatch(
  get_eurostat("nrg_ti_gas", time_format = "date"),
  error = function(e) { cat("  nrg_ti_gas failed:", e$message, "\n"); NULL }
)

cat("Downloading total gas supply (nrg_cb_gas)...\n")
gas_sup_raw <- tryCatch(
  get_eurostat("nrg_cb_gas", time_format = "date"),
  error = function(e) { cat("  nrg_cb_gas failed:", e$message, "\n"); NULL }
)

russian_gas <- NULL
if (!is.null(gas_imp_raw) && !is.null(gas_sup_raw)) {
  imp_cols <- names(gas_imp_raw)
  sup_cols <- names(gas_sup_raw)
  partner_col <- intersect(imp_cols, c("partner", "partners"))[1]
  siec_imp    <- intersect(imp_cols, c("siec"))[1]
  siec_sup    <- intersect(sup_cols, c("siec"))[1]
  bal_col     <- intersect(sup_cols, c("nrg_bal"))[1]

  if (!is.na(partner_col) && !is.na(siec_imp) &&
      !is.na(siec_sup) && !is.na(bal_col)) {

    imp_units <- unique(gas_imp_raw$unit)
    sup_units <- unique(gas_sup_raw$unit)
    candidate <- intersect(c("TJ_GCV", "MIO_M3"), intersect(imp_units, sup_units))
    use_unit  <- if (length(candidate) > 0) candidate[1]
                 else intersect(imp_units, sup_units)[1]

    imp_ru <- gas_imp_raw %>%
      filter(
        geo                %in% gas_baseline,
        !!sym(partner_col) == "RU",
        !!sym(siec_imp)    == "G3000",
        unit               == use_unit
      ) %>%
      rename(date = TIME_PERIOD) %>%
      mutate(year = year(date)) %>%
      filter(year == 2021) %>%
      group_by(geo) %>%
      summarise(ru_imports = sum(values, na.rm = TRUE), .groups = "drop")

    bal_avail <- intersect(unique(gas_sup_raw[[bal_col]]),
                            c("GAE", "IC_OBS", "GIC", "AFC", "IC_CAL_MG"))

    sup_total <- NULL
    for (bal_try in bal_avail) {
      tmp <- gas_sup_raw %>%
        filter(
          geo             %in% gas_baseline,
          !!sym(siec_sup) == "G3000",
          !!sym(bal_col)  == bal_try,
          unit            == use_unit
        ) %>%
        rename(date = TIME_PERIOD) %>%
        mutate(year = year(date)) %>%
        filter(year == 2021) %>%
        group_by(geo) %>%
        summarise(total_supply = sum(values, na.rm = TRUE), .groups = "drop")
      if (nrow(tmp) >= 5) {
        sup_total <- tmp
        break
      }
    }

    if (!is.null(sup_total)) {
      candidate_gas <- tibble(geo = gas_baseline) %>%
        left_join(imp_ru,    by = "geo") %>%
        left_join(sup_total, by = "geo") %>%
        mutate(
          ru_imports        = replace_na(ru_imports, 0),
          russian_gas_share = ifelse(!is.na(total_supply) & total_supply > 0,
                                      100 * ru_imports / total_supply,
                                      NA_real_)
        ) %>%
        select(geo, russian_gas_share)

      n_ok <- sum(!is.na(candidate_gas$russian_gas_share))
      if (n_ok >= 5) {
        russian_gas <- candidate_gas %>%
          mutate(russian_gas_share = replace_na(russian_gas_share, 0))
      }
    }
  }
}

if (is.null(russian_gas)) {
  cat("Falling back to Wikipedia for Russian-gas data...\n")
  wiki_url  <- "https://en.wikipedia.org/wiki/Russia_in_the_European_energy_sector"
  wiki_page <- tryCatch(read_html(wiki_url),
                        error = function(e) { cat("  rvest failed:", e$message, "\n"); NULL })

  pct_extract <- function(x) suppressWarnings(
    as.numeric(str_extract(as.character(x), "[0-9]+(\\.[0-9]+)?"))
  )

  if (!is.null(wiki_page)) {
    wiki_tables <- wiki_page %>%
      html_elements("table.wikitable") %>% html_table(fill = TRUE)

    best_tbl <- NULL; best_score <- 0
    for (tbl in wiki_tables) {
      if (ncol(tbl) < 2 || nrow(tbl) < 4) next
      country_hits <- sapply(tbl, function(col) {
        sum(str_squish(as.character(col)) %in% names(en_to_iso))
      })
      if (max(country_hits) < 4) next
      country_col <- which.max(country_hits)
      pct_hits <- sapply(seq_along(tbl), function(j) {
        if (j == country_col) return(-1)
        v <- pct_extract(tbl[[j]])
        sum(!is.na(v) & v >= 0 & v <= 100)
      })
      if (max(pct_hits) < 4) next
      share_col <- which.max(pct_hits)
      score <- country_hits[country_col] + pct_hits[share_col]
      if (score > best_score) {
        best_score <- score
        best_tbl   <- tibble(country_en = str_squish(as.character(tbl[[country_col]])),
                              russian_gas_share = pct_extract(tbl[[share_col]]))
      }
    }

    if (!is.null(best_tbl)) {
      russian_gas <- best_tbl %>%
        mutate(geo = en_to_iso[country_en]) %>%
        filter(!is.na(geo), geo %in% gas_baseline) %>%
        group_by(geo) %>%
        summarise(russian_gas_share = mean(russian_gas_share, na.rm = TRUE),
                  .groups = "drop")
      russian_gas <- tibble(geo = gas_baseline) %>%
        left_join(russian_gas, by = "geo") %>%
        mutate(russian_gas_share = replace_na(russian_gas_share, 0))
    }
  }
}

if (is.null(russian_gas) || nrow(russian_gas) == 0) {
  stop("Could not fetch Russian-gas-share data from any source (Eurostat / Wikipedia).")
}

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

trade_raw <- try_eurostat("ext_lt_intratrd")
if (is.null(trade_raw)) trade_raw <- try_eurostat("tet00047")
if (is.null(trade_raw)) trade_raw <- try_eurostat("ext_lt_maineu")

trade_exposure <- NULL

if (!is.null(trade_raw)) {
  cols <- names(trade_raw)
  partner_col  <- intersect(cols, c("partner", "partners", "geo_partner"))[1]
  reporter_col <- intersect(cols, c("declarant", "reporter", "geo"))[1]
  time_col     <- intersect(cols, c("TIME_PERIOD", "time", "date"))[1]
  flow_col     <- intersect(cols, c("flow", "stk_flow", "indic_et", "trade_type"))[1]

  if (!is.na(partner_col) && !is.na(reporter_col)) {
    if (!is.na(flow_col)) {
      flow_vals <- unique(trade_raw[[flow_col]])
      exp_code  <- intersect(flow_vals, c("EXP", "X", "EXPORT", "2", "export"))
      if (length(exp_code) == 0) exp_code <- flow_vals[1]
      exp_code <- exp_code[1]
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
    }
  }
}

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

    trade_exposure <- exports_data %>%
      mutate(
        export_share = export_openness * 0.52,
        country      = geo
      ) %>%
      select(country, export_share)
  }
}

if (is.null(trade_exposure) || nrow(trade_exposure) == 0) {
  stop("Could not obtain trade exposure data from any Eurostat source.")
}

panel <- unemp %>%
  left_join(debt,        by = c("geo" = "geo")) %>%
  left_join(gdppc,       by = c("geo", "year")) %>%
  left_join(pop_dens,    by = c("geo", "year")) %>%
  left_join(tour_share,  by = "geo") %>%
  left_join(youth_share, by = "geo") %>%
  left_join(russian_gas, by = "geo") %>%
  left_join(ecb_monthly, by = "date") %>%
  mutate(
    high_debt        = as.integer(geo %in% treated),
    post             = as.integer(date >= treatment_date),
    post_feb2022     = as.integer(date >= as.Date("2022-02-01")),
    did_term         = high_debt * post,
    gas_post_feb22   = russian_gas_share * post_feb2022,
    months_rel       = as.integer(round(
      as.numeric(difftime(date, treatment_date, units = "days")) / 30.44
    )),
    group_label      = ifelse(high_debt == 1, "Treated (High-Debt)", "Control (Low-Debt)"),
    country_name     = country_names[geo],
    country_fe       = as.factor(geo),
    time_fe          = as.factor(date)
  )

panel <- panel %>%
  group_by(geo) %>%
  fill(debt_gdp_2021, debt_gdp_2019, delta_debt_19_21,
       gdppc, log_gdppc, pop_density,
       tourism_share, youth_share, russian_gas_share,
       .direction = "downup") %>%
  ungroup()

panel <- panel %>%
  left_join(trade_exposure %>% rename(geo = country), by = "geo") %>%
  mutate(export_share = replace_na(export_share, 0))

cat("\nPanel summary:\n")
cat("  Countries:", n_distinct(panel$geo), "\n")
cat("  Months:", n_distinct(panel$date), "\n")
cat("  Observations:", nrow(panel), "\n")

saveRDS(panel,          "data/panel_clean.rds")
saveRDS(debt,           "data/debt_2021.rds")
saveRDS(trade_exposure, "data/trade_exposure.rds")
saveRDS(ecb_monthly,    "data/ecb_rate_monthly.rds")
write_csv(panel,        "data/panel_clean.csv")

cat("\nSaved: data/panel_clean.rds, data/panel_clean.csv\n")
cat("Done: 1_clean_data.R\n")
