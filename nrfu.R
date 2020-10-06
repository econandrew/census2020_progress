library(tidyverse)
library(purrr)
library(pdftools)
library(fuzzyjoin)
library(lubridate)
library(geofacet)
library(readxl)

############################################################################## #
# Plot settings
################################################################################

ggplot2::theme_set(theme_minimal())
plot_device <- partial(png, res = 96)
plot_ext <- ".png"

############################################################################## #
# Constants - some dates that are important
################################################################################

aug31 <- as.Date("2020-08-31")
sep30 <- as.Date("2020-09-30")
oct5 <- as.Date("2020-10-05")
oct31 <- as.Date("2020-10-31")

############################################################################## #
# Load and parse data
################################################################################

# NRFU reports by state ########################################################

parse_nrfu_pdf <- function(filename) {
  message(paste("Parsing", filename))
  
  # Get page 1 data
  strings <- pdf_data(filename)[[1]]
  
  strings <- strings %>% filter(width > 2)
  
  report_date_y = strings %>% filter(text == "date:") %>% pull(y)
  report_date = strings %>%
    filter(y == report_date_y) %>%
    arrange(x) %>%
    pull(text) %>%
    last() %>%
    as.Date(format = "%m/%d/%Y")
  
  as_of_date_y = strings %>% filter(text == "percentage") %>% pull(y)
  as_of_date = strings %>%
    filter(y == as_of_date_y) %>%
    arrange(x) %>%
    pull(text) %>%
    nth(3) %>%
    as.Date(format = "%m/%d/%Y")
  
  col_def <- tribble(
    ~col,         ~start, ~end,
    "state",            50,  220,
    "self_response",   221,  320,
    "nrfu",            321,  450,
    "enumerated",      451,  540
  )
  
  row_def <- strings %>%
    filter(y >= 161, y <= 619) %>%
    select(y, height) %>%
    unique() %>%
    arrange(y) %>%
    transmute(
      start = y,
      end = replace_na(lead(start) - 1, 619)
    ) %>%
    mutate(row = row_number())
  
  strings_grid <- strings %>%
    select(-width, -height, -space) %>%
    fuzzy_inner_join(col_def, by = c("x" = "start", "x" = "end"), match_fun = list(`>=`, `<=`)) %>%
    fuzzy_inner_join(row_def, by = c("y" = "start", "y" = "end"), match_fun = list(`>=`, `<=`)) %>%
    select(-start.x, -start.y, -end.x, -end.y, -x, -y)
  
  strings_grid <- strings_grid %>% filter(trimws(text) != ".")
  
  nrfu <- strings_grid %>%
    pivot_wider(names_from = col, values_from = text, values_fn = function(v) { paste(v, collapse = " ")}) %>%
    mutate(state = trimws(state, whitespace="[ \t\r\n.]"))
  
  nrfu <- nrfu %>% transmute(
    state,
    report_date = report_date,
    as_of_date = as_of_date,
    self_response = as.numeric(trimws(self_response, whitespace="%")),
    nrfu = as.numeric(trimws(nrfu, whitespace="%")),
    enumerated = as.numeric(trimws(enumerated, whitespace="%"))
  )
  
  nrfu
}

if (!file.exists("working/progress.csv")) {
  filenames <- Sys.glob("inputs/census2020_nrfu_pdf/*.pdf")
  nrfu_reports <- map(filenames, parse_nrfu_pdf)
  progress <- bind_rows(nrfu_reports)
  write_csv(progress, "working/progress.csv")
} else {
  # Delete this file to rebuild from PDFs
  progress <- read_csv("working/progress.csv")
}

progress_first_date <- min(progress$as_of_date)
progress_last_date <- max(progress$as_of_date)

# Population ###################################################################

# We include three possible true population scenarios here for Apr 1 2020

# But first we file away the Apr 1 2010 census population
popest_raw <- read_csv("inputs/uscb_population/nst-est2019-alldata.csv")
pop_2010_apr1 <- popest_raw %>%
  filter(STATE != "00") %>%
  select(
    state = NAME,
    population = CENSUS2010POP
  )

# Variation 1: linear extrap. from statewise 2018-2019 trend. Rough and ready.
popest <- popest_raw %>%
  filter(STATE != "00") %>%
  select(
    state = NAME,
    POPESTIMATE2010:POPESTIMATE2019
  ) %>%
  pivot_longer(POPESTIMATE2010:POPESTIMATE2019, names_to = "year", values_to = "population") %>%
  mutate(year = as.Date(paste(str_replace(year, "POPESTIMATE", ""),7,1,sep="-")))

pop_apr1_whitby <- popest_raw %>%
  filter(STATE != "00") %>%
  transmute(
    state = NAME,
    variant = "whitby",
    population = POPESTIMATE2019 + (POPESTIMATE2019 - POPESTIMATE2018)*0.75 # 9 months in
  )

# Variations 2 and 3: The Urban and Brookings population estimates from the ASA dataset
# Neither have DC so we add it back in from the sources
pop_apr1_asa <- read_csv("inputs/asa_auerbach/census_population.csv") %>%
  select(
    state = State,
    brookings = `Brooking Population 2020`,
    urban = `Urban Population 2020`
  ) %>%
  bind_rows(data.frame(
    state = "District of Columbia",
    brookings = (1 + .178) * # 17.8% growth on the Brookings Map
      (pop_2010_apr1 %>% filter(state == "District of Columbia") %>% pull(population)),
    urban = 719800 # Directly from the report
  )) %>%
  pivot_longer(c(brookings, urban), names_to = "variant", values_to = "population")

# Bundle them all together
pop_apr1_variants <- pop_apr1_asa %>% bind_rows(pop_apr1_whitby)

# State-level operational/error data ###########################################

# Mail response rates by state, from 2010 (~= self-response 2020)
mail_response_2010 <- read_csv("inputs/census2010_response_rates/mail_response.csv")

# Census coverage components by state, from 2010 based on post-enumeration survey
census_coverage_2010 <- read_csv("inputs/census2010_ccm_reports/2010_ccm_g4_table5.csv") %>%
  left_join(data.frame(state = state.name, state_abbr = state.abb))

# Urban Institute (Elliott 2019, Table 2) projected miscounts
urban_miscount_2020_variants <- read_csv("inputs/urbaninst_elliot_2019/urban_elliott2019_table2.csv") %>%
  select(state, low, medium, high) %>%
  pivot_longer(c(low, medium, high), names_to = "variant", values_to = "ui_undercount") %>%
  mutate(ui_undercount = -ui_undercount) # Warning: we use the opposite convention to Urban hence minus

# State-level Medicaid data ####################################################

# BEA personal income data, which enters the FMAP formula
# Checked: 2020:Q1 does not seem to show any Covid effect (We could argue about
# whether we/ASA authors should use a quarter that does. But this is a ten-year
# horizon so I think this a fair choice.)
bea_personal_income <- read_csv("inputs/asa_auerbach/bea_income.csv") %>%
  filter(Description == "Personal income (millions of dollars, seasonally adjusted)") %>% 
  transmute(
    state = State,
    pers_income = 1e6 * `2020:Q1`
  )

# Based on Andrew Reamer's work at GWU, which finds overall $1.5T annually directed by census
reamer_fy15_expenditures <- read_csv("inputs/asa_auerbach/reamer_table_3_1.csv") %>% 
  transmute(
    state = State,
    medicaid = 100 * `Medicaid Traditional` / FMAP + 100 * `Medicaid Medicare Part D Clawback` / EFMAP
  )

# The Reamer numbers used in the ASA model are FY15 which is quite a long time ago
# CMS publishes up to FY18 in a form that's a bit messy. It's hard to get the totals
# for the FY15 report in this series to match the Reamer FY 15 numbers, but it seems
# like Reamer changed the source for his data since then. The FY17 version of this
# report does match the state-by-state totals in Brief #5 from Reamer's project so
# I think we're safe to use the same for FY18.

# Although the way FMAP applies to expenses is actually quite complicated, we assume
# that it applies to all but administrative expenses, roughly per
# https://www.macpac.gov/subtopic/matching-rates/
medicaid_filename <- "inputs/cms_medicaid_expenditure/financial-management-report-fy2018/FY 2018 FMR NET EXPENDITURES.xlsx"
fy18_medicaid_expenses <-  state.name  %>% map(function(state) {
      sheet_map <- read_xlsx(medicaid_filename, sheet = paste0("MAP - ", state), skip = 6)
      sheet_adm <- read_xlsx(medicaid_filename, sheet = paste0("ADM - ", state), skip = 6)
      
      map_total <- sheet_map %>% filter(`Service Category` == "Total Net Expenditures") %>% pull(`Total \r\n Computable`)
      map_federal <- sheet_map %>% filter(`Service Category` == "Total Net Expenditures") %>% pull(`Federal \r\n Share`)
      adm_total <- sheet_adm %>% filter(`Service Category` == "Total Net Expenditures") %>% pull(`Total \r\n Computable`)
      adm_federal <- sheet_adm %>% filter(`Service Category` == "Total Net Expenditures") %>% pull(`Federal Share`)
  
      data.frame(state, map_total, map_federal, adm_total, adm_federal)
  }) %>%
  bind_rows()

fy18_medicaid_expenses <- fy18_medicaid_expenses %>% mutate(
  medicaid = map_total # Don't include administrative expenses adm_* in final total
)

# To check these totals, we can also infer a medicaid state expenditure by scaling
# the federal portion back up by the FMAP, in case of inclusion/exclusion errors.
fmap_rates <- read_xlsx(
  "inputs/macstats/EXHIBIT-6.-Federal-Medical-Assistance-Percentages-and-Enhanced-Federal-Medical-Assistance-Percentages-by-State-FYs-2018–2021.xlsx",
  skip=4,
  col_names=c("state",
             "FMAP_fy18", "FMAP_fy19", "FMAP_fy20", "FMAP_fy20_covid", "FMAP_fy21",
             "EFMAP_fy18", "EFMAP_fy19", "EFMAP_fy20", "EFMAP_fy20_covid", "EFMAP_fy21"
            )
)

fy18_medicaid_expenses_inferred <- fy18_medicaid_expenses %>%
  left_join(fmap_rates) %>%
  mutate(
    medicaid = map_federal / FMAP_fy18
  )

############################################################################## #
# Parameters to switch
################################################################################

# Which population projection should we use? Apportionment is sensitive to this
# (apportionment is sensitive to everything by nature), but Medicaid much less so
pop_apr1 <- pop_apr1_variants %>% filter(variant == "urban")

# Highly debatable, but since self-response has basically matched 2010, we go with "low risk"
# as our October baseline. This adjusts 2010 only for demographic changes in states.
urban_miscount_2020 <- urban_miscount_2020_variants %>% filter(variant == "low")

# We can use a variety of Medicaid expenditure alternatives:
#   (a) reamer_fy15_expenditures - the original ASA version, but old
#   (b) fy18_medicaid_expenses - most up to date, larger impacts
#   (c) fy18_medicaid_expenses_inferred - even larger impacts
# We choose up-to-date but conservative and use (b)
medicaid_expenses <- fy18_medicaid_expenses

############################################################################## #
# Initial exploration & viz
################################################################################

# Total enumeration trends by state
# Note: y-axis limits mean some early points are hidden for some states
plt_progress <- ggplot(progress, aes(as_of_date, enumerated)) +
  geom_point(size = 0.1) +
  geom_vline(xintercept = sep30, color = "red", linetype = 2, size = 0.25) +
  facet_wrap(vars(state)) +
  scale_y_continuous(breaks = 0:10*10, limits = c(70, 100), minor_breaks = 0:20*5) +
  scale_x_date(
    breaks = c(aug31, sep30, oct31),
    limits = c(progress_first_date, oct31),
    date_labels = "%m/%d"
  )

plot_device(paste0("outputs/plt_progress",plot_ext), units = "in", width = 10, height = 8)
plt_progress
dev.off()

# Same, but with three potential simple projection models 
plt_progress_proj_simple <- plt_progress +
  geom_smooth(
    method = "lm", fullrange = TRUE, se = FALSE,
    color = "green", size = 0.25
  ) +
  geom_smooth(
    method = "nls", 
    formula = y ~ 100 / (1 + exp(-alpha - beta * x)), 
    method.args = list(start=c(alpha = 1, beta = 1e-10)), 
    se = FALSE,
    fullrange = TRUE,
    color = "orange",
    size = 0.25
  ) +
  geom_smooth(
    data = . %>% filter(as_of_date > max(as_of_date) - 5),
    method = "lm", fullrange = TRUE, se = FALSE,
    color = "blue", size = 0.25
  )
  
plot_device(paste0("outputs/plt_progress_proj_simple",plot_ext), units = "in", width = 10, height = 8)
plt_progress_proj_simple
dev.off()

############################################################################## #
# Increments model (!= ASA paper)
################################################################################

# This model predicts a day's incremental progress based on the previous days
# remaining unresolved housing units, so calculate those two variables
progress <- progress %>%
  group_by(state) %>%
  arrange(as_of_date) %>%
  mutate(
    daily_increment = enumerated - lag(enumerated),
    lag_unresolved = 100 - lag(enumerated)
  ) %>%
  ungroup()

# We do some minor outlier exclusion for fitting our increment model. Pragmatically,
# nls won't fit this model to data that is too far away from a log-ish shape, but
# in these cases you can also see some trend breaks that justify it:
# e.g Alaska - very high increments when 20-30% unresolved, probably still RA enumeration ongoing
#     southern states - some slow starts, reportedly due to covid
# Update: the final days progress for Alabama is so extreme we can't fit it any more
progress_model_data <- progress %>% 
  filter(!is.na(daily_increment)) %>% # first day, so no lagged variable possible
  mutate(outlier = (
    daily_increment > 1.5 |
    (state %in% c("Alabama") & (lag_unresolved < 4 | lag_unresolved > 20)) | 
    (state %in% c("Alaska", "Arizona", "Mississippi", "Louisiana", "Wyoming") & lag_unresolved > 15) |
    (state %in% c("Georgia") & lag_unresolved > 20)
  )) 

plt_increments_v_unresolved <- progress_model_data %>%
  ggplot() +
    aes(lag_unresolved, daily_increment, color = outlier) +
    geom_point(size = 0.5) +
    scale_x_continuous(limits = c(0, 30)) +
    scale_y_continuous(limits = c(0, 3)) +
    geom_smooth(
      data = . %>% filter(!outlier),
      method = "nls", 
      formula = y ~ K / (1 + exp(-beta * x)) - (K/2), 
      method.args = list(start=c(K = 1, beta = 0.17)), 
      se = FALSE,
      fullrange = TRUE,
      color = "orange",
      size = 0.25
    ) +
    facet_wrap(~ state)

plot_device(paste0("outputs/plt_increments_v_unresolved", plot_ext), units = "in", width = 10, height = 8)
plt_increments_v_unresolved
dev.off()

# Generate our blank forecast table
forecast <- expand.grid(
  state = unique(progress$state),
  as_of_date = seq(from = progress_last_date + 1, to=oct31, by = 1),
  type = "forecast",
  enumerated = NA,
  daily_increment = NA,
  lag_unresolved = NA
)

# Add the final observation, which will feed into the first forecast
forecast <- bind_rows(
  progress %>%
    filter(as_of_date == progress_last_date) %>%
    select(state, as_of_date, enumerated, daily_increment, lag_unresolved),
  forecast
)

# Generate forecast iteratively. Ugly, but unless there is a closed-form solution
# to the differential equation driving this, I don't know a better way to do this.
for (s in unique(progress$state)) {
  model <- nls(
    formula = daily_increment ~ K / (1 + exp(-beta * lag_unresolved)) - (K/2), 
    data = progress_model_data %>% filter(!outlier),
    start = c(K = 1, beta = 0.17),
  )
  
  for (d in seq(from = progress_last_date + 1, to=oct31, by = 1)) {
    forecast <- within(forecast, {
      lag_unresolved[state == s & as_of_date == d] <-
        100 - enumerated[state == s & as_of_date == d-1]
        
      daily_increment[state == s & as_of_date == d] <-
        predict(model, forecast[state == s & as_of_date == d-1,])
      
      enumerated[state == s & as_of_date == d] <-
        enumerated[state == s & as_of_date == d-1] +
        daily_increment[state == s & as_of_date == d]
    })
  }
}

# Drop the lead-in observation, now that we're done with it
forecast <- forecast %>% filter(as_of_date != progress_last_date)

# Combine the past with the future, and also calculate which day total enumeration
# passes 99%, which we take as "finished" according to the Census Bureau threshold
progress_forecast <- progress %>%
  mutate(type = "observation") %>%
  select(state, as_of_date, type, enumerated, daily_increment, self_response, nrfu) %>%
  bind_rows(forecast) %>%
  group_by(state) %>%
  mutate(
    unresolved = 100 - enumerated,
    finished = min(as_of_date[which(enumerated >= 99)])
  ) %>%
  ungroup()

plt_daily_increments <- ggplot(progress_forecast, aes(as_of_date, daily_increment, color = type)) +
  geom_point(size = 0.2) +
  facet_wrap(~ state)

plot_device(paste0("outputs/plt_daily_increments", plot_ext), units = "in", width = 10, height = 8)
plt_daily_increments
dev.off()

# Plot for the increment model forecasts
plt_progress_proj_final_base <- ggplot(
  progress_forecast,
  aes(as_of_date, enumerated, linetype = type, color = finished <= as.Date("2020-09-30"))
) +
  geom_line() +
  scale_y_continuous(breaks = 0:10*10, limits = c(70, 100), minor_breaks = 0:20*5) +
  scale_x_date(
    breaks = c(aug31, sep30, oct31),
    limits = c(progress_first_date, oct31),
    date_labels = "%m/%d"
  ) +
  scale_linetype_manual(values = c(observation = 1, forecast = 2)) +
  scale_color_manual(values = c("red", "black")) +
  geom_vline(xintercept = as.Date("2020-09-30"), linetype = 2, color = "grey") +
  theme_minimal() +
  theme(axis.title = element_blank(), legend.position = "none")

# Versus the ASA paper forecast for comparison. The increment model forecasts are
# more optimistic than the ASA ones.
plt_progress_proj_final_vs_ASA <- plt_progress_proj_final_base +
  facet_wrap(vars(state)) +
  geom_smooth(data = . %>% filter(type == "observation"),
              method="nls", 
              formula=y ~ 100 / (1 + exp(-alpha - beta * x)), 
              method.args = list(start=c(alpha = 1, beta = 1e-10)), 
              se = FALSE,
              fullrange = TRUE,
              color = "orange",
              size = 0.25)

plot_device(paste0("outputs/plt_progress_proj_final_vs_ASA", plot_ext), units = "in", width = 10, height = 8)
plt_progress_proj_final_vs_ASA
dev.off()

# A clean version, including PR and US
plt_progress_proj_final <- plt_progress_proj_final_base +
  facet_wrap(vars(state)) +
  ylab("Housing units resolved (%)") +
  labs(title = "Projected progress of Census 2020") +
  theme(axis.title.y = element_text())

plot_device(paste0("outputs/plt_progress_proj_final", plot_ext), units = "in", width = 10, height = 8)
plt_progress_proj_final
dev.off()

# A pretty version, which drops PR and US
plt_progress_proj_final_geo <- plt_progress_proj_final_base +
  facet_geo(~ state) +
  ylab("Housing units resolved (%)") +
  labs(title = "Projected progress of Census 2020") +
  theme(axis.title.y = element_text())

plot_device(paste0("outputs/plt_progress_proj_final_geo", plot_ext), units = "in", width = 12, height = 8)
plt_progress_proj_final_geo
dev.off()

# The data to recreate the above figure
progress_forecast %>%
  select(state, as_of_date, enumerated, type, finished) %>%
  write_csv("outputs/plt_progress_proj_final.csv")

# A bar chart of unresolved housing units
plt_unresolved <- progress_forecast %>%
  filter(as_of_date %in% c(sep30, oct5, as.Date("2020-10-15"), oct31)) %>%
  mutate(state = fct_reorder(state, unresolved, first)) %>%
  ggplot() +
    aes(state, unresolved) +
    geom_col() +
    scale_y_continuous(expand = expansion(add = c(0, 1.5)), breaks = c(0,0.5,1:4)) +
    coord_flip() +
    facet_grid(
      (state != "U.S. Total") ~ as_of_date,
      labeller = labeller(as_of_date = partial(format.Date, format = "%b %d")),
      scales = "free_y", space = "free_y"
    ) +
    geom_hline(yintercept = 0.5, color = "red", linetype = 3) +
    geom_hline(yintercept = 1, color="red", linetype = 1) +
    ylab("Unresolved housing units (%)") +
    theme(
      panel.grid.major.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_blank()
    ) + labs(
      title = "Proportion of housing units projected to remain unenumerated (unresolved) at different end dates."
    )

plot_device(paste0("outputs/plt_unresolved", plot_ext), units = "in", width = 12, height = 8)
plt_unresolved
dev.off()

# The data to recreate the above figure
progress_forecast %>%
  select(state, as_of_date, unresolved) %>%
  filter(as_of_date %in% c(sep30, as.Date("2020-10-15"), oct31)) %>%
  mutate(state = fct_reorder(state, unresolved, first)) %>%
  pivot_wider(names_from = as_of_date, values_from = unresolved) %>%
  write_csv("outputs/plt_unresolved.csv")

############################################################################## #
# Compare model with latest data against original Sep 23 publication (Sep 18 data)
################################################################################

# We can check the current point-in-time latest-data forecast against the numbers
# we used in the online publication, to see how we're going.

forecast_sep23_vintage <-
  read_csv("inputs/frozen_outputs/plt_progress_proj_final_nyt_online_Sep23.csv")

vintage_comparison <- progress_forecast %>%
  left_join(forecast_sep23_vintage, by = c("state", "as_of_date"), suffix = c("", "_sep18"))

state_forecast_error <- vintage_comparison %>%
  filter(type == "observation", type_sep18 == "forecast") %>%
  mutate(err = enumerated - enumerated_sep18)

plt_forecast_error <- ggplot(state_forecast_error, aes(x = err)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~ as_of_date) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(
    data = . %>% group_by(as_of_date) %>% summarize(err_med = median(err)),
    aes(xintercept = err_med),
    color = "blue"
  ) +
  labs(title = "Distribution of forecast errors by state since online pub (cumulative enumerated, blue = median)")

plot_device(paste0("outputs/plt_forecast_error", plot_ext), units = "in", width = 12, height = 8)
plt_forecast_error
dev.off()

############################################################################## #
# Translate progress into undercounts (!= ASA paper)
################################################################################

# The driving assumption of the ASA paper is not that an incomplete enumeration
# means housing units will be uncounted (directly), but that they will be
# counted by less accurate methods, which in turn leads to an under (or over)
# count. It's a good way to think about quality in terms of quantity.

# But instead of constructing undercounts bottom-up for each operation as they
# attempt to do, we work top-down. First we assume that under an October 31,
# deadline, time is no constraint and a "good" count is achieved. That's
# consistent with our progress model, which reaches >99.5% for all states by then.
# (Here we part ways with the ASA paper, which does not envisage a good count in
# all states until December.)

# As our model for good count, we take the Urban Institute's 2019 paper (Elliot
# et al 2019 in the ASA reference list). Then, we allow that for scenarios in
# which counting stops before that, each percentage point of unresolved cases
# translates into increased undercount - based on the state-level correlation
# from 2010.

# We do not model the effect of forfeited increases in self-response due to an
# earlier stop.

# Sidenote: The ASA authors do a quick analysis to show that % HH undercount
# correlates with % person undercount (fig 5, 6). That's a useful side-trip but as
# they've done it we rely on it.

# We'll estimate the association between net undercount rate and imputation rate
# (~= unresolved %). We do this using statewise data. That provides far fewer
# observations than county data would, but there's a good reason: for apportionment
# and our Medicaid analysis, we only care about state-level undercount; and
# county-level undercount does not aggregate to state-level undercount (as someone
# can be counted in the right state, wrong county).

undercount_model_data <- mail_response_2010 %>%
  full_join(census_coverage_2010) %>%
  left_join(pop_2010_apr1) %>%
  transmute(
    state,
    pct_undercount,
    self_response = mrr_2010 * 100,
    unresolved = whole_person_imputation,
    pop_2010_apr = population
  )

# Plot of what we're going to model: it only explains a bit, but reasonably strongly
plt_undercount_model <- ggplot(
  undercount_model_data,
  aes(unresolved, pct_undercount, size = pop_2010_apr)
) +
  geom_point() +
  geom_smooth(method = "lm", mapping = aes(weight = pop_2010_apr)) +
  theme(legend.position = "none")

plot_device(paste0("outputs/plt_undercount_model", plot_ext), units = "in", width = 12, height = 8)
plt_undercount_model
dev.off()

# Build the model, statewise, weighted by population
undercount_model <- lm(
  pct_undercount ~ unresolved,
  data = undercount_model_data,
  weights = undercount_model_data$pop_2010_apr
)

# Now we apply this model to each state and date
count_forecast <- data.frame(
  state = progress_forecast$state,
  as_of_date = progress_forecast$as_of_date,
  est_undercount = predict(undercount_model, progress_forecast)
) 
  
# But: since this is modelled from 2010, it will predict an overall slight
# overcount, as in 2010. That doesn't seem realistic. Moreover, we threw away
# all the state-specific undercounts by pooling into the regression. What we want
# to do is use only the slope information from the model, but ignore the intercept.
# We do that, inserting a new (statewise) intercept based on the Urban Institute
# modelling from 2019, which we assign to Oct 31.
count_forecast <- count_forecast %>%
  filter(!(state %in% c("Puerto Rico", "U.S. Total"))) %>%
  full_join(urban_miscount_2020) %>%
  full_join(
    count_forecast %>%
    filter(as_of_date == oct31) %>%
    transmute(state, est_undercount_oct31 = est_undercount)
  ) %>%
  mutate(est_undercount = est_undercount - est_undercount_oct31 + ui_undercount) %>%
  select(-est_undercount_oct31, -ui_undercount, -variant)

# Checked: count_forecast %>% filter(as_of_date == oct31) gives back the Urban projection

# How do the undercount distributions look by month?
plt_undercount_by_month <- ggplot(
  count_forecast %>% filter(as_of_date >= sep30),
  aes(as_of_date, est_undercount, group = as_of_date)
) +
  geom_boxplot()

plot_device(paste0("outputs/plt_undercount_by_month", plot_ext), units = "in", width = 12, height = 8)
plt_undercount_by_month
dev.off()

# Now apply the undercount to the true population to get a census_count
count_forecast <- count_forecast %>%
  full_join(pop_apr1) %>%
  mutate(census_count = population * (1 - est_undercount/100.0))

# Finally, we pull out the two dates we really care about
scenarios <- count_forecast %>%
  filter(as_of_date %in% c(sep30, oct31)) %>%
  transmute(
    state,
    short_date = format.Date(as_of_date, "%b%d"),
    est_undercount,
    population,
    census_count
  ) %>%
  pivot_wider(names_from = short_date, values_from = c(census_count, est_undercount))

############################################################################## #
# Apportionment (= ASA)
################################################################################

# Huntingon-Hill / equal proportions apportionment
# Accepts a vector of populations, returns a vector of seats
# Adapted with minor changes from ASA paper, Auerbach & Pierson (2020)
apportion <- function(pop) {
  census_multiplier <- function(n) { 1/sqrt(n * (n - 1)) }
  
  #1. calculate priority values for each state and 2 to 60 seats
  state_seat <- expand.grid(state = seq_along(pop), seats = 2:60)
  state_seat$priority_value <-  mapply(
    function(i, j) { census_multiplier(j) * pop[i] },
    i = state_seat$state,
    j = state_seat$seats
  )
  
  #2. each state gets one "free" seat
  assignment <- tibble(state = seq_along(pop), seats = rep(1, length(pop)))
  
  #3. rank state seats by priority value and assign the first 385 seats
  for(rank in 1:385) {
    state <- assignment$state[
      state_seat$state[order(state_seat$priority_value, decreasing = TRUE)][rank]
    ]
    assignment$seats[assignment$state == state] <-
      assignment$seats[assignment$state == state] + 1
  }
  assignment$seats
}

# DC is not a state and is not apportioned. (NTWR!)
scenarios_ex_DC <- scenarios %>% filter(state != "District of Columbia")

# Calculate apportionment under the two scenarios and the true population,
# and the differences between them
apport_impact <- data.frame(
  state = scenarios_ex_DC$state,
  seats_sep30 = apportion(scenarios_ex_DC$census_count_Sep30),
  seats_oct31 = apportion(scenarios_ex_DC$census_count_Oct31),
  seats_true = apportion(scenarios_ex_DC$population)
) %>% mutate(
  gained_oct31_rel_sep30 = seats_oct31 - seats_sep30
)

write_csv(
  apport_impact %>%
    filter(gained_oct31_rel_sep30 != 0) %>%
    select(state, gained_oct31_rel_sep30),
  "outputs/apportionment_impact.csv"
)

############################################################################## #
# Medicaid
################################################################################

# Adapted with minor changes from ASA paper, Auerbach & Pierson (2020)
apply_fmap <- function(state, pop) {
  census_count_usa <- sum(pop)
  pers_income_usa <- bea_personal_income %>% filter(state == "U.S.") %>% pull(pers_income)
  pers_income_pc_usa <- pers_income_usa / census_count_usa
  
  bea_personal_income %>%
    right_join(data.frame(state = state, pop = pop)) %>%
    mutate(pers_income_pc = pers_income/pop) %>%
    mutate(fmap = 1 - .45 * (pers_income_pc/pers_income_pc_usa)^2) %>%
    mutate(fmap = ifelse(fmap < .5, .5, ifelse(fmap > .83, .83, fmap))) %>%
    select(state, fmap) %>%
    filter(!(state %in% c("District of Columbia"))) %>%
    left_join(medicaid_expenses) %>%
    mutate(federal_portion = medicaid * fmap)
}

medicaid_impact <- data.frame(
  state = apply_fmap(scenarios$state, scenarios$census_count_Sep30)$state,
  federal_portion_sep30 = apply_fmap(scenarios$state, scenarios$census_count_Sep30)$federal_portion,
  federal_portion_oct31 = apply_fmap(scenarios$state, scenarios$census_count_Oct31)$federal_portion,
  federal_portion_true = apply_fmap(scenarios$state, scenarios$population)$federal_portion
) %>% mutate(
  gained_oct31_rel_sep30 = federal_portion_oct31 - federal_portion_sep30,
  gained_true_rel_oct31 = federal_portion_true - federal_portion_oct31
)

print("Total that gaining states would gain by continuing to Oct 31:")
sum(ifelse(medicaid_impact$gained_oct31_rel_sep30 > 0, medicaid_impact$gained_oct31_rel_sep30, 0))

print("Total that losing states would lose by continuing to Oct 31:")
sum(ifelse(medicaid_impact$gained_oct31_rel_sep30 < 0, medicaid_impact$gained_oct31_rel_sep30, 0))

# I guess there's no particular reason this has to be zero
print("Net gain to all states from fed, by continuing to Oct 31:")
sum(medicaid_impact$gained_oct31_rel_sep30)

plt_medicaid_impact <- medicaid_impact %>%
  mutate(state = fct_reorder(state, state, .desc = TRUE)) %>%
  mutate(state = fct_reorder(state, gained_oct31_rel_sep30)) %>%
  ggplot() +
    aes(state, gained_oct31_rel_sep30, fill = gained_oct31_rel_sep30 > 0) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("darkgreen", "orangered")) +
    scale_y_continuous(labels = function(x) round(x/1e6)) +
    ylab("Federal funds ($ million annually)") +
    theme(
      panel.grid.major.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    ) +
    labs(
      title = "Federal Medicaid funds lost due to a September 30 end to counting",
      caption = "Note: AK, CA, CO, CT, MD, MA, NH, NJ, NY, VA, WA and WY already receive the minimum FMAP and are therefore unaffected.\nMN was in that group but is projected to leave it all scenarios. DC's FMAP is fixed at 70%."
    )

plot_device(paste0("outputs/plt_medicaid_impact", plot_ext), units = "in", width = 8, height = 10)
plt_medicaid_impact
dev.off()

# The data to recreate the above figure
medicaid_impact %>%
  mutate(state = fct_reorder(state, state, .desc = TRUE)) %>%
  mutate(state = fct_reorder(state, gained_oct31_rel_sep30)) %>%
  write_csv("outputs/plt_medicaid_impact.csv")
