library("dplyr")
library("ggplot2")
library("here")
library("readr")

files <- list.files(
  path = here("RPD-Project-Forecasting/input/"),
  pattern = "Program Cashflow Report All",
  full.names = TRUE
)

# not using period 0 at this moment
# There are some parsing warnings, but seems fine
for (ii in 1:length(files)) {
  current <- readr::read_csv(files[ii]) |>
    mutate(Period = ii, .before = everything())

  if (ii == 1) {
    cashflow <- current
  } else {
    # I use full_join instead of rbind because column number varies between reports
    cashflow <- full_join(cashflow, current)
  }
}

cashflow_cleaned <- cashflow |>
  rename_with(.cols = everything(), ~ stringr::str_to_title(.x)) |>
  rename_with(.cols = everything(), ~ gsub("\\s+", "", .x)) |>
  rename_with(.cols = everything(), ~ gsub("[()/#%-]", "", .x))

qual_data <- cashflow_cleaned |>
  select(Period:ForecastingTheme, AuthorizationToProceedE:OverallProjectHealth)

quant_data <- cashflow_cleaned |>
  select(
    Period,
    Region,
    KahuaProjectNumber,
    ProjectPhase,
    ProjectStatus,
    PlannedEstimate:VowInvoiced,
    BudgetPreviousFys:`2040-41`
  )
