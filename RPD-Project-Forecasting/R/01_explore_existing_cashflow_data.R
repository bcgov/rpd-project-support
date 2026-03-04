library("dplyr")
library("ggplot2")
library("here")
library("readr")

options(scipen = 999)

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
  rename_with(.cols = everything(), ~ gsub("[()/#%-]", "", .x)) |>
  rename_with(.cols = `201819`:`204041`, ~ paste0("FY", .x))

qual_data <- cashflow_cleaned |>
  select(Period:ForecastingTheme, AuthorizationToProceedE:OverallProjectHealth)

quant_data <- cashflow_cleaned |>
  select(
    Period,
    Region,
    KahuaProjectNumber,
    ProjectPhase,
    ProjectStatus,
    ProjectCloseoutA,
    PlannedEstimate:VowInvoiced,
    BudgetPreviousFys:FY204041
  ) |>
  # deal with characters that prevent conversion to numbers
  mutate(across(
    PlannedEstimate:FY204041 & where(is.character),
    ~ gsub("[%,\\$]", "", .x)
  )) |>
  # handle use of () in financial accounting to indicate negative values
  mutate(across(
    PlannedEstimate:FY204041,
    ~ gsub("[(]", "-", .x)
  )) |>
  mutate(across(
    PlannedEstimate:FY204041,
    ~ gsub("[)]", "", .x)
  )) |>
  # finally convert to double
  mutate(across(
    PlannedEstimate:FY204041,
    ~ as.double(.x)
  ))

quant_summary <- quant_data |>
  mutate(ProjectCloseoutA = as.Date(ProjectCloseoutA, format = "%m/%d/%Y")) |>
  filter(
    ProjectCloseoutA >= as.Date("2025-04-01", format = "%Y-%m-%d") |
      is.na(ProjectCloseoutA)
  ) |>
  select(Period, KahuaProjectNumber, PlannedEstimate:BudgetLastFy) |>
  group_by(Period) |>
  summarise(
    PlannedEstimate = sum(PlannedEstimate, na.rm = TRUE),
    PreliminaryBudget = sum(PreliminaryBudget, na.rm = TRUE),
    ApprovedBudget = sum(ApprovedBudget, na.rm = TRUE),
    ApprovedBudgetChanges = sum(ApprovedBudgetChanges, na.rm = TRUE),
    CurrentBudget = sum(CurrentBudget, na.rm = TRUE),
    PendingChanges = sum(PendingChanges, na.rm = TRUE),
    Forecast = sum(Forecast, na.rm = TRUE),
    OriginalCommitted = sum(OriginalCommitted, na.rm = TRUE),
    ApprovedChanges = sum(ApprovedChanges, na.rm = TRUE),
    CurrentCommitted = sum(CurrentCommitted, na.rm = TRUE),
    # BudgetCommitted = sum(BudgetCommitted, na.rm = TRUE),
    Invoiced = sum(Invoiced, na.rm = TRUE),
    # CommitmentInvoiced = sum(CommitmentInvoiced, na.rm = TRUE),
    Retained = sum(Retained, na.rm = TRUE),
    Paid = sum(Paid, na.rm = TRUE),
    PreviousFysVowComplete = sum(PreviousFysVowComplete, na.rm = TRUE),
    PreviouslyReportedCurrentFyYtdVowComplete = sum(
      PreviouslyReportedCurrentFyYtdVowComplete,
      na.rm = TRUE
    ),
    CurrentFyYtdVowComplete = sum(CurrentFyYtdVowComplete, na.rm = TRUE),
    TotalVowComplete = sum(TotalVowComplete, na.rm = TRUE),
    # BudgetEarned = sum(BudgetEarned, na.rm = TRUE),
    BudgetVow = sum(BudgetVow, na.rm = TRUE),
    CommittedVow = sum(CommittedVow, na.rm = TRUE),
    VowInvoiced = sum(VowInvoiced, na.rm = TRUE),
    BudgetPreviousFys = sum(BudgetPreviousFys, na.rm = TRUE),
    BudgetLastFy = sum(BudgetLastFy, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(Period = as.factor(Period))

name_set <- quant_summary |>
  select(-c(Period)) |>
  colnames()

for (ii in 1:length(name_set)) {
  y_var <- name_set[ii]

  p <- ggplot(
    data = quant_summary,
    mapping = aes(x = Period, y = .data[[y_var]])
  ) +
    geom_col(fill = "#13265C") +
    scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    labs(y = NULL, title = paste(y_var, "Open or Closed this FY")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave(
    filename = here(paste0(
      "RPD-Project-Forecasting/charts/",
      ii,
      "-",
      name_set[ii],
      "-basic-chart.png"
    )),
    bg = "white"
  )
}

# better analysis perhaps ###
pivot_set <- cashflow_cleaned |>
  select(
    Period,
    KahuaProjectNumber,
    ProjectPhase,
    ProjectStatus,
    AuthorizationToProceedA,
    FeasibilityCompleteA,
    DesignDevelopmentCompleteA,
    TenderAwardA,
    ConstructionDocumentsCompleteA,
    SubstantialCompletionA,
    DeficienciesListCompleteA,
    ProjectCloseoutA,
    PlannedEstimate,
    PreliminaryBudget,
    ApprovedBudget,
    ApprovedBudgetChanges,
    CurrentBudget,
    PendingChanges,
    Forecast,
    OriginalCommitted,
    ApprovedChanges,
    CurrentCommitted,
    Invoiced,
    Paid
  ) |>
  mutate(across(
    AuthorizationToProceedA:ProjectCloseoutA,
    ~ as.Date(.x, format = "%m/%d/%Y")
  )) |>
  mutate(Period = as.factor(Period)) |>
  group_by(KahuaProjectNumber) |>
  filter(
    any(
      ProjectCloseoutA >= as.Date("2025-04-01", format = "%Y-%m-%d")
    ) |
      all(is.na(ProjectCloseoutA))
  ) |>
  filter(
    !all(
      ProjectStatus == "Lost"
    )
  ) |>
  ungroup() |>
  # filter(
  #   AuthorizationToProceedA >= as.Date("2025-04-01", format = "%Y-%m-%d")
  # ) |>
  arrange(KahuaProjectNumber, Period)
