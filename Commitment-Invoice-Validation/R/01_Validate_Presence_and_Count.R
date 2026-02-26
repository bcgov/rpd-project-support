library(dplyr)
library(openxlsx2)
library(here)
library(stringr)

# Load input data
InvoiceDetails <- openxlsx2::read_xlsx(
  here("Commitment-Invoice-Validation/input/2026-02-26-InvoiceDetails.xlsx"),
  start_row = 4
)

CommitmentLine <- openxlsx2::read_xlsx(
  here(
    "Commitment-Invoice-Validation/input/2026-02-26-CommitmentLineItemDetails.xlsx"
  ),
  start_row = 5
)

# Find unique Project and PO Combos
InvoiceIntermediate <- InvoiceDetails |>
  group_by(`Kahua Project #`, `Kahua Contract/PO ID`) |>
  summarise(InvoiceOccurrence = n())

CommitmentIntermediate <- CommitmentLine |>
  group_by(`Kahua Project ID`, `Kahua Contract/PO ID`) |>
  summarise(CommitmentOccurrence = n())

# Combine both to identify overlap
MatchedLines <- InvoiceIntermediate |>
  full_join(
    CommitmentIntermediate,
    by = join_by(
      `Kahua Project #` == `Kahua Project ID`,
      `Kahua Contract/PO ID`
    )
  ) |>
  mutate(across(
    c(InvoiceOccurrence, CommitmentOccurrence),
    ~ tidyr::replace_na(.x, 0)
  ))

# Rejoin to original datasets to generate output
InvoiceOutput <- InvoiceDetails |>
  left_join(
    MatchedLines,
    by = join_by(`Kahua Project #`, `Kahua Contract/PO ID`)
  ) |>
  mutate(
    PresenceFlag = case_when(
      CommitmentOccurrence == 0 ~ "Absent",
      .default = "Present"
    ),
    .before = everything()
  ) |>
  relocate(InvoiceOccurrence, CommitmentOccurrence, .after = PresenceFlag) |>
  mutate(across(where(is.character), ~ tidyr::replace_na(.x, "")))

CommitmentOutput <- CommitmentLine |>
  left_join(
    MatchedLines,
    by = join_by(
      `Kahua Project ID` == `Kahua Project #`,
      `Kahua Contract/PO ID`
    )
  ) |>
  mutate(
    PresenceFlag = case_when(
      InvoiceOccurrence == 0 ~ "Absent",
      .default = "Present"
    ),
    .before = everything()
  ) |>
  relocate(CommitmentOccurrence, InvoiceOccurrence, .after = PresenceFlag) |>
  mutate(across(where(is.character), ~ tidyr::replace_na(.x, "")))

# Create output files
openxlsx2::write_xlsx(
  InvoiceOutput,
  file = here::here(paste0(
    "Commitment-Invoice-Validation/output/",
    Sys.Date(),
    "-InvoiceDetailsOutput.xlsx"
  ))
)

openxlsx2::write_xlsx(
  CommitmentOutput,
  file = here::here(paste0(
    "Commitment-Invoice-Validation/output/",
    Sys.Date(),
    "-CommitmentLineOutput.xlsx"
  ))
)
