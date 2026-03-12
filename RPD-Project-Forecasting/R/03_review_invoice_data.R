library(dplyr)
library(here)
library(tidyr)
library(tools)

DimInvoiceData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-03-DimInvoiceData.csv"
))

DimInvoice <- DimInvoiceData |>
  select_if(~ !all(is.na(.))) |>
  select_if(~ !all(. == 0)) |>
  select_if(~ !all(. == '-1')) |>
  select_if(~ !all(. == "N/A")) |>
  select_if(~ !all(. == "-"))

FactInvoiceData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-03-FactInvoiceData.csv"
))

FactInvoice <- FactInvoiceData |>
  select_if(~ !all(is.na(.))) |>
  select_if(~ !all(. == 0)) |>
  select_if(~ !all(. == '-1')) |>
  select_if(~ !all(. == "N/A")) |>
  select_if(~ !all(. == "-")) |>
  select(
    edp_update_ts,
    edp_create_ts,
    source_created_ts,
    source_modified_ts,
    source_unique_id,
    source_system_code,
    project_skey,
    invoice_status,
    invoice_desc,
    invoice_skey,
    invoice_item_skey,
    invoice_item_id,
    project_activity_skey,
    contract_skey,
    contract_line_skey,
    change_order_skey,
    change_order_item_skey,
    to_company_skey,
    from_company_skey,
    to_contact_skey,
    from_contact_skey,
    line_number,
    record_type,
    current_payment_due,
    work_completed_to_date,
    work_retainage_percent,
    work_retainage,
    total_retainage,
    total_earned_to_date,
    scheduled_value,
    payables_billed_total,
    payables_withheld_total,
    payables_remitted_total,
    previous_work_completed,
    previous_total_earned,
    total_to_date_percent,
    total_to_date_percent,
    balance_to_finish_with_retainage,
    balance_to_finish
  ) |>
  mutate(
    across(
      c(
        edp_update_ts,
        edp_create_ts,
        source_created_ts,
        source_modified_ts
      ),
      as.POSIXct
    )
  )

FactInvoiceFY202526 <- FactInvoice |>
  filter(
    source_created_ts >= as.POSIXct("2025-04-01", format = "%Y-%m-%d") &
      source_created_ts <= as.POSIXct("2026-03-31", format = "%Y-%m-%d")
  )

n_distinct(FactInvoiceFY202526$project_skey)
