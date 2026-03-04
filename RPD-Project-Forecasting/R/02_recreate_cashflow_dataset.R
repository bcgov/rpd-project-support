library(base64enc)
library(dplyr)
library(here)
library(httr2)
library(tidyr)
library(tools)

source("C:/Projects/rpd-utilities/R/cbre_api_function.R")

# Milestones ####
# FactMilestoneData <- extract_cbre_data("pjm_fact_milestone_vw")
# write.csv(
#   FactMilestoneData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-FactMilestoneData.csv"
#   )),
#   row.names = FALSE
# )

FactMilestoneData <- read.csv(
  here(
    "RPD-Project-Forecasting/output/2026-03-02-FactMilestoneData.csv"
  )
)

FactMilestone <- FactMilestoneData |>
  select_if(~ !all(is.na(.))) |>
  select_if(~ !all(. == 0)) |>
  select_if(~ !all(. == '-1')) |>
  select_if(~ !all(. == "N/A")) |>
  select_if(~ !all(. == "-")) |>
  select(
    project_skey,
    # milestone_skey,
    # milestone_id,
    # milestone_notes,
    # serial_number,
    milestone_desc,
    estimated_start_date,
    revised_start_date,
    actual_start_date,
    estimated_end_date,
    revised_end_date,
    actual_end_date
  ) |>
  # bench mark this versus stringr::str_to_title
  # microbenchmark::microbenchmark(
  # test1 <- FactMilestone |> mutate(milestone_desc = tools::toTitleCase(milestone_desc)),
  # test2 <- FactMilestone |> mutate(milestone_desc = stringr::str_to_title(milestone_desc)),
  # times = 1
  # )
  mutate(milestone_desc = stringr::str_to_title(milestone_desc)) |>
  mutate(milestone_desc = trimws(milestone_desc)) |>
  mutate(milestone_desc = gsub("[-–]", "", milestone_desc)) |>
  mutate(milestone_desc = gsub("\\s+", "_", milestone_desc)) |>
  # table(FactMilestone$milestone_desc)
  filter(
    milestone_desc %in%
      c(
        "Authorization_To_Proceed",
        # "Business_Case_Complete",
        # "Commence_Construction",
        # "Concept_Plan_Complete",
        "Construction_Documents_Complete",
        "Deficiencies_List_Complete",
        # "Design_Complete",
        "Design_Development_Complete",
        # "Facility_Open_For_Business",
        "Feasibility_Complete",
        # "Financial_Submission_Complete",
        # "First_Nations_Consultation_Complete",
        # "Fixtures_Furniture_And_Equipment_Complete",
        # "Municipal_Consultation_Complete",
        # "Out_To_Tender",
        # "Project_Board_Approval_For_Contract_Award_Complete",
        # "Project_Board_Approval_For_Shortlist_Proponents_Complete",
        "Project_Closeout",
        # "Public_Consultation_Complete",
        # "RFP",
        # "Schedule_Submission_Complete",
        # "Shadow_Team_Formed",
        # "Submission_To_Treasury_Board_To_Go_To_Business_Case_Complete",
        "Substantial_Completion",
        # "Technical_Closeout",
        # "Technical_Submission_Complete",
        "Tender_Award"
        # ,
        # "Treasury_Board_Approval_To_Go_To_Business_Case_Received",
        # "Treasury_Board_Procurement_Approval_Complete",
        # "Warranty"
      )
  ) |>
  pivot_wider(
    # id_cols = project_skey,
    names_from = milestone_desc,
    values_from = c(estimated_start_date:actual_end_date),
    names_glue = "{milestone_desc}_{.value}"
  ) |>
  select_if(~ !all(is.na(.))) # drop from 167 columns to 96 columns.

# FactBudgetData <- extract_cbre_data("pjm_fact_budget_vw")
# 568 pages

# Fact Project ####
# FactProjectData <- extract_cbre_data("pjm_fact_project_vw")

# write.csv(
#   FactProjectData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-FactProjectData.csv"
#   )),
#   row.names = FALSE
# )

FactProjectData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-02-FactProjectData.csv"
))

FactProject <- FactProjectData |>
  select_if(~ !all(is.na(.))) |>
  select_if(~ !all(. == 0)) |>
  select_if(~ !all(. == '-1')) |>
  select_if(~ !all(. == "N/A")) |>
  select_if(~ !all(. == "-"))

select()
# Dim Project ####
# DimProjectData <- extract_cbre_data("pjm_dim_project_vw")

# write.csv(
#   DimProjectData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-DimProjectData.csv"
#   )),
#   row.names = FALSE
# )

DimProjectData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-02-DimProjectData.csv"
))

DimProject <- DimProjectData |>
  select_if(~ !all(is.na(.))) |>
  select_if(~ !all(. == 0)) |>
  select_if(~ !all(. == '-1')) |>
  select_if(~ !all(. == "N/A")) |>
  select_if(~ !all(. == "-")) |>
  select(
    project_skey,
    project_number,
    project_id,
    project_name,
    standard_project_type,
    fusion_project_type,
    csf_projecttype,
    csf_projectsubtype,
    csf_migratedprojectid,
    csf_wsiregion,
    scope_desc,
    csf_estimatetype,
    client_project_number,
    csf_aresnumber, # ares,
    csf_ministryparentorg,
    csf_branchchildorg,
    csf_agreementnumber,
    csf_leaseid,
    csf_fundingsource,
    csf_fundingtype,
    project_type,
    csf_servicetype,
    csf_programtype,
    csf_complexity,
    csf_pjmfeepercentage,
    csf_identifiedproject,
    csf_pmosource, # is the accurate service provider? correct for spot check
    csf_projectadministrationnotes,
    csf_previouslyreportedcurrentfyytdvowcomplete,
    risk_status,
    budget_health,
    schedule_health,
    overall_project_health,
    csf_scopehealth
  )

subset_proj_dim <- DimProject |>
  filter(project_number == "K1012077") |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = everything(), names_to = "column", values_to = "value")

select()
# Change Order ####
# FactChangeOrderData <- extract_cbre_data("pjm_fact_change_order_vw")

# write.csv(
#   FactChangeOrderData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-FactChangeOrderData.csv"
#   )),
#   row.names = FALSE
# )

FactChangeOrderData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-02-FactChangeOrderData.csv"
))

# Contract ####
# FactContractData <- extract_cbre_data("pjm_fact_contract_vw")

# write.csv(
#   FactContractData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-FactContractData.csv"
#   )),
#   row.names = FALSE
# )

FactContractData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-02-FactContractData.csv"
))

# Dim Invoice ####
# test <- extract_cbre_data("fact_invoice_vw") # 219 pages
# test <- extract_cbre_data("fin_dim_invoice_line_vw") # empty data
# test <- extract_cbre_data("fin_dim_invoice_vw") # 54 pages

# DimInvoiceData <- extract_cbre_data("pjm_dim_invoice_vw") # 52 pages
# write.csv(
#   DimInvoiceData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-DimInvoiceData.csv"
#   )),
#   row.names = FALSE
# )

DimInvoiceData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-02-DimInvoiceData.csv"
))

# Fact Invoice ####
# FactInvoiceData <- extract_cbre_data("pjm_fact_invoice_vw") # 219 pages
# write.csv(
#   FactInvoiceData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-FactInvoiceData.csv"
#   )),
#   row.names = FALSE
# )

FactInvoiceData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-02-FactInvoiceData.csv"
))

FactInvoice <- FactInvoiceData |>
  select_if(~ !all(is.na(.))) |>
  select_if(~ !all(. == 0)) |>
  select_if(~ !all(. == '-1')) |>
  select_if(~ !all(. == "N/A")) |>
  select_if(~ !all(. == "-")) #|>
select(
  edp_update_ts,
  source_created_ts,
  source_modified_ts,
  current_payment_due,
  project_skey,
  invoice_skey,
  invoice_item_skey,
  record_type,
  invoice_item_id,
  contract_skey,
  contract_line_skey,
  change_order_skey,
  change_order_item_skey,
)


select()
# Project Activity ####
# 111 pages
# FactProjActivityData <- extract_cbre_data("pjm_fact_project_activity_vw")

# Project Risk ####
# FactProjRiskData <- extract_cbre_data("pjm_fact_project_risk_vw")

# write.csv(
#   FactProjRiskData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-FactProjRiskData.csv"
#   )),
#   row.names = FALSE
# )

FactProjRiskData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-02-FactProjRiskData.csv"
))

# Project Role ####
# FactProjRoleData <- extract_cbre_data("pjm_fact_project_role_vw")

# write.csv(
#   FactProjRoleData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-FactProjRoleData.csv"
#   )),
#   row.names = FALSE
# )

FactProjRoleData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-02-FactProjRoleData.csv"
))

# Project Service ####
# FactProjServiceData <- extract_cbre_data("pjm_fact_project_service_vw")

# write.csv(
#   FactProjServiceData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-FactProjServiceData.csv"
#   )),
#   row.names = FALSE
# )

FactProjServiceData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-02-FactProjServiceData.csv"
))

# Value Creation ####
# FactValueCreationData <- extract_cbre_data("pjm_fact_value_creation_vw")

# write.csv(
#   FactValueCreationData,
#   here(paste0(
#     "RPD-Project-Forecasting/output/",
#     Sys.Date(),
#     "-FactValueCreationData.csv"
#   )),
#   row.names = FALSE
# )

FactValueCreationData <- read.csv(here(
  "RPD-Project-Forecasting/output/2026-03-02-FactValueCreationData.csv"
))
