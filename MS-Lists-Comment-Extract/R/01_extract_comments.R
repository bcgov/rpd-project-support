library(dplyr)
library(AzureAuth)
library(httr2)

# Using CITZ-RPD-Spacity-Dataflow-TEST application registration for simple testing.
# Maybe create a dedicated app if this is ever needed again.
AppID <- "281e1d12-7c06-4113-a2f7-54d8a292bd3d"
tenantID <- "6fdb5200-3d0d-4a8a-b036-d3685e359adc"

# Get Bearer token
token <- AzureAuth::get_azure_token(
  resource = "https://graph.microsoft.com",
  tenant = tenantID,
  app = AppID,
  auth_type = "authorization_code"
)

site_id <- "18f1e509-5567-4ff3-8906-d5d204e4d227"
url <- paste0("https://graph.microsoft.com/v1.0/sites/", site_id, "/lists/")

req <- request(url) |>
  req_auth_bearer_token(token$credentials$access_token) |>
  req_perform()

resp <- req |> resp_body_json()

lists <- resp |>
  purrr::pluck("value") |>
  tibble::enframe() |>
  tidyr::unnest_wider(value, names_sep = "-") |>
  select(
    Name = `value-name`,
    GUID = `value-id`,
    LastModified = `value-lastModifiedDateTime`
  )

TrackerSite <- lists |>
  filter(Name == "AcM Project Status Tracker Test 1") |>
  select(GUID) |>
  pull()

url <- paste0(
  "https://graph.microsoft.com/v1.0/sites/",
  site_id,
  "/lists/",
  TrackerSite,
  "/items?expand=fields"
)

next_url <- url
all_data <- list()

while (!is.null(next_url)) {
  req <- request(next_url) |>
    req_auth_bearer_token(token$credentials$access_token) |>
    req_perform()

  resp <- req |> resp_body_json()

  # Extract current page data
  page_data <- resp[["value"]]
  all_data <- append(all_data, page_data)

  # Update next_url if present
  next_url <- resp[["@odata.nextLink"]]
}

SiteContents <- all_data |>
  tibble::enframe() |>
  tidyr::unnest_wider(value, names_sep = "-") |>
  tidyr::unnest_wider(`value-fields`, names_sep = "-") #|>
select(
  `value-fields-UID`,
  `value-fields-Comments`
) |>
  filter(!is.na(`value-fields-Comments`))
