## code to prepare `generate_sample_data` dataset goes here
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

#### FAKE SURGERY ####
## Make some fake data on "surgical" patients
surg <- read_csv("data-raw/sample_codes/surgical_pts.csv", col_types = "ccd") %>%
  mutate(icd_code = stringr::str_remove(icd_code, "\\."))

fake_surgery <- function(n_pts, icd_version){
  # PARAMS:
  # n_patients: single interger with the number of patients to generate

  icd_version <- as.character(icd_version)
  icd_version <- rlang::arg_match(icd_version, c("9", "10"))

  icd_version <- switch (icd_version,
                   "10" = "I10",
                   "9"  = "I09")


  tibble(pt_id = 1:n_pts,
         n_dx = rpois(n_pts, 3),
         n_pr = rbinom(n_pts, size=150, prob=0.01) + 1) %>%
    mutate(n_dx = ifelse(n_dx<1, 1, n_dx)) %>%

    mutate(
      pr = map(n_pr, function(n){
        surg %>%
          filter(version==paste0(icd_version, "PR")) %>%
          # slice_sample(n=n, weight_by=freq) %>%
          slice_sample(n=n) %>%
          pull(icd_code)}),
      dx = map(n_dx, function(n){
        surg %>%
          filter(version==paste0(icd_version, "DX")) %>%
          # slice_sample(n=n, weight_by=freq) %>%
          slice_sample(n=n) %>%
          pull(icd_code)}),
      version = icd_version)
}
set.seed(123)
surg10 <- fake_surgery(30, "10")
surg9 <- fake_surgery(30, "9")

# Write to JSON for reproducibility
# When reading back to file, you must add `simplifyVector = T`
surg9  %>% jsonlite::write_json("data-raw/fake_data/surg9.json")
surg10 %>% jsonlite::write_json("data-raw/fake_data/surg10.json")

# jsonlite::read_json("data-raw/fake_data/surg9.json", simplifyVector = T)


usethis::use_data(surg9, overwrite = TRUE)
usethis::use_data(surg10, overwrite = TRUE)
rm(surg, surg9, surg10, fake_surgery)






# Work in progress!
dx9  <- read_csv("data-raw/sample_codes/icd_9dx.csv")
dx10 <- read_csv("data-raw/sample_codes/icd_10dx.csv")

