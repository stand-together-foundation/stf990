
library(tidyverse)

'%not_in%' <- function(x,y)!('%in%'(x,y))

ntee_xwalk_raw <- read_csv("activity_ntee_xwalk.csv") %>%
  pivot_longer(
    cols = starts_with("ntee"),
    names_to = "rank",
    values_to = "ntee_cc",
    values_drop_na = TRUE
  ) %>% 
  mutate(
    ntee_major_group = case_when(
      stringr::str_sub(ntee_cc, 1, 2) == 'CC' ~ 'COMMON', 
      stringr::str_sub(ntee_cc, 1, 2) != 'CC' ~ stringr::str_sub(ntee_cc, 1, 1),
      TRUE ~ NA_character_
    ),
    ntee_category_long = case_when(
      stringr::str_sub(ntee_cc, 1, 2) == 'CC' ~ 'Research, Fundraising, & Technical Assistance',
      ntee_major_group %in% LETTERS[1] ~ 'I. Arts, Culture, and Humanities', 
      ntee_major_group %in% LETTERS[2] ~ 'II. Education', 
      ntee_major_group %in% LETTERS[3:4] ~ 'III. Environment and Animals', 
      ntee_major_group %in% LETTERS[5:8] ~ 'IV. Health', 
      ntee_major_group %in% LETTERS[9:16] ~ 'V. Human Services', 
      ntee_major_group %in% LETTERS[17] ~ 'VI. International, Foreign Affairs', 
      ntee_major_group %in% LETTERS[18:23] ~ 'VII. Public, Societal Benefit',       
      ntee_major_group %in% LETTERS[24] ~ 'VIII. Religion Related',       
      ntee_major_group %in% LETTERS[25] ~ 'IX. Mutual/Membership Benefit',       
      ntee_major_group %in% LETTERS[26] ~ 'X. Unknown, Unclassified',
      TRUE ~ NA_character_
    ), 
    ntee_category_short = case_when(
      stringr::str_sub(ntee_cc, 1, 2) == 'CC' ~ 'CC',
      ntee_major_group %in% LETTERS[1] ~ 'I', 
      ntee_major_group %in% LETTERS[2] ~ 'II', 
      ntee_major_group %in% LETTERS[3:4] ~ 'III', 
      ntee_major_group %in% LETTERS[5:8] ~ 'IV', 
      ntee_major_group %in% LETTERS[9:16] ~ 'V', 
      ntee_major_group %in% LETTERS[17] ~ 'VI', 
      ntee_major_group %in% LETTERS[18:23] ~ 'VII',       
      ntee_major_group %in% LETTERS[24] ~ 'VIII',       
      ntee_major_group %in% LETTERS[25] ~ 'IX',       
      ntee_major_group %in% LETTERS[26] ~ 'X',
      TRUE ~ NA_character_
    ), 
    ntee_category_hhs = case_when(
      ntee_category_short %in% c("IV", "V") ~ TRUE, 
      TRUE ~ FALSE
    )
  )



## Begin with the Exempt Organization Master Business File
## https://www.irs.gov/charities-non-profits/exempt-organizations-business-master-file-extract-eo-bmf

col_types <- cols(
  .default = col_character(),
  RULING = col_integer(),
  AFFILIATION = col_double(),
  CLASSIFICATION = col_character(),
  DEDUCTIBILITY = col_double(),
  ORGANIZATION = col_double(),
  TAX_PERIOD = col_double(),
  ASSET_CD = col_double(),
  INCOME_CD = col_double(),
  PF_FILING_REQ_CD = col_double(),
  ASSET_AMT = col_double(),
  INCOME_AMT = col_double(),
  REVENUE_AMT = col_double()
)

eobmf_raw <- dplyr::bind_rows(
  read_csv("https://www.irs.gov/pub/irs-soi/eo1.csv", col_types = col_types),
  read_csv("https://www.irs.gov/pub/irs-soi/eo2.csv", col_types = col_types),
  read_csv("https://www.irs.gov/pub/irs-soi/eo3.csv", col_types = col_types),
  read_csv("https://www.irs.gov/pub/irs-soi/eo4.csv", col_types = col_types)
)

eobmf <- eobmf_raw %>% 
  mutate(
    ntee_major_group = case_when(
      stringr::str_sub(NTEE_CD, 1, 2) == 'CC' ~ 'COMMON', 
      stringr::str_sub(NTEE_CD, 1, 2) != 'CC' ~ stringr::str_sub(NTEE_CD, 1, 1),
      TRUE ~ NA_character_
    ),
    ntee_category_long = case_when(
      stringr::str_sub(NTEE_CD, 1, 2) == 'CC' ~ 'Research, Fundraising, & Technical Assistance',
      ntee_major_group %in% LETTERS[1] ~ 'I. Arts, Culture, and Humanities', 
      ntee_major_group %in% LETTERS[2] ~ 'II. Education', 
      ntee_major_group %in% LETTERS[3:4] ~ 'III. Environment and Animals', 
      ntee_major_group %in% LETTERS[5:8] ~ 'IV. Health', 
      ntee_major_group %in% LETTERS[9:16] ~ 'V. Human Services', 
      ntee_major_group %in% LETTERS[17] ~ 'VI. International, Foreign Affairs', 
      ntee_major_group %in% LETTERS[18:23] ~ 'VII. Public, Societal Benefit',       
      ntee_major_group %in% LETTERS[24] ~ 'VIII. Religion Related',       
      ntee_major_group %in% LETTERS[25] ~ 'IX. Mutual/Membership Benefit',       
      ntee_major_group %in% LETTERS[26] ~ 'X. Unknown, Unclassified',
      TRUE ~ NA_character_
    ), 
    ntee_category_short = case_when(
      stringr::str_sub(NTEE_CD, 1, 2) == 'CC' ~ 'CC',
      ntee_major_group %in% LETTERS[1] ~ 'I', 
      ntee_major_group %in% LETTERS[2] ~ 'II', 
      ntee_major_group %in% LETTERS[3:4] ~ 'III', 
      ntee_major_group %in% LETTERS[5:8] ~ 'IV', 
      ntee_major_group %in% LETTERS[9:16] ~ 'V', 
      ntee_major_group %in% LETTERS[17] ~ 'VI', 
      ntee_major_group %in% LETTERS[18:23] ~ 'VII',       
      ntee_major_group %in% LETTERS[24] ~ 'VIII',       
      ntee_major_group %in% LETTERS[25] ~ 'IX',       
      ntee_major_group %in% LETTERS[26] ~ 'X',
      TRUE ~ NA_character_
    ), 
    ntee_category_hhs = case_when(
      ntee_category_short %in% c("IV", "V") ~ TRUE, 
      TRUE ~ FALSE
    )
  ) %>%
  tidyr::separate(
    col = ACTIVITY, 
    into = c("ACTIVITY1", "ACTIVITY2", "ACTIVITY3"),
    sep = c(3, 6), 
    remove = FALSE
  ) %>% left_join(
    ntee_xwalk_raw %>%
      group_by(
        legacy_activity_code
      ) %>%
      summarise(
        is_hhs_activity1 = max(
          ntee_category_hhs
        )
      ), 
    by = c("ACTIVITY1" = "legacy_activity_code")
  ) %>%
  left_join(
    ntee_xwalk_raw %>%
      group_by(
        legacy_activity_code
      ) %>%
      summarise(
        is_hhs_activity2 = max(
          ntee_category_hhs
        )
      ), 
    by = c("ACTIVITY2" = "legacy_activity_code")
  ) %>%
  left_join(
    ntee_xwalk_raw %>%
      group_by(
        legacy_activity_code
      ) %>%
      summarise(
        is_hhs_activity3 = max(
          ntee_category_hhs
        )
      ), 
    by = c("ACTIVITY3" = "legacy_activity_code")
  ) %>%
  mutate(
    any_hhs = pmax(
      ntee_category_hhs, 
      is_hhs_activity1, 
      is_hhs_activity2, 
      is_hhs_activity3 
    ), 
    any_hhs = ifelse(
      is.na(
        any_hhs
      ), 
      0, 
      any_hhs
    )
  )
