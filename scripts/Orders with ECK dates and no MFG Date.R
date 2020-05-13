
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(readxl)
  library(writexl)
  
  orders_w_nomfg_have_eck <- read_excel("~/R/R Data/S&OP/Engineering/orders_w_nomfg_have_eck.xlsx", 
                                        col_types = c("text", "text", "text", 
                                                      "text", "text", "text", "text", "text", 
                                                      "text", "text", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "text", "text", "text", 
                                                      "text")) %>%
    clean_names()
  
  no_mfg_has_eck <- orders_w_nomfg_have_eck %>%
    filter(
      is.na(mfg_scheduled_start_date),
      !is.na(eck_scheduled_completion_date),
      bucket %in% c("JUL-2020", "AUG-2020","SEP-2020","OVER 6")
    ) %>%
    mutate(
      eck_scheduled_completion_date = mdy(eck_scheduled_completion_date)
    )
  
  no_mfg_not_eck <- no_mfg_has_eck %>%
    mutate(
      brand = 
        if_else(
          str_detect(
            transaction_type, 
            regex("css", ignore_case = TRUE)
          ),
          "CSS",
          division
        )
    ) %>%
    filter(
      eck_scheduled_completion_date >= Sys.Date(),
      eck_scheduled_completion_date < "2029-10-31"
    )

already_eck <- no_mfg_has_eck %>%
    mutate(
      brand = 
        if_else(
          str_detect(
            transaction_type, 
            regex("css", ignore_case = TRUE)
          ),
          "CSS",
          division
        )
    ) %>%
    filter(
      eck_scheduled_completion_date <= Sys.Date()
    )
  

write_xlsx(no_mfg_not_eck, 
           "//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/no_mfg_not_eck.xlsx")
