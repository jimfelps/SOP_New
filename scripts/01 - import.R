

############# SALESFORCE OPPORTUNITIES --------------------------------------------------------------

    ## BUTLER ##

butler_opportunities <- read_csv("~/R/R Data/S&OP/Salesforce/butler_opportunities.csv", 
                                 col_types = cols(`Account Name` = col_character(), 
                                                  Age = col_double(), `Amount of Expected Opportunity` = col_double(), 
                                                  `Amount of Expected Opportunity Currency` = col_skip(), 
                                                  `Created Date` = col_date(format = "%m/%d/%Y"), 
                                                  `Engineering Hours` = col_double(), 
                                                  `Engineering Hours per Ton` = col_double(), 
                                                  `Expected Close Date` = col_date(format = "%m/%d/%Y"), 
                                                  `Factored Amount` = col_double(), 
                                                  `Factored Amount Currency` = col_skip(), 
                                                  `Fiscal Period` = col_character(), 
                                                  `Lead Source` = col_skip(), 
                                                  `Next Step` = col_skip(), `Opportunity Name` = col_character(), 
                                                  `Opportunity Owner` = col_character(), 
                                                  `Order Ship Date` = col_date(format = "%m/%d/%Y"), 
                                                  `Owner Role` = col_character(), `Probability (%)` = col_double(), 
                                                  Stage = col_character(), `Total Tons` = col_double(), 
                                                  Type = col_skip()), na = "0") %>%
  rename(Amount = `Amount of Expected Opportunity`,
         `Expected Revenue` = `Factored Amount`,               
         `Close Date` = `Expected Close Date`,
         `Ship Date` = `Order Ship Date`) %>%
  clean_names("snake") %>%
  replace_na(list(total_tons = 0))

    ## VP ##

vp_opportunities <- read_csv("~/R/R Data/S&OP/Salesforce/vp_opportunities.csv", 
                             col_types = cols(`Account Name` = col_character(), 
                                              Age = col_double(), Amount = col_double(), 
                                              `Close Date` = col_date(format = "%m/%d/%Y"), 
                                              `Created Date` = col_date(format = "%m/%d/%Y"), 
                                              `Engineering Hours` = col_double(), 
                                              `Engineering Hours Per Ton` = col_double(), 
                                              `Expected Revenue` = col_double(), 
                                              `Fiscal Period` = col_character(), 
                                              `Lead Source` = col_skip(), `Next Step` = col_skip(), 
                                              `Opportunity Name` = col_character(), 
                                              `Opportunity Owner` = col_character(), 
                                              `Owner Role` = col_character(), `Probability (%)` = col_double(), 
                                              `Ship Date` = col_date(format = "%m/%d/%Y"), 
                                              Stage = col_character(), `Total Tons` = col_double(), 
                                              Type = col_skip()), na = "0") %>%
  clean_names("snake") %>%
  replace_na(list(total_tons = 0))

    ## Combine VP and Butler ##

all_opportunities <- bind_rows("VP" = vp_opportunities,
                               "BUTLER" = butler_opportunities,
                               .id = "brand")


############# ORDER ENTRY - SALESFORCE CLOSED/WON OPPORTUNITES -----------------------------------------------------

## BUTLER ##

butler_salesforce_order_entry <- read_csv("~/R/R Data/S&OP/Salesforce/butler_salesforce_order_entry.csv", 
                                          col_types = cols(`Amount of Expected Opportunity Currency` = col_skip(), 
                                                           `Close Month` = col_date(format = "%m/%d/%Y"), 
                                                           `Created Date` = col_date(format = "%m/%d/%Y"), 
                                                           `Factored Amount Currency` = col_skip(), 
                                                           `Margin $ Currency` = col_skip())) %>%
  rename(Amount = `Amount of Expected Opportunity`,
         `Expected Revenue` = `Factored Amount`,
         Margin = `Margin $`) %>%
  clean_names("snake")

## VP ##

vp_salesforce_order_entry <- read_csv("~/R/R Data/S&OP/Salesforce/vp_salesforce_order_entry.csv", 
                                      col_types = cols(`Close Month` = col_date(format = "%m/%d/%Y"),
                                                       `Created Date` = col_date(format = "%m/%d/%Y"))) %>%
  rename(Margin = `Factored Margin $`) %>%
  clean_names("snake")

## COMBINE BRANDS INTO ONE TABLE

order_entry_all <- bind_rows("VP" = vp_salesforce_order_entry,
                             "BUTLER" = butler_salesforce_order_entry,
                             .id = "brand")



############# ENGINEERING COMPLEXITY LOOKUP TABLE  -----------------------------------------------------------------

eng_complexity_lookup <- read_csv("~/R/R Data/S&OP/Engineering/eng_complexity_lookup.csv", 
                                  col_types = cols(`Order Number` = col_character())) %>%
  clean_names("snake") %>%
  mutate(proj_num = str_sub(order_number, 1,8)) %>%
  select(proj_num,
         region,
         complexity)

############# BACKLOG REPORT ------------------------------------------------------------------------------------

backlog_tons_detail <- read_excel("~/R/R Data/S&OP/Salesforce/backlog_tons_detail.xlsx",
                                  skip = 3) %>%
  clean_names("snake") %>%
  mutate(bucket = factor(bucket, 
                         levels = c("PRIOR",
                                    str_to_upper(as.character(format(current_month, "%b-%Y"))),
                                    str_to_upper(as.character(format(floor_date(current_month + months(1), "month"), "%b-%Y"))),
                                    str_to_upper(as.character(format(floor_date(current_month + months(2), "month"), "%b-%Y"))),
                                    str_to_upper(as.character(format(floor_date(current_month + months(3), "month"), "%b-%Y"))),
                                    str_to_upper(as.character(format(floor_date(current_month + months(4), "month"), "%b-%Y"))),
                                    str_to_upper(as.character(format(floor_date(current_month + months(5), "month"), "%b-%Y"))),
                                    "OVER 6")),
         proj_num = str_sub(order_number, 1,8))

## BACKUP IF THERE'S AN ISSUE WITH #c("PRIOR",
#  "MAR-2020",
#  "APR-2020",
#  "MAY-2020",
#  "JUN-2020",
#  "JUL-2020",
#  "AUG-2020",
#  "OVER 6")


############# MBR CHARTS ----------------------------------------------------------------------------------------------

mbr_charts <- read_excel("~/R/R Data/S&OP/Engineering/Engineering Metrics.xlsx", 
                         sheet = "Historical Data", col_types = c("text", "numeric", "text", 
                                                                  "text", "text", "text", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric"))

# set factor levels so months appear in correct order
mbr_charts$fiscal_year <- factor(mbr_charts$fiscal_year,
                                 levels = c("FY20",
                                            "FY19",
                                            "FY18"))

mbr_charts$period <- factor(mbr_charts$period, 
                            levels = c("JUL",
                                       "AUG",
                                       "SEP",
                                       "OCT",
                                       "NOV",
                                       "DEC",
                                       "JAN",
                                       "FEB",
                                       "MAR",
                                       "APR",
                                       "MAY",
                                       "JUN",
                                       "YTD"))


############# AVQ BY BRAND AND COMPLEXITY ----------------------------------------------------------------------------

Engineering_Metrics <- read_excel("~/R/R Data/S&OP/Engineering/Engineering Metrics.xlsx", 
                                  sheet = "Compass Order Summary", col_types = c("text","text","text","text","text",
                                                                                 "text","date","date","date","text",
                                                                                 "text","text","text","text","text",
                                                                                 "skip","date","date","date","numeric",
                                                                                 "skip","numeric","numeric","skip","numeric",
                                                                                 "skip","skip","skip","numeric","skip",
                                                                                 "skip","skip","skip","skip","skip",
                                                                                 "skip","skip","skip","skip","skip",
                                                                                 "skip","skip","text","date","date",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","skip","date","date",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","skip","skip","skip",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","text","date","date",
                                                                                 "date","date","text","text"), 
                                  skip = 41) %>%
  clean_names("snake") %>%
  mutate(proj_num = str_sub(order_number, 1,8))


############# NEW LEAD TIMES BY COMPLEXITY CHART

lead_times_summary <- read_excel("~/R/R Data/Engineering/Lead Times Summary.xlsx", 
                                 col_types = c("date", "text", "numeric", "numeric")) %>%
  clean_names() %>% 
  mutate(complexity = factor(complexity,
                             levels = c("Very Simple",
                                        "Simple",
                                        "Moderate",
                                        "Complex",
                                        "High Complex",
                                        "Very High Complex")))
