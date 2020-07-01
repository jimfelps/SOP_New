

vp_quote_activity <- read_csv("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/S&OP/Salesforce Docs/vp_quote_activity_20200701.csv", 
                                       col_types = cols(`Created Date` = col_date(format = "%m/%d/%Y"), 
                                                        `Total Tons` = col_double()), na = "0") %>% 
  clean_names() %>% 
  select(opportunity_name,
         created_date,
         total_tons,
         amount) %>% 
  replace_na(
    list(
      total_tons = 0,
      amount = 0
    )
  ) %>% 
  group_by(
    opportunity_name, created_date
  ) %>% 
  summarise(
    across(
      c("total_tons", "amount"),
      mean
       )
  ) %>% 
  ungroup() %>% 
  mutate(
    quote_week = ceiling_date(created_date, "weeks", week_start = getOption("lubridate.week.start", 5))
  )

butler_quote_activity <- read_csv("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/S&OP/Salesforce Docs/butler_quote_activity_20200701.csv", 
                                           col_types = cols(`Created Date` = col_date(format = "%m/%d/%Y"))) %>% 
  rename(Amount = `Amount of Expected Opportunity`) %>% 
  clean_names() %>% 
  select(opportunity_name,
         created_date,
         total_tons,
         amount) %>% 
  replace_na(
    list(
      total_tons = 0,
      amount = 0
    )
  ) %>% 
  group_by(
    opportunity_name, created_date
  ) %>% 
  summarise(
    across(
      c("total_tons", "amount"),
      mean
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    quote_week = ceiling_date(created_date, "weeks", week_start = getOption("lubridate.week.start", 5))
  ) 

all_quote_activity <- bind_rows("VP" = vp_quote_activity,
                               "BUTLER" = butler_quote_activity,
                               .id = "brand") 
write_xlsx(all_quote_activity, str_c("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/S&OP/Salesforce Docs/output/quote_activity_",{current_day},".xlsx"))

write_csv(all_quote_activity, str_c("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/S&OP/Salesforce Docs/output/quote_activity_",{current_day},".csv"))

all_quote_activity %>%
  filter(opportunity_name != "Veritiv Reynosa") %>% 
  ggplot(aes(quote_week, total_tons)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(brand))
