
######### SALESFORCE CLEANING --------------------------------------------------------------------------------------

opp_with_complexity <- all_opportunities %>%
  replace_na(list(
    engineering_hours = 0,
    engineering_hours_per_ton = 0,
    total_tons = 0)) %>%
  mutate(complexity = if_else(engineering_hours == 0, "No Engineering",
                              if_else(engineering_hours > 0 & engineering_hours <= 100, "Simple (<=100)",
                                      if_else(engineering_hours > 100 & engineering_hours <= 200, "Moderate (100-200)",
                                              if_else(engineering_hours > 200 & engineering_hours <= 400, "Complex (200-400)",
                                                      if_else(engineering_hours > 400 & engineering_hours <= 800, "High Complex (400-800)",
                                                              if_else(engineering_hours > 800, "Very High Complex (>800)", "Parts Order"))))))) %>%
  mutate(complexity = factor(complexity, levels = c("No Engineering",
                                                    "CII/FT",
                                                    "Simple (<=100)", 
                                                    "Moderate (100-200)", 
                                                    "Complex (200-400)",
                                                    "High Complex (400-800)",
                                                    "Very High Complex (>800)",
                                                    "Parts Order")),
         close_month = floor_date(close_date, unit = "month"),
         ship_month = floor_date(ship_date, unit = "month"),
         factored_tons = round(total_tons * (probability_percent/100),2),
         factored_hours = round(engineering_hours * (probability_percent/100),2),
         costco_flag = if_else(str_detect(opportunity_name, regex("costco", ignore_case = TRUE)), "Yes", "No"),
         bucket = if_else(ship_month < current_month, "PRIOR",
                          if_else(ship_month == current_month, str_to_upper(as.character(format(current_month, "%b-%Y"))),
                          if_else(ship_month == floor_date(current_month + months(1), "month"), str_to_upper(as.character(format(floor_date(current_month + months(1), "month"), "%b-%Y"))),
                          if_else(ship_month == floor_date(current_month + months(2), "month"), str_to_upper(as.character(format(floor_date(current_month + months(2), "month"), "%b-%Y"))),
                          if_else(ship_month == floor_date(current_month + months(3), "month"), str_to_upper(as.character(format(floor_date(current_month + months(3), "month"), "%b-%Y"))),
                          if_else(ship_month == floor_date(current_month + months(4), "month"), str_to_upper(as.character(format(floor_date(current_month + months(4), "month"), "%b-%Y"))),
                          if_else(ship_month == floor_date(current_month + months(5), "month"), str_to_upper(as.character(format(floor_date(current_month + months(5), "month"), "%b-%Y"))),
                          if_else(ship_month > floor_date(current_month + months(5), "month"), "OVER 6", "Other"))))))))) %>%
  filter(!is.na(bucket))

# use str_to_upper to correct case formatting then figure the rest out

opp_with_complexity$bucket <- str_to_upper(opp_with_complexity$bucket)

opp_with_complexity$bucket <- factor(opp_with_complexity$bucket,
                                     levels = c("PRIOR",
                                                str_to_upper(as.character(format(current_month, "%b-%Y"))),
                                                str_to_upper(as.character(format(floor_date(current_month + months(1), "month"), "%b-%Y"))),
                                                str_to_upper(as.character(format(floor_date(current_month + months(2), "month"), "%b-%Y"))),
                                                str_to_upper(as.character(format(floor_date(current_month + months(3), "month"), "%b-%Y"))),
                                                str_to_upper(as.character(format(floor_date(current_month + months(4), "month"), "%b-%Y"))),
                                                str_to_upper(as.character(format(floor_date(current_month + months(5), "month"), "%b-%Y"))),
                                                "OVER 6"))

## BACKUP IF THERE'S AN ISSUE WITH #c("PRIOR",
#  "MAR-2020",
#  "APR-2020",
#  "MAY-2020",
#  "JUN-2020",
#  "JUL-2020",
#  "AUG-2020",
#  "OVER 6")

backlog_w_complexity <- backlog_tons_detail %>%
  filter(!is.na(bucket)) %>%
  mutate(costco_flag = if_else(str_detect(project_name, regex("costco", ignore_case = TRUE)), "Yes", "No")) %>%
  left_join(eng_complexity_lookup, by = c("proj_num" = "proj_num")) %>%
  select(-region.y) %>%
  rename("brand" = "division",
         "account_name" = "customer_name") %>%
  mutate(complexity = if_else(is.na(complexity), "Parts Order", complexity))

backlog_w_complexity$complexity <- factor(backlog_w_complexity$complexity,
                                          levels = c("No Engineering",
                                                     "CII/FT",
                                                     "Simple (<=100)",
                                                     "Moderate (100-200)",
                                                     "Complex (200-400)",
                                                     "High Complex (400-800)",
                                                     "Very High Complex (>800)",
                                                     "Parts Order"))

backlog_join <- backlog_w_complexity %>%
  select(brand,
         account_name,
         total_tons,
         bucket,
         complexity,
         costco_flag)

opp_join <- opp_with_complexity %>%
  select(brand,
         account_name,
         factored_tons,
         bucket,
         complexity,
         costco_flag) %>%
  rename("total_tons" = "factored_tons")

backlog_and_opportun <- bind_rows("BACKLOG" = backlog_join,
                                  "OPP" = opp_join,
                                  .id = "type")

backlog_and_opportun$bucket <- factor(backlog_and_opportun$bucket,
                                      levels = c("PRIOR",
                                                 str_to_upper(as.character(format(current_month, "%b-%Y"))),
                                                 str_to_upper(as.character(format(floor_date(current_month + months(1), "month"), "%b-%Y"))),
                                                 str_to_upper(as.character(format(floor_date(current_month + months(2), "month"), "%b-%Y"))),
                                                 str_to_upper(as.character(format(floor_date(current_month + months(3), "month"), "%b-%Y"))),
                                                 str_to_upper(as.character(format(floor_date(current_month + months(4), "month"), "%b-%Y"))),
                                                 str_to_upper(as.character(format(floor_date(current_month + months(5), "month"), "%b-%Y"))),
                                                 "OVER 6"))

write_csv(opp_with_complexity, "~/R/R Data/S&OP/charts/opp_with_complexity.csv")
write_csv(backlog_and_opportun, "~/R/R Data/S&OP/charts/backlog_and_opportun.csv")



########## ORDER ENTRY CLEANING (BASED ON SF.COM CLOSED/WON) -----------------------------------------------------------------------

order_entry_w_complexity <- order_entry_all %>%
  replace_na(list(
    engineering_hours = 0
  )) %>%
  mutate(complexity = if_else(engineering_hours > 0 & engineering_hours <= 100, "Simple",
                              if_else(engineering_hours > 100 & engineering_hours <= 200, "Moderate",
                                      if_else(engineering_hours > 200 & engineering_hours <= 400, "Complex",
                                              if_else(engineering_hours > 400 & engineering_hours <= 800, "High Complexity",
                                                      if_else(engineering_hours > 800, "Very High Complexity", "No Engineering")))))) %>%
  group_by(brand, complexity, close_month) %>%
  summarise(tons = round(sum(total_tons),2),
            sales = round(sum(expected_revenue),2),
            eng_hours = round(sum(engineering_hours),2),
            margin = round(sum(margin),2),
            margin_per_hour = if_else(is.infinite(margin/eng_hours), 0, round(margin/eng_hours,2)),
            order_count = n()) %>%
  mutate(phpt = round(eng_hours/tons,2))

order_entry_w_complexity$complexity <- factor(order_entry_w_complexity$complexity,
                                              levels = c("No Engineering",
                                                         "Simple",
                                                         "Moderate",
                                                         "Complex",
                                                         "High Complexity",
                                                         "Very High Complexity"))

order_entry_w_complexity_detail <- order_entry_all %>%
  replace_na(list(
    engineering_hours = 0
  )) %>%
  mutate(complexity = if_else(engineering_hours > 0 & engineering_hours <= 100, "Simple",
                              if_else(engineering_hours > 100 & engineering_hours <= 200, "Moderate",
                                      if_else(engineering_hours > 200 & engineering_hours <= 400, "Complex",
                                              if_else(engineering_hours > 400 & engineering_hours <= 800, "High Complexity",
                                                      if_else(engineering_hours > 800, "Very High Complexity", "No Engineering"))))))
order_entry_w_complexity_detail$complexity <- factor(order_entry_w_complexity_detail$complexity,
                                                     levels = c("No Engineering",
                                                                "Simple",
                                                                "Moderate",
                                                                "Complex",
                                                                "High Complexity",
                                                                "Very High Complexity"))

write_xlsx(order_entry_w_complexity_detail,"~/R/R Data/S&OP/charts/oe_detail_for_meeting.xlsx")

########## MBR CHARTS ------------------------------------------------------------------------------------------------------

mbr_chart_metrics_all <-  mbr_charts %>%
  filter(fiscal_year == "FY20"| fiscal_year == "FY19") %>%
  replace_na(list(
    A_Hrs = 0,
    Act_Tons = 0,
    Utilized_Hrs = 0,
    Total_Hrs = 0,
    Sold_Hrs = 0,
    Quote_Tons = 0,
    OT_ECK = 0,
    num_ECK = 0
  )) %>%
  mutate(phpt = round(A_Hrs/Act_Tons,2),
         utilization = round(Utilized_Hrs/Total_Hrs,2),
         AvQ_Hrs = round(A_Hrs/Sold_Hrs,2),
         AvQ_Tons = round(Act_Tons/Quote_Tons,2),
         eck_ot = round(OT_ECK/num_ECK,2)) 


########## AVQ BRAND/COMPLEX CLEANING -------------------------------------------------------------------------------------

new_oswt <- Engineering_Metrics %>%
  mutate(brand = if_else(str_detect(order_type, "CLAIM"), "CLAIM",
                         if_else(str_detect(order_type, "PART"), "PARTS ORDER",
                                 if_else(str_detect(order_type, "GB"), "BUTLER",
                                         if_else(str_detect(order_type, "VP"), "VP",
                                                 if_else(str_detect(order_type, "CSS/CONV"), "CSS",
                                                         if_else(str_detect(order_type, "CSS/HS"), "HEAVY STRUCTURES",
                                                                 if_else(str_sub(order_type, 1,4) == "ROOF", "BUTLER", "Other"))))))),
         avq_hours = round(actual_hours/budgeting_hours,2),
         corp = c("BBNA"),
         act_tons = if_else(order_status == "EN", planned_tons, eng_mfst_tons),
         quote_tons = planned_tons,
         avq_tons = round(act_tons/quote_tons,2)) %>%
  select(corp,
         brand,
         avq_hours,
         project_name,
         order_number,
         proj_num,
         project_manager,
         builder_name,
         actual_hours,
         budgeting_hours,
         act_tons,
         quote_tons,
         avq_tons) %>%
  filter(brand == "BUTLER" |
           brand == "VP",
         !is.nan(avq_hours),
         !is.infinite(avq_hours)) %>%
  left_join(eng_complexity_lookup, by = c("proj_num" = "proj_num")) %>%
  group_by(brand, complexity) %>%
  mutate(pct_hours = round(actual_hours/sum(actual_hours),2),
         five_pct = if_else(pct_hours >= 0.05, "Yes", "No")) %>%
  ungroup() %>%
  filter(!is.na(complexity))                                             # removing NA's but need to review in future months (only one in January when developing)

new_oswt$complexity <- factor(new_oswt$complexity,
                              levels = c("CII/FT",
                                         "Simple (<=100)",
                                         "Moderate (100-200)",
                                         "Complex (200-400)",
                                         "High Complex (400-800)",
                                         "Very High Complex (>800)"))

new_oswt$complexity <- fct_recode(new_oswt$complexity,
                                  "Simple" = "Simple (<=100)",
                                  "Moderate" = "Moderate (100-200)",
                                  "Complex" = "Complex (200-400)",
                                  "High Complexity" = "High Complex (400-800)",
                                  "Very High Complexity" = "Very High Complex (>800)")

new_oswt <- new_oswt %>%
  mutate(over_hours = if_else(complexity == "CII/FT" & actual_hours > 40, "Yes",
                              if_else(complexity == "Simple" & actual_hours > 100, "Yes",
                                      if_else(complexity == "Moderate" & actual_hours > 200, "Yes",
                                              if_else(complexity == "Complex" & actual_hours > 400, "Yes",
                                                      if_else(complexity == "High Complexity" & actual_hours > 800, "Yes",
                                                              if_else(complexity == "Very High Complexity" & actual_hours > 1000, "Yes", "No")))))),
         highlighted = if_else(brand == "VP" & project_name == "19-2614 NEXUS CENTER PEMB - LA" |
                                 brand == "BUTLER" & project_name == "COSTCO-ONTARIO,CA (BUSINESS CE", "Yes", "No"))

butler_test_oswt <- new_oswt %>%
  filter(highlighted == "Yes",
         brand == "BUTLER")

butler_avg <- new_oswt %>%
  group_by(brand) %>%
  summarise(
    brand_mean = round(mean(avq_hours),2),
    brand_median = round(median(avq_hours),2)
  ) %>%
  filter(brand == "BUTLER")
vp_avg <- new_oswt %>%
  group_by(brand) %>%
  summarise(
    brand_mean = round(mean(avq_hours),2),
    brand_median = round(median(avq_hours),2)
  ) %>%
  filter(brand == "VP")
butler_complex_avg <- new_oswt %>%
  group_by(brand, complexity) %>%
  summarise(
    complex_mean = round(sum(actual_hours)/sum(budgeting_hours),2),
    complex_median = round(median(avq_hours),2),
    act_hours = sum(actual_hours),
    bud_hours = sum(budgeting_hours),
    hours_over = sum(actual_hours) - sum(budgeting_hours * 1.14),
    tons_mean = round(sum(act_tons)/sum(quote_tons),2),
    tons_variance = sum(act_tons) - sum(quote_tons)
  ) %>%
  filter(brand == "BUTLER") %>%
  mutate(
    expectation = c(1.14),
    finance_impact = complex_mean - expectation,
    info = paste0("Financial Impact of AvQ over\ngoal (1.14) = ", scales::comma(hours_over * -95))
  )
vp_complex_avg <- new_oswt %>%
  group_by(brand, complexity) %>%
  summarise(
    complex_mean = round(sum(actual_hours)/sum(budgeting_hours),2),
    complex_median = round(median(avq_hours),2),
    act_hours = sum(actual_hours),
    bud_hours = sum(budgeting_hours),
    hours_over = sum(actual_hours) - sum(budgeting_hours * 1.14),
    tons_mean = round(sum(act_tons)/sum(quote_tons),2),
    tons_variance = sum(act_tons) - sum(quote_tons)
  ) %>%
  filter(brand == "VP") %>%
  mutate(
    expectation = c(1.14),
    finance_impact = complex_mean - expectation,
    info = paste0("Financial Impact of AvQ over\ngoal (1.14) = ", scales::comma(hours_over * -95))
  )
financial_impact_table <- new_oswt %>%
  group_by(brand, complexity) %>%
  summarise(
    complex_mean = round(sum(actual_hours)/sum(budgeting_hours),2),
    hours_over = sum(actual_hours) - sum(budgeting_hours * 1.14)
  ) %>%
  mutate(financial_impact = hours_over * -95) %>%
  select(-hours_over, -complex_mean) %>%
  pivot_wider(names_from = brand, values_from = financial_impact) %>%
  rename("Complexity" = "complexity") %>%
  adorn_totals("row") %>%
  mutate(BUTLER = scales::comma(BUTLER),
         VP = scales::comma(VP),
         Notes = c(NA,NA,NA,NA,NA,NA,NA)) %>%
  replace_na(list(
    Notes = ""
  ))
