#Title: Scenario MIP Paper - Section 3.2 (CDR and CCS) data extraction
#Purpose: Extract the numbers requested for the "LN marker" writeup (4 requirement blocks)
#         and export them to a single, readable Excel workbook.
#Note: Reuses the data-loading logic of main.R (same joins, same unit conversions,
#      same cumulative-integration method per model reporting frequency).
#Run from: LN_Marker/R  (working directory), i.e. Rscript output_LN_analysis.R

#Package load-------------------------------------------------------------------
library(tidyverse)
library(stringr)
library(openxlsx)

#Setting------------------------------------------------------------------------
v_download <- "20260803"

if (!dir.exists("../output/table/main")) dir.create("../output/table/main", recursive = TRUE)

year_cols <- paste0("X", seq(2020, 2100, 5))

#define import--------------------------------------------------------------
df_define  <- read_csv("../define/define.csv", locale = locale(encoding = "shift-jis"), show_col_types = FALSE) %>%
  mutate(year1 = as.character(year1))
df_variable <- read_csv("../define/variable.csv", locale = locale(encoding = "shift-jis"), show_col_types = FALSE)

#ScenarioMIP (7 IAM) data import---------------------------------------------
df_snap <- data.frame()
for (i in df_define$model4[!is.na(df_define$model4)]) {
  df_snap <- df_snap %>%
    bind_rows(read.csv(paste0("../data/", i, v_download, ".csv"), header = TRUE))
}

df_snap <- df_snap %>%
  filter(str_detect(Variable, paste(df_define$filter_variable, collapse = "|"))) %>%
  select(Model, Scenario, Region, Variable, Unit, all_of(year_cols)) %>%
  pivot_longer(cols = -c(Model, Scenario, Region, Variable, Unit), names_to = "Year", values_to = "Value", names_prefix = "X") %>%
  inner_join(select(df_define, model, scenario, scenario_category, SSP),
             by = c("Model" = "model", "Scenario" = "scenario")) %>%
  mutate(Scenario_SSP = paste(SSP, scenario_category, sep = "_")) %>%
  inner_join(select(df_define, model1, model2, model3), by = c("Model" = "model1")) %>%
  mutate(Model = model2, Model_Initial = model3, Scenario = scenario_category) %>%
  select(-c(model2, model3, scenario_category)) %>%
  mutate(Value = case_when(Unit == "Mt CO2/yr" ~ Value / 1000,
                            Unit == "Mt CO2-equiv/yr" ~ Value / 1000,
                            TRUE ~ Value)) %>%
  mutate(Unit = case_when(Unit == "Mt CO2/yr" ~ "Gt CO2/yr",
                           Unit == "Mt CO2-equiv/yr" ~ "Gt CO2-equiv/yr",
                           TRUE ~ Unit)) %>%
  filter(Region == "World")

#Gross positive CO2 emissions = Net CO2 emissions + Carbon Removal (per Model/Scenario_SSP/Year)
df_snap <- df_snap %>%
  bind_rows(df_snap %>%
              filter(Variable %in% c("Emissions|CO2", "Carbon Removal")) %>%
              select(Model, Model_Initial, Region, Scenario, SSP, Scenario_SSP, Unit, Year, Variable, Value) %>%
              pivot_wider(names_from = Variable, values_from = Value) %>%
              filter(!is.na(`Emissions|CO2`), !is.na(`Carbon Removal`)) %>%
              mutate(Value = `Emissions|CO2` + `Carbon Removal`, Variable = "Gross Positive CO2 Emissions") %>%
              select(-c(`Emissions|CO2`, `Carbon Removal`)))

#Cumulative CCS / CDR, following main.R's three reporting-frequency groups----
#  group1 (5-yr steps throughout):            AIM, GCAM, WITCH
#  group2 (5-yr to 2060, 10-yr after):        MESSAGEix-GLOBIOM, REMIND-MAgPIE
#  group3 (5-yr to 2050, 10-yr after):        IMAGE, COFFEE
add_cumulative <- function(df, models) {
  df %>%
    filter(Model %in% models) %>%
    mutate(Year = paste0("X", Year), Variable = paste(Variable, "cumulative", sep = "|")) %>%
    pivot_wider(names_from = Year, values_from = Value)
}

cum_group1 <- add_cumulative(df_snap, c("AIM", "GCAM", "WITCH")) %>%
  mutate(X2100 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060+X2065+X2070+X2075+X2080+X2085+X2090+X2095+X2100/2)*5) %>%
  mutate(X2090 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060+X2065+X2070+X2075+X2080+X2085+X2090/2)*5) %>%
  mutate(X2080 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060+X2065+X2070+X2075+X2080/2)*5) %>%
  mutate(X2070 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060+X2065+X2070/2)*5) %>%
  mutate(X2060 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5) %>%
  mutate(X2050 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5) %>%
  mutate(X2040 = (X2020/2+X2025+X2030+X2035+X2040/2)*5) %>%
  mutate(X2030 = (X2020/2+X2025+X2030/2)*5) %>%
  mutate(X2020 = X2020)

cum_group2 <- add_cumulative(df_snap, c("MESSAGEix-GLOBIOM", "REMIND-MAgPIE")) %>%
  mutate(X2100 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5+(X2060/2+X2070+X2080+X2090+X2100/2)*10) %>%
  mutate(X2090 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5+(X2060/2+X2070+X2080+X2090/2)*10) %>%
  mutate(X2080 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5+(X2060/2+X2070+X2080/2)*10) %>%
  mutate(X2070 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5+(X2060/2+X2070/2)*10) %>%
  mutate(X2060 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5) %>%
  mutate(X2050 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5) %>%
  mutate(X2040 = (X2020/2+X2025+X2030+X2035+X2040/2)*5) %>%
  mutate(X2030 = (X2020/2+X2025+X2030/2)*5) %>%
  mutate(X2020 = X2020)

cum_group3 <- add_cumulative(df_snap, c("IMAGE", "COFFEE")) %>%
  mutate(X2100 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5+(X2050/2+X2060+X2070+X2080+X2090+X2100/2)*10) %>%
  mutate(X2090 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5+(X2050/2+X2060+X2070+X2080+X2090/2)*10) %>%
  mutate(X2080 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5+(X2050/2+X2060+X2070+X2080/2)*10) %>%
  mutate(X2070 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5+(X2050/2+X2060+X2070/2)*10) %>%
  mutate(X2060 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5+(X2050/2+X2060/2)*10) %>%
  mutate(X2050 = (X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5) %>%
  mutate(X2040 = (X2020/2+X2025+X2030+X2035+X2040/2)*5) %>%
  mutate(X2030 = (X2020/2+X2025+X2030/2)*5) %>%
  mutate(X2020 = X2020)

df_cumulative <- bind_rows(cum_group1, cum_group2, cum_group3) %>%
  pivot_longer(cols = all_of(intersect(names(.), paste0("X", seq(2020,2100,10)))), names_to = "Year", values_to = "Value", names_prefix = "X") %>%
  mutate(Unit = str_replace_all(Unit, pattern = "/yr", replacement = ""))

df_snap <- bind_rows(df_snap, df_cumulative)

#AR6 database (benchmarking, section 3)--------------------------------------
df_AR6 <- read.csv("../data/AR6_Scenario_Database.csv", header = TRUE) %>%
  filter(str_detect(Variable, paste(df_define$filter_variable, collapse = "|"))) %>%
  pivot_longer(cols = !c(Model, Scenario, Region, Variable, Unit), names_to = "Year", values_to = "Value", names_prefix = "X") %>%
  filter(!(Value %in% NA)) %>%
  left_join(openxlsx::read.xlsx("../data/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx", sheet = "meta_Ch3vetted_withclimate") %>%
              select(Model, Scenario, Category)) %>%
  filter(!(Category %in% NA)) %>%
  filter(Year %in% df_define$year1) %>%
  filter(Region == "World") %>%
  mutate(Value = case_when(Unit == "Mt CO2/yr" ~ Value / 1000,
                            Unit == "Mt CO2-equiv/yr" ~ Value / 1000,
                            TRUE ~ Value)) %>%
  mutate(Unit = case_when(Unit == "Mt CO2/yr" ~ "Gt CO2/yr",
                           Unit == "Mt CO2-equiv/yr" ~ "Gt CO2-equiv/yr",
                           TRUE ~ Unit))

df_AR6 <- df_AR6 %>%
  bind_rows(df_AR6 %>%
              filter(Variable %in% c("Carbon Sequestration|Land Use", "Carbon Sequestration|CCS|Biomass",
                                      "Carbon Sequestration|Direct Air Capture", "Carbon Sequestration|Enhanced Weathering")) %>%
              group_by(Model, Region, Unit, Year, Scenario, Category) %>%
              summarize(Value = sum(abs(Value)), .groups = "drop") %>%
              mutate(Variable = "Carbon Removal")) %>%
  bind_rows(df_AR6 %>%
              filter(Variable == "Carbon Sequestration|CCS" | Variable == "Carbon Sequestration|Direct Air Capture") %>%
              group_by(Model, Region, Unit, Year, Scenario, Category) %>%
              summarize(Value = sum(abs(Value)), .groups = "drop") %>%
              mutate(Variable = "Carbon Capture|Geological Storage"))

#AR6 cumulative (decadal data -> 10-yr trapezoid, matches main.R)
df_AR6_cum <- df_AR6 %>%
  filter(Variable %in% c("Carbon Removal", "Carbon Capture|Geological Storage")) %>%
  mutate(Year = paste0("X", Year), Variable = paste(Variable, "cumulative", sep = "|")) %>%
  pivot_wider(names_from = Year, values_from = Value) %>%
  mutate(X2100 = (X2020/2+X2030+X2040+X2050+X2060+X2070+X2080+X2090+X2100/2)*10) %>%
  select(Model, Region, Variable, Unit, Scenario, Category, X2100) %>%
  pivot_longer(cols = all_of(intersect(names(.), paste0("X", seq(2020,2100,10)))), names_to = "Year", values_to = "Value", names_prefix = "X") %>%
  mutate(Unit = str_replace_all(Unit, pattern = "/yr", replacement = ""))

df_AR6 <- bind_rows(df_AR6, df_AR6_cum)

#=============================================================================
# helper
#=============================================================================
get_val <- function(df, model, scen, var, yr) {
  v <- df %>% filter(Model == model, Scenario_SSP == scen, Variable == var, Year == as.character(yr)) %>% pull(Value)
  if (length(v) == 0) NA_real_ else v[1]
}

marker6 <- c("SSP3_H", "SSP2_M", "SSP2_ML", "SSP2_L", "SSP1_VL", "SSP2_LN")
label6  <- c("AIM_SSP3_H", "AIM_SSP2_M(SSP2_M)", "AIM_SSP2_ML(SSP2_ML)", "AIM_SSP2_L(SSP2_L)", "AIM_SSP1_VL(SSP1_VL)", "AIM_SSP2_LN(SSP2_LN)")

wb <- createWorkbook()

#=============================================================================
# 1. LN's position within AIM
#=============================================================================

## 1-1 Carbon Removal annual values (2050, 2100) for the 6 AIM scenarios
t1_cdr <- expand_grid(Scenario_SSP = marker6, Year = c(2050, 2100)) %>%
  rowwise() %>%
  mutate(`Carbon Removal (Gt CO2/yr)` = get_val(df_snap, "AIM", Scenario_SSP, "Carbon Removal", Year)) %>%
  ungroup() %>%
  left_join(tibble(Scenario_SSP = marker6, Label = label6), by = "Scenario_SSP") %>%
  select(Label, Scenario_SSP, Year, `Carbon Removal (Gt CO2/yr)`)

## 1-2 LN peak CDR value & year
ln_cdr_series <- df_snap %>% filter(Model == "AIM", Scenario_SSP == "SSP2_LN", Variable == "Carbon Removal") %>%
  mutate(Year = as.numeric(Year)) %>% arrange(Year)
t1_peak <- ln_cdr_series %>% filter(Value == max(Value, na.rm = TRUE)) %>%
  transmute(Scenario_SSP = "SSP2_LN", `Peak Year` = Year, `Peak Carbon Removal (Gt CO2/yr)` = Value)

## 1-3 Year LN's CDR first exceeds VL's CDR (cross year)
ln_series <- df_snap %>% filter(Model == "AIM", Scenario_SSP == "SSP2_LN", Variable == "Carbon Removal") %>%
  transmute(Year = as.numeric(Year), LN = Value)
vl_series <- df_snap %>% filter(Model == "AIM", Scenario_SSP == "SSP1_VL", Variable == "Carbon Removal") %>%
  transmute(Year = as.numeric(Year), VL = Value)
cross_df <- inner_join(ln_series, vl_series, by = "Year") %>% arrange(Year) %>% mutate(LN_gt_VL = LN > VL)
cross_year <- cross_df %>% filter(LN_gt_VL) %>% slice(1) %>% pull(Year)
t1_cross <- cross_df %>% mutate(`LN > VL` = LN_gt_VL)
t1_cross_summary <- tibble(`Comparison` = "SSP2_LN vs SSP1_VL (AIM, Carbon Removal)",
                            `First year LN exceeds VL` = ifelse(length(cross_year) == 0, NA, cross_year))

## 1-4 Carbon Capture|Geological Storage annual values (2050, 2100)
t1_ccs <- expand_grid(Scenario_SSP = marker6, Year = c(2050, 2100)) %>%
  rowwise() %>%
  mutate(`Carbon Capture|Geological Storage (Gt CO2/yr)` = get_val(df_snap, "AIM", Scenario_SSP, "Carbon Capture|Geological Storage", Year)) %>%
  ungroup() %>%
  left_join(tibble(Scenario_SSP = marker6, Label = label6), by = "Scenario_SSP") %>%
  select(Label, Scenario_SSP, Year, `Carbon Capture|Geological Storage (Gt CO2/yr)`)

## 1-5 Cumulative CCS 2020-2100 (all 6 marker scenarios, AIM)
t1_cumccs <- tibble(Scenario_SSP = marker6) %>%
  rowwise() %>%
  mutate(`Cumulative CCS 2020-2100 (Gt CO2)` = get_val(df_snap, "AIM", Scenario_SSP, "Carbon Capture|Geological Storage|cumulative", 2100)) %>%
  ungroup() %>%
  left_join(tibble(Scenario_SSP = marker6, Label = label6), by = "Scenario_SSP") %>%
  select(Label, Scenario_SSP, `Cumulative CCS 2020-2100 (Gt CO2)`)

## 1-6 LN/L CDR ratio in 2100
ln_2100 <- get_val(df_snap, "AIM", "SSP2_LN", "Carbon Removal", 2100)
l_2100  <- get_val(df_snap, "AIM", "SSP2_L", "Carbon Removal", 2100)
ratio_ln_l <- ln_2100 / l_2100
t1_ratio <- tibble(`AIM_SSP2_LN 2100 Carbon Removal (Gt CO2/yr)` = ln_2100,
                    `AIM_SSP2_L 2100 Carbon Removal (Gt CO2/yr)` = l_2100,
                    `LN / L ratio` = ratio_ln_l,
                    `Sentence` = sprintf("In AIM, returning to the 1.5C pathway after overshoot (LN) requires about %.1fx more CDR in 2100 (%.2f Gt CO2/yr) than staying on the low-overshoot L pathway (%.2f Gt CO2/yr).",
                                         ratio_ln_l, ln_2100, l_2100))

addWorksheet(wb, "1_CDR_annual_AIM6")
writeData(wb, "1_CDR_annual_AIM6", t1_cdr)
addWorksheet(wb, "1_LN_peak")
writeData(wb, "1_LN_peak", t1_peak)
addWorksheet(wb, "1_LN_vs_VL_crossyear")
writeData(wb, "1_LN_vs_VL_crossyear", t1_cross_summary)
writeData(wb, "1_LN_vs_VL_crossyear", t1_cross, startRow = 4)
addWorksheet(wb, "1_CCS_annual_AIM6")
writeData(wb, "1_CCS_annual_AIM6", t1_ccs)
addWorksheet(wb, "1_CumCCS_2020_2100_AIM6")
writeData(wb, "1_CumCCS_2020_2100_AIM6", t1_cumccs)
addWorksheet(wb, "1_LN_L_ratio_2100")
writeData(wb, "1_LN_L_ratio_2100", t1_ratio)

#=============================================================================
# 2. LN vs VL (AIM only, detailed comparison)
#=============================================================================

## 2-1 CDR annual pathway: VL stabilization year/level, LN peak year/level
vl_cdr_series <- df_snap %>% filter(Model == "AIM", Scenario_SSP == "SSP1_VL", Variable == "Carbon Removal") %>%
  mutate(Year = as.numeric(Year)) %>% arrange(Year)
t2_cdr_path <- bind_rows(
  ln_cdr_series %>% mutate(Scenario = "LN"),
  vl_cdr_series %>% mutate(Scenario = "VL")
) %>% select(Scenario, Year, Variable, Unit, Value)
#stabilization year for VL = first year value is within 1% of the final(2100) value going forward and stays flat
vl_stab_level <- vl_cdr_series %>% filter(Year == 2100) %>% pull(Value)
vl_stab_year <- vl_cdr_series %>% filter(abs(Value - vl_stab_level) <= 0.01 * abs(vl_stab_level)) %>% slice(1) %>% pull(Year)
t2_vl_stab <- tibble(Scenario = "SSP1_VL (AIM)", `Stabilization year (within 1% of 2100 level)` = vl_stab_year,
                      `Stabilization level (Gt CO2/yr)` = vl_stab_level)
t2_ln_peak <- t1_peak %>% mutate(Scenario = "SSP2_LN (AIM)") %>% select(Scenario, everything(), -Scenario_SSP)

## 2-2 DAC 2100 (LN, VL)
t2_dac <- tibble(Scenario_SSP = c("SSP2_LN", "SSP1_VL")) %>% rowwise() %>%
  mutate(`Carbon Removal|Geological Storage|Direct Air Capture 2100 (Gt CO2/yr)` =
           get_val(df_snap, "AIM", Scenario_SSP, "Carbon Removal|Geological Storage|Direct Air Capture", 2100)) %>% ungroup()

## 2-3 Biomass 2100 (LN, VL)
t2_bio <- tibble(Scenario_SSP = c("SSP2_LN", "SSP1_VL")) %>% rowwise() %>%
  mutate(`Carbon Removal|Geological Storage|Biomass 2100 (Gt CO2/yr)` =
           get_val(df_snap, "AIM", Scenario_SSP, "Carbon Removal|Geological Storage|Biomass", 2100)) %>% ungroup()

## 2-4 Other CDR techs (weathering, afforestation, etc.) 2100 (LN, VL) - individual + aggregated "other"
other_cdr_vars <- c("Carbon Removal|Enhanced Weathering", "Carbon Removal|Land Use|Re/Afforestation",
                     "Carbon Removal|Land Use|Biochar", "Carbon Removal|Land Use|Soil Carbon Management",
                     "Carbon Removal|Land Use", "Carbon Removal|Land Use|Forest Management",
                     "Carbon Removal|Land Use|Agroforestry", "Carbon Removal|Land Use|Other",
                     "Carbon Removal|Long-Lived Materials", "Carbon Removal|Ocean")
t2_other_detail <- expand_grid(Scenario_SSP = c("SSP2_LN", "SSP1_VL"), Variable = other_cdr_vars) %>%
  rowwise() %>% mutate(`Value 2100 (Gt CO2/yr)` = get_val(df_snap, "AIM", Scenario_SSP, Variable, 2100)) %>% ungroup() %>%
  filter(!is.na(`Value 2100 (Gt CO2/yr)`))
t2_other_sum <- df_snap %>%
  filter(Model == "AIM", Scenario_SSP %in% c("SSP2_LN", "SSP1_VL"), Year == "2100",
         Variable %in% c("Carbon Removal", "Carbon Removal|Geological Storage|Direct Air Capture", "Carbon Removal|Geological Storage|Biomass")) %>%
  select(Scenario_SSP, Variable, Value) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(`Other CDR (= Carbon Removal - DAC - Biomass) 2100 (Gt CO2/yr)` =
           `Carbon Removal` - coalesce(`Carbon Removal|Geological Storage|Direct Air Capture`, 0) - coalesce(`Carbon Removal|Geological Storage|Biomass`, 0)) %>%
  select(Scenario_SSP, `Carbon Removal`, `Other CDR (= Carbon Removal - DAC - Biomass) 2100 (Gt CO2/yr)`)

## 2-5 CCS annual pathway: VL stabilization, LN peak
ln_ccs_series <- df_snap %>% filter(Model == "AIM", Scenario_SSP == "SSP2_LN", Variable == "Carbon Capture|Geological Storage") %>%
  mutate(Year = as.numeric(Year)) %>% arrange(Year)
vl_ccs_series <- df_snap %>% filter(Model == "AIM", Scenario_SSP == "SSP1_VL", Variable == "Carbon Capture|Geological Storage") %>%
  mutate(Year = as.numeric(Year)) %>% arrange(Year)
t2_ccs_path <- bind_rows(ln_ccs_series %>% mutate(Scenario = "LN"), vl_ccs_series %>% mutate(Scenario = "VL")) %>%
  select(Scenario, Year, Variable, Unit, Value)
ln_ccs_peak <- ln_ccs_series %>% filter(Value == max(Value, na.rm = TRUE)) %>%
  transmute(Scenario = "SSP2_LN (AIM)", `Peak Year` = Year, `Peak CCS (Gt CO2/yr)` = Value)
vl_ccs_stab_level <- vl_ccs_series %>% filter(Year == 2100) %>% pull(Value)
vl_ccs_stab_year <- vl_ccs_series %>% filter(abs(Value - vl_ccs_stab_level) <= 0.01 * abs(vl_ccs_stab_level)) %>% slice(1) %>% pull(Year)
t2_ccs_stab <- tibble(Scenario = "SSP1_VL (AIM)", `Stabilization year (within 1% of 2100 level)` = vl_ccs_stab_year,
                       `Stabilization level (Gt CO2/yr)` = vl_ccs_stab_level)

## 2-6 Cumulative CCS 2020-2100 (LN, VL)
t2_cumccs <- tibble(Scenario_SSP = c("SSP2_LN", "SSP1_VL")) %>% rowwise() %>%
  mutate(`Cumulative CCS 2020-2100 (Gt CO2)` = get_val(df_snap, "AIM", Scenario_SSP, "Carbon Capture|Geological Storage|cumulative", 2100)) %>% ungroup()

addWorksheet(wb, "2_CDR_pathway_LN_VL")
writeData(wb, "2_CDR_pathway_LN_VL", t2_cdr_path)
writeData(wb, "2_CDR_pathway_LN_VL", t2_ln_peak, startCol = 7)
writeData(wb, "2_CDR_pathway_LN_VL", t2_vl_stab, startCol = 7, startRow = 4)
addWorksheet(wb, "2_DAC_2100")
writeData(wb, "2_DAC_2100", t2_dac)
addWorksheet(wb, "2_Biomass_2100")
writeData(wb, "2_Biomass_2100", t2_bio)
addWorksheet(wb, "2_OtherCDR_2100")
writeData(wb, "2_OtherCDR_2100", t2_other_sum)
writeData(wb, "2_OtherCDR_2100", t2_other_detail, startRow = 6)
addWorksheet(wb, "2_CCS_pathway_LN_VL")
writeData(wb, "2_CCS_pathway_LN_VL", t2_ccs_path)
writeData(wb, "2_CCS_pathway_LN_VL", ln_ccs_peak, startCol = 7)
writeData(wb, "2_CCS_pathway_LN_VL", t2_ccs_stab, startCol = 7, startRow = 4)
addWorksheet(wb, "2_CumCCS_2020_2100_LN_VL")
writeData(wb, "2_CumCCS_2020_2100_LN_VL", t2_cumccs)

#=============================================================================
# 3. Feasibility benchmark against AR6 database
#=============================================================================

## 3-1 AR6 C2 category, 2100 annual CDR distribution
ar6_c2_cdr2100 <- df_AR6 %>% filter(Region == "World", Year == "2100", Variable == "Carbon Removal", Category %in% c("C1","C2","C3","C4","C5","C6","C7","C8")) %>%
  group_by(Category) %>%
  summarize(n = n(), median = median(Value, na.rm = TRUE), p75 = quantile(Value, 0.75, na.rm = TRUE),
            p95 = quantile(Value, 0.95, na.rm = TRUE), max = max(Value, na.rm = TRUE), .groups = "drop")
t3_cdr_c2 <- ar6_c2_cdr2100 %>% filter(Category == "C2")

## 3-2 AR6 C2, cumulative CCS 2020-2100 distribution
ar6_c2_cumccs <- df_AR6 %>% filter(Region == "World", Year == "2100", Variable == "Carbon Capture|Geological Storage|cumulative",
                                    Category %in% c("C1","C2","C3","C4","C5","C6","C7","C8")) %>%
  group_by(Category) %>%
  summarize(n = n(), median = median(Value, na.rm = TRUE), p75 = quantile(Value, 0.75, na.rm = TRUE),
            p95 = quantile(Value, 0.95, na.rm = TRUE), max = max(Value, na.rm = TRUE), .groups = "drop")
t3_cumccs_c2 <- ar6_c2_cumccs %>% filter(Category == "C2")

## 3-3 AIM_SSP2_LN values to compare against the AR6 C2 distribution above
t3_aim_ln <- tibble(
  `AIM_SSP2_LN 2100 Carbon Removal (Gt CO2/yr)` = get_val(df_snap, "AIM", "SSP2_LN", "Carbon Removal", 2100),
  `AIM_SSP2_LN Cumulative CCS 2020-2100 (Gt CO2)` = get_val(df_snap, "AIM", "SSP2_LN", "Carbon Capture|Geological Storage|cumulative", 2100)
)

## 3-4 Geological storage capacity reference (Gidden et al. 2025 / van Vuuren et al. 2026)
df_geo_country <- openxlsx::read.xlsx("../data/gidden_et_al_geologic_carbon_storage.xlsx", sheet = "country")
t3_storage_ref <- tibble(
  `Reference` = "Gidden et al. (2025) / van Vuuren et al. (2026)",
  `World Planetary (Prudent) Storage Limit (Gt CO2)` = sum(df_geo_country$Planetary_Limit, na.rm = TRUE),
  `World Technical Potential (Gt CO2)` = sum(df_geo_country$Technical_Potential, na.rm = TRUE),
  `Cited reference value (Gt CO2)` = 1460,
  `AIM_SSP2_LN cumulative CCS 2020-2100 (Gt CO2)` = t3_aim_ln$`AIM_SSP2_LN Cumulative CCS 2020-2100 (Gt CO2)`,
  `Share of Planetary Limit used by AIM_SSP2_LN (%)` = 100 * t3_aim_ln$`AIM_SSP2_LN Cumulative CCS 2020-2100 (Gt CO2)` / sum(df_geo_country$Planetary_Limit, na.rm = TRUE)
)

## 3-5 Deployment speed: AIM_SSP2_LN CDR growth 2080->2090 (per decade / 10 = per-yr rate), and max 10-yr rate for other LN models
decade_pairs <- tibble(y0 = seq(2020, 2090, 10), y1 = seq(2030, 2100, 10))
cdr_growth_all <- df_snap %>% filter(Scenario_SSP %in% c("SSP2_LN", "SSP1_VL", "SSP2_L"), Variable == "Carbon Removal") %>%
  mutate(Year = as.numeric(Year)) %>%
  select(Model, Scenario_SSP, Year, Value)

growth_rate_func <- function(model_name, scen) {
  s <- cdr_growth_all %>% filter(Model == model_name, Scenario_SSP == scen) %>% arrange(Year)
  decade_pairs %>% rowwise() %>%
    mutate(v0 = s$Value[s$Year == y0][1], v1 = s$Value[s$Year == y1][1],
           `10yr rate (Gt CO2/yr per yr)` = (v1 - v0) / 10) %>% ungroup() %>%
    mutate(Model = model_name, Scenario_SSP = scen) %>%
    select(Model, Scenario_SSP, y0, y1, v0, v1, `10yr rate (Gt CO2/yr per yr)`)
}

t3_aim_ln_growth <- growth_rate_func("AIM", "SSP2_LN")
t3_aim_ln_growth_2080_2090 <- t3_aim_ln_growth %>% filter(y0 == 2080, y1 == 2090)

models_LN <- c("AIM", "COFFEE", "GCAM", "IMAGE", "MESSAGEix-GLOBIOM", "REMIND-MAgPIE", "WITCH")
t3_multimodel_growth <- map_dfr(models_LN, ~growth_rate_func(.x, "SSP2_LN"))
t3_multimodel_maxrate <- t3_multimodel_growth %>% filter(!is.na(`10yr rate (Gt CO2/yr per yr)`)) %>%
  group_by(Model) %>% slice_max(`10yr rate (Gt CO2/yr per yr)`, n = 1, with_ties = FALSE) %>% ungroup() %>%
  rename(`Peak decade start` = y0, `Peak decade end` = y1) %>% select(-v0, -v1)

addWorksheet(wb, "3_AR6_C2_CDR_2100")
writeData(wb, "3_AR6_C2_CDR_2100", ar6_c2_cdr2100)
addWorksheet(wb, "3_AR6_C2_CumCCS")
writeData(wb, "3_AR6_C2_CumCCS", ar6_c2_cumccs)
addWorksheet(wb, "3_AIM_LN_vs_AR6C2")
writeData(wb, "3_AIM_LN_vs_AR6C2", t3_aim_ln)
writeData(wb, "3_AIM_LN_vs_AR6C2", t3_cdr_c2, startRow = 4)
writeData(wb, "3_AIM_LN_vs_AR6C2", t3_cumccs_c2, startRow = 8)
addWorksheet(wb, "3_StorageCapacity_ref")
writeData(wb, "3_StorageCapacity_ref", t3_storage_ref)
addWorksheet(wb, "3_DeploymentRate_AIM_LN")
writeData(wb, "3_DeploymentRate_AIM_LN", t3_aim_ln_growth_2080_2090)
writeData(wb, "3_DeploymentRate_AIM_LN", t3_aim_ln_growth, startRow = 5)
addWorksheet(wb, "3_DeploymentRate_MultiModel_max")
writeData(wb, "3_DeploymentRate_MultiModel_max", t3_multimodel_maxrate)
writeData(wb, "3_DeploymentRate_MultiModel_max", t3_multimodel_growth, startRow = 12)

#=============================================================================
# 4. Multi-model comparison (LN scenario only, 7 models)
#=============================================================================

t4_cdr <- expand_grid(Model = models_LN, Year = c(2050, 2100)) %>% rowwise() %>%
  mutate(`Carbon Removal (Gt CO2/yr)` = get_val(df_snap, Model, "SSP2_LN", "Carbon Removal", Year)) %>% ungroup()

t4_netco2 <- tibble(Model = models_LN) %>% rowwise() %>%
  mutate(`Net CO2 Emissions 2100 (Gt CO2/yr)` = get_val(df_snap, Model, "SSP2_LN", "Emissions|CO2", 2100)) %>% ungroup()

t4_grosspos <- tibble(Model = models_LN) %>% rowwise() %>%
  mutate(`Gross Positive CO2 Emissions 2100 (Gt CO2/yr) [Net + CDR]` = get_val(df_snap, Model, "SSP2_LN", "Gross Positive CO2 Emissions", 2100)) %>% ungroup()

t4_cumccs <- tibble(Model = models_LN) %>% rowwise() %>%
  mutate(`Cumulative Carbon Capture|Geological Storage 2020-2100 (Gt CO2)` =
           get_val(df_snap, Model, "SSP2_LN", "Carbon Capture|Geological Storage|cumulative", 2100)) %>% ungroup()

t4_price <- tibble(Model = models_LN) %>% rowwise() %>%
  mutate(`Price|Carbon 2100` = get_val(df_snap, Model, "SSP2_LN", "Price|Carbon", 2100)) %>% ungroup()

t4_combined <- t4_cdr %>%
  pivot_wider(names_from = Year, values_from = `Carbon Removal (Gt CO2/yr)`, names_prefix = "CarbonRemoval_") %>%
  left_join(t4_netco2, by = "Model") %>%
  left_join(t4_grosspos, by = "Model") %>%
  left_join(t4_cumccs, by = "Model") %>%
  left_join(t4_price, by = "Model")

addWorksheet(wb, "4_MultiModel_LN_combined")
writeData(wb, "4_MultiModel_LN_combined", t4_combined)
addWorksheet(wb, "4_MultiModel_CDR_annual")
writeData(wb, "4_MultiModel_CDR_annual", t4_cdr)
addWorksheet(wb, "4_MultiModel_NetCO2_2100")
writeData(wb, "4_MultiModel_NetCO2_2100", t4_netco2)
addWorksheet(wb, "4_MultiModel_GrossPosCO2_2100")
writeData(wb, "4_MultiModel_GrossPosCO2_2100", t4_grosspos)
addWorksheet(wb, "4_MultiModel_CumCCS")
writeData(wb, "4_MultiModel_CumCCS", t4_cumccs)
addWorksheet(wb, "4_MultiModel_CarbonPrice_2100")
writeData(wb, "4_MultiModel_CarbonPrice_2100", t4_price)

#=============================================================================
# README
#=============================================================================
readme <- tibble(Note = c(
  "This workbook was produced by LN_Marker/R/output_LN_analysis.R",
  "Data sources: LN_Marker/data/{AIM,COFFEE,GCAM,IMAGE,MESSAGEix,REMIND,WITCH}20260803.csv (ScenarioMIP submissions), LN_Marker/data/AR6_Scenario_Database.csv + metadata xlsx, LN_Marker/data/gidden_et_al_geologic_carbon_storage.xlsx",
  "Units: Gt CO2/yr for flows, Gt CO2 for cumulative stocks (converted from Mt CO2/yr where source data used Mt)",
  "Scenario naming: Scenario_SSP = paste(SSP, scenario_category, sep='_'), e.g. SSP2_LN = AIM's Low-overshoot-to-1.5C ('LN') marker scenario under SSP2",
  "Cumulative CCS/CDR integration: trapezoidal, computed at each model's native reporting frequency (5-yr for AIM/GCAM/WITCH; 5-yr to 2060 then 10-yr for MESSAGEix-GLOBIOM/REMIND-MAgPIE; 5-yr to 2050 then 10-yr for IMAGE/COFFEE; 10-yr throughout for the AR6 database)",
  "Gross Positive CO2 Emissions = Net CO2 Emissions (Emissions|CO2) + Carbon Removal, per the requested 'net + CDR' method",
  "AR6 C2 quantiles computed across all vetted C2-category scenarios (meta_Ch3vetted_withclimate), World region only",
  "Section 3 storage reference: World Planetary (Prudent) Storage Limit ~1460 GtCO2, from Gidden et al. geologic carbon storage dataset (matches the ~1460 GtCO2 figure cited by Gidden et al., 2025 / van Vuuren et al., 2026)",
  "Deployment rate = (CDR[year+10] - CDR[year]) / 10, i.e. average annual increase in CDR over each decade"
))
addWorksheet(wb, "README")
writeData(wb, "README", readme)
setColWidths(wb, "README", cols = 1, widths = 140)

saveWorkbook(wb, "../output/table/main/LN_CDR_CCS_Analysis.xlsx", overwrite = TRUE)

cat("Done. Workbook written to LN_Marker/output/table/main/LN_CDR_CCS_Analysis.xlsx\n")
