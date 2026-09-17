#Title: Scenario MIP Paper 
#Date: 2026/4/5
#Name: Osamu Nishiura

#Package load-------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(openxlsx)
library(scales)
#library(gridExtra)

#Setting------------------------------------------------------------------------
v_download <- "20260803"

theme1<-theme(
  panel.background = element_rect(fill = "transparent", colour = "black"),
  panel.grid.major.y = element_line(color = "grey",linewidth=0.2),
  panel.grid.major.x = element_line(color = "grey",linewidth=0.2),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  legend.key = element_blank(),
  strip.text.y = element_text(size = 18), 
  strip.text.x = element_text(size = 18),
  axis.title = element_text(size = 20),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20),
  legend.direction = "vertical",
  legend.position ="right",
  plot.title = element_text(size = 20),
  axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 15),
  axis.text.y = element_text(size = 15)
)

#Directory Preparation----------------------------------------------------------

if (dir.exists("../output/figure/main") == "FALSE") {dir.create("../output/figure/main", recursive = T)}
if (dir.exists("../output/table/main") == "FALSE") {dir.create("../output/table/main", recursive = T)}
if (dir.exists("../output/figure/other") == "FALSE") {dir.create("../output/figure/other", recursive = T)}
if (dir.exists("../output/table/other") == "FALSE") {dir.create("../output/table/other", recursive = T)}
v_path <- c(fig="../output/figure",
            fig_main="../output/figure/main",
            fig_other="../output/figure/other",
            tab="../output/table",
            tab_main="../output/table/main")

#define import-----------------------------------------------------------------

df_define  <- read_csv("../define/define.csv", locale = locale(encoding = "shift-jis"),show_col_types = FALSE)%>%
  mutate(year1=as.character(year1))
names(df_define$color_scenario)<-df_define$scenario1
names(df_define$color_model)<-df_define$model2
names(df_define$color_ar6)<-df_define$ar6_database

df_variable <- read_csv("../define/variable.csv", locale = locale(encoding = "shift-jis"),show_col_types = FALSE)

#ScenarioMIP Data import--------------------------------------------------------

df_snap<-data.frame()
for (i in df_define$model4[!is.na(df_define$model4)]) {
df_snap <- df_snap%>%
  bind_rows(read.csv(paste0("../data/",i,v_download,".csv"), header=T))
}
  
df_snap <- df_snap%>%  
  filter(str_detect(Variable,paste(df_define$filter_variable,collapse="|")))%>%
  select(Model,Scenario,Region,Variable,Unit,X2020,X2025,X2030,X2035,X2040,X2045,X2050,X2055,X2060,X2065,X2070,X2075,X2080,X2085,X2090,X2095,X2100)%>%
  pivot_longer(cols=-c(Model,Scenario,Region,Variable,Unit), names_to='Year', values_to='Value', names_prefix='X')%>%
  inner_join(select(df_define, model, scenario, scenario_category, SSP),
             by=c("Model"="model","Scenario"="scenario"))%>%
  mutate(Scenario_SSP=paste(SSP, scenario_category, sep="_"))%>%
  inner_join(select(df_define,model1,model2,model3),by=c("Model"="model1"))%>%
  mutate(Model=model2,
         Model_Initial=model3,
         Scenario=scenario_category)%>%
  select(-c("model2","model3","scenario_category"))%>%
  mutate(Value=case_when(Unit == "Mt CO2/yr" ~ Value/1000,
                         Unit == "Mt CO2-equiv/yr" ~ Value/1000,
                         TRUE ~ Value))%>%
  mutate(Unit=case_when(Unit == "Mt CO2/yr" ~ "Gt CO2/yr",
                        Unit == "Mt CO2-equiv/yr" ~ "Gt CO2-equiv/yr",
                        TRUE ~ Unit))%>%  
  filter(Region %in% c("World",df_define$region5 ))

df_snap <- df_snap%>%
  bind_rows(df_snap%>%
              filter(Variable=="GDP|MER")%>%
              left_join(df_snap%>%
                          filter(Variable=="GDP|MER",Scenario_SSP=="SSP2_M")%>%
                          select(Model,BaUVal=Value,Year,Region))%>% 
              mutate(Value=(Value-BaUVal)*100/BaUVal,
                     Unit="%",
                     Variable="GDP change from SSP2_M")%>%
              select(-BaUVal))%>%
filter(Model=="AIM"|Scenario_SSP=="SSP2_LN")

df_snap <- df_snap%>%
  bind_rows(df_snap%>%
              filter(Variable %in% df_variable$air_pollutant_energy[!is.na(df_variable$air_pollutant_energy)])%>%
              left_join(df_variable%>%select(air_pollutant,air_pollutant_energy),by=c("Variable" = "air_pollutant_energy"))%>%
              left_join(df_snap%>%
                          filter(Variable %in% df_variable$air_pollutant[!is.na(df_variable$air_pollutant)])%>%
                          select(Region,Unit,Variable,Year,Model,Scenario_SSP,tot=Value),
                        by=c("air_pollutant"="Variable",
                             "Model"="Model",
                             "Scenario_SSP"="Scenario_SSP",
                             "Region"="Region",
                             "Unit"="Unit",
                             "Year"="Year"
                        ))%>%
              mutate(Value=Value*100/tot,
                     Variable=paste0(Variable,"/total"),
                     Unit="%")%>%
              select(-c(air_pollutant,tot)))

#For cumulative and growth rate
df_snap <- df_snap%>%
  bind_rows(df_snap%>%
  filter(Model=="AIM"|Model=="GCAM"|Model=="WITCH")%>%
  mutate(Year=paste("X",Year,sep=""),
         Variable=paste(Variable,"cumulative",sep="|"))%>%
  pivot_wider(names_from = Year, values_from = Value)%>%
  mutate(X2100=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060+X2065+X2070+X2075+X2080+X2085+X2090+X2095+X2100/2)*5)%>%
  mutate(X2090=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060+X2065+X2070+X2075+X2080+X2085+X2090/2)*5)%>%
  mutate(X2080=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060+X2065+X2070+X2075+X2080/2)*5)%>% 
  mutate(X2070=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060+X2065+X2070/2)*5)%>%
  mutate(X2060=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5)%>%
  mutate(X2050=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5)%>%
  mutate(X2040=(X2020/2+X2025+X2030+X2035+X2040/2)*5)%>%
  mutate(X2030=(X2020/2+X2025+X2030/2)*5)%>%
  mutate(X2020=(X2020))%>%
  pivot_longer(cols=-c(Model,Model_Initial,Region,Variable,Unit,Scenario,SSP,Scenario_SSP),names_to='Year',values_to='Value')%>%
  filter(Year=="X2020"|Year=="X2030"|Year=="X2040"|Year=="X2050"|Year=="X2060"|Year=="X2070"|Year=="X2080"|Year=="X2090"|Year=="X2100")%>%
  mutate(Year= case_when(
    Year == "X2020" ~ "2020",
    Year == "X2030" ~ "2030",
    Year == "X2040" ~ "2040",
    Year == "X2050" ~ "2050",
    Year == "X2060" ~ "2060",
    Year == "X2070" ~ "2070",
    Year == "X2080" ~ "2080",
    Year == "X2090" ~ "2090",
    Year == "X2100" ~ "2100"))%>%
  mutate(Unit = str_replace_all(Unit, pattern="/yr", replacement="")))%>%
  bind_rows(df_snap%>%
  filter(Model=="MESSAGEix-GLOBIOM"|Model=="REMIND-MAgPIE")%>%
  mutate(Year=paste("X",Year,sep=""),
         Variable=paste(Variable,"cumulative",sep="|"))%>%
  pivot_wider(names_from = Year, values_from = Value)%>%
  mutate(X2100=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5+(X2060/2+X2070+X2080+X2090+X2100/2)*10)%>%
  mutate(X2090=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5+(X2060/2+X2070+X2080+X2090/2)*10)%>%
  mutate(X2080=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5+(X2060/2+X2070+X2080/2)*10)%>% 
  mutate(X2070=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5+(X2060/2+X2070/2)*10)%>%
  mutate(X2060=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050+X2055+X2060/2)*5)%>%
  mutate(X2050=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5)%>%
  mutate(X2040=(X2020/2+X2025+X2030+X2035+X2040/2)*5)%>%
  mutate(X2030=(X2020/2+X2025+X2030/2)*5)%>%
  mutate(X2020=(X2020))%>%
  pivot_longer(cols=-c(Model,Model_Initial,Region,Variable,Unit,Scenario,SSP,Scenario_SSP),names_to='Year',values_to='Value')%>%
  filter(Year=="X2020"|Year=="X2030"|Year=="X2040"|Year=="X2050"|Year=="X2060"|Year=="X2070"|Year=="X2080"|Year=="X2090"|Year=="X2100")%>%
  mutate(Year= case_when(
    Year == "X2020" ~ "2020",
    Year == "X2030" ~ "2030",
    Year == "X2040" ~ "2040",
    Year == "X2050" ~ "2050",
    Year == "X2060" ~ "2060",
    Year == "X2070" ~ "2070",
    Year == "X2080" ~ "2080",
    Year == "X2090" ~ "2090",
    Year == "X2100" ~ "2100"
  ))%>%
    mutate(Unit = str_replace_all(Unit, pattern="/yr", replacement="")))%>%
  bind_rows(df_snap%>%
              filter(Model=="IMAGE"|Model=="COFFEE")%>%
              mutate(Year=paste("X",Year,sep=""),
                     Variable=paste(Variable,"cumulative",sep="|"))%>%
              pivot_wider(names_from = Year, values_from = Value)%>%
              mutate(X2100=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5+(X2050/2+X2060+X2070+X2080+X2090+X2100/2)*10)%>%
              mutate(X2090=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5+(X2050/2+X2060+X2070+X2080+X2090/2)*10)%>%
              mutate(X2080=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5+(X2050/2+X2060+X2070+X2080/2)*10)%>% 
              mutate(X2070=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5+(X2050/2+X2060+X2070/2)*10)%>%
              mutate(X2060=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5+(X2050/2+X2060/2)*10)%>%
              mutate(X2050=(X2020/2+X2025+X2030+X2035+X2040+X2045+X2050/2)*5)%>%
              mutate(X2040=(X2020/2+X2025+X2030+X2035+X2040/2)*5)%>%
              mutate(X2030=(X2020/2+X2025+X2030/2)*5)%>%
              mutate(X2020=(X2020))%>%
              pivot_longer(cols=-c(Model,Model_Initial,Region,Variable,Unit,Scenario,SSP,Scenario_SSP),names_to='Year',values_to='Value')%>%
              filter(Year=="X2020"|Year=="X2030"|Year=="X2040"|Year=="X2050"|Year=="X2060"|Year=="X2070"|Year=="X2080"|Year=="X2090"|Year=="X2100")%>%
              mutate(Year= case_when(
                Year == "X2020" ~ "2020",
                Year == "X2030" ~ "2030",
                Year == "X2040" ~ "2040",
                Year == "X2050" ~ "2050",
                Year == "X2060" ~ "2060",
                Year == "X2070" ~ "2070",
                Year == "X2080" ~ "2080",
                Year == "X2090" ~ "2090",
                Year == "X2100" ~ "2100"
              ))%>%
              mutate(Unit = str_replace_all(Unit, pattern="/yr", replacement="")))%>%
  bind_rows(df_snap%>%
  mutate(Year=paste("X",Year,sep=""),
         Variable=paste(Variable,"[2020=1]",sep=""))%>%
  pivot_wider(names_from = Year, values_from = Value)%>%
  mutate(X2100=(X2100/X2020))%>%
  mutate(X2090=(X2090/X2020))%>%
  mutate(X2080=(X2080/X2020))%>% 
  mutate(X2070=(X2070/X2020))%>%
  mutate(X2060=(X2060/X2020))%>%
  mutate(X2050=(X2050/X2020))%>%
  mutate(X2040=(X2040/X2020))%>%
  mutate(X2030=(X2030/X2020))%>%
  mutate(X2020=(X2020/X2020))%>%
  filter(!is.na(X2020)|X2020==0)%>%
  pivot_longer(cols=-c(Model,Model_Initial,Region,Variable,Unit,Scenario,SSP,Scenario_SSP),names_to='Year',values_to='Value')%>%
  filter(Year=="X2020"|Year=="X2030"|Year=="X2040"|Year=="X2050"|Year=="X2060"|Year=="X2070"|Year=="X2080"|Year=="X2090"|Year=="X2100")%>%
  mutate(Year= case_when(
    Year == "X2020" ~ "2020",
    Year == "X2030" ~ "2030",
    Year == "X2040" ~ "2040",
    Year == "X2050" ~ "2050",
    Year == "X2060" ~ "2060",
    Year == "X2070" ~ "2070",
    Year == "X2080" ~ "2080",
    Year == "X2090" ~ "2090",
    Year == "X2100" ~ "2100"
  )))%>%
  bind_rows(df_snap%>%
     mutate(Year=paste("X",Year,sep=""),
            Variable=paste(Variable,"[change from 2020]",sep=""))%>%
     pivot_wider(names_from = Year, values_from = Value)%>%
     mutate(X2100=(X2100-X2020))%>%
     mutate(X2090=(X2090-X2020))%>%
     mutate(X2080=(X2080-X2020))%>% 
     mutate(X2070=(X2070-X2020))%>%
     mutate(X2060=(X2060-X2020))%>%
     mutate(X2050=(X2050-X2020))%>%
     mutate(X2040=(X2040-X2020))%>%
     mutate(X2030=(X2030-X2020))%>%
     mutate(X2020=(X2020-X2020))%>%
     filter(!is.na(X2020)|X2020==0)%>%
     pivot_longer(cols=-c(Model,Model_Initial,Region,Variable,Unit,Scenario,SSP,Scenario_SSP),names_to='Year',values_to='Value')%>%
     filter(Year=="X2020"|Year=="X2030"|Year=="X2040"|Year=="X2050"|Year=="X2060"|Year=="X2070"|Year=="X2080"|Year=="X2090"|Year=="X2100")%>%
     mutate(Year= case_when(
       Year == "X2020" ~ "2020",
       Year == "X2030" ~ "2030",
       Year == "X2040" ~ "2040",
       Year == "X2050" ~ "2050",
       Year == "X2060" ~ "2060",
       Year == "X2070" ~ "2070",
       Year == "X2080" ~ "2080",
       Year == "X2090" ~ "2090",
       Year == "X2100" ~ "2100"
     )))%>%
  na.omit()

#climate assessment
for (i in df_define$model1[!is.na(df_define$model1)]) {
  df_snap <- df_snap%>%
    bind_rows(read.csv(paste0("../data/climate-assessment/",i,"/assessed-warming-timeseries-quantiles_",i,".csv"), header=T)%>%
                mutate(Variable=paste(variable," ",round(quantile*100,1),"th",sep=""))%>%
                select(Model=model,Scenario=scenario,Region=region,Variable,Unit=unit,X2020,X2025,X2030,X2035,X2040,X2045,X2050,X2055,X2060,X2065,X2070,X2075,X2080,X2085,X2090,X2095,X2100)%>%
                pivot_longer(cols=-c(Model,Scenario,Region,Variable,Unit), names_to='Year', values_to='Value', names_prefix='X')%>%
                inner_join(select(df_define, model, scenario, scenario_category, SSP),
                           by=c("Model"="model","Scenario"="scenario"))%>%
                mutate(Scenario_SSP=paste(SSP, scenario_category, sep="_"))%>%
                inner_join(select(df_define,model1,model2,model3),by=c("Model"="model1"))%>%
                mutate(Model=model2,
                       Model_Initial=model3,
                       Scenario=scenario_category)%>%
                select(-c("model2","model3","scenario_category"))%>%
                filter(Model=="AIM"|Scenario_SSP=="SSP2_LN"))
}


#Other Data import--------------------------------------------------------

df_AR6 <- read.csv("../data/AR6_Scenario_Database.csv", header=T)%>%
  filter(str_detect(Variable,paste(df_define$filter_variable,collapse="|")))%>%
  pivot_longer(cols=!c(Model,Scenario,Region,Variable,Unit),names_to = "Year",values_to="Value", names_prefix='X')%>%
  filter(!(Value %in% NA))%>%
  left_join(read.xlsx("../data/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx",sheet = "meta_Ch3vetted_withclimate")%>%
              select("Model","Scenario","Category"))%>%
  filter(!(Category %in% NA))%>%
  filter(Year %in% df_define$year1)%>%
  mutate(Value=case_when(Unit == "Mt CO2/yr" ~ Value/1000,
                         Unit == "Mt CO2-equiv/yr" ~ Value/1000,
                         TRUE ~ Value))%>%
  mutate(Unit=case_when(Unit == "Mt CO2/yr" ~ "Gt CO2/yr",
                        Unit == "Mt CO2-equiv/yr" ~ "Gt CO2-equiv/yr",
                        TRUE ~ Unit))%>%
  mutate(Unit=case_when(Unit == "US$2010/t CO2" ~ "USD_2010/t CO2",
                        TRUE ~ Unit))

df_AR6<-df_AR6%>%
  bind_rows(df_AR6%>%
  filter(Variable=="Carbon Sequestration|Land Use"|
           Variable=="Carbon Sequestration|CCS|Biomass"|
           Variable=="Carbon Sequestration|Direct Air Capture"|
           Variable=="Carbon Sequestration|Enhanced Weathering")%>%
  group_by(Model,Region,Unit,Year,Scenario,Category)%>%
  summarize(Value=sum(abs(Value)))%>%
  ungroup()%>%
  mutate(Variable="Carbon Removal"))%>%
  bind_rows(df_AR6%>%
            filter(Variable=="Carbon Sequestration|Land Use")%>%
            group_by(Model,Region,Unit,Year,Scenario,Category)%>%
            summarize(Value=sum(abs(Value)))%>%
            ungroup()%>%
            mutate(Variable="Carbon Removal|Conventional"))%>%
  bind_rows(df_AR6%>%
              filter(Variable=="Carbon Sequestration|CCS|Biomass"|
                       Variable=="Carbon Sequestration|Direct Air Capture"|
                       Variable=="Carbon Sequestration|Enhanced Weathering")%>%
              group_by(Model,Region,Unit,Year,Scenario,Category)%>%
              summarize(Value=sum(abs(Value)))%>%
              ungroup()%>%
              mutate(Variable="Carbon Removal|Novel"))%>%
  bind_rows(df_AR6%>%
            filter(Variable=="Carbon Sequestration|CCS"|
                     Variable=="Carbon Sequestration|Direct Air Capture")%>%
            group_by(Model,Region,Unit,Year,Scenario,Category)%>%
            summarize(Value=sum(abs(Value)))%>%
            ungroup()%>%
            mutate(Variable="Carbon Capture|Geological Storage"))%>%
  replace(is.na(.), 0)

df_AR6 <- df_AR6%>%
  bind_rows(df_AR6%>%
              filter(Variable %in% df_variable$air_pollutant_energy[!is.na(df_variable$air_pollutant_energy)])%>%
              left_join(df_variable%>%select(air_pollutant,air_pollutant_energy),by=c("Variable" = "air_pollutant_energy"))%>%
              left_join(df_AR6%>%
                          filter(Variable %in% df_variable$air_pollutant[!is.na(df_variable$air_pollutant)])%>%
                          select(Region,Unit,Variable,Year,Model,Scenario,Category,tot=Value),
                        by=c("air_pollutant"="Variable",
                             "Model"="Model",
                             "Scenario"="Scenario",
                             "Category"="Category",
                             "Region"="Region",
                             "Unit"="Unit",
                             "Year"="Year"
                        ))%>%
              mutate(Value=Value*100/tot,
                     Variable=paste0(Variable,"/total"),
                     Unit="%")%>%
              select(-c(air_pollutant,tot)))

df_AR6 <- df_AR6%>% 
  bind_rows(df_AR6%>%
              mutate(Year=paste("X",Year,sep=""),
                     Variable=paste(Variable,"[change from 2020]",sep=""))%>%
              pivot_wider(names_from = Year, values_from = Value)%>%
              mutate(X2100=(X2100-X2020))%>%
              mutate(X2090=(X2090-X2020))%>%
              mutate(X2080=(X2080-X2020))%>% 
              mutate(X2070=(X2070-X2020))%>%
              mutate(X2060=(X2060-X2020))%>%
              mutate(X2050=(X2050-X2020))%>%
              mutate(X2040=(X2040-X2020))%>%
              mutate(X2030=(X2030-X2020))%>%
              mutate(X2020=(X2020-X2020))%>%
              filter(!is.na(X2020)|X2020==0)%>%
              pivot_longer(cols=-c(Model,Region,Variable,Unit,Scenario,Category),names_to='Year',values_to='Value')%>%
              filter(Year=="X2020"|Year=="X2030"|Year=="X2040"|Year=="X2050"|Year=="X2060"|Year=="X2070"|Year=="X2080"|Year=="X2090"|Year=="X2100")%>%
              mutate(Year= case_when(
                Year == "X2020" ~ "2020",
                Year == "X2030" ~ "2030",
                Year == "X2040" ~ "2040",
                Year == "X2050" ~ "2050",
                Year == "X2060" ~ "2060",
                Year == "X2070" ~ "2070",
                Year == "X2080" ~ "2080",
                Year == "X2090" ~ "2090",
                Year == "X2100" ~ "2100"
              )))

df_AR6 <- df_AR6%>%bind_rows(df_AR6%>%
              mutate(Year=paste("X",Year,sep=""),
                     Variable=paste(Variable,"cumulative",sep="|"))%>%
              pivot_wider(names_from = Year, values_from = Value)%>%
              mutate(X2100=(X2020/2+X2030+X2040+X2050+X2060+X2070+X2080+X2090+X2100/2)*10)%>%
              mutate(X2090=(X2020/2+X2030+X2040+X2050+X2060+X2070+X2080+X2090/2)*10)%>%
              mutate(X2080=(X2020/2+X2030+X2040+X2050+X2060+X2070+X2080/2)*10)%>% 
              mutate(X2070=(X2020/2+X2030+X2040+X2050+X2060+X2070/2)*10)%>%
              mutate(X2060=(X2020/2+X2030+X2040+X2050+X2060/2)*10)%>%
              mutate(X2050=(X2020/2+X2030+X2040+X2050/2)*10)%>%
              mutate(X2040=(X2020/2+X2030+X2040/2)*10)%>%
              mutate(X2030=(X2020/2+X2030/2)*10)%>%
              mutate(X2020=(X2020))%>%
              pivot_longer(cols=-c(Model,Region,Variable,Unit,Scenario,Category),names_to='Year',values_to='Value')%>%
              filter(Year=="X2020"|Year=="X2030"|Year=="X2040"|Year=="X2050"|Year=="X2060"|Year=="X2070"|Year=="X2080"|Year=="X2090"|Year=="X2100")%>%
              mutate(Year= case_when(
                Year == "X2020" ~ "2020",
                Year == "X2030" ~ "2030",
                Year == "X2040" ~ "2040",
                Year == "X2050" ~ "2050",
                Year == "X2060" ~ "2060",
                Year == "X2070" ~ "2070",
                Year == "X2080" ~ "2080",
                Year == "X2090" ~ "2090",
                Year == "X2100" ~ "2100"))%>%
             mutate(Unit = str_replace_all(Unit, pattern="/yr", replacement="")))

df_AR6_EmiRem<-df_AR6%>%
  filter(Variable=="Emissions|CO2|cumulative"|Variable=="Carbon Removal|Novel|cumulative"|Variable=="Carbon Removal|Conventional|cumulative")%>%
  pivot_wider(names_from = Variable, values_from = Value)%>%
  filter(Year=="2100",Category=="C1"|Category=="C2"|Category=="C3"|Category=="C4")

df_AR6<-df_AR6%>%
            bind_rows(df_AR6%>%
            filter(Variable=="AR6 climate diagnostics|Surface Temperature (GSAT)|CICERO-SCM|50.0th Percentile"|
                     Variable=="AR6 climate diagnostics|Surface Temperature (GSAT)|FaIRv1.6.2|50.0th Percentile"|
                     Variable=="AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile")%>%
            mutate(Variable="Surface Temperature (GSAT) 50th"))

df_AR6 <- df_AR6%>% 
  group_by(Category,Year,Unit,Region,Variable)%>%
  summarise(max=max(Value),
            min=min(Value),
            up5=quantile(Value,
                         probs=0.95,
                         na.rm =T),
            up25=quantile(Value,
                          probs=0.75,
                          na.rm =T),
            med=median(Value,
                       na.rm =T),
            lo25=quantile(Value,
                          probs=0.25,
                          na.rm =T),
            lo5=quantile(Value,
                         probs=0.05,
                         na.rm =T))%>%
  ungroup()

df_geo<-read.xlsx("../data/gidden_et_al_geologic_carbon_storage.xlsx","country")%>%
  select(c("NODE","Region5","Technical_Potential","Planetary_Limit"))%>%
  group_by(Region5)%>%
  summarize(Technical_Potential=1000*sum(Technical_Potential),
            Planetary_Limit=1000*sum(Planetary_Limit))%>%
  ungroup()%>%
  drop_na()%>%
  mutate(Region=case_when(Region5 ==  "R5ASIA" ~ "Asia (R5)",
                          Region5 ==  "R5LAM" ~ "Latin America (R5)",
                          Region5 ==  "R5MAF" ~ "Middle East & Africa (R5)",
                          Region5 ==  "R5OECD90+EU" ~ "OECD & EU (R5)",
                          Region5 ==  "R5REF" ~ "Reforming Economies (R5)"))%>%
  select(c("Region","Technical_Potential","Planetary_Limit"))

df_geo<-bind_rows(df_geo,df_geo%>%
                    summarize(Technical_Potential=sum(Technical_Potential),
                              Planetary_Limit=sum(Planetary_Limit))%>%
                    mutate(Region="World"))

df_CCS <- read.csv("../data/CCS_ref.csv", header=T)%>%
  mutate(Year=as.character(Year))

df_land <- read.csv("../data/Inputs_LandUse_E_All_Area_Groups_NOFLAG.csv", header=T)%>%
  select(c("Area","Item","Unit","Y1990","Y2000","Y2010","Y2020"))%>%
  filter(Area=="World",Unit=="1000 ha")%>%
  pivot_longer(cols=-c(Area,Item,Unit), names_to='Year', values_to='Value', names_prefix='Y')%>%
  replace_na(replace = list(Value = 0))
  
df_land<-df_land%>%
  filter(Item=="Permanent meadows and pastures")%>%
  group_by(Area,Unit,Year)%>%
  summarize(Value=sum(Value)/1000)%>%
  ungroup()%>%
  mutate(Unit="million ha",Variable="Land Cover|Pasture[change from 2020]")%>%
  bind_rows(df_land%>%
  filter(Item=="Other land")%>%
  group_by(Area,Unit,Year)%>%
  summarize(Value=sum(Value)/1000)%>%
  ungroup()%>%
  mutate(Unit="million ha",Variable="Land Cover|Other Natural[change from 2020]"))%>%
  bind_rows(df_land%>%
  filter(Item=="Cropland")%>%
  group_by(Area,Unit,Year)%>%
  summarize(Value=sum(Value)/1000)%>%
  ungroup()%>%
  mutate(Unit="million ha",Variable="Land Cover|Cropland|Non-Energy Crops[change from 2020]"))%>%
  bind_rows(df_land%>%
  filter(Item=="Planted Forest")%>%
  group_by(Area,Unit,Year)%>%
  summarize(Value=sum(Value)/1000)%>%
  ungroup()%>%
  mutate(Unit="million ha",Variable="Land Cover|Forest|Planted[change from 2020]"))%>%
  bind_rows(df_land%>%
              filter(Item=="Forest land"|Item=="Planted Forest")%>%
              pivot_wider(names_from = Item, values_from = Value)%>%
              mutate(Unit="million ha",
                     Value=(`Forest land`-`Planted Forest`)/1000,
                     Variable="Land Cover|Forest|Natural[change from 2020]")%>%
              select(-c("Forest land","Planted Forest")))%>%
  filter(Value!=0)

df_land<-df_land%>%
  left_join(df_land%>%
              filter(Year=="2020")%>%
              pivot_wider(names_from = Year, values_from = Value),by=c("Area","Unit","Variable"))%>%
  mutate(Value=Value-`2020`,
         Model="FAOstat",
         Variable= factor(Variable,levels=c("Land Cover|Other Natural[change from 2020]",
                                            "Land Cover|Forest|Natural[change from 2020]",
                                            "Land Cover|Forest|Planted|Plantation[change from 2020]",
                                            "Land Cover|Pasture[change from 2020]",
                                            "Land Cover|Cropland|Non-Energy Crops[change from 2020]",
                                            "Land Cover|Cropland|Energy Crops[change from 2020]")),
         Year = factor(Year,levels=c("1970","1980","1990","2000","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100")))

df_dummy<-data.frame(Year=factor(df_define$year2[!is.na(df_define$year2)],df_define$year2[!is.na(df_define$year2)]))%>%
  mutate(Value=0)

#define function----------------------------------------------------------------

#line plot
f_fig_line1 <- function(v_name, v_var) {
  df_fig1<-filter(df_snap,Variable %in% v_var, 
                  Model=="AIM", Region=="World",
                  Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)])%>%
    mutate(Variable = factor(Variable,levels=v_var),
           Scenario_SSP = factor(Scenario_SSP,levels=df_define$marker_scenario[!is.na(df_define$marker_scenario)] ),
           Scenario = factor(Scenario,levels=df_define$scenario1[!is.na(df_define$scenario1)]))
  df_fig2<-filter(df_snap,Variable %in% v_var, Year %in% df_define$year1,
                  Scenario=="LN", Region=="World")%>%
    mutate(Variable = factor(Variable,levels=v_var))
  df_fig3<-filter(df_snap,Variable %in% v_var, Year %in% df_define$year1,
                  Scenario=="LN", Region=="World")%>%
    summarise(max=max(Value),min=min(Value),.by = c(Scenario,Year,Region,Variable,Unit))%>%
    mutate(Variable = factor(Variable,levels=v_var))
  df_fig4<-filter(df_AR6,Variable %in% v_var, Region=="World", Year=="2100")%>%
    mutate(Variable = factor(Variable,levels=v_var))
  p<-ggplot() +
    geom_point(data = df_dummy, aes(x=Year,y=Value), alpha=0)+
    geom_line(data=df_fig1,aes(x=Year,y=Value,group=Scenario_SSP,color=Scenario),linewidth=0.8,alpha=0.9)+
    geom_line(data=df_fig2,aes(x=Year,y=Value,group=interaction(Scenario_SSP,Model),color=Scenario),linewidth=0.2,alpha=0.6,linetype="solid")+
    geom_ribbon(data=df_fig3,aes(x=Year,ymin=min,ymax=max, group=Scenario,),alpha=0.1,fill=df_define$color_scenario["LN"]) +
    geom_linerange(data=df_fig4,aes(x=Category,ymin=lo5,ymax=up5, group=Category),color="grey",alpha=0.4,linewidth=5,show.legend = FALSE) +  
    geom_linerange(data=df_fig4,aes(x=Category,ymin=lo25,ymax=up25, group=Category),color="grey",alpha=0.5,linewidth=5,show.legend = FALSE) +  
    facet_wrap(Variable~Unit,scales = "free",nrow=2) +
    scale_x_discrete(breaks=c(df_define$year1[!is.na(df_define$year1)],df_define$ar6_database[!is.na(df_define$ar6_database)]))+
    scale_colour_manual(values=c(df_define$color_scenario)) +
    ylab("") + xlab("Year") + theme1 
  png(paste(v_path["fig_main"],"/",v_name,"_line.png",sep=""), width = length(v_var)/2*1800+450, height = 3200,res = 300)
  print(p)
  dev.off()
}
f_fig_line2 <- function(v_name, v_var) {
  df_fig1<-filter(df_snap,Variable %in% v_var, 
                  Model=="AIM", Region=="World",
                  Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)])%>%
    mutate(Variable = factor(Variable,levels=v_var),
           Scenario_SSP = factor(Scenario_SSP,levels=df_define$marker_scenario[!is.na(df_define$marker_scenario)] ),
           Scenario = factor(Scenario,levels=df_define$scenario1[!is.na(df_define$scenario1)]))
  df_fig2<-filter(df_snap,Variable %in% v_var, Year %in% df_define$year1,
                  Scenario=="LN", Region=="World")%>%
    mutate(Variable = factor(Variable,levels=v_var))
  df_fig3<-filter(df_snap,Variable %in% v_var, Year %in% df_define$year1,
                  Scenario=="LN", Region=="World")%>%
    summarise(max=max(Value),min=min(Value),.by = c(Scenario,Year,Region,Variable,Unit))%>%
    mutate(Variable = factor(Variable,levels=v_var))
  df_fig4<-filter(df_AR6,Variable %in% v_var, Region=="World", Year=="2100")%>%
    mutate(Variable = factor(Variable,levels=v_var))
  p<-ggplot() +
    geom_point(data = df_dummy, aes(x=Year,y=Value), alpha=0)+
    geom_line(data=df_fig1,aes(x=Year,y=Value,group=Scenario_SSP,color=Scenario),linewidth=0.8,alpha=0.9)+
    geom_line(data=df_fig2,aes(x=Year,y=Value,group=interaction(Scenario_SSP,Model),color=Scenario),linewidth=0.2,alpha=0.6,linetype="solid")+
    geom_ribbon(data=df_fig3,aes(x=Year,ymin=min,ymax=max, group=Scenario,),alpha=0.1,fill=df_define$color_scenario["LN"]) +
    geom_linerange(data=df_fig4,aes(x=Category,ymin=lo5,ymax=up5, group=Category),color="grey",alpha=0.4,linewidth=5,show.legend = FALSE) +  
    geom_linerange(data=df_fig4,aes(x=Category,ymin=lo25,ymax=up25, group=Category),color="grey",alpha=0.5,linewidth=5,show.legend = FALSE) +  
    facet_wrap(Variable~Unit,scales = "free",nrow=1) +
    scale_x_discrete(breaks=c(df_define$year1[!is.na(df_define$year1)],df_define$ar6_database[!is.na(df_define$ar6_database)]))+
    scale_colour_manual(values=c(df_define$color_scenario)) +
    ylab("") + xlab("Year") + theme1 
  png(paste(v_path["fig_main"],"/",v_name,"_line.png",sep=""), width = length(v_var)*1800+450, height = 1800,res = 300)
  print(p)
  dev.off()
}
f_fig_line3 <- function(v_name, v_var) {
  df_fig1<-filter(df_snap,Variable %in% v_var, 
                  Model=="AIM", Region %in% df_define$region5,
                  Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)])%>%
    mutate(Variable = factor(Variable,levels=v_var),
           Scenario_SSP = factor(Scenario_SSP,levels=df_define$marker_scenario[!is.na(df_define$marker_scenario)]),
           Scenario = factor(Scenario,levels=df_define$scenario1[!is.na(df_define$scenario1)]))
  df_fig2<-filter(df_snap,Variable %in% v_var, Year %in% df_define$year1,
                  Scenario=="LN", Region %in% df_define$region5)%>%
    mutate(Variable = factor(Variable,levels=v_var))
  df_fig3<-filter(df_snap,Variable %in% v_var, Year %in% df_define$year1,
                  Scenario=="LN", Region %in% df_define$region5)%>%
    summarise(max=max(Value),min=min(Value),.by = c(Scenario,Year,Region,Variable,Unit))%>%
    mutate(Variable = factor(Variable,levels=v_var))
  df_fig4<-filter(df_AR6,Variable %in% v_var, Region %in% df_define$region5, Year=="2100")%>%
    mutate(Variable = factor(Variable,levels=v_var))
  p<-ggplot() +
    geom_point(data = df_dummy, aes(x=Year,y=Value), alpha=0)+
    geom_line(data=df_fig1,aes(x=Year,y=Value,group=Scenario_SSP,color=Scenario),linewidth=0.8,alpha=0.9)+
    geom_line(data=df_fig2,aes(x=Year,y=Value,group=interaction(Scenario_SSP,Model),color=Scenario),linewidth=0.2,alpha=0.6,linetype="solid")+
    geom_ribbon(data=df_fig3,aes(x=Year,ymin=min,ymax=max, group=Scenario,),alpha=0.1,fill=df_define$color_scenario["LN"]) +
    geom_linerange(data=df_fig4,aes(x=Category,ymin=lo5,ymax=up5, group=Category),color="grey",alpha=0.4,linewidth=5,show.legend = FALSE) +  
    geom_linerange(data=df_fig4,aes(x=Category,ymin=lo25,ymax=up25, group=Category),color="grey",alpha=0.5,linewidth=5,show.legend = FALSE) +  
    facet_wrap(Variable~Region,scales = "free",nrow=3) +
    scale_x_discrete(breaks=c(df_define$year1[!is.na(df_define$year1)],df_define$ar6_database[!is.na(df_define$ar6_database)]))+
    scale_colour_manual(values=c(df_define$color_scenario)) +
    ylab("") + xlab("Year") + theme1 + theme(legend.direction = "horizontal", legend.position ="bottom",  legend.title = element_text(size = 25), legend.text = element_text(size = 25),)
  png(paste(v_path["fig_main"],"/",v_name,"_line_region5.png",sep=""), width = 6000, height = 6000,res = 300)
  print(p)
  dev.off()
}
f_fig_line4 <- function(v_name, v_var) {
  df_fig1<-filter(df_snap,Variable %in% v_var, 
                  Model=="AIM", Region=="World",
                  Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)])%>%
    mutate(Variable = factor(Variable,levels=v_var),
           Scenario_SSP = factor(Scenario_SSP,levels=df_define$marker_scenario[!is.na(df_define$marker_scenario)] ),
           Scenario = factor(Scenario,levels=df_define$scenario1[!is.na(df_define$scenario1)]))
  df_fig2<-filter(df_snap,Variable %in% v_var, Year %in% df_define$year1,
                  Scenario=="LN", Region=="World")%>%
    mutate(Variable = factor(Variable,levels=v_var))
  df_fig3<-filter(df_snap,Variable %in% v_var, Year %in% df_define$year1,
                  Scenario=="LN", Region=="World")%>%
    summarise(max=max(Value),min=min(Value),.by = c(Scenario,Year,Region,Variable,Unit))%>%
    mutate(Variable = factor(Variable,levels=v_var))
  df_fig4<-filter(df_AR6,Variable %in% v_var, Region=="World", Year=="2100")%>%
    mutate(Variable = factor(Variable,levels=v_var))
  p<-ggplot() +
    geom_point(data = df_dummy, aes(x=Year,y=Value), alpha=0)+
    geom_line(data=df_fig1,aes(x=Year,y=Value,group=Scenario_SSP,color=Scenario),linewidth=0.8,alpha=0.9)+
    geom_line(data=df_fig2,aes(x=Year,y=Value,group=interaction(Scenario_SSP,Model),color=Scenario),linewidth=0.2,alpha=0.6,linetype="solid")+
    geom_ribbon(data=df_fig3,aes(x=Year,ymin=min,ymax=max, group=Scenario,),alpha=0.1,fill=df_define$color_scenario["LN"]) +
#    geom_linerange(data=df_fig4,aes(x=Category,ymin=lo5,ymax=up5, group=Category,color=Category),alpha=0.4,linewidth=5) +  
#    geom_linerange(data=df_fig4,aes(x=Category,ymin=lo25,ymax=up25, group=Category,color=Category),alpha=0.5,linewidth=5) +  
    facet_wrap(Variable~Unit,scales = "free",nrow=1) +
    scale_x_discrete(breaks=c(df_define$year1[!is.na(df_define$year1)],df_define$ar6_database[!is.na(df_define$ar6_database)]))+
    scale_colour_manual(values=c(df_define$color_scenario)) +
    ylab("") + xlab("Year") + theme1 
  png(paste(v_path["fig_main"],"/",v_name,"_line.png",sep=""), width = length(v_var)*1800+450, height = 1800,res = 300)
  print(p)
  dev.off()
}
f_fig_line5 <- function(v_name, v_var) {
  df_fig1<-filter(df_snap,Variable %in% v_var, 
                  Model=="AIM", Region=="World",
                  Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)])%>%
    mutate(Variable = factor(Variable,levels=v_var),
           Scenario_SSP = factor(Scenario_SSP,levels=df_define$marker_scenario[!is.na(df_define$marker_scenario)] ))
  df_fig2<-filter(df_snap,Variable %in% v_var, Year %in% df_define$year1,
                  Scenario=="LN", Region=="World")%>%
    mutate(Variable = factor(Variable,levels=v_var))
  df_fig3<-filter(df_snap,Variable %in% v_var, Year %in% df_define$year1,
                  Scenario=="LN", Region=="World")%>%
    summarise(max=max(Value),min=min(Value),.by = c(Scenario,Year,Region,Variable,Unit))%>%
    mutate(Variable = factor(Variable,levels=v_var))
  df_fig4<-filter(df_AR6,Variable %in% v_var, Region=="World", Year=="2100")%>%
    mutate(Variable = factor(Variable,levels=v_var))
  p<-ggplot() +
#    geom_point(data = df_dummy, aes(x=Year,y=Value), alpha=0)+
    geom_line(data=df_fig1,aes(x=Year,y=Value,group=Scenario_SSP,color=Scenario),linewidth=0.8,alpha=0.9)+
    geom_line(data=df_fig2,aes(x=Year,y=Value,group=interaction(Scenario_SSP,Model),color=Scenario),linewidth=0.2,alpha=0.6,linetype="solid")+
    geom_ribbon(data=df_fig3,aes(x=Year,ymin=min,ymax=max, group=Scenario,),alpha=0.1,fill=df_define$color_scenario["LN"]) +
#    geom_linerange(data=df_fig4,aes(x=Category,ymin=lo5,ymax=up5, group=Category,color=Category),alpha=0.4,linewidth=5) +  
#    geom_linerange(data=df_fig4,aes(x=Category,ymin=lo25,ymax=up25, group=Category,color=Category),alpha=0.5,linewidth=5) +  
    facet_wrap(Variable~Unit,scales = "free",nrow=3) +
    scale_x_discrete(breaks=c(df_define$year1[!is.na(df_define$year1)],df_define$ar6_database[!is.na(df_define$ar6_database)]))+
    scale_colour_manual(values=c(df_define$color_scenario)) +
    ylab("") + xlab("Year") + theme1 
  png(paste(v_path["fig_main"],"/",v_name,"_line.png",sep=""), width = length(v_var)/3*2400+800, height = 5200,res = 300)
  print(p)
  dev.off()
}
f_fig_line6 <- function(v_name, v_var, v_nrow=1) {
  df_fig1<-filter(df_snap,Variable %in% v_var,
                  Scenario=="LN", Region=="World")%>%
    mutate(Variable = factor(Variable,levels=v_var),
           Model = factor(Model,levels=df_define$model2[!is.na(df_define$model2)]))
  p<-ggplot() +
    geom_point(data = df_dummy, aes(x=Year,y=Value), alpha=0)+
    geom_line(data=df_fig1,aes(x=Year,y=Value,group=Model,color=Model),linewidth=0.8,alpha=0.9)+
    facet_wrap(Variable~Unit,scales = "free",nrow=v_nrow) +
    scale_x_discrete(breaks=c(df_define$year1[!is.na(df_define$year1)]))+
    scale_colour_manual(values=c(df_define$color_model)) +
    ylab("") + xlab("Year") + labs(color="Model (LN scenario)") + theme1 +
    theme(legend.title = element_text(size = 16), legend.text = element_text(size = 14))
  v_ncol <- ceiling(length(v_var)/v_nrow)
  v_height <- if (v_nrow==2) 3200 else 1800
  png(paste(v_path["fig_main"],"/",v_name,"_model_line.png",sep=""), width = v_ncol*1800+900, height = v_height,res = 300)
  print(p)
  dev.off()
}
#area plot
f_fig_area <- function(v_name, v_area, v_line) {
df_fig1<-df_snap%>%
  filter(Variable %in% v_area,Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)], Region=="World",Model=="AIM")%>%
  mutate(Variable=factor(Variable, levels=v_area),
         Scenario_SSP = factor(Scenario_SSP,levels=df_define$marker_scenario[!is.na(df_define$marker_scenario)] ),
         Scenario = factor(Scenario,levels=df_define$scenario1[!is.na(df_define$scenario1)]))
df_fig2<-df_snap%>%
  filter(Variable %in% v_line, Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)], Region=="World",Model=="AIM")%>%
  mutate(Variable=factor(Variable, levels=v_line),
         Scenario_SSP = factor(Scenario_SSP,levels=df_define$marker_scenario[!is.na(df_define$marker_scenario)] ))
p<-ggplot() +
  geom_area(data=df_fig1,aes(x=Year,y=Value,group=Variable,fill=Variable),linewidth=1, alpha=0.7)+
  geom_line(data = df_fig2, aes(x=Year,y=Value,group=Variable,linetype=Variable), alpha=0.7)+
  geom_hline(yintercept=0,linetype="longdash",color = "black") +
  scale_fill_manual(values=df_define$color_palette) +
  facet_wrap(Scenario_SSP~.,scales = "fixed",nrow=1) +
  scale_x_discrete(breaks=c(df_define$year1[!is.na(df_define$year1)]))+
  ylab("") + xlab("") + labs(fill = "Category", linetype = "") + theme1 +theme(legend.position="bottom")+guides(fill = guide_legend(ncol = 2),linetype = guide_legend(ncol = 1))
  scale_x_discrete(breaks=df_define$year1[!is.na(df_define$year1)])
png(paste(v_path["fig_main"],"/",v_name,"area_.png",sep=""), width = length(df_define$marker_scenario[!is.na(df_define$marker_scenario)])*800, height = 2400+length(v_area)*20,res = 300)
print(p)
dev.off()
}
#bar plot
f_fig_bar <- function(v_name, v_area, v_point) {
  df_fig1<-df_snap%>%
    filter(Year=="2050"|Year=="2100",Variable %in% v_area,Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)], Region=="World",Model=="AIM")%>%
    mutate(Variable=factor(Variable, levels=v_area),
           Scenario_SSP_Model = factor(paste0(Model,"_",Scenario_SSP),levels= df_define$scenario_model[!is.na(df_define$scenario_model)]),
           Scenario = factor(Scenario,levels=df_define$scenario1[!is.na(df_define$scenario1)]))
  df_fig2<-df_snap%>%
    filter(Year=="2050"|Year=="2100",Variable %in% v_area, Region=="World",Scenario=="LN",Model!="AIM")%>%
    mutate(Variable=factor(Variable, levels=v_area),
           Scenario_SSP_Model = factor(paste0(Model,"_",Scenario_SSP),levels= df_define$scenario_model[!is.na(df_define$scenario_model)]))  
  df_fig3<-df_snap%>%
    filter(Year=="2050"|Year=="2100",Variable %in% v_point, Region=="World",Scenario=="LN",Model!="AIM")%>%
    mutate(Variable=factor(Variable, levels=v_point),
           Scenario_SSP_Model = factor(paste0(Model,"_",Scenario_SSP),levels= df_define$scenario_model[!is.na(df_define$scenario_model)]))%>%
    bind_rows(df_snap%>%
                filter(Year=="2050"|Year=="2100",Variable %in% v_point,Scenario_SSP %in% df_define$marker_scenario[!is.na(df_define$marker_scenario)], Region=="World",Model=="AIM")%>%
                mutate(Variable=factor(Variable, levels=v_point),
                       Scenario_SSP_Model = factor(paste0(Model,"_",Scenario_SSP),levels= df_define$scenario_model[!is.na(df_define$scenario_model)])))
  p<-ggplot() +
    geom_bar(data=df_fig1,aes(x=Scenario_SSP_Model,y=Value,group=Variable,fill=Variable),stat="identity", alpha=0.7)+
    geom_bar(data=df_fig2,aes(x=Scenario_SSP_Model,y=Value,group=Variable,fill=Variable),stat="identity", alpha=0.7)+
    geom_point(data = df_fig3, aes(x=Scenario_SSP_Model,y=Value,group=Variable,shape=Variable), alpha=0.8,size=4,stroke=1.2)+
    geom_hline(yintercept=0,linetype="longdash",color = "black") +
    scale_fill_manual(values=df_define$color_palette) +
    scale_shape_manual(values=c(4,2,3,5)) +
    facet_wrap(Year~.,scales = "fixed",nrow=1) +
    ylab("") + xlab("") + labs(fill = "Category", linetype = "") + theme1 +
  png(paste(v_path["fig_main"],"/",v_name,"_bar.png",sep=""), width = 5000, height = 3000,res = 300)
  print(p)
  dev.off()
}

#Plot------------------------------------

f_fig_line1("GHG_Emissions",df_variable$GHG[!is.na(df_variable$GHG)])
f_fig_line1("Air_Pollutant",df_variable$air_pollutant[!is.na(df_variable$air_pollutant)])
f_fig_line1("CDR_CCS",df_variable$CDR_CCS[!is.na(df_variable$CDR_CCS)])  
f_fig_line1("Air_Pollutant_Ratio",df_variable$air_pollutant_energy_ratio[!is.na(df_variable$air_pollutant_energy_ratio)])  
f_fig_line5("SDG",df_variable$sdg[!is.na(df_variable$sdg)])
f_fig_line2("Primary_Energy","Primary Energy")
f_fig_line2("Final_Energy","Final Energy")
f_fig_line2("Agricultural_Production","Agricultural Production")
f_fig_line3("Food_Availability",df_variable$food[!is.na(df_variable$food)])
f_fig_line4("Economic_indicator",df_variable$economic_impact[!is.na(df_variable$economic_impact)])
f_fig_line6("CDR_CCS",df_variable$CDR_CCS[!is.na(df_variable$CDR_CCS)], v_nrow=2)

f_fig_area("CO2",df_variable$CO2_sector[!is.na(df_variable$CO2_sector)],"Emissions|CO2")
f_fig_area("CDR",df_variable$CDR[!is.na(df_variable$CDR)],"Carbon Removal")
f_fig_area("Final_Energy_Sector",df_variable$final_energy_sector[!is.na(df_variable$final_energy_sector)],"Final Energy")
f_fig_area("Final_Energy_Source",df_variable$final_energy_source[!is.na(df_variable$final_energy_source)],"Final Energy")
f_fig_area("Primary_Energy",df_variable$primary_energy[!is.na(df_variable$primary_energy)],"Primary Energy")
f_fig_area("Land_Cover",df_variable$land_cover[!is.na(df_variable$land_cover)],NA)
f_fig_area("Agricultural_Production",df_variable$agricultural_production[!is.na(df_variable$agricultural_production)],"Agricultural Production")


f_fig_bar("CO2",df_variable$CO2_sector[!is.na(df_variable$CO2_sector)],"Emissions|CO2")
f_fig_bar("CDR",df_variable$CDR[!is.na(df_variable$CDR)],"Carbon Removal")
f_fig_bar("Final_Energy_Sector",df_variable$final_energy_sector[!is.na(df_variable$final_energy_sector)],"Final Energy")
f_fig_bar("Final_Energy_Source",df_variable$final_energy_source[!is.na(df_variable$final_energy_source)],"Final Energy")
f_fig_bar("Primary_Energy",df_variable$primary_energy[!is.na(df_variable$primary_energy)],"Primary Energy")
f_fig_bar("Land_Cover",df_variable$land_cover[!is.na(df_variable$land_cover)],NA)
f_fig_bar("Agricultural_Production",df_variable$agricultural_production[!is.na(df_variable$agricultural_production)],"Agricultural Production")




