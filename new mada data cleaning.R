## Data cleaning of new Madagascar data set, received April 2018
## Set working directory
setwd("D:/TB and HIV in Madagascar/New Madagascar data")
## Load packages
library(tidyverse)
library(readr)
library(readxl)

## Import data

Mada_TB <-Mada_TB <- read_csv("D:/TB and HIV in Madagascar/New Madagascar data/Mada_TB.csv", na = "NA") 

# Create new variables, total sn and ep tb cases
Mada_TB$new_sn<-Mada_TB$new_sn_u4+Mada_TB$new_sn_5to14+Mada_TB$new_sn_o15
Mada_TB$new_ep<-Mada_TB$new_ep_u4+Mada_TB$new_ep_5to14+Mada_TB$new_ep_o15
Mada_TB$total_tb<-Mada_TB$new_sp+Mada_TB$new_sn+Mada_TB$new_ep

# I think there are some issues with the district names and populations
# Need to figure out how to fix this

#Create new data frames for analysis and plotting
tb_dist<-Mada_TB%>%
  group_by(district, year)%>%
  summarise(new_sp=sum(new_sp, na.rm=TRUE), new_sn=sum(new_sn, na.rm=TRUE),
            new_ep=sum(new_ep, na.rm=TRUE), total_tb=sum(total_tb, na.rm=TRUE),
            district_pop=mean(district_pop, na.rm=TRUE))
tb_dist_sum<-tb_dist%>%
  group_by(district)%>%
  summarise(tb_per_100000=sum())
tb_total_year<-tb_dist%>%
  group_by(year)%>%
  summarise(new_sp=sum(new_sp, na.rm=TRUE), new_sn=sum(new_sn, na.rm=TRUE),
            new_ep=sum(new_ep, na.rm=TRUE), total_tb=sum(total_tb, na.rm=TRUE),
            pop=sum(district_pop, na.rm=TRUE))%>%
  mutate(sp_per_100000=new_sp/pop*100000, sn_per_100000=new_sn/pop*100000,
         ep_per_100000=new_ep/pop*100000, total_tb_per_100000=total_tb/pop*100000)

## Stacked filled line of types of TB over time
f<-gather(tb_total_year,tb_type,rate_per_100000,sp_per_100000:ep_per_100000, factor_key=TRUE)
ggplot(f, aes(year, rate_per_100000)) +
  geom_area(aes(fill=tb_type))+
  scale_fill_discrete(name="TB type", labels=c("Smear positive","Smear negative","Extrapulmonary"))+
labs(x="Year",y="Notifications per 100,000",title="Reported TB cases at Centers for Diagnosis and Treatment in Madagascar")
  ggsave("tb_plot.jpg", width=10, height=5)

# Analysis of types of TB by age group
# tb_age<-Mada_TB %>%
#   mutate(new_sp_o15=new_sp_15to24+new_sp_25to34+new_sp_35to44+new_sp_45to54+new_sp_55to64+new_sp_o64)%>%
#   group_by(year) %>%
#   summarise(percent_sn_u4=sum(new_sn_u4, na.rm=TRUE)/(sum(new_sn_u4,new_sn_5to14,new_sn_o15, na.rm=TRUE)),
#             percent_sn_5to14=sum(new_sn_5to14, na.rm=TRUE)/(sum(new_sn_u4,new_sn_5to14,new_sn_o15, na.rm=TRUE)),
#             percent_sn_o15=sum(new_sn_o15, na.rm=TRUE)/(sum(new_sn_u4,new_sn_5to14,new_sn_o15, na.rm=TRUE)),
#             percent_ep_u4=sum(new_ep_u4, na.rm=TRUE)/(sum(new_ep_u4,new_ep_5to14,new_ep_o15, na.rm=TRUE)),
#             percent_ep_5to14=sum(new_ep_5to14, na.rm=TRUE)/(sum(new_ep_u4,new_ep_5to14,new_ep_o15, na.rm=TRUE)),
#             percent_ep_o15=sum(new_ep_o15, na.rm=TRUE)/(sum(new_ep_u4,new_ep_5to14,new_ep_o15, na.rm=TRUE)),
#             percent_sp_u4=sum(new_sp_u4, na.rm=TRUE)/(sum(new_sp_u4,new_sp_5to14,new_sp_o15, na.rm=TRUE)),
#             percent_sp_5to14=sum(new_sp_5to14, na.rm=TRUE)/(sum(new_sp_u4,new_sp_5to14,new_sp_o15, na.rm=TRUE)),
#             percent_sp_o15=sum(new_sp_o15, na.rm=TRUE)/(sum(new_sp_u4,new_sp_5to14,new_sp_o15, na.rm=TRUE)))
#   
# tb_age_sp<-gather(tb_age, age,percent,percent_sp_u4:percent_sp_o15, factor_key=TRUE)
# tb_age_sn<-gather(tb_age, age,percent,percent_sn_u4:percent_sn_o15, factor_key=TRUE)
# tb_age_ep<-gather(tb_age, age,percent,percent_ep_u4:percent_ep_o15, factor_key=TRUE)
  
tb_age<-Mada_TB %>%
  mutate(new_sp_o15=new_sp_15to24+new_sp_25to34+new_sp_35to44+new_sp_45to54+new_sp_55to64+new_sp_o64)%>%
   group_by(year) %>%
  summarise(sp_u4_rate=sum(new_sp_u4, na.rm=TRUE)/25000000*100000,
             sn_u4_rate=sum(new_sn_u4, na.rm=TRUE)/25000000*100000,
             ep_u4_rate=sum(new_ep_u4, na.rm=TRUE)/25000000*100000,
             sp_5to14_rate=sum(new_sp_5to14, na.rm=TRUE)/25000000*100000,
             sn_5to14_rate=sum(new_sn_5to14, na.rm=TRUE)/25000000*100000,
             ep_5to14_rate=sum(new_ep_5to14, na.rm=TRUE)/25000000*100000,
             sp_o15_rate=sum(new_sp_o15, na.rm=TRUE)/25000000*100000,
             sn_o15_rate=sum(new_sn_o15, na.rm=TRUE)/25000000*100000,
             ep_o15_rate=sum(new_ep_o15, na.rm=TRUE)/25000000*100000)
             
 tb_type_u4<-tb_age%>%
   select(year,sp_u4_rate,sn_u4_rate,ep_u4_rate)%>%
   gather(type,rate,sp_u4_rate:ep_u4_rate, factor_key=TRUE)
 tb_type_5to14<-tb_age%>%
   select(year,sp_5to14_rate,sn_5to14_rate,ep_5to14_rate)%>%
   gather(type,rate,sp_5to14_rate:ep_5to14_rate, na.rm=TRUE,factor_key=TRUE)
 tb_type_o15<-tb_age%>%
   select(year,sp_o15_rate,sn_o15_rate,ep_o15_rate)%>%
   gather(type,rate,sp_o15_rate:ep_o15_rate, factor_key=TRUE)

 # Plot notification rates of types of tb for each age group
 ggplot(tb_type_u4, aes(x=year, y=rate))+
   geom_area(aes(fill=type))+
   scale_fill_discrete(name="TB type", labels=c("Smear positive","Smear negative","Extrapulmonary"))+
   labs(x="Year",y="Notifications per 100,000",title="Reported TB cases in children under 5 years")
 ggsave("tb_u5.jpg", width=10, height=5)
 ggplot(tb_type_5to14, aes(x=year, y=rate))+
   geom_area(aes(fill=type))+
   scale_fill_discrete(name="TB type", labels=c("Smear positive","Smear negative","Extrapulmonary"))+
   labs(x="Year",y="Notifications per 100,000",title="Reported TB cases in children between 5 and 14 years")
 ggsave("tb_5to14.jpg", width=10, height=5)
 ggplot(tb_type_o15, aes(x=year, y=rate))+
   geom_area(aes(fill=type))+
   scale_fill_discrete(name="TB type", labels=c("Smear positive","Smear negative","Extrapulmonary"))+
   labs(x="Year",y="Notifications per 100,000",title="Reported TB cases in people 15 years and older")
 ggsave("tb_o15.jpg", width=10, height=5)
 


# Plot of types of TB by age group
ggplot(tb_age_sp, aes(year, percent)) +
  geom_area(aes(fill=age))+
  scale_fill_discrete(name="Age", labels=c("Under 5 years","5 to 14 years","15 years and older"))+
  labs(x="Year",y="Proportion",title="Reported smear positive TB cases by age")
ggsave("tb_age_sp.jpg", width=10, height=5)
ggplot(tb_age_sn, aes(year, percent)) +
  geom_area(aes(fill=age))+
  scale_fill_discrete(name="Age", labels=c("Under 5 years","5 to 14 years","15 years and older"))+
  labs(x="Year",y="Proportion",title="Reported smear negative TB cases by age")
ggsave("tb_age_sn.jpg", width=10, height=5)
ggplot(tb_age_ep, aes(year, percent)) +
  geom_area(aes(fill=age))+
  scale_fill_discrete(name="Age", labels=c("Under 5 years","5 to 14 years","15 years and older"))+
  labs(x="Year",y="Proportion",title="Reported extrapulmonary TB cases by age")
ggsave("tb_age_ep.jpg", width=10, height=5)
# Do the inverse of these graphs



# Analysis of regional hospitals for HIV
regional_cases <- read_csv("D:/TB and HIV in Madagascar/New Madagascar data/Mada_HIV_reg_cases.csv", na = "NA") 
regional_tests <- read_csv("D:/TB and HIV in Madagascar/New Madagascar data/Mada_HIV_reg_screen.csv", na = "NA") 
#Note: don't have regional population, so can't do notification rate
regional_hiv<-merge(regional_cases, regional_tests, by=c("region","year"))
regional_hiv_year <- regional_hiv%>%
  group_by(year)%>%
    summarise(regional_prop_year=sum(total_hiv, na.rm=TRUE)/sum(total_test, na.rm=TRUE))
#Plot regional hiv by year
ggplot(regional_hiv_year, aes(x=year, y=hiv_prop_year))+
  geom_line()+
  labs(x="Year",y="Proportion of positive HIV tests",title="Proportion of positive HIV tests at regional and academic hospitals")
ggsave("hiv_region_year.jpg", width=10, height=5)

#Analysis of district hospitals HIV
district_cases <- read_csv("D:/TB and HIV in Madagascar/New Madagascar data/Mada_HIV_dist_cases.csv", na = "NA") 
district_tests <- read_csv("D:/TB and HIV in Madagascar/New Madagascar data/Mada_HIV_dist_screen.csv", na = "NA") 

district_hiv<-merge(district_cases, district_tests, by=c("district","year"))
district_hiv_year <- district_hiv%>%
  group_by(year)%>%
summarise(district_prop_year=sum(total_hiv, na.rm=TRUE)/sum(total_test, na.rm=TRUE))
#Plot of district hiv by year
ggplot(district_hiv_year, aes(x=year, y=hiv_prop_year))+
  geom_line()+
  labs(x="Year",y="Proportion of positive HIV tests",title="Proportion of positive HIV tests at district hospitals")
ggsave("hiv_district_year.jpg", width=10, height=5)


#Analysis of basic health centers hiv
#This is an excel file with multiple worksheets, so it has to be imported in a different way
path <- "D:/TB and HIV in Madagascar/New Madagascar data/Mada_HIV_bhc_cases.xlsx"
basic_cases <- path %>%
  excel_sheets() %>%
  set_names() %>% 
  map_df(~ read_excel(path = path, sheet = .x, range = "A1:Q3204"), .id = "sheet")%>%
  filter(!is.na(basic_health_center))%>%
  mutate(year=as.numeric(sheet))
path <- "D:/TB and HIV in Madagascar/New Madagascar data/Mada_HIV_bhc_screen.xlsx"
basic_tests <- path %>%
  excel_sheets() %>%
  set_names() %>% 
  map_df(~ read_excel(path = path, sheet = .x, range = "A1:Q3204"), .id = "sheet")%>%
  filter(!is.na(basic_health_center))%>%
  mutate(year=as.numeric(sheet))
#Plot bhc hiv
basic_hiv<-merge(basic_cases, basic_tests, by=c("basic_health_center","sheet")) 
basic_hiv$year<-as.numeric(basic_hiv$sheet)
basic_hiv_year<-basic_hiv%>%
  group_by(year)%>%
  summarise(basic_prop_year=sum(total_hiv, na.rm=TRUE)/sum(total_test, na.rm=TRUE))

# Merge regional, district, and basic health centers for plotting
hiv<-merge(basic_hiv_year, district_hiv_year, by="year")
hiv<-merge(hiv, regional_hiv_year, by="year")
# Plot regional, district, and basic health centers for plotting
hiv<-gather(hiv,health_center,proportion,basic_prop_year:regional_prop_year, factor_key=TRUE)
ggplot(hiv, aes(year, proportion, group=health_center, col=health_center)) +
  geom_line()+
  scale_color_discrete(name="Health Center", labels=c("Basic Health Center","District Health Center","Regional and Academic Hospital"))+
  labs(x="Year",y="Proportion",title="Proportion of HIV positive tests")
ggsave("hiv_year.jpg", width=10, height=5)

## Try to do calculation for % positive tests for all risk factors by year in each dataset
# Start with regional hospitals
regional_risk_year<-regional_hiv%>%
  group_by(year)%>%
  summarise(total_prop=sum(total_hiv, na.rm=TRUE)/sum(total_test, na.rm=TRUE),
            sw_prop=sum(sw_hiv, na.rm=TRUE)/sum(sw_test, na.rm=TRUE),
            sti_prop=sum(sti_hiv, na.rm=TRUE)/sum(sti_test, na.rm=TRUE),
            msm_prop=sum(msm_hiv, na.rm=TRUE)/sum(msm_test, na.rm=TRUE),
            idu_prop=sum(idu_hiv, na.rm=TRUE)/sum(idu_test, na.rm=TRUE),
            detained_prop=sum(detained_hiv, na.rm=TRUE)/sum(detained_test, na.rm=TRUE),
            otherrisk_prop=sum(otherrisk_hiv, na.rm=TRUE)/sum(otherrisk_test, na.rm=TRUE),
            pregnant_prop=sum(pregnant_hiv, na.rm=TRUE)/sum(pregnant_test, na.rm=TRUE),
            child_prop=sum(child_hiv, na.rm=TRUE)/sum(child_test, na.rm=TRUE),
            tb_prop=sum(tb_hiv, na.rm=TRUE)/sum(tb_test, na.rm=TRUE),
            other_prop=sum(other_hiv, na.rm=TRUE)/sum(other_test, na.rm=TRUE))%>%
  gather(risk_group,proportion,total_prop:other_prop, factor_key=TRUE)

ggplot(regional_risk_year, aes(year, proportion, group=risk_group, col=risk_group))+
  geom_line()+
  scale_color_discrete(name="Risk Factor", labels=c("Total","Sex workers","STI patients",
                                                    "MSM","Intravenous drug users","Detained",
                                                    "Other risk groups","Pregnant women",
                                                    "Children of mothers with HIV",
                                                    "TB patients","Others"))
# district hospitals
district_risk_year<-district_hiv%>%
  group_by(year)%>%
  summarise(total_prop=sum(total_hiv, na.rm=TRUE)/sum(total_test, na.rm=TRUE),
            sw_prop=sum(sw_hiv, na.rm=TRUE)/sum(sw_test, na.rm=TRUE),
            sti_prop=sum(sti_hiv, na.rm=TRUE)/sum(sti_test, na.rm=TRUE),
            msm_prop=sum(msm_hiv, na.rm=TRUE)/sum(msm_test, na.rm=TRUE),
            idu_prop=sum(idu_hiv, na.rm=TRUE)/sum(idu_test, na.rm=TRUE),
            detained_prop=sum(detained_hiv, na.rm=TRUE)/sum(detained_test, na.rm=TRUE),
            otherrisk_prop=sum(otherrisk_hiv, na.rm=TRUE)/sum(otherrisk_test, na.rm=TRUE),
            pregnant_prop=sum(pregnant_hiv, na.rm=TRUE)/sum(pregnant_test, na.rm=TRUE),
            child_prop=sum(child_hiv, na.rm=TRUE)/sum(child_test, na.rm=TRUE),
            tb_prop=sum(tb_hiv, na.rm=TRUE)/sum(tb_test, na.rm=TRUE),
            other_prop=sum(other_hiv, na.rm=TRUE)/sum(other_test, na.rm=TRUE))%>%
  gather(risk_group,proportion,total_prop:other_prop, factor_key=TRUE)

ggplot(district_risk_year, aes(year, proportion, group=risk_group, col=risk_group))+
  geom_line()+
  scale_color_discrete(name="Risk Factor", labels=c("Total","Sex workers","STI patients",
                                                    "MSM","Intravenous drug users","Detained",
                                                    "Other risk groups","Pregnant women",
                                                    "Children of mothers with HIV",
                                                    "TB patients","Others"))
# basic health centers
basic_risk_year<-basic_hiv%>%
  group_by(year)%>%
  summarise(total_prop=sum(total_hiv, na.rm=TRUE)/sum(total_test, na.rm=TRUE),
            sw_prop=sum(sw_hiv, na.rm=TRUE)/sum(sw_test, na.rm=TRUE),
            sti_prop=sum(sti_hiv, na.rm=TRUE)/sum(sti_test, na.rm=TRUE),
            msm_prop=sum(msm_hiv, na.rm=TRUE)/sum(msm_test, na.rm=TRUE),
            idu_prop=sum(idu_hiv, na.rm=TRUE)/sum(idu_test, na.rm=TRUE),
            detained_prop=sum(detained_hiv, na.rm=TRUE)/sum(detained_test, na.rm=TRUE),
            otherrisk_prop=sum(otherrisk_hiv, na.rm=TRUE)/sum(otherrisk_test, na.rm=TRUE),
            pregnant_prop=sum(pregnant_hiv, na.rm=TRUE)/sum(pregnant_test, na.rm=TRUE),
            tb_prop=sum(tb_hiv, na.rm=TRUE)/sum(tb_test, na.rm=TRUE),
            other_prop=sum(other_hiv, na.rm=TRUE)/sum(other_test, na.rm=TRUE))%>%
  gather(risk_group,proportion,total_prop:other_prop, factor_key=TRUE)%>%

ggplot(basic_risk_year, aes(year, proportion, group=risk_group, col=risk_group))+
  geom_line()+
  scale_color_discrete(name="Risk Group", labels=c("Total","Sex workers","STI patients",
                                                    "MSM","Intravenous drug users","Detained",
                                                    "Other risk groups","Pregnant women",
                                                    "TB patients","Others"))+
  labs(x="Year",y="Proportion of positive tests",title="Proportion of positive HIV tests by risk group")
ggsave("hiv risk factors_basic.jpg",width=10, height=5)

# Plot of male HIV percent vs female test percent
basic_hiv%>%
  group_by(year)%>%
  summarise(male_female_hiv=sum(male_hiv, na.rm=TRUE)/sum(female_hiv, na.rm=TRUE))%>%
ggplot(aes(x=year, y=male_female_hiv))+
  geom_line()

# Plot of TB and HIV in pregnant women for every year
basic_pregnant<-basic_cases%>%
  group_by(year)%>%
  summarise(basic_pregnant=sum(pregnant_hiv, na.rm=TRUE))
district_pregnant<-district_cases%>%
  group_by(year)%>%
  summarise(district_pregnant=sum(pregnant_hiv, na.rm=TRUE))
regional_pregnant<-regional_cases%>%
  group_by(year)%>%
  summarise(regional_pregnant=sum(pregnant_hiv, na.rm=TRUE))
pregnant_hiv<-basic_pregnant%>%
  merge(district_pregnant)%>%
  merge(regional_pregnant)%>%
  mutate(all_pregnant_hiv_rate=(basic_pregnant+district_pregnant+regional_pregnant)/25000000*100000)
         
ggplot(pregnant_hiv, aes(x=year, y=all_pregnant_hiv_rate))+
  geom_point()+ #probably want to fit a curve to this
  labs(title="HIV notification rate for pregnant women",x="Year",y="Notifications per 100,000 people per year")
ggsave("pregnant hiv.jpg",width=10, height=5)
# Plot of HIV:TB ratio over time
basic_year<-basic_cases%>%
  group_by(year)%>%
  summarise(basic_cases=sum(total_hiv, na.rm=TRUE))
district_year<-district_cases%>%
  group_by(year)%>%
  summarise(district_cases=sum(total_hiv, na.rm=TRUE))
all_hiv_year<-regional_cases%>%
  group_by(year)%>%
  summarise(regional_cases=sum(total_hiv, na.rm=TRUE))%>%
  merge(district_year)%>%
  merge(basic_year)%>%
  mutate(all_hospital_hiv=(basic_cases+district_cases+regional_cases))%>%
  mutate(all_hospital_hiv_rate=all_hospital_hiv/25000000*100000)
hiv_tb_ratio<-tb_total_year%>%
  merge(all_hiv_year)%>%
  mutate(hiv_ep_ratio=all_hospital_hiv_rate/ep_per_100000)%>%
  mutate(hiv_tb_ratio=all_hospital_hiv_rate/total_tb_per_100000)

ggplot(hiv_tb_ratio, aes(x=year, y=hiv_tb_ratio))+
  geom_point()+
  labs(x="Year",y="Ratio of HIV to TB",title="Ratio of HIV to Extrapulmonary TB Notification Rate")
ggsave("Ratio of HIV to EPTB.jpg",width=10,height=5)
ggplot(hiv_tb_ratio, aes(x=year, y=hiv_ep_ratio))+
  geom_point()+
  labs(x="Year",y="Ratio of HIV to TB",title="Ratio of HIV to All TB Notification Rate")
ggsave("Ratio of HIV to TB.jpg",width=10,height=5)

## I'm making a change to this code

