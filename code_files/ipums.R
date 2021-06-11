library(ipumsr)
library(here)
library(tidyverse)
#install.packages('ipumsr')


read_ipums_micro()

setwd(here('data/full count census'))

ddi <- read_ipums_ddi("usa_00003.xml")
data <- read_ipums_micro(ddi)
nrow(data)

ddi <- read_ipums_ddi("1930_5_percent_sample/usa_00004.xml")
data <- read_ipums_micro(ddi)

ddi <- read_ipums_ddi("1930_5_percent_sample/usa_00005.xml")
data <- read_ipums_micro(ddi)


names_count <- data %>% 
  count(NAMEFRST, sort = T)


data <- data %>% 
  mutate(first_name_lower = NAMEFRST %>% str_to_lower(),
         first_name_jefferson = (str_detect(first_name_lower, 'jefferson')) * 1,
         first_name_stonewall = (str_detect(first_name_lower, 'stonewall')) * 1,
         first_name_robert_e = (str_detect(first_name_lower, 'robert e')) * 1,
         first_name_robert_l = (str_detect(first_name_lower, 'robert l')) * 1
         )

names_count %>% 
  filter(str_detect(NAMEFRST %>% str_to_lower(), 'jefferson'))



names_by_county <- data %>% 
  mutate(n_obs = 1) %>% 
  group_by(STATEFIP , COUNTYICP) %>% 
  summarize_at(vars(c('first_name_jefferson', 'first_name_stonewall', 'first_name_robert_e',
                      'first_name_robert_l', 'n_obs')), sum)
  


names_by_county <- names_by_county %>% 
  mutate(COUNTYICP_tirm = ifelse((as.character(COUNTYICP) %>% str_sub(start = -1) == '0') & (nchar(as.character(COUNTYICP)) ==4 ), 
                            as.character(COUNTYICP) %>% str_sub(start = 1, end = 3), as.character(COUNTYICP)), 
    county_icp_pad = str_pad(COUNTYICP_tirm, 3, pad = "0"),
    state_icp_pad = str_pad(as.character(STATEFIP), 2, pad = "0"),
    fips = str_c(state_icp_pad, county_icp_pad))



names_by_county <- names_by_county %>% 
  mutate(COUNTYICP_tirm = as.character(COUNTYICP) %>% str_sub( end = -2), 
         county_icp_pad = str_pad(COUNTYICP_tirm, 3, pad = "0"),
         state_icp_pad = str_pad(as.character(STATEFIP), 2, pad = "0"),
         fips = str_c(state_icp_pad, county_icp_pad))


names_by_county %>% 
  write_csv('southern_first_names_by_county_1930.csv')

county_data <- read_csv("C:\\Users\\marti\\OneDrive\\Plocha\\sherman_march\\data\\acharya_et_al_2016_county_data.csv")



county_data_inner <- county_data %>% 
  mutate(fips = str_pad(fips, 5, pad = "0")) %>% 
  inner_join(names_by_county, by = 'fips') #%>% View()





county_data_inner %>% 
  filter(state.abb %in% c('GA', 'NC', 'SC')) %>% 
  dplyr::select(state.abb, fips, county_name, STATEFIP, COUNTYICP, COUNTYICP_tirm) %>% 
  View()
  

# SC has fips code 45

"55510" %>% 
  str_sub( end = -2)

# Stonewall

names_count %>% 
  filter(str_detect(NAMEFRST %>% str_to_lower(), 'stonewall'))



names_count %>% 
  filter(str_detect(NAMEFRST %>% str_to_lower(), 'jeb'))


names_count %>% 
  filter(str_detect(NAMEFRST %>% str_to_lower(), 'robert m'))
