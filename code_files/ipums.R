library(ipumsr)
library(here)
library(tidyverse)
#install.packages('ipumsr')


setwd(here('data/full count census'))


# 1930 census data
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







file.choose()


# 1880 full count census data

ddi <- read_ipums_ddi("1880_100_percent_sample/ipumsi_00001.xml")
data <- read_ipums_micro(ddi)

data %>% 
  head(100) %>% 
  View()


data_after_1865 <- data %>% 
  filter(BIRTHYR >= 1864)


# we will also restrict ourserves to white people
#100 White 200 Black 210 Mulatto (1850-1910) 300 American Indian/Alaska Native

data_after_1865 <- data_after_1865 %>% 
  filter(RACEUS == 100)




names_count <- data %>% 
  count(NAMEFRST, sort = T)


data_after_1865 <- data_after_1865 %>% 
  mutate(first_name_lower = NAMEFRST %>% str_to_lower(),
         first_name_jefferson = (str_detect(first_name_lower, 'jefferson')) * 1,
         first_name_stonewall = (str_detect(first_name_lower, 'stonewall')) * 1,
         first_name_robert_e = (str_detect(first_name_lower, 'robert e')) * 1,
         first_name_robert_l = (str_detect(first_name_lower, 'robert l')) * 1
  )


data_after_1865 %>% 
  count(first_name_robert_e)

names_count %>% 
  filter(str_detect(NAMEFRST %>% str_to_lower(), 'jefferson'))

names(data_after_1865)

names_by_county <- data_after_1865 %>% 
  mutate(n_obs = 1) %>% 
  group_by(GEO1_US1880 , NHGISJOIN, COUNTYUS) %>% 
  summarize_at(vars(c('first_name_jefferson', 'first_name_stonewall', 'first_name_robert_e',
                      'first_name_robert_l', 'n_obs')), sum)


names_by_county %>% 
  write_csv('southern_first_names_by_county_1880.csv')

names_by_county <- read_csv("southern_first_names_by_county_1880.csv")







names_by_county <- names_by_county %>% 
  mutate(county_pad = str_pad(as.character(COUNTYUS) %>% str_sub( end = -2), 3, pad = "0"),
         state_pad = str_pad(as.character(GEO1_US1880), 2, pad = "0"),
         fips = str_c(state_pad, county_pad))

# Just to check how well we did match 
county_data_inner <- county_data %>% 
  mutate(fips = str_pad(fips, 5, pad = "0")) %>% #View()
  inner_join(names_by_county, by = 'fips') #%>% View()


names_by_county %>% 
  write_csv('southern_first_names_by_county_1880_final.csv')












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
