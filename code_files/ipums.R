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

ddi <- read_ipums_ddi("1930_5_percent_sample/usa_00006.xml")
data <- read_ipums_micro(ddi)


names_count <- data %>% 
  count(NAMEFRST, sort = T)


data <- data %>% 
  mutate(first_name_lower = NAMEFRST %>% str_to_lower(),
         first_name_jefferson = (str_detect(first_name_lower, 'jefferson')) * 1,
         #first_name_jefferson_d = (str_detect(first_name_lower, 'jefferson d')) * 1,
         first_name_jeb = (str_detect(first_name_lower, 'jeb')) * 1,
         first_name_braxton = (str_detect(first_name_lower, 'braxton')) * 1,
         first_name_jubal = (str_detect(first_name_lower, 'jubal')) * 1,
         first_name_stonewall = (str_detect(first_name_lower, 'stonewall')) * 1,
         first_name_robert_e = (str_detect(str_c(first_name_lower %>% str_remove(' jr$'), " "), 'robert e[d ]')) * 1,
         first_name_robert_l = (str_detect(str_c(first_name_lower %>% str_remove(' jr$'), "  "), 'robert l[e ][e ]')) * 1
         )

names_count %>% 
  filter(str_detect(NAMEFRST %>% str_to_lower(), 'jefferson'))

data %>% 
  count(first_name_jeb)

data %>% 
  count(first_name_stonewall)

data %>% 
  count(first_name_robert_e)

data %>% 
  count(first_name_robert_l)

data %>% 
  count(first_name_jubal)


data %>% 
  count(RACE)

data %>% 
  filter(first_name_robert_l == 1) %>% 
  View()

	
str_detect("robert edward	fv", "robert edward|robert e")
str_detect("robert edward	fv", "robert e[d ]")
str_detect("robert eugene", "robert e[d ]")
str_detect("robert leo", "robert l[e ][e ]")
str_detect("robert lee", "robert l[e ][e ]")
str_detect("robert jr lee" %>% str_remove(' jr$'), "robert l[e ][e ]")



names_by_county <- data %>% 
  filter(BIRTHYR >= 1865, SEX == 1, RACE == 1) %>% #  sex == 1 corresponds to males, race == 1 corresponds to white
  mutate(n_obs = 1) %>% 
  group_by(STATEFIP , COUNTYICP) %>% 
  summarize_at(vars(c('first_name_jefferson', 'first_name_stonewall', 'first_name_robert_e', 'first_name_jeb',
                      'first_name_robert_l', 'first_name_braxton', 'first_name_jubal',   'n_obs')), sum)
  


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

ddi <- read_ipums_ddi("1880_100_percent_sample/ipumsi_00002.xml")
data <- read_ipums_micro(ddi)




data_after_1865 <- data %>% 
  filter(BIRTHYR >= 1865)


# we will also restrict ourserves to white males
#100 White 200 Black 210 Mulatto (1850-1910) 300 American Indian/Alaska Native
# 1 corresponds to male 2 to female
data_after_1865 <- data_after_1865 %>% 
  filter(RACEUS == 100, SEX == 1)




#names_count <- data %>% 
#  count(NAMEFRST, sort = T)


data_after_1865 <- data_after_1865 %>% 
  mutate(first_name_lower = NAMEFRST %>% str_to_lower(),
         first_name_jefferson = (str_detect(first_name_lower, 'jefferson')) * 1,
         #first_name_jefferson_d = (str_detect(first_name_lower, 'jefferson d')) * 1,
         first_name_braxton = (str_detect(first_name_lower, 'braxton')) * 1,
         first_name_jubal = (str_detect(first_name_lower, 'jubal')) * 1,
         first_name_stonewall = (str_detect(first_name_lower, 'stonewall')) * 1,
         first_name_robert_e = (str_detect(str_c(first_name_lower %>% str_remove(' jr$'), " "), 'robert e[d ]')) * 1,
         first_name_robert_l = (str_detect(str_c(first_name_lower %>% str_remove(' jr$'), "  "), 'robert l[e ][e ]')) * 1
  )


data_after_1865 %>% 
  count(first_name_robert_l)

names_count %>% 
  filter(str_detect(NAMEFRST %>% str_to_lower(), 'jefferson'))

names(data_after_1865)

names_by_county <- data_after_1865 %>% 
  mutate(n_obs = 1) %>% 
  group_by(GEO1_US1880 , NHGISJOIN, COUNTYUS) %>% 
  summarize_at(vars(c('first_name_jefferson', 'first_name_stonewall', 'first_name_robert_e',
                      'first_name_robert_l', 'first_name_braxton', 'first_name_jubal', 'n_obs')), sum)


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







data <- data %>% 
  filter(RACEUS == 100, SEX == 1)



data <- data %>% 
  mutate(first_name_lower = NAMEFRST %>% str_to_lower(),
         first_name_jefferson = (str_detect(first_name_lower, 'jefferson d')) * 1,
         #first_name_jefferson_d = (str_detect(first_name_lower, 'jefferson d')) * 1,
        # first_name_jeb = (str_detect(first_name_lower, 'jeb')) * 1,
         first_name_braxton = (str_detect(first_name_lower, 'braxton')) * 1,
         first_name_jubal = (str_detect(first_name_lower, 'jubal')) * 1,
         first_name_stonewall = (str_detect(first_name_lower, 'stonewall')) * 1,
         first_name_robert_e = (str_detect(str_c(first_name_lower %>% str_remove(' jr$'), " "), 'robert e[d ]')) * 1,
         first_name_robert_l = (str_detect(str_c(first_name_lower %>% str_remove(' jr$'), "  "), 'robert l[e ][e ]')) * 1
  )



data <- data %>% 
  mutate(birth_year_decade = case_when(BIRTHYR <= 1850 ~ "before 1850",
                                       (BIRTHYR >= 1851) & (BIRTHYR <= 1860) ~ "1851 to 1860",
                                       (BIRTHYR >= 1861) & (BIRTHYR <= 1870) ~ "1861 to 1870",
                                       (BIRTHYR >= 1871) & (BIRTHYR <= 1880) ~ "1871 to 1880"))




# state abb 13 - Georgia
# state abb 37 - North Carolina
# state abb 45 - South Carolina





names_by_county <- data %>% 
  mutate(n_obs = 1) %>% 
  filter(GEO1_US1880 %in% c(13, 37, 45)) %>% 
  group_by(birth_year_decade) %>% 
  summarize_at(vars(c('first_name_jefferson', 'first_name_stonewall', 'first_name_robert_e',
                      'first_name_robert_l', 'first_name_braxton', 'first_name_jubal', 'n_obs')), sum)



names_by_county <- names_by_county %>%
  mutate(all_conf_names = first_name_jefferson + first_name_stonewall +first_name_robert_l + first_name_robert_e+ first_name_braxton +first_name_jubal,
         all_conf_names_pct = (all_conf_names/n_obs) * 100,
         conf_names_no_jeff = first_name_stonewall + first_name_robert_e+ first_name_braxton +first_name_jubal+ first_name_robert_l,
         conf_names_no_jeff_pct =  (conf_names_no_jeff/n_obs) * 100, 
         lee_names_pct = (conf_names_no_jeff/n_obs) * 100,
         birth_year_decade = fct_relevel(birth_year_decade, "before 1850", "1851 to 1860", "1861 to 1870"))


names_by_county %>% 
  ggplot(aes(x = birth_year_decade, y = all_conf_names_pct, group = 1)) + geom_point() + geom_line()


names_by_county %>% 
  ggplot(aes(x = birth_year_decade, y = conf_names_no_jeff_pct, group = 1)) + geom_point() + geom_line()+
  labs(y = "Share of white males with Confederate first name (in %)", x = "Year of birth")+ 
  theme_minimal()+
  theme(axis.line = element_line(size = 1), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        text = element_text(size=16)#, axis.text.x = element_text(angle = x_labels_angle, hjust = x_labels_hjust)
        ) + expand_limits(y = 0)
  
ggsave(here::here("figures/conf_first_names_by_decade.pdf"))










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
