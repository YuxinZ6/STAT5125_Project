
####### environments and setup#######
# calling required packages
library(readr)
library(tidyverse)

# setting working directory 
setwd("~/Documents/Grad/Spring 2023/STAT_5125/course project /project/STAT5125_Project")


####### Cleaning dataset 11_Direct_Investment-related_Indicators #######
# read the investment indicators dataset
investment_indicators <- read_csv("11_Direct_Investment-related_Indicators.csv")

# initial investigation in the dataset. 
investment_indicators %>% glimpse()
# subset the dataset to discard unwanted columns
investment_indicators <- investment_indicators %>% 
  select(Country, ISO2, ISO3, Indicator, Unit, `CTS Code`, `CTS Name`, Sector,
         `2005`:`2018`)
investment_indicators %>% glimpse()


# creating dictionary for the country with ISO2 and ISO3. 
Country_label <- investment_indicators %>% select(Country, ISO2, ISO3) %>% distinct()

# creating dictionary for the sectors.
Sectors_label <- investment_indicators %>% select(Sector) %>% distinct()
Sectors_label <- Sectors_label %>% 
  mutate(label = sapply(1:length(Sector), function(i) paste0("sector_", i)))

# creating dictionary for the 8 types of CO2 emissions. 
CTS_label <- investment_indicators %>% select(`CTS Code`, `CTS Name`) %>% distinct()

# creating dictionary for unit
unique(investment_indicators$Unit)
Unit_label <- matrix(c("Metric tons of CO2", "MT", 
                       "Metric tons of CO2 per million US$ of output", "MTPMDO",
                       "Metric tons per million US$", "MTPMD"), 
                     ncol = 2, byrow = TRUE) %>% as.data.frame()
colnames(Unit_label) <- c("Unit", "Unit_abb")


# match Units in dataset with its abbreviations, using the dictionary
# match Sectors in dataset with its abbreviations, using the dictionary
investment_indicators <- investment_indicators %>% 
  mutate(Unit_abb = sapply(Unit, 
                           function(x) Unit_label$Unit_abb[match(x, Unit_label$Unit)])) %>%
  relocate(Unit_abb, .before = Unit) %>% 
  mutate(Sector_abb = sapply(Sector, 
                             function(x) Sectors_label$label[match(x, Sectors_label$Sector)])) %>%
  relocate(Sector_abb, .before = Sector) %>% 
  glimpse()


# select the columns of interest, save it temporarily.
tmp <- investment_indicators %>% select(ISO3, `CTS Code`, Sector_abb, 
                                        `2005`:`2018`)
# pivot longer the year and pivot wider the indicators, save as emission_df
emission_df <- tmp %>% pivot_longer(cols = `2005`:`2018`,
                                    names_to = "Year",
                                    values_to = "CO2_emission") %>%
  pivot_wider(names_from = `CTS Code`,
              values_from = CO2_emission)


# remove data that will not be used again
rm(tmp, investment_indicators)



### Summary: all dictionaries have ending with "_label". 
### We will be using emission_df for further analysis. 



####### Cleaning dataset 24_Climate-related_Diasters_Frequency#######
disasters_frequency <- read_csv("24_Climate-related_Disasters_Frequency.csv")

# CTS Code for climate related disasters frequency is ECCD. 
# Adding ECCD to CTS_label dictionary. 
# Obtain the CTS code and name for disasters
ECCD_code <- disasters_frequency$`CTS Code` %>% unique()
ECCD_name <- disasters_frequency$`CTS Name` %>% unique()
# adding the CTS code and name for disasters to CTS dictionary
ECCD_df <- data.frame(ECCD_code, ECCD_name)
names(ECCD_df) <- c("CTS Code", "CTS Name")
CTS_label <- rbind(CTS_label, ECCD_df)


# Subset the dataset to discard unwanted data. 
# Narrow down the data to focus on years from 2005 to 2018 (range of years
# in the emission_df).
disasters_frequency <- disasters_frequency %>%
  select(ISO3, Indicator, `CTS Code`, `2005`:`2018`)

# mutate the Indicator column to trim away repeated information
disasters_frequency <- disasters_frequency %>% 
  mutate(Indicator_abb = 
           str_extract(Indicator, "(?<=: ).*")) %>%
  relocate(Indicator_abb, .after = Indicator) %>%
  select(-Indicator) %>%
  rename(Indicator = Indicator_abb)

# filter to only total count of disasters.
# MAY NEED ADJUSTMENTS!!
disasters_frequency <- disasters_frequency %>%
  filter(Indicator == "TOTAL") %>% 
  select(-Indicator)

# replace all na with zeros
disasters_frequency <- disasters_frequency %>%
  mutate_all(~replace_na(., 0)) %>% glimpse()

disasters_df <- disasters_frequency %>% 
  pivot_longer(cols = `2005`:`2018`, 
               names_to = "Year",
               values_to = "Count") %>%
  pivot_wider(names_from = `CTS Code`, 
              values_from = Count)



#######Combining the two data sets#######
df <- emission_df %>% left_join(disasters_df, by = c("ISO3", "Year"))

# remove data not going to be used
rm(ECCD_df, disasters_frequency)



#####More Manipulation to the Dataset#####
df <- df %>% relocate(Year, .before = Sector_abb)
df <- df %>% mutate_all(~ ifelse(is.na(.), 0, .))
df <- df %>% rename(Export_Domestics = ECBIXD,
                           Export_Foreign = ECBIXF,
                           Output_Domestic = ECBIOD,
                           Output_Foreign = ECBIOF,
                           PerUnitOutput_Foreign = ECBIPF,
                           PerUnitOutput_Domestic = ECBIPD,
                           Gross_Fixed_Capital_Formation = ECBIFR,
                           Gross_Fixed_Capital_Formation_Final_Demand_Ratio = ECBIFF,
                           Disaster_Frequency = ECCD
)

df_long <- df %>% pivot_longer(cols = Export_Domestics:Disaster_Frequency,
                           names_to = "Category",
                           values_to = "Value")
df_wide <- df_long %>% pivot_wider(names_from = c("Sector_abb", "Category"), 
                                   values_from = Value, 
                                   values_fill = 0,
                                   names_glue = "{Category}_{Sector_abb}")












