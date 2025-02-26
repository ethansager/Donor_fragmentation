# Cleaning datasets script 
# Set up and packages  ----------------------------------------------------
setwd("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/")

if (!require("pacman")) 
  install.packages("pacman"); 

pacman::p_load(
  tidyverse,
  here,
  readr,
  janitor,
  sjmisc,
  plm,
  scales,
  psych,
  tidylog,
  units,
  DescTools,
  zoo,
  ### GEO Packages
  sf,
  spdep,
  spatstat,
  geodata,
  terra,
  giscoR,
  nngeo
)

## Load dat
dat<-read_csv("00_rawdata/GODAD_projectlevel.csv")

# Key missing info by donor 
countries_iso3 <- c(
  "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CAF", "TCD", "COG", "COD",
  "DJI", "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
  "CIV", "KEN", "LSO", "LBR", "MDG", "MWI", "MLI", "MRT", "MOZ", "NAM",
  "NER", "NGA", "RWA", "SEN", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
  "TGO", "UGA", "ZMB", "ZWE"
)


# Round 3	2005-2006	18	
# Round 4	2008-2009	20	
# Round 5	2011-2013	34	
# Round 6	2014-2015	36

# Clean CRS data as it does not have IATI codes
dat <- dat %>% 
  mutate(
    sector_main = as.numeric(str_split_fixed(sector_codes, "\\|", 2)[,1]),
    early_impact = case_when(
      sector_main %in% 110:160 ~ 0,
      sector_main %in% 210:230 ~ 1,
      sector_main %in% 231 ~ 0,
      sector_main %in% 232:323 ~ 1,
      sector_main %in% 331:998 ~ 0,
      TRUE ~ NA
    ),
    group_yr = case_when(
      paymentyear %in% 2005:2008 ~ 1,
      paymentyear %in% 2009:2011 ~ 2,
      paymentyear %in% 2012:2015 ~ 3
      ))%>%
  filter(
           !is.na(gid_2)& 
           !is.na(disb_loc_evensplit) & 
           !is.na(paymentyear) & 
           disb != 0 & 
           gid_0 %in% countries_iso3 & 
           paymentyear >= 2004 & paymentyear <= 2015
         )


## Create a hhi to merge back 
hhi_results_admin2 <- dat %>%
  # Aggregate total disbursements per donor within each group
  group_by(gid_0, gid_2, group_yr, donor) %>%
  summarise(
    total_disb_admin2 = sum(abs(disb_loc_evensplit), na.rm = TRUE), 
    total_early_projects = sum(early_impact == 1, na.rm = TRUE),
    total_late_projects = sum(early_impact == 0, na.rm = TRUE),
    .groups = "drop") %>%
  
  # Group by region and year to calculate HHI and other metrics
  group_by(gid_0, gid_2, group_yr) %>%
  summarise(
    # Count of project types 
    total_early_admin2 = sum(total_early_projects),
    total_late_admin2 = sum(total_late_projects),
    total_proj_admin2 = sum(total_late_projects, total_early_projects),
    # Sum total aid across all donors in the group
    total_aid_admin2 = sum(total_disb_admin2, na.rm = TRUE),
    
    # Count the distinct number of donors
    donor_count_admin2 = n_distinct(donor),
    
    # Calculate the HHI (summing squares of all percentage contributions)
    hhi_admin2 = sum((total_disb_admin2 / total_aid_admin2)^2, na.rm = TRUE),
    
    # Calculate the fragility index as 1 minus HHI
    frag_index_admin2 = 1 - hhi_admin2,
    
    # Largest total_disb / total_aid from 1
    frag_1_admin2 = 1 -  max(total_disb_admin2 / total_aid_admin2, na.rm = TRUE),
    
    # Largest 3 total_disb / total_aid from 1
    frag_3_admin2 = sum(head(sort(total_disb_admin2 / total_aid_admin2, decreasing = TRUE), 3), na.rm = TRUE),
    
    # Count the number of donors below 10% of total_disb / total_aid
    frag_below10_admin2 = sum((total_disb_admin2 / total_aid_admin2) < 0.10, na.rm = TRUE),
    
    .groups = "drop"
  )



## Create a hhi to merge back 
hhi_results_admin1 <- dat %>%
  
  # Aggregate total disbursements per donor within each group
  group_by(gid_0, gid_1, group_yr, donor) %>%
  summarise(
    total_disb_admin1 = sum(abs(disb_loc_evensplit), na.rm = TRUE), 
    total_early_projects = sum(early_impact == 1, na.rm = TRUE),
    total_late_projects = sum(early_impact == 0, na.rm = TRUE),
            .groups = "drop") %>%
  
  # Group by region and year to calculate HHI and other metrics
  group_by(gid_0, gid_1, group_yr) %>%
  summarise(
    # Count of project types 
    total_early_admin1 = sum(total_early_projects),
    total_late_admin1 = sum(total_late_projects),
    # Sum total aid across all donors in the group
    total_aid_admin1 = sum(total_disb_admin1, na.rm = TRUE),
    
    # Count the distinct number of donors
    donor_count_admin1 = n_distinct(donor),
    
    # Calculate the HHI (summing squares of all percentage contributions)
    hhi_admin1 = sum((total_disb_admin1 / total_aid_admin1)^2, na.rm = TRUE),
    
    # Calculate the fragility index as 1 minus HHI
    frag_index_admin1 = 1 - hhi_admin1,
    
    # Largest total_disb / total_aid from 1
    frag_1_admin1 = 1 -  max(total_disb_admin1 / total_aid_admin1, na.rm = TRUE),
    
    # Largest 3 total_disb / total_aid from 1
    frag_3_admin1 = sum(head(sort(total_disb_admin1 / total_aid_admin1, decreasing = TRUE), 3), na.rm = TRUE),
    
    # Count the number of donors below 10% of total_disb / total_aid
    frag_below10_admin1 = sum((total_disb_admin1 / total_aid_admin1) < 0.10, na.rm = TRUE),
    
    .groups = "drop"
  )



summary(hhi_results_admin2)
