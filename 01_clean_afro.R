# Set up and packages  ----------------------------------------------------
setwd("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/")

if (!require("pacman")) 
  install.packages("pacman"); 

merge_surveys <- function(survey_list, var_harmonization) {
  
  if (any(!c("filename", "var_name_orig", "var_name_target", "var_label") %in% names(var_harmonization))) {
    stop(
      "var_harmonization must contain ",
      paste(c("filename", "var_name_orig", "var_name_target", "var_label"), collapse = ", "),
      "."
    )
  }
  
  fn_merge <- function(dat) {
    # Filter relevant harmonization rows
    select_vars <- var_harmonization %>% 
      filter(.data$filename == attr(dat, "filename"))
    
    # Warning for missing 'rowid'
    if (!"rowid" %in% select_vars$var_name_orig) {
      warning("rowid is not selected from ", attr(dat, "filename"))
    }
    
    # Select and rename variables
    tmp <- dat %>%
      select(all_of(select_vars$var_name_orig)) %>%
      rlang::set_names(nm = select_vars$var_name_target)
    
    # Ensure unique column names
    if (any(duplicated(names(tmp)))) {
      warning("Duplicate column names detected in ", attr(dat, "filename"), ". Making column names unique.")
      names(tmp) <- make.unique(names(tmp))
    }
    
    # Handle labelled variables
    labelled_vars <- names(tmp)[vapply(tmp, haven::is.labelled, logical(1))]
    
    if (length(labelled_vars) > 0) {
      fn_relabel <- function(x) as_labelled_spss_survey(x, id = attr(tmp, "id"))
      tmp <- tmp %>%
        mutate(across(any_of(labelled_vars), fn_relabel))
    }
    
    # Assign variable labels if provided
    if (!is.null(select_vars$var_label)) {
      labelled_items <- vapply(tmp, is.labelled_spss_survey, logical(1))
      labelled_items <- names(labelled_items)[labelled_items]
      
      labelling <- select_vars %>%
        select(all_of(c("var_name_target", "var_label")))
      
      fn_relabelling <- function(x) {
        labelling$var_label[which(labelling$var_name_target == x)]
      }
      
      for (x in labelled_items) {
        attr(tmp[[x]], "label") <- fn_relabelling(x)
      }
    }
    
    tmp
  }
  
  lapply(survey_list, fn_merge)
}

pacman::p_load(
  tidyverse,
  haven,
  sjmisc,
  labelled,
  here,
  readr,
  janitor,
  plm,
  scales,
  psych,
  retroharmonize,
  readxl,
  assertr,
  ### GEO Packages
  sf,
  spdep,
  spatstat,
  geodata,
  terra
)


folder <- "C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/00_rawdata/ab_raw"
output_folder <- file.path(folder, "processed") # Create a subdirectory for cleaned files

# Ensure the output folder exists
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}


files <- list.files(folder, pattern = "data.sav", full.names = TRUE, recursive = TRUE)

data_list <- list()
meta_list <- list()  # To accumulate the metadata



# Assuming 'survey' is your data list, and 'filename' and 'id' are defined.
metadata_list <- function(survey_data) {
  
  # extract filename
  filename <- paste0(unique(sub("_\\d+$", "", survey_data$rowid)), ".sav")
  # Extract variable labels
  var_label_orig <- lapply(survey_data, labelled::var_label)
  
  # extract value labels 
  
  val_labels_orig <- lapply(survey_data, labelled::val_labels)
  
  # Get the class for each column
  class_orig <- vapply(survey_data, function(x) class(x)[1], character(1))
  
  
  # Create the metadata tibble
  metadata <- tibble::tibble(
    filename = filename,  # Assuming filename is defined in your context
    id = NULL,              # Assuming id is defined in your context
    var_name_orig = names(survey_data),  # Column names
    class_orig = class_orig,  # Class for each variable
    label_orig = var_label_normalize(var_label_orig),
    labels = val_labels_orig
    
  )
  
  return(metadata)
}

# Process each file and write the cleaned version
data_list <- c(data_list, lapply(files, FUN = function(x) {
  
  # Clean the data
  cleaned_data <- haven::read_sav(x) %>%
    rename_all(tolower) %>%
    mutate(country = retroharmonize::as_character(country)) %>%
    filter(country %in% c("Nigeria", "Mozambique", "Malawi", "Kenya", "Ghana")) %>%
    set_variable_labels(
      country = "Country",
      respno = "respondent number"
    )
  
  # Generate a new filename with "_cleaned" added
  cleaned_filename <- file.path(output_folder, paste0(tools::file_path_sans_ext(basename(x)), "_cleaned.sav"))

  
  # Write the cleaned data to a new file
  haven::write_sav(cleaned_data, cleaned_filename)
  
}))

## Create Admin 1 Panel ##
ab <- dir("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/00_rawdata/ab_raw/processed", pattern = "sav$")
afrobarometer_rounds <- file.path("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/00_rawdata/ab_raw/processed", ab)

ab_waves <- read_surveys(afrobarometer_rounds, .f='read_spss')

# Create metadata for the cleaned data
meta <- lapply(ab_waves, metadata_list)

# Bind the current metadata to the previous metadata (if any)
ab_metadata <- bind_rows(meta)

to_harmonize <- ab_metadata %>%
  filter (var_name_orig %in% 
             c("rowid", "respno", "dateintr", "country", "withinwt", 	
               "ea_svc_a", "ea_svc_b", "ea_svc_c", "ea_svc_d",
               "ea_fac_a", "ea_fac_b", "ea_fac_c", "ea_fac_d",
               "ea_fac_e"
               ) |
             grepl("local government", label_orig) |
             grepl("corruption local", label_orig) |
             grepl("trust your local council", label_orig) |
             grepl("local gov", label_orig)) %>%
  mutate(label_orig = case_when(
    var_name_orig == "withinwt" ~ "withinwt",
    var_name_orig == "rowid" ~ "rowid",
    var_name_orig == "ea_svc_a" ~ first(label_orig[startsWith(var_name_orig, "ea_svc_a")]),
    var_name_orig == "ea_svc_b" ~ first(label_orig[startsWith(var_name_orig, "ea_svc_b")]),
    var_name_orig == "ea_svc_c" ~ first(label_orig[startsWith(var_name_orig, "ea_svc_c")]),
    var_name_orig == "ea_svc_d" ~ first(label_orig[startsWith(var_name_orig, "ea_svc_d")]),
    var_name_orig == "ea_fac_a" ~ first(label_orig[startsWith(var_name_orig, "ea_fac_a")]),
    var_name_orig == "ea_fac_b" ~ first(label_orig[startsWith(var_name_orig, "ea_fac_b")]),
    var_name_orig == "ea_fac_c" ~ first(label_orig[startsWith(var_name_orig, "ea_fac_c")]),
    var_name_orig == "ea_fac_d" ~ first(label_orig[startsWith(var_name_orig, "ea_fac_d")]),
    var_name_orig == "ea_fac_e" ~ first(label_orig[startsWith(var_name_orig, "ea_fac_e")]),
    TRUE ~ label_orig  # Keep other values unchanged
  ))%>%
  mutate (var_label = var_label_normalize(label_orig)) %>%
  mutate (var_name = val_label_normalize(var_label))


merged_ab<-retroharmonize::merge_waves(ab_waves, 
                           var_harmonization = to_harmonize)


#### MERGING DATA #####
#WAVE 3
r3 <- merged_ab[[1]]%>%
  janitor::remove_empty()%>%
  mutate(wave = 3)%>%
  select(respondent_number,
         country,
         date_of_interview,
         wave,
         trust_your_elected_local_government_council = trust_your_local_council,
         corruption_local_government_councilors,
         contact_local_government_councilor =contact_local_government_councillor,
         local_govt_handling_maintaining_roads,
         local_government_councilors_listen,
         local_government_councilors_listen
         )%>%
  mutate_if ( is.labelled_spss_survey, to_factor)
  

descr(r3)

data_list <- list()

folder <- "C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/00_rawdata/ab_raw_geo"

files <- list.files(folder, pattern = "r3.csv", full.names = TRUE, recursive = TRUE)

# Read each CSV and append to the list
data_list <- c(data_list, lapply(files, function(x) read_xlsx(x)%>%
                                   select(respno, latitude, longitude, precision_code)))


# Combine all dataframes into one
geo_r3<- do.call(rbind, data_list)

r3_merged <- tidylog::left_join(geo_r3,
                       r3, 
                       by = c("respno" = "respondent_number"))

#WAVE 4

r4 <- merged_ab[[2]]%>%
  janitor::remove_empty()%>%
  mutate(wave = 4)%>%
  select(respondent_number,
         country,
         date_of_interview,
         wave,
         starts_with("ea_svc"), #services 
         starts_with("ea_fac"), #facilities 
         corruption_local_government_councilors,
         contact_local_government_councilor =contact_local_government_councillor,
         local_govt_handling_maintaining_roads,
         local_govt_handling_maintaining_local_markets,
         performance_local_government_councilor,
         trust_your_elected_local_government_council,
         local_government_councilors_listen,
  )%>%
  mutate_if ( is.labelled_spss_survey, to_factor)

descr(r4)

data_list <- list()

folder <- "C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/00_rawdata/ab_raw_geo"

files <- list.files(folder, pattern = "r4.csv", full.names = TRUE, recursive = TRUE
)


# Read each CSV and append to the list
data_list <- c(data_list, lapply(files, function(x) read_xlsx(x)%>%
                                   select(respno, latitude, longitude, precision_code)))


# Combine all dataframes into one
geo_r4<- do.call(rbind, data_list)

r4_merged <- tidylog::left_join(geo_r4,
                                r4, 
                                by = c("respno" = "respondent_number"))

# WAVE 5 # 
r5 <- merged_ab[[3]]%>%
  janitor::remove_empty()%>%
  mutate(wave = 5)%>%
  select(respondent_number,
         country,
         date_of_interview,
         wave,
         starts_with("ea_svc"), #services 
         starts_with("ea_fac"), #facilities 
         corruption_local_government_councilors = corruption_local_government_councillors,
         contact_local_government_councilor =contact_local_government_councillor,
         local_govt_handling_maintaining_roads,
         local_govt_handling_maintaining_local_markets,
         performance_local_government_councilor = performance_local_government_councillor,
         trust_your_elected_local_government_council,
         local_government_councilors_listen = local_government_councillors_listen,
  )%>%
  mutate_if ( is.labelled_spss_survey, to_factor)
descr(r5)

data_list <- list()

folder <- "C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/00_rawdata/ab_raw_geo"

files <- list.files(folder, pattern = "r5.csv", full.names = TRUE, recursive = TRUE
)


# Read each CSV and append to the list
data_list <- c(data_list, lapply(files, function(x) read_xlsx(x)%>%
                                   select(respno, latitude, longitude, precision_code)))


# Combine all dataframes into one
geo_r5<- do.call(bind_rows, data_list)


r5_merged <- tidylog::left_join(geo_r5,
                                r5, 
                                by = c("respno" = "respondent_number"))

#wave 6# 
r6 <- merged_ab[[4]]%>%
  janitor::remove_empty()%>%
  mutate(wave = 6)%>%
  select(respondent_number,
         country,
         date_of_interview,
         wave,
         starts_with("ea_svc"), #services 
         starts_with("ea_fac"), #facilities 
         corruption_local_government_councilors,
         contact_local_government_councilor,
         local_govt_handling_maintaining_roads,
         local_govt_handling_maintaining_local_markets,
         performance_local_government_councilor,
         trust_your_elected_local_government_council,
         local_government_councilors_listen
  )%>%
  mutate_if ( is.labelled_spss_survey, to_factor)
descr(r6)

to_factor
data_list <- list()

folder <- "C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/00_rawdata/ab_raw_geo"

files <- list.files(folder, pattern = "r6.csv", full.names = TRUE, recursive = TRUE
)


# Read each CSV and append to the list
data_list <- c(data_list, lapply(files, function(x) read_xlsx(x)%>%
                                   select(respno, latitude, longitude, precision_code)))


# Combine all dataframes into one
geo_r6<- do.call(bind_rows, data_list)


r6_merged <- tidylog::left_join(geo_r6,
                                r6, 
                                by = c("respno" = "respondent_number"))


afro_merged <- bind_rows(r3_merged, r4_merged, r5_merged, r6_merged)


### OKAY NOW WE CAN CREATE THE INDEX ####

sapply(afro_merged, function(x) if(is.factor(x)) levels(x) else NULL)


afro_merged <-afro_merged %>%
  tidylog::mutate(
    #self report
    #corruption should be reverse coded 
    corruption_rec = case_match(
      as.character(corruption_local_government_councilors), 
      "None" ~4,
      "Some of them" ~3,
      "Most of them" ~2,
      "All of them"~1,
      #Everything else is NA
      .default = NA_real_
    ),
    trust_rec = case_match(
      as.character(trust_your_elected_local_government_council),
      "Not at all"~1,
      "Just a little"~2,
      "Somewhat"~3,
      "A lot"~4,
      #Everything else is NA
      .default = NA_real_
    ),
    contact_rec = case_match(
      as.character(contact_local_government_councilor),
      "Never"~1,
      "Only once"~2,
      "A few times"~3,
      "Often"~4,
      #Everything else is NA
      .default = NA_real_
    ),
    #maintian 4 point scales
    maintian_road_rec = case_match(
      as.character(local_govt_handling_maintaining_roads),
      "Very Badly"~1,
      "Fairly Badly"~2,
      "Fairly Well"~3,
      "Very Well"~4,
      #Everything else is NA
      .default = NA_real_
    ),
    maintian_market_rec = case_match(
      as.character(local_govt_handling_maintaining_local_markets),
      "Very Badly"~1,
      "Fairly Badly"~2,
      "Fairly Well"~3,
      "Very Well"~4,
      #Everything else is NA
      .default = NA_real_
    ),
    # preformance and listen
    preformance_rec = case_match(
      as.character(performance_local_government_councilor),
      "Strongly Disapprove"~1,
      "Disapprove"~2,
      "Approve"~3,
      "Strongly Approve"~4,
      #Everything else is NA
      .default = NA_real_
    ),
    listen_rec =case_match(
      as.character(local_government_councilors_listen),
      "Never"~1,
      "Only sometimes"~2,
      "Only Sometimes"~2,
      "Often"~3,
      "Always"~4,
      #Everything else is NA
      .default = NA_real_
    ))

#Descriptive 
descrips<-afro_merged %>%
  group_by(wave, country) %>%
  summarise(across(ends_with("_rec"), 
                   list(mean = ~mean(. , na.rm = TRUE), 
                        missing_count = ~sum(is.na(.))),
                   .names = "{.col}_{.fn}"))

#construct index subgovernemnt qaulity index 
afro_merged <- afro_merged %>%
  rowwise() %>%
  mutate(
    sub_gov_qual = sum(c_across(ends_with("rec")), na.rm = TRUE) / 
      sum(!is.na(c_across(ends_with("rec")))),
    non_miss = sum(!is.na(c_across(ends_with("rec"))))
  ) %>%
  ungroup()%>%
  mutate(
    sgqi = scales::rescale(sub_gov_qual, to = c(0, 100))
  )

afro_merged %>%
  group_by(wave, country) %>%
  summarise(mean.sqgi = mean(sgqi, na.rm =TRUE))%>%
  print(n=100)

#clean ea items 
afro_merged <-afro_merged %>%
  tidylog::mutate(across(
    starts_with("ea_"), # Select variables starting with "ea_"
    ~ if_else(as.character(.) == "Yes", 1, 0), # Recode: 1 for "Yes", 0 for others
    .names = "{.col}_rec" # Create new variables with "_rec" suffix
  ))

descrips2 <- afro_merged %>%
  group_by(wave, country) %>%
  summarise(across(
    starts_with("ea_") & ends_with("_rec"), # Select variables starting with "ea_" and ending with "_rec"
    list(
      mean = ~mean(. , na.rm = TRUE),        # Calculate mean, ignoring NA values
      missing_count = ~sum(is.na(.))        # Count missing values
    ),
    .names = "{.col}_{.fn}"                 # Append function name to new column names
  ))

#construct ea indexes 
afro_merged <- afro_merged %>%
  rowwise() %>%
  tidylog::mutate(
    ea_svc_index = sum(c_across(starts_with("ea_svc") & ends_with("_rec")), na.rm = TRUE),
    ea_fac_index = sum(c_across(starts_with("ea_fac") & ends_with("_rec")), na.rm = TRUE),
    non_miss_ea = sum(!is.na(c_across(starts_with("ea_") & ends_with("_rec"))))
  ) %>%
  ungroup()

write_sav(afro_merged, paste0(output_folder, "/afrobarometer_w3_w6_geomerged.sav"))

