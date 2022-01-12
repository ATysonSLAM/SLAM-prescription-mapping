# Adele Tyson, SLaM, Adele.Tyson@slam.nhs.uk
# 02/11/2021
# Imports and write all data for mapping of GP practices and IMD scores for
# SLaM boroughs

################################################################################

# GP practices within SLaM boroughs





################################################################################

# Cost of mental health drugs by GP practice

# Data for drug cost is from
# "OpenPrescribing.net, EBM DataLab, University of Oxford, 2017"

# NB: inside SLaM offices, pulling data from Open Prescribing doesn't work,
# therefore the data is written to Excel and can be read from there instead.

# Set up drug definitions
drugs <- data.frame(drug_full = c("4.1.1: Hypnotics",
                                  "4.1.2: Anxiolytics",
                                  "4.1.3: Barbiturates",
                                  "4.2.1: Antipsychotic drugs",
                                  "4.2.2: Antipsychotic depot injections",
                                  "4.2.3: Drugs used for mania and hypomania",
                                  "4.3.1: Tricyclic and related antidepressant drugs",
                                  "4.3.2: Monoamine-oxidase inhibitors (maois)",
                                  "4.3.3: Selective serotonin re-uptake inhibitors",
                                  "4.3.4: Other antidepressant drugs",
                                  "4.7.1: Non-opioid analgesics and compound preparations",
                                  "4.7.2: Opioid analgesics",
                                  "4.7.3: Neuropathic pain",
                                  "4.7.4: Antimigraine drugs",
                                  "4.10.1: Alcohol dependence",
                                  "4.10.2: Nicotine dependence",
                                  "4.10.3: Opioid dependence"))

drugs_classes <- data.frame(drug_class_full = c("4.1: Hypnotics and anxiolytics",
                                                "4.2: Drugs used in psychoses and related disorders",
                                                "4.3: Antidepressant drugs",
                                                "4.7: Analgesics",
                                                "4.10: Drugs used in substance dependence"))

# Bring together all useful drug info
drugs_classes <- drugs_classes %>%
  mutate(drug_class_temp = drug_class_full) %>%
  separate(col = drug_class_temp, 
           into = c("drug_class_code", "drug_class_name"), 
           sep = ": ")

drugs <- drugs %>%
  mutate(drug_temp = drug_full) %>%
  separate(col = drug_temp, 
           into = c("drug_code", "drug_name"), 
           sep = ": ") %>%
  mutate(drug_temp2 = drug_code) %>%
  separate(col = drug_temp2, 
           into = c("drug_class_pre", "drug_class_mid", NA),
           sep = "[.]") %>%
  unite(col = "drug_class_code", 
        drug_class_pre:drug_class_mid, 
        sep = ".", 
        remove = TRUE) %>%
  merge(y = drugs_classes, by = "drug_class_code", all = TRUE) %>%
  relocate(drug_class_code, .after = drug_class_full)


# Set up API URL definitions and pull data
url_root_spending <- "https://openprescribing.net/api/1.0/spending_by_practice/"

get_presc_data <- function(url_root_fun, 
                           BNF_code_fun, 
                           borough_fun) { 
  # Function constructs URL and pulls and filters associated data
  # Returns data for one BNF code across practices and boroughs as a data.frame
  # NB: borough_fun not currently used in function but will be once we have
  # list of practices by borough
  
  # Construct URL
  url_full <- paste0(url_root_fun, 
                     "?code=", 
                     BNF_code_fun, 
                     "&org=72Q,36L", #72Q = SE Lon CCG, 36L = SW Lon CCG
                     "&format=csv")
  
  # Pull in associated data
  data_pull <- GET(url_full) %>%
    content() %>%
    as.data.frame() %>%
    mutate(drug_code = BNF_code_fun)
  
  # Would like to filter out practices outside SLaM catchment but don't have
  # a list of practices by borough (LSLC_GP_ids)
  #
  # data_pull_filtered <- data_pull %>%
  #   filter(row_id %in% LSLC_GP_ids$GP_code)
  # 
  # if (borough == "Southwark" | borough == "Croydon" | borough == "Lambeth" | borough == "Lewisham") {
  #   data_pull_filtered <- data_pull_filtered %>%
  #     filter(row_id %in% LSLC_GP_ids[which(LSLC_GP_ids$Borough==borough),]$GP_code)
  # }
  
  return(data_pull)
}

# Set up empty list for pulled data which will then be bound into single df
datalist <- NULL

# Pull data
for (i in c(1:nrow(drugs))) {
  #print(i)
  #print(drugs$drug_code[i])
  data_temp <- get_presc_data(url_root_spending, drugs$drug_code[i])
  datalist[[i]] <- data_temp
}

# Bind data lists into single data.frame
data <- do.call(rbind, datalist)

# Clean data
data <- data %>%
  relocate(row_id, row_name, ccg, date, actual_cost) %>%
  mutate(date_temp = date, .after = date) %>%
  separate(col = date_temp, into = c("year", "month", "day"), sep = "-")

data <- merge(x = data, y = drugs, 
              by.x = "drug_code", by.y = "drug_code", 
              all.x = TRUE) %>%
  relocate(drug_code, .after = drug_full)

# Write data to excel - first set working directory to current project 
# Session -> Set working directory -> To source file location
write_xlsx(data, "Data_outputs/GP-drugs.xlsx", col_names = TRUE)

################################################################################

# Locations of GP practices

# Data for GP practice locations is from
# "OpenPrescribing.net, EBM DataLab, University of Oxford, 2017" 

# NB: inside SLaM offices, pulling data from Open Prescribing doesn't work,
# therefore the data is written to Excel and can be read from there instead.

# Get GP locations
url_practice <- "https://openprescribing.net/api/1.0/org_location/?q=72Q,36L&format=json"
practices = fromJSON(url_practice)

# Extract practice names and locations into a data.frame
# NB: couldn't find a way to avoid turning the geometry column into LAT and
# LONG columns only to turn them back into a geometry column later :(
practices_df <- data.frame(prac = practices[["features"]][["properties"]],
                           coords = practices[["features"]][["geometry"]])
practices_df <- practices_df %>%
  separate(col = coords.coordinates,
           into = c("LAT_temp", "LONG_temp"),
           sep = ",") %>% # NB: warning for SUTTON HORIZONS GYNAE CLINIC as no coordinates in pulled data
  separate(col = LAT_temp, into = c(NA, "LAT"), sep = "[(]") %>%
  separate(col = LONG_temp, into = c("LONG", NA), sep = "[)]") %>%
  na.omit() %>%  # Get rid of the SUTTON row
  transform(LAT = as.numeric(LAT), LONG = as.numeric(LONG)) %>%
  select(-prac.code, -prac.setting, -coords.type) #%>%
#distinct()

write_xlsx(practices_df, "Data_outputs/GP-practices.xlsx", col_names = TRUE)

################################################################################
################################################################################