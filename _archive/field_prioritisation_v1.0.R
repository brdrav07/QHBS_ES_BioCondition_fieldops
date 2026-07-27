#########################################################
### BioCondition Field Operations Prioritisation Tool ###
#########################################################

# Authored by Brodie Verrall and Gabrielle Lebbink
# Last updated 06/07/2026

# 0. Workspace setup -------------------------------------------------------------------------------------------------------------------------------------------
# 1.1) Run renv/activate.R to set project environment
# renv::status()
rm(list=ls())
gc()
save.image()

# 1.2) Install and load project library
# List of required packages
packages <- c("dplyr", "tidyr", "stringr", "arrow", "sf", "cli", "lubridate")
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}
lapply(packages, library, character.only = TRUE)

# 1.3) Create directories and set wd
current_dir <- basename(getwd())
if (current_dir == "project_data") {
  project_root <- getwd()
} else {
  project_root <- file.path(getwd(), "project_data")
}
inputs_folder  <- file.path(project_root, "inputs")
outputs_folder <- file.path(project_root, "outputs")
for (folder in c(project_root, inputs_folder, outputs_folder)) {
  if (!dir.exists(folder)) {
    dir.create(folder)
    cat("Created folder:", folder, "\n")
  } else {
    cat("Folder already exists:", folder, "\n")
  }
}
setwd(project_root)

# 1.4) Import data
Alldata <- read.csv("inputs/bc/All_summary_data.csv")
Bm_Mast_14 <- read.csv("inputs/bc/BENCHMARK_MASTER_LIST_v14.0.csv")
sbc_defs <- read.csv("inputs/sbc/20250904_SBC_trainingData_deficit.csv")
RE_v14_2025 <- read.csv("inputs/re/regional_ecosystem.csv") # TODO: ensure this is the latest version (v14)
RE_PC_v14 <- st_read("inputs/re/preclear_v14_3577.gpkg") %>% # TODO: read .gbd from BRI_data/RE_release and write as .gpkg in QGIS
  select("OBJECTID", "RE", "RE1", "PERCENT", "PC1", "BVG1M", "DBVG1M", "Shape_Length", "Shape_Area") %>%
  st_drop_geometry(RE_PC_v14)
gc()
RE_RE_v14 <- st_read("inputs/re/remnant_v14_3577.gpkg") %>% # TODO: read .gbd from BRI_data/RE_release and write as .gpkg in QGIS
  select("OBJECTID", "RE", "RE1", "PERCENT", "PC1", "BVG1M", "DBVG1M", "Shape_Length", "Shape_Area") %>%
  st_drop_geometry(RE_RE_v14)
gc()

# 1. Data processing -------------------------------------------------------------------------------------------------------------------------------------------
# 1.1) Manipulate regional ecosystem dataframes
RE_v14_2025 <- RE_v14_2025 %>%
  mutate(temp = NAME) %>%
  separate(temp, into = c("BR_code", "LZ_code", "VC_code"), sep = "\\.") %>%
  mutate(temp = MAPPED_EXTENTS) %>%
  separate(temp, into = c("Preclear_ha_REDD", "Remnant_ha_REDD"), sep = "\\;")
RE_v14_2025$Preclear_ha_REDD <- as.numeric(gsub("[^0-9]", "", RE_v14_2025$Preclear_ha_REDD))
RE_v14_2025$Remnant_ha_REDD <- as.numeric(gsub("2023|[^0-9]", "", RE_v14_2025$Remnant_ha_REDD))

# 1.2) Filter preclear and remnant polygons
RE_PC_v14 <- RE_PC_v14 %>%
  mutate(temp = RE1) %>%
  separate(temp, into = c("BR_code", "LZ_code", "VC_code"), sep = "\\.") %>%
  filter(!is.na(LZ_code) | LZ_code != "1")
RE_RE_v14 <- RE_RE_v14 %>%
  mutate(temp = RE1) %>%
  separate(temp, into = c("BR_code", "LZ_code", "VC_code"), sep = "\\.") %>%
  filter(!is.na(LZ_code) | LZ_code != "1")

# 1.3) Calculate cumulative preclear and remnant area of each unique RE1
RE_PC_v14_area <- RE_PC_v14 %>%
  group_by(RE1) %>%
  summarise(
    Preclear_polygons = n(),
    Preclear_ha = sum(Shape_Area, na.rm = TRUE) / 10000
  ) %>%
  ungroup()
RE_RE_v14_area <- RE_RE_v14 %>%
  group_by(RE1) %>%
  summarise(
    Remnant_polygons = n(),
    Remnant_ha = sum(Shape_Area, na.rm = TRUE) / 10000
  ) %>%
  ungroup()

# 1.4) Left join remnant to preclear
RE_extent_v14 <- RE_PC_v14_area %>%
  left_join(RE_RE_v14_area, by = c("RE1" = "RE1")) %>%
  mutate(
    Difference_ha    = Preclear_ha - Remnant_ha
  )
RE_extent_v14$RE_parent <- gsub("([a-zA-Z].*)$", "", RE_extent_v14$RE1)




# 3.5) Add training site numbers for SBC mapped RE1s to be modelled
BC_RE1 <- RE_extent_v14 %>%
  mutate(temp = RE1) %>%
  separate(temp, into = c("BR_code", "LZ_code", "VC_code"), sep = "\\.") #%>%
  # left_join(td_all_ref_counts, by = c("RE1" = "V13_RE")) %>%
  # mutate(
  #   TD_ref_sites = ifelse(is.na(TD_ref_sites), 0, TD_ref_sites),
  #   TD_supplementation_ref_sites = ifelse(is.na(TD_supplementation_ref_sites), 0, TD_supplementation_ref_sites),
  #   TD_analogous_ref_sites = ifelse(is.na(TD_analogous_ref_sites), 0, TD_analogous_ref_sites),
  #   TD_combined_ref_sites = ifelse(is.na(TD_combined_ref_sites), 0, TD_combined_ref_sites)
  # )
BC_RE1 <- BC_RE1 %>%
  left_join(
    RE_v14_2025 %>%
      select(
        NAME, PARENT, SHORT_DESCRIPTION, DESCRIPTION, STRUCTURE_CODE, BVG1M, DEFUNCT, Preclear_ha_REDD, Remnant_ha_REDD, EXTENT_WITHIN_PROTECTED_AREAS, 
        PROTECTED_AREAS, HABITAT, DISTRIBUTION, PUBLIC_COMMENTS, VMA_CLASS, BIODIVERSITY_STATUS, BIODIVERSITY_STATUS_NOTES 
      ),
    by = c("RE1" = "NAME") 
  )




#####################################

### ACCOUNTED FOR ABOVE ### DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE
# #Merging V14 and BM info
# V14sel<-V14%>%
#   select("NAME","MAPPED","DEFUNCT","CHANGED_TO","STRUCTURE_CODE","BVG1M","SHORT_DESCRIPTION")
# 
# V14_hasel<-V14_ha%>%
#   select(RE,ha_RE_PreClearing,ha_RE_2023)%>%
#   rename(REM_AREA_HA = ha_RE_2023, PRE_AREA_HA = ha_RE_PreClearing, NAME = RE)
### ACCOUNTED FOR ABOVE ### DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE_DELETE

BMsel<-Bm_Mast_14%>%
  select(NAME,BM,On.web_published,Creation.Date,Revision.Date,Reliability.Rating,Analogous.RE.s.used)

# clean BMsel








#Representative site counts for all data
Sitecountsall<-Alldata%>%
  filter(
    (database == "QBEIS" & SAMPLE_FLORISTICS %in% c("A", "B", "C") & REPRESENTATIVE == 1) |
      (database == "QBERD" & grepl("reference|remnant|ref", site_type, ignore.case = TRUE))|
      (database == "QBERD" & REPRESENTATIVE == 1 & grepl("CORVEG", site_type, ignore.case = TRUE))
  )%>%
  select(RE,database,QBERD_siteid,QBEIS_siteid)%>%
  distinct()%>%
  group_by(RE,database)%>%
  mutate(sitecount = n ())%>%
  filter(RE != "" & RE  != "No near match" & RE != "Cannot be assigned")%>%
  select(RE,database,sitecount)%>%
  distinct()%>%
  pivot_wider(values_from = sitecount, names_from = database)%>%
  rename(QBERD_rep = QBERD, QBEIS_rep = QBEIS)

#Representative site counts for all data with large trees
SitecountsLargetrees<-Alldata%>%
  filter(large_tree_data == 1)%>%
  filter(
    (database == "QBEIS" & SAMPLE_FLORISTICS %in% c("A", "B", "C") & REPRESENTATIVE == 1) |
      (database == "QBERD" & grepl("reference|remnant|ref", site_type, ignore.case = TRUE))|
      (database == "QBERD" & REPRESENTATIVE == 1 & grepl("CORVEG", site_type, ignore.case = TRUE))
  )%>%
  select(RE,database,QBERD_siteid,QBEIS_siteid)%>%
  distinct()%>%
  group_by(RE,database)%>%
  mutate(sitecount = n ())%>%
  filter(RE != "" & RE  != "No near match" & RE != "Cannot be assigned")%>%
  select(RE,database,sitecount)%>%
  distinct()%>%
  pivot_wider(values_from = sitecount, names_from = database)%>%
  rename(QBEIS_rep_LT = QBEIS,QBERD_rep_LT = QBERD)


Sitecountjoin<-SitecountsLargetrees%>%
  full_join(Sitecountsall)%>%
  rename(NAME = RE)



####################

BC_RE1_joined <- BC_RE1 %>%
  left_join(
    ungroup(Sitecountjoin),
    by = c("RE1" = "NAME")
  ) %>%
  left_join(
    BMsel,
    by = c("RE1" = "NAME")
  )








BM_priority_new<-V14sel%>%
  left_join(V14_hasel)%>%
  left_join(Sitecountjoin)%>%
  left_join(BMsel)

#Give site priority and combine with area priority based on number of QBEIS and QBERD sites for grasslands and QBEIS w wood and QBERD sites for wooded ecosystems
BM_prior_ranked <- BM_priority_new %>%
  # Filter rows where BM is "N"
  filter(BM == "N") %>%
  # Add new columns using mutate
  mutate(
    # Calculate total_sites based on STRUCTURE_CODE, as to exclude large tree data need for grassy sites
    total_sites = case_when(
      str_detect(STRUCTURE_CODE, regex("grassland|herbland|forbland|bare|sedgeland", ignore_case = TRUE)) ~ 
        coalesce(QBEIS_rep, 0) + coalesce(QBERD_rep, 0),  # Sum QBEIS_rep and QBERD_rep, handling NAs
      TRUE ~ coalesce(QBEIS_rep_LT, 0) + coalesce(QBERD_rep_LT, 0)  # Sum QBEIS_rep_LT and QBERD_rep_LT, handling NAs
    ),
    # Avoid divide-by-zero by setting total_sites to 1 if it's 0 or NA
    total_sites = if_else(total_sites == 0 | is.na(total_sites), 1, total_sites),
    
    # Calculate survey priority score as a ratio between area and total sites
    survey_priority_score = PRE_AREA_HA / total_sites,
    
    # Rank survey need based on survey_priority_score
    RANK_surveyneed = rank(-survey_priority_score, ties.method = "min")
  ) %>%
  
  # Remove intermediate columns
  select(-survey_priority_score, -total_sites) %>%
  
  # Extract Bioregion from NAME
  mutate(Bioregion = str_extract(NAME, "^[0-9]+")) %>%
  
  # Group by Bioregion
  group_by(Bioregion) %>%
  
  # Add new columns for grouped calculations
  mutate(
    # Calculate total_sites_BR based on STRUCTURE_CODE
    total_sites_BR = case_when(
      str_detect(STRUCTURE_CODE, regex("grassland|herbland|forbland|bare|sedgeland", ignore_case = TRUE)) ~ 
        coalesce(QBEIS_rep, 0) + coalesce(QBERD_rep, 0),  # Sum QBEIS_rep and QBERD_rep, handling NAs
      TRUE ~ coalesce(QBEIS_rep_LT, 0) + coalesce(QBERD_rep_LT, 0)  # Sum QBEIS_rep_LT and QBERD_rep_LT, handling NAs
    ),
    # Avoid divide-by-zero by setting total_sites_BR to 1 if it's 0 or NA
    total_sites_BR = if_else(total_sites_BR == 0 | is.na(total_sites_BR), 1, total_sites_BR),
    
    # Calculate survey priority score for each Bioregion
    survey_priority_score = PRE_AREA_HA / total_sites_BR,
    
    # Rank survey need by Bioregion based on survey_priority_score
    RANK_surveyneed_by_Bio = rank(-survey_priority_score, ties.method = "min")
  ) %>%
  
  # Remove intermediate columns
  select(-survey_priority_score, -total_sites_BR) %>%
  
  # Ungroup the data
  ungroup()











# 2. Calculate deficits ----------------------------------------------------------------------------------------------------------------------------------------

# BM deficits = less than 3 sites (all attributes and composite attributes)

# 3.6) Calculate area-weighted training data site deficits (robust + simpler)
SBC_RE1 <- SBC_RE1 %>%
  mutate(
    Preclear_ha = as.numeric(Preclear_ha),
    Remnant_ha  = as.numeric(Remnant_ha),
    
    # Required reference sites based on area (1–5 range)
    required_ref_sites = pmin(5, pmax(1, ceiling(Preclear_ha / 400))),
    
    # Deficits under different counting rules
    TD_ref_deficit                 = required_ref_sites - TD_ref_sites,
    TD_ref_supplementation_deficit = required_ref_sites - TD_supplementation_ref_sites,
    TD_ref_analogous_deficit       = required_ref_sites - TD_analogous_ref_sites,
    TD_ref_combined_deficit        = required_ref_sites - TD_combined_ref_sites
  )

# 3.7) Generate training data deficit dataframe
SBC_TD_DEFICIT <- SBC_RE1 %>%
  select(RE1, PARENT, BR_code, LZ_code, VC_code, SHORT_DESCRIPTION, DESCRIPTION, STRUCTURE_CODE, Preclear_ha_REDD, Remnant_ha_REDD, Preclear_ha, Preclear_polygons, Remnant_ha, Remnant_polygons, Difference_ha, 
         TD_ref_sites, TD_ref_deficit, TD_supplementation_ref_sites, TD_ref_supplementation_deficit, TD_analogous_ref_sites, TD_ref_analogous_deficit, TD_combined_ref_sites, 
         TD_ref_combined_deficit, EXTENT_WITHIN_PROTECTED_AREAS, PROTECTED_AREAS, HABITAT, DISTRIBUTION, PUBLIC_COMMENTS, VMA_CLASS, BIODIVERSITY_STATUS, BIODIVERSITY_STATUS_NOTES)

SBC_TD_DEFICIT$Preclear_ha <- as.numeric(as.character(SBC_TD_DEFICIT$Preclear_ha))
SBC_TD_DEFICIT$Remnant_ha  <- as.numeric(as.character(SBC_TD_DEFICIT$Remnant_ha))




