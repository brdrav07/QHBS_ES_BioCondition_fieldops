#read in data
BM_priority <- read.csv("C:00_raw_data/Benchmark_Prioritisation_20251110_all.csv")
V14 <- read.csv("C:00_raw_data/regional_ecosystemV14.csv")
V14_ha<-read.csv("C:00_raw_data/RE_hectares.csv")
Alldata<-read.csv("C:00_raw_data/All_summary_data.csv")
Bm_Mast_14<-read.csv("C:00_raw_data/BENCHMARK_MASTER_LIST_v14.0.csv")

#renv::init()
#renv::snapshot()
#renv::restore()
#renv::activate()

#PURPOSE
#Create BM_priority shapefile and new rank based on survey need
#BM_priority top 50 of each bioregion
library(dplyr)
library(stringr)
library(sf)
library(cli)
library(tidyr)


#Merging V14 and BM info
V14sel<-V14%>%
  select("NAME","MAPPED","DEFUNCT","CHANGED_TO","STRUCTURE_CODE","BVG1M","SHORT_DESCRIPTION")

V14_hasel<-V14_ha%>%
  select(RE,ha_RE_PreClearing,ha_RE_2023)%>%
  rename(REM_AREA_HA = ha_RE_2023, PRE_AREA_HA = ha_RE_PreClearing, NAME = RE)

BMsel<-Bm_Mast_14%>%
  select(NAME,BM,On.web_published,Creation.Date,Revision.Date,Reliability.Rating,Analogous.RE.s.used)

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



#trim to top 20 in each bioregion
BM_prior_top20 <- BM_prior_ranked %>%
  # Group by that leading number
  group_by(Bioregion) %>%
  # Take the top 20 highest RANK_PRE values within each group
  slice_min(order_by = RANK_surveyneed, n = 20, with_ties = FALSE) %>%
  ungroup()%>%
  group_by (Bioregion)%>%
  mutate(count = n ())


#Path to your geodatabase and feature class name
gdb_path <- "00_raw_data/RE_V14.gdb"
st_layers(gdb_path)
layer_name <- "qld_reg_eco_remnant_2021"   # <-- change to your layer name in the GDB

#Read the polygon layer from the GDB
shp <- st_read(dsn = gdb_path, layer = layer_name)

#Ensure join keys match in case/whitespace
BM_prior_top20 <- BM_prior_top20 %>%
  rename(RE1 = NAME)

shp <- shp %>%
  mutate(across(starts_with("NAME"), ~ trimws(tolower(.))))

# Start progress bar
shp_long <- shp %>%
  pivot_longer(
    cols = c(RE1, RE2, RE3, RE4),
    names_to = "RE_field",
    values_to = "RE_code",
    values_drop_na = TRUE
  )

shp_filtered <- shp_long %>%
  filter(
    !is.na(RE_code),                            # drop NAs
    RE_code != "",                              # drop blank strings
    RE_code %in% BM_prior_top20$RE1             # keep only matches
  )


#Join BM_prior_top50 repeatedly by RE1-4
shp_joined <- shp_filtered %>%
  # join by RE1
  left_join(BM_prior_top20, by = c("RE_code" = "RE1"))

shp_joined1<-shp_joined%>%
  select(RE,RE_field,RE_code,everything())%>%
  select(-RE5)%>%
  rename(RE_priority = RE_code, RE_priority_dom = RE_field, BVG1M = BVG1M.x)%>%
  select(-BVG1M.y)

#write to gpkg
st_write(
  shp_joined1,
  dsn = "BM_priority.gpkg",
  layer = "BM_priority_top20_surveyranked",
  delete_layer = TRUE
)