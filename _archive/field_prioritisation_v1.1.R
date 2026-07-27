###########################################################
### BioCondition Field Operations Prioritisation Tool
###########################################################
#
# Authors: Brodie Verrall, Gabrielle Lebbink
# Last updated: 2026-07-06
#
# Purpose:
# Prioritise REs for BioCondition field operations using
# regional ecosystem extent, benchmark information,
# training data availability, and representative site counts.
#
###########################################################

######### TODO: Fix where benchmark is available but still calculating deficits and prioritisation (e.g. 11.8.5)


#===============================================================================
### 0. Environment Setup
#===============================================================================
# 0.1 renv
#-------------------------------------------------------------------------------
# Activate project environment
# source("renv/activate.R")
# renv::status()

rm(list = ls())
gc()
save.image()

# 0.2 Load Required Packages
#-------------------------------------------------------------------------------
packages <- c(
  "dplyr",
  "tidyr",
  "stringr",
  "arrow",
  "sf",
  "cli",
  "lubridate"
)

installed_packages <- rownames(installed.packages())

for (pkg in packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

invisible(lapply(packages, library, character.only = TRUE))

# 0.3 Create Project Directories
#-------------------------------------------------------------------------------
current_dir <- basename(getwd())

project_root <- if (current_dir == "project_data") {
  getwd()
} else {
  file.path(getwd(), "project_data")
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

#===============================================================================
# 1. Import Source Data
#===============================================================================
# TODO:
# Verify latest RE and BM datasets before each run.

# 1.1 Import RE and BM tabular datasets
#-------------------------------------------------------------------------------
all_data   <- read.csv("inputs/bc/All_summary_data.csv")
bm_master  <- read.csv("inputs/bc/BENCHMARK_MASTER_LIST_v14.0.csv")
bm_analogous <- read.csv("inputs/bc/ranked_re_cleanlong_EDL(in).csv")
bm_reliability <- read.csv("inputs/bc/BM_automisation_data.csv") ############################### PLACEHOLDER
sbc_defs   <- read.csv("inputs/sbc/20250904_SBC_trainingData_deficit.csv")
re_v14 <- read.csv("inputs/re/regional_ecosystem.csv")

# bm_requests <- read.csv(inputs/bm/requests.csv) ############################### PLACEHOLDER

# 1.2 Import Pre-clearing Extents
#-------------------------------------------------------------------------------
re_pc_v14 <- st_read("inputs/re/preclear_v14_3577.gpkg") %>%
  select(
    OBJECTID,
    RE,
    RE1,
    PERCENT,
    PC1,
    BVG1M,
    DBVG1M,
    Shape_Length,
    Shape_Area
  ) %>%
  st_drop_geometry()

gc()

# 1.3 Import Remnant Extents
#-------------------------------------------------------------------------------
re_re_v14 <- st_read("inputs/re/remnant_v14_3577.gpkg") %>%
  select(
    OBJECTID,
    RE,
    RE1,
    PERCENT,
    PC1,
    BVG1M,
    DBVG1M,
    Shape_Length,
    Shape_Area
  ) %>%
  st_drop_geometry()

gc()

#===============================================================================
### 2. Process Regional Ecosystem Extents
#===============================================================================
# 2.1 Parse RE Metadata
#-------------------------------------------------------------------------------
re_v14 <- re_v14 %>%
  mutate(temp = NAME) %>%
  separate(
    temp,
    into = c("BR_code", "LZ_code", "VC_code"),
    sep = "\\."
  ) %>%
  mutate(temp = MAPPED_EXTENTS) %>%
  separate(
    temp,
    into = c("Preclear_ha_REDD", "Remnant_ha_REDD"),
    sep = "\\;"
  )

re_v14$Preclear_ha_REDD <-
  as.numeric(gsub("[^0-9]", "", re_v14$Preclear_ha_REDD))

re_v14$Remnant_ha_REDD <-
  as.numeric(gsub("2023|[^0-9]", "", re_v14$Remnant_ha_REDD))

# # 2.2 Filter Valid RE Variants
# #-------------------------------------------------------------------------------
# re_pc_v14 <- re_pc_v14 %>%
#   mutate(temp = RE1) %>%
#   separate(
#     temp,
#     into = c("BR_code", "LZ_code", "VC_code"),
#     sep = "\\."
#   ) #%>%
#   #filter(!is.na(LZ_code) & LZ_code != "1")
# 
# re_re_v14 <- re_re_v14 %>%
#   mutate(temp = RE1) %>%
#   separate(
#     temp,
#     into = c("BR_code", "LZ_code", "VC_code"),
#     sep = "\\."
#   ) #%>%
#   #filter(!is.na(LZ_code) & LZ_code != "1")

# 2.3 Calculate RE Extent Statistics
#-------------------------------------------------------------------------------
re_pc_area <- re_pc_v14 %>%
  group_by(RE1) %>%
  summarise(
    Preclear_polygons = n(),
    Preclear_ha = sum(Shape_Area, na.rm = TRUE) / 10000,
    .groups = "drop"
  )

re_re_area <- re_re_v14 %>%
  group_by(RE1) %>%
  summarise(
    Remnant_polygons = n(),
    Remnant_ha = sum(Shape_Area, na.rm = TRUE) / 10000,
    .groups = "drop"
  )

# 2.4 Combine Pre-clearing and Remnant Statistics
#-------------------------------------------------------------------------------
re_extent_v14 <- re_pc_area %>%
  left_join(re_re_area, by = "RE1") %>%
  mutate(
    Difference_ha = Preclear_ha - Remnant_ha,
    RE_parent = gsub("([a-zA-Z].*)$", "", RE1)
  )

#===============================================================================
### 3. Build RE Assessment Dataset
#===============================================================================
# 3.1 Create RE Assessment Table
#-------------------------------------------------------------------------------
bc_re1 <- re_extent_v14 %>%
  mutate(temp = RE1) %>%
  separate(
    temp,
    into = c("BR_code", "LZ_code", "VC_code"),
    sep = "\\."
  )

# Future join:
# td_all_ref_counts

# 3.2 Join RE Descriptions and Attributes
#-------------------------------------------------------------------------------
bc_re1 <- bc_re1 %>%
  left_join(
    re_v14 %>%
      select(
        NAME,
        PARENT,
        SHORT_DESCRIPTION,
        DESCRIPTION,
        STRUCTURE_CODE,
        BVG1M,
        DEFUNCT,
        Preclear_ha_REDD,
        Remnant_ha_REDD,
        EXTENT_WITHIN_PROTECTED_AREAS,
        PROTECTED_AREAS,
        HABITAT,
        DISTRIBUTION,
        PUBLIC_COMMENTS,
        VMA_CLASS,
        BIODIVERSITY_STATUS,
        BIODIVERSITY_STATUS_NOTES
      ),
    by = c("RE1" = "NAME")
  )

#===============================================================================
# 4. Benchmark Information
#===============================================================================
# 4.1 Select Benchmark Attributes
#-------------------------------------------------------------------------------
bm_selected <- bm_master %>%
  select(
    NAME,
    BM,
    On.web_published,
    Creation.Date,
    Revision.Date,
    Reliability.Rating,
    Analogous.RE.s.used
  )

# 4.2 Retain Most Recent Benchmark Record
#-------------------------------------------------------------------------------
bm_selected <- bm_selected %>%
  filter(trimws(NAME) != "") %>%
  mutate(
    Creation.Date = suppressWarnings(dmy(Creation.Date)),
    Revision.Date = suppressWarnings(dmy(Revision.Date)),
    latest_date = coalesce(Revision.Date, Creation.Date),
    published = On.web_published == "Y"
  ) %>%
  arrange(
    NAME,
    desc(latest_date),
    desc(published)
  ) %>%
  distinct(NAME, .keep_all = TRUE) %>%
  select(-latest_date, -published)

# 4.3 Benchmark Requests
#-------------------------------------------------------------------------------
# TODO: load benchmark requests, scale and attach to bm_selected
# bm_selected$BM_requests <- sample(0:5, nrow(bm_selected), replace = TRUE) ############# PLACEHOLDER

# 4.3 Benchmark Analogues
#-------------------------------------------------------------------------------
process_analogues <- function(bm_analogous) {
  
  bm_analogous %>%
    mutate(
      analogue_weight = case_when(
        Rank == "rank_1" ~ 8,
        Rank == "rank_2" ~ 4,
        Rank == "rank_3" ~ 2,
        Rank == "rank_4" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    group_by(re) %>%
    summarise(
      n_analogues = n(),
      n_rank1 = sum(Rank == "rank_1"),
      n_rank2 = sum(Rank == "rank_2"),
      n_rank3 = sum(Rank == "rank_3"),
      n_rank4 = sum(Rank == "rank_4"),
      
      analogue_influence = sum(analogue_weight),
      
      .groups = "drop"
    )
}

analogue_scores <- process_analogues(bm_analogous)

# 4.3 Benchmark Reliability From Automisation
#-------------------------------------------------------------------------------



#===============================================================================
### 5. Generate Site Counts
#===============================================================================
# 5.1 Calculate eligible sites from QBEIS and QBERD datasets
#-------------------------------------------------------------------------------
site_counts_all <- all_data %>%
  filter(
    (database == "QBEIS" &
       SAMPLE_FLORISTICS %in% c("A", "B", "C") &
       REPRESENTATIVE == 1) |
      (database == "QBERD" &
         grepl("reference|remnant|ref",
               site_type,
               ignore.case = TRUE)) |
      (database == "QBERD" &
         REPRESENTATIVE == 1 &
         grepl("CORVEG",
               site_type,
               ignore.case = TRUE))
  ) %>%
  select(
    RE,
    database,
    QBERD_siteid,
    QBEIS_siteid
  ) %>%
  distinct() %>%
  group_by(RE, database) %>%
  mutate(sitecount = n()) %>%
  filter(
    !RE %in% c(
      "",
      "No near match",
      "Cannot be assigned"
    )
  ) %>%
  select(RE, database, sitecount) %>%
  distinct() %>%
  pivot_wider(
    values_from = sitecount,
    names_from = database
  ) %>%
  rename(
    QBERD_rep = QBERD,
    QBEIS_rep = QBEIS
  )

# 5.2 Calculate Eligible Site Counts With Large Trees
#-------------------------------------------------------------------------------
site_counts_large_trees <- all_data %>%
  filter(
    large_tree_data == 1
  ) %>%
  filter(
    (database == "QBEIS" &
       SAMPLE_FLORISTICS %in% c("A", "B", "C") &
       REPRESENTATIVE == 1) |
      
      (database == "QBERD" &
         grepl(
           "reference|remnant|ref",
           site_type,
           ignore.case = TRUE
         )) |
      
      (database == "QBERD" &
         REPRESENTATIVE == 1 &
         grepl(
           "CORVEG",
           site_type,
           ignore.case = TRUE
         ))
  ) %>%
  select(
    RE,
    database,
    QBERD_siteid,
    QBEIS_siteid
  ) %>%
  distinct() %>%
  group_by(
    RE,
    database
  ) %>%
  mutate(
    sitecount = n()
  ) %>%
  filter(
    RE != "",
    RE != "No near match",
    RE != "Cannot be assigned"
  ) %>%
  select(
    RE,
    database,
    sitecount
  ) %>%
  distinct() %>%
  pivot_wider(
    values_from = sitecount,
    names_from = database
  ) %>%
  rename(
    QBEIS_rep_LT = QBEIS,
    QBERD_rep_LT = QBERD
  )

# 5.3 Combine Representative Site Counts
#-------------------------------------------------------------------------------
site_counts_join <- site_counts_large_trees %>%
  full_join(
    site_counts_all
  ) %>%
  rename(
    NAME = RE
  )

# 5.4 Join Site Counts and Benchmark Information
#-------------------------------------------------------------------------------
bc_re1 <- bc_re1 %>%
  left_join(
    ungroup(site_counts_join),
    by = c(
      "RE1" = "NAME"
    )
  ) %>%
  left_join(
    bm_selected,
    by = c(
      "RE1" = "NAME"
    )
  ) %>% 
  filter(
    !is.na(VC_code),
    VC_code != ""
  )

#===============================================================================
### 6. Generate Prioritisation Ranking
#===============================================================================

# factors for prioritisation ranking
# 1) preclear extent (ha)
# 2) extent decline (ha)
# 3) BM requests (#) ------------------------------------> Yet to implement
# 4) Analogue power (ranked)
# 5) SBC model deficits
# 6) BM reliability (but most likely accounted for in 1-2)

calc_benchmark_priority <- function(
    df,
    analogue_scores,
    sbc_defs,
    bm_reliability,
    benchmark_target = 3,
    
    # Need score weights
    wt_deficit = 0.175,      # benchmark site deficit
    wt_decline = 0.125,      # proportional vegetation loss
    wt_extent = 0.200,       # preclear extent
    wt_remnant = 0.050,      # remnant extent
    wt_sbc = 0.025,          # SBC modelling deficit
    wt_reliability = 0.100,  # automated benchmark reliability
    # wt_request = x.xx  # BM requests --------------------------------------------------------------> PLACEHOLDER
    # Information return weight
    wt_return = 0.25         # analogue influence
) {
  
  norm01 <- function(x) {
    
    rng <- range(x, na.rm = TRUE)
    
    if ((rng[2] - rng[1]) == 0) {
      return(rep(0, length(x)))
    }
    
    (x - rng[1]) / (rng[2] - rng[1])
    
  }
  
  # Median reliability used to fill missing values
  reliability_median <- median(
    bm_reliability$overall_reliability,
    na.rm = TRUE
  )
  
  out <- df %>%
    
    #----------------------------------------------------------
  # Join analogue influence metrics
  #----------------------------------------------------------
  left_join(
    analogue_scores,
    by = c("RE1" = "re")
  ) %>%
    
    #----------------------------------------------------------
  # Join SBC modelling deficits
  #----------------------------------------------------------
  left_join(
    sbc_defs %>%
      select(
        RE1,
        TD_ref_combined_deficit
      ),
    by = "RE1"
  ) %>%
    
    #----------------------------------------------------------
  # Join benchmark reliability
  #----------------------------------------------------------
  left_join(
    bm_reliability %>%
      select(
        RE,
        overall_reliability
      ) %>%
      distinct(),
    by = c("RE1" = "RE")
  ) %>%
    
    mutate(
      
      #--------------------------------------------------------
      # Identify non-wooded ecosystems
      #--------------------------------------------------------
      is_non_wooded = str_detect(
        STRUCTURE_CODE,
        regex(
          "grassland|herbland|forbland|bare|sedgeland",
          ignore_case = TRUE
        )
      ),
      
      #--------------------------------------------------------
      # Number of standard benchmark sites
      #--------------------------------------------------------
      standard_sites =
        coalesce(QBEIS_rep, 0) +
        coalesce(QBERD_rep, 0),
      
      #--------------------------------------------------------
      # Number of sites with large tree data
      #--------------------------------------------------------
      lt_sites =
        coalesce(QBEIS_rep_LT, 0) +
        coalesce(QBERD_rep_LT, 0),
      
      #--------------------------------------------------------
      # Benchmark sites used for benchmark generation
      #--------------------------------------------------------
      benchmark_sites = case_when(
        is_non_wooded ~ standard_sites,
        TRUE ~ lt_sites
      ),
      
      #--------------------------------------------------------
      # Benchmark deficit
      #--------------------------------------------------------
      benchmark_deficit =
        pmax(
          benchmark_target - benchmark_sites,
          0
        ),
      
      #--------------------------------------------------------
      # LT deficit
      #--------------------------------------------------------
      lt_deficit = case_when(
        is_non_wooded ~ NA_real_,
        TRUE ~ pmax(
          benchmark_target - lt_sites,
          0
        )
      ),
      
      #--------------------------------------------------------
      # SBC deficit
      #--------------------------------------------------------
      sbc_deficit =
        pmax(
          coalesce(TD_ref_combined_deficit, 0),
          0
        ),
      
      #--------------------------------------------------------
      # Benchmark reliability
      #
      # Missing values are assigned the dataset median.
      # Lower reliability will increase priority.
      #--------------------------------------------------------
      reliability_score =
        coalesce(
          overall_reliability,
          reliability_median
        ),
      
      #--------------------------------------------------------
      # Proportional decline since pre-clearing
      #--------------------------------------------------------
      decline_prop =
        if_else(
          Preclear_ha > 0,
          Difference_ha / Preclear_ha,
          0
        )
      
    ) %>%
    
    mutate(
      
      #--------------------------------------------------------
      # Initial prioritisation category
      #--------------------------------------------------------
      prioritisation_flag = case_when(
        
        BM == "Y" ~
          "Benchmark available",
        
        !is_non_wooded &
          standard_sites >= benchmark_target &
          lt_sites < benchmark_target ~
          "Supplementary LT data required",
        
        benchmark_deficit == 0 ~
          "Benchmark possible",
        
        TRUE ~
          "Data required"
        
      ),
      
      #--------------------------------------------------------
      # Append SBC gap label
      #--------------------------------------------------------
      prioritisation_flag = case_when(
        
        sbc_deficit > 0 ~
          paste0(
            prioritisation_flag,
            " - SBC gap"
          ),
        
        TRUE ~
          prioritisation_flag
        
      ),
      
      #--------------------------------------------------------
      # Scoring required?
      #--------------------------------------------------------
      score_required =
        benchmark_deficit > 0 |
        sbc_deficit > 0
      
    ) %>%
    
    mutate(
      
      analogue_influence =
        coalesce(analogue_influence, 0),
      
      #--------------------------------------------------------
      # Scale predictors
      #--------------------------------------------------------
      deficit_scaled =
        norm01(benchmark_deficit),
      
      decline_scaled =
        norm01(decline_prop),
      
      extent_scaled =
        norm01(log1p(Preclear_ha)),
      
      remnant_scaled =
        norm01(log1p(Remnant_ha)),
      
      influence_scaled =
        norm01(analogue_influence),
      
      sbc_scaled =
        norm01(sbc_deficit),
      
      reliability_scaled =
        norm01(reliability_score),
      
      #--------------------------------------------------------
      # Invert reliability so low reliability receives a
      # higher prioritisation contribution.
      #--------------------------------------------------------
      reliability_deficit_scaled =
        1 - reliability_scaled
      
    ) %>%
    
    mutate(
      
      #--------------------------------------------------------
      # Need score
      #--------------------------------------------------------
      need_score = case_when(
        
        score_required ~
          
          wt_deficit     * deficit_scaled +
          wt_decline     * decline_scaled +
          wt_extent      * extent_scaled +
          wt_remnant     * remnant_scaled +
          wt_sbc         * sbc_scaled +
          wt_reliability * reliability_deficit_scaled,
        
        TRUE ~
          NA_real_
        
      ),
      
      #--------------------------------------------------------
      # Return score
      #--------------------------------------------------------
      return_score = case_when(
        
        score_required ~
          influence_scaled,
        
        TRUE ~
          NA_real_
        
      ),
      
      #--------------------------------------------------------
      # Final priority score
      #--------------------------------------------------------
      priority_score = case_when(
        
        score_required ~
          
          need_score *
          (1 + wt_return * return_score),
        
        TRUE ~
          NA_real_
        
      )
      
    ) %>%
    
    mutate(
      
      #--------------------------------------------------------
      # Statewide rank
      #--------------------------------------------------------
      priority_rank = case_when(
        
        score_required ~
          
          rank(
            -priority_score,
            ties.method = "min"
          ),
        
        TRUE ~
          NA_real_
        
      )
      
    )
  
  #------------------------------------------------------------
  # Statewide deciles
  #------------------------------------------------------------
  out$priority_decile <- NA_integer_
  
  idx <- which(out$score_required)
  
  if (length(idx) > 0) {
    
    out$priority_decile[idx] <-
      ntile(
        -out$priority_score[idx],
        10
      )
    
  }
  
  #------------------------------------------------------------
  # Bioregional ranks and deciles
  #------------------------------------------------------------
  out <- out %>%
    
    group_by(BR_code) %>%
    
    mutate(
      
      priority_rank_BR = case_when(
        
        score_required ~
          
          rank(
            -priority_score,
            ties.method = "min"
          ),
        
        TRUE ~
          NA_real_
        
      )
      
    ) %>%
    
    group_modify(~{
      
      x <- .x
      
      x$priority_decile_BR <- NA_integer_
      
      idx <- which(x$score_required)
      
      if (length(idx) > 0) {
        
        x$priority_decile_BR[idx] <-
          ntile(
            -x$priority_score[idx],
            10
          )
        
      }
      
      x
      
    }) %>%
    
    ungroup() %>%
    
    mutate(
      
      #--------------------------------------------------------
      # Statewide tiers
      #--------------------------------------------------------
      priority_tier = case_when(
        
        priority_decile %in% c(1, 2) ~ "Very High",
        priority_decile %in% c(3, 4) ~ "High",
        priority_decile %in% c(5, 6) ~ "Medium",
        priority_decile %in% c(7, 8) ~ "Low",
        priority_decile %in% c(9, 10) ~ "Very Low",
        
        TRUE ~ NA_character_
        
      ),
      
      #--------------------------------------------------------
      # Bioregional tiers
      #--------------------------------------------------------
      priority_tier_BR = case_when(
        
        priority_decile_BR %in% c(1, 2) ~ "Very High",
        priority_decile_BR %in% c(3, 4) ~ "High",
        priority_decile_BR %in% c(5, 6) ~ "Medium",
        priority_decile_BR %in% c(7, 8) ~ "Low",
        priority_decile_BR %in% c(9, 10) ~ "Very Low",
        
        TRUE ~ NA_character_
        
      )
      
    ) %>%
    
    arrange(
      is.na(priority_rank),
      priority_rank
    )
  
  out
  
}

# 6.2 Join to working df and clean up
#-----------------------------------------------------------
bc_re1_priority <- calc_benchmark_priority(
  df = bc_re1,
  analogue_scores = analogue_scores,
  sbc_defs = sbc_defs,
  bm_reliability = bm_reliability
)

#===============================================================================
### 7. Generate Prioritisation Outputs
#===============================================================================
# 7.1 Fields to retain in outputs
#-------------------------------------------------------------------------------
priority_fields <- c(
  "RE1", "PARENT", "BM", "On.web_published", "Reliability.Rating",
  "BR_code", "LZ_code", "VC_code",
  "SHORT_DESCRIPTION", "DESCRIPTION", "STRUCTURE_CODE",
  "Preclear_ha_REDD", "Remnant_ha_REDD",
  "Preclear_ha", "Preclear_polygons",
  "Remnant_ha", "Remnant_polygons",
  "Difference_ha",
  "QBEIS_rep", "QBEIS_rep_LT",
  "QBERD_rep", "QBERD_rep_LT",
  "standard_sites", "lt_sites",
  "benchmark_sites", "benchmark_deficit",
  "lt_deficit", "sbc_deficit",
  "prioritisation_flag",
  "priority_score",
  "priority_rank",
  "priority_decile",
  "priority_tier",
  "priority_rank_BR",
  "priority_decile_BR",
  "priority_tier_BR",
  "EXTENT_WITHIN_PROTECTED_AREAS",
  "PROTECTED_AREAS",
  "HABITAT",
  "DISTRIBUTION",
  "PUBLIC_COMMENTS",
  "VMA_CLASS",
  "BIODIVERSITY_STATUS",
  "BIODIVERSITY_STATUS_NOTES"
)

bc_priority <- bc_re1_priority %>%
  select(all_of(priority_fields))

# 7.2 Export prioritisation table
#-------------------------------------------------------------------------------
outputs_folder <- "outputs"
today_date <- format(Sys.Date(), "%Y%m%d")

csv_file <- file.path(
  outputs_folder,
  paste0(today_date, "_BC_deficit.csv")
)

write.csv(
  bc_priority,
  file = csv_file,
  row.names = FALSE
)

cat("CSV file created:", csv_file, "\n")


# 7.3 Read RE spatial layers and clean geometries
#-------------------------------------------------------------------------------
re_pc_v14_sf <- st_read(
  "inputs/re/preclear_v14_3577.gpkg",
  quiet = TRUE
)

re_re_v14_sf <- st_read(
  "inputs/re/remnant_v14_3577.gpkg",
  quiet = TRUE
)

re_pc_v14_sf <- re_pc_v14_sf %>%
  st_zm(drop = TRUE) %>%
  st_cast("MULTIPOLYGON")

re_re_v14_sf <- re_re_v14_sf %>%
  st_zm(drop = TRUE) %>%
  st_cast("MULTIPOLYGON")


# 7.4 Join prioritisation attributes
#-------------------------------------------------------------------------------
re_pc_v14_sf <- re_pc_v14_sf %>%
  left_join(
    bc_priority,
    by = "RE1"
  ) %>%
  filter(
    !is.na(VC_code),
    VC_code != ""
  )

re_re_v14_sf <- re_re_v14_sf %>%
  left_join(
    bc_priority,
    by = "RE1"
  ) %>%
  filter(
    !is.na(VC_code),
    VC_code != ""
  )


# 7.5 Select output fields
#-------------------------------------------------------------------------------
spatial_fields <- c(
  "OBJECTID", "RE", "RE1",
  "PERCENT", "PC1",
  "VERSION", "DBVG1M",
  priority_fields
)

re_pc_v14_sf <- re_pc_v14_sf %>%
  select(any_of(spatial_fields))

re_re_v14_sf <- re_re_v14_sf %>%
  select(any_of(spatial_fields))


# 7.6 Write prioritised GeoPackages
#-------------------------------------------------------------------------------

gpkg_pc <- file.path(
  outputs_folder,
  paste0(today_date, "_RE_PC_v14_PRIORITISED.gpkg")
)

st_write(
  re_pc_v14_sf,
  gpkg_pc,
  delete_dsn = TRUE,
  quiet = TRUE
)

cat("GeoPackage created:", gpkg_pc, "\n")

gpkg_re <- file.path(
  outputs_folder,
  paste0(today_date, "_RE_RE_v14_PRIORITISED.gpkg")
)

st_write(
  re_re_v14_sf,
  gpkg_re,
  delete_dsn = TRUE,
  quiet = TRUE
)

cat("GeoPackage created:", gpkg_re, "\n")