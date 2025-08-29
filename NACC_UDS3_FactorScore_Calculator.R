################################################################################
# NACC UDS-3 Factor Score Calculator
#
# Author: Emily F. Matusz, University of Florida (2025)
# Version: NACC dataset release "nacc70" (June 2025)
#
# Description:
#   This script calculates composite factor scores for the UDS-3
#   neuropsychological battery (Processing Speed/Executive, Attention,
#   Language, Memory) using Blom-transformed scores and standardized
#   factor loadings. Both raw composites and CN-referenced z-scores are
#   generated, along with “clean” versions requiring ≥2 contributing tests.
#
# CN group definition:
#   Cognitively Normal (CN) participants were identified as:
#     - NACCUDSD == 1
#     - CDRGLOB == 0
#     - No biomarker positivity:
#         AMYLPET != 1
#         AMYLCSF != 1
#         TAUPETAD != 1
#         CSFTAU != 1
#
# Interpretation of z-scores:
#   - Z-scores are standardized relative to the CN anchor group above.
#   - A z-score of 0 represents the average performance for CN, biomarker-
#     negative, clinically normal older adults.
#   - Positive values = better-than-average performance relative to CN.
#   - Negative values = worse-than-average performance relative to CN.
#
# File paths:
#   Replace "filepath" placeholders with your own local directory paths
#   when reading or writing files.
#
# Output:
#   - Composite raw scores: UDS3_SPD_EXEC, UDS3_ATTN, UDS3_LANG, UDS3_MEM
#   - CN-referenced z-scores: UDS3_*_Z
#   - Clean versions requiring ≥2 indicators: UDS3_*_cln and UDS3_*_Z_cln
#
################################################################################


# -----------------------------------------------------------------------------
# Set up & read crosswalks
# -----------------------------------------------------------------------------

# Install/load packages as needed
# install.packages(c("readxl", "readr", "dplyr", "tidyverse"))
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)

# >>>> EDIT THIS: set your working directory if desired (optional) <<<<
# Example: setwd("~/projects/uds3")
# If you don't want to set a working directory, you can keep this line commented.
# setwd("<<YOUR_WORKING_DIRECTORY>>")

#### Read in Normalizing Crosswalks
# >>>> EDIT THIS: put the path to your Blom crosswalk .xlsx file <<<<
# Example: "data/NACC_UDS3_FactorScore_Calculator.xlsx"
Blom <- read_excel("<<PATH_TO_CROSSWALK_XLSX>>/NACC_UDS3_FactorScore_Calculator.xlsx",
                   sheet = "Blom Crosswalks")

# Clean up crosswalk column names and drop header rows per the original spreadsheet layout
colnames(Blom) = paste0(rep(as.character(na.omit(t(Blom[1,]))[,1]), each = 2), c("_r","_t"))
Blom = Blom[3:nrow(Blom),]
Blom = as.data.frame(Blom)

# Turn each variable numeric and round to 2 decimals
for (i in 1:ncol(Blom)) {
  Blom[, i] = as.numeric(Blom[, i])
  Blom[, i] = round(Blom[, i], 2)
}

# -----------------------------------------------------------------------------
# Read NACC dataset
# -----------------------------------------------------------------------------

# >>>> EDIT THIS: put the path/filename to your NACC CSV <<<<
# Example: "data/investigator_nacc70_June2025.csv"
NACC <- read_csv("<<PATH_TO_NACC_CSV>>/investigator_nacc70_June2025.csv")
# If you see parsing warnings, run: problems(NACC)

# -----------------------------------------------------------------------------
# Pull the variables needed for composites and clean their missing codes
# -----------------------------------------------------------------------------

#### Use Crosswalks to Create Normalized Scores
temp = NACC %>%
  filter(is.na(NACCVNUM) == FALSE) %>%
  select(NACCID, NACCVNUM,
         TRAILALI, TRAILA, TRAILBLI, TRAILB, UDSVERTN,
         DIGFORSL, DIGBACLS, UDSBENTC,
         ANIMALS, VEG, MINTTOTS,
         CRAFTVRS, CRAFTDVR, UDSBENTD)

# Clean up Missing Values (per NACC dictionary conventions used here)
temp$TRAILALI[which(temp$TRAILALI == -4 | temp$TRAILALI > 94)] = NA
temp$TRAILA[which( temp$TRAILA   == -4 |  temp$TRAILA   > 994)] = NA
temp$TRAILBLI[which(temp$TRAILBLI == -4 | temp$TRAILBLI > 94)] = NA
temp$TRAILB[which( temp$TRAILB   == -4 |  temp$TRAILB   > 994)] = NA
temp$UDSVERTN[which(temp$UDSVERTN == -4 | temp$UDSVERTN > 94)] = NA

temp$DIGFORSL[which(temp$DIGFORSL == -4 | temp$DIGFORSL > 94)] = NA
temp$DIGBACLS[which(temp$DIGBACLS == -4 | temp$DIGBACLS > 94)] = NA
temp$UDSBENTC[which(temp$UDSBENTC == -4 | temp$UDSBENTC > 94)] = NA

temp$ANIMALS[which(temp$ANIMALS == -4 | temp$ANIMALS > 94)] = NA
temp$VEG[which(    temp$VEG     == -4 |     temp$VEG   > 94)] = NA
temp$MINTTOTS[which(temp$MINTTOTS == -4 | temp$MINTTOTS > 94)] = NA

temp$CRAFTVRS[which(temp$CRAFTVRS == -4 | temp$CRAFTVRS > 94)] = NA
temp$CRAFTDVR[which(temp$CRAFTDVR == -4 | temp$CRAFTDVR > 94)] = NA
temp$UDSBENTD[which(temp$UDSBENTD == -4 | temp$UDSBENTD > 94)] = NA

# New derived variables used in the speed/executive domain
temp$TRAILArate = temp$TRAILALI / temp$TRAILA
temp$TRAILBrate = temp$TRAILBLI / temp$TRAILB

# Round to 2 decimals to match Blom crosswalk grid
for (i in 3:ncol(temp)) {
  temp[, i] = round(temp[, i], 2)
}

# -----------------------------------------------------------------------------
# Merge in normalized (Blom) scores using the crosswalks
# -----------------------------------------------------------------------------
# Note: *_r columns are raw grid points; *_t are transformed (Blommed) values

# Exec Func
temp = merge(temp, na.omit(Blom[, c("TRAILArate_r","TRAILArate_t")]),
             by.x = "TRAILArate", by.y = "TRAILArate_r", all.x = TRUE, all.y = FALSE)

temp = merge(temp, na.omit(Blom[, c("TRAILBrate_r","TRAILBrate_t")]),
             by.x = "TRAILBrate", by.y = "TRAILBrate_r", all.x = TRUE, all.y = FALSE)

temp = merge(temp, na.omit(Blom[, c("UDSVERTN_r","UDSVERTN_t")]),
             by.x = "UDSVERTN", by.y = "UDSVERTN_r", all.x = TRUE, all.y = FALSE)

# Attn
temp = merge(temp, na.omit(Blom[, c("DIGFORSL_r","DIGFORSL_t")]),
             by.x = "DIGFORSL", by.y = "DIGFORSL_r", all.x = TRUE, all.y = FALSE)

temp = merge(temp, na.omit(Blom[, c("DIGBACLS_r","DIGBACLS_t")]),
             by.x = "DIGBACLS", by.y = "DIGBACLS_r", all.x = TRUE, all.y = FALSE)

temp = merge(temp, na.omit(Blom[, c("UDSBENTC_r","UDSBENTC_t")]),
             by.x = "UDSBENTC", by.y = "UDSBENTC_r", all.x = TRUE, all.y = FALSE)

# Lang
temp = merge(temp, na.omit(Blom[, c("ANIMALS_r","ANIMALS_t")]),
             by.x = "ANIMALS", by.y = "ANIMALS_r", all.x = TRUE, all.y = FALSE)

temp = merge(temp, na.omit(Blom[, c("VEG_r","VEG_t")]),
             by.x = "VEG", by.y = "VEG_r", all.x = TRUE, all.y = FALSE)

temp = merge(temp, na.omit(Blom[, c("MINTTOTS_r","MINTTOTS_t")]),
             by.x = "MINTTOTS", by.y = "MINTTOTS_r", all.x = TRUE, all.y = FALSE)

# Mem
temp = merge(temp, na.omit(Blom[, c("CRAFTVRS_r","CRAFTVRS_t")]),
             by.x = "CRAFTVRS", by.y = "CRAFTVRS_r", all.x = TRUE, all.y = FALSE)

temp = merge(temp, na.omit(Blom[, c("CRAFTDVR_r","CRAFTDVR_t")]),
             by.x = "CRAFTDVR", by.y = "CRAFTDVR_r", all.x = TRUE, all.y = FALSE)

temp = merge(temp, na.omit(Blom[, c("UDSBENTD_r","UDSBENTD_t")]),
             by.x = "UDSBENTD", by.y = "UDSBENTD_r", all.x = TRUE, all.y = FALSE)

# -----------------------------------------------------------------------------
# Turn Blommed scores into Z (within-test) then apply standardized loadings
# to form domain "shared variance" pieces (per your model)
# -----------------------------------------------------------------------------

# Exec
temp$TRAILArate_shared_var = ((temp$TRAILArate_t - 0) / 1)   * 0.689202273846894
temp$TRAILBrate_shared_var = ((temp$TRAILBrate_t - 0) / 1)   * 0.793124227673078
temp$UDSVERTN_shared_var   = ((temp$UDSVERTN_t   - 0) / 1)   * 0.704195103084655

# Attn
temp$DIGFORSL_shared_var   = ((temp$DIGFORSL_t   - 0) / .95) * 0.640820299669356
temp$DIGBACLS_shared_var   = ((temp$DIGBACLS_t   - 0) / .95) * 0.819908114098237
temp$UDSBENTC_shared_var   = ((temp$UDSBENTC_t + .02) / .93) * 0.448374016249853

# Lang
temp$ANIMALS_shared_var    = ((temp$ANIMALS_t    - 0) / 1)   * 0.8704139818872
temp$VEG_shared_var        = ((temp$VEG_t        - 0) / .99) * 0.806456809344006
temp$MINTTOTS_shared_var   = ((temp$MINTTOTS_t + .02) / .94) * 0.673388004122083

# Mem
temp$CRAFTVRS_shared_var   = ((temp$CRAFTVRS_t   - 0) / .99) * 0.780202688127002
temp$CRAFTDVR_shared_var   = ((temp$CRAFTDVR_t - .01) / .97) * 0.811285152988147
temp$UDSBENTD_shared_var   = ((temp$UDSBENTD_t - .01) / .96) * 0.737144924554598

# -----------------------------------------------------------------------------
# Sum shared variance pieces to create domain composites
# -----------------------------------------------------------------------------

temp$UDS3_SPD_EXEC = rowSums(temp[, c("TRAILArate_shared_var","TRAILBrate_shared_var","UDSVERTN_shared_var")], na.rm = TRUE)
temp$UDS3_SPD_EXEC[which(rowSums(is.na(temp[, c("TRAILArate_shared_var","TRAILBrate_shared_var","UDSVERTN_shared_var")])) == 3)] = NA

temp$UDS3_ATTN     = rowSums(temp[, c("DIGFORSL_shared_var","DIGBACLS_shared_var","UDSBENTC_shared_var")], na.rm = TRUE)
temp$UDS3_ATTN[which(rowSums(is.na(temp[, c("DIGFORSL_shared_var","DIGBACLS_shared_var","UDSBENTC_shared_var")])) == 3)] = NA

temp$UDS3_LANG     = rowSums(temp[, c("ANIMALS_shared_var","VEG_shared_var","MINTTOTS_shared_var")], na.rm = TRUE)
temp$UDS3_LANG[which(rowSums(is.na(temp[, c("ANIMALS_shared_var","VEG_shared_var","MINTTOTS_shared_var")])) == 3)] = NA

temp$UDS3_MEM      = rowSums(temp[, c("CRAFTVRS_shared_var","CRAFTDVR_shared_var","UDSBENTD_shared_var")], na.rm = TRUE)
temp$UDS3_MEM[which(rowSums(is.na(temp[, c("CRAFTVRS_shared_var","CRAFTDVR_shared_var","UDSBENTD_shared_var")])) == 3)] = NA

# Keep only essentials for merge back
temp = select(temp, NACCID, NACCVNUM, TRAILArate, TRAILBrate, UDS3_SPD_EXEC, UDS3_ATTN, UDS3_LANG, UDS3_MEM)

# Merge composites back into full NACC and drop temp
NACC = merge(NACC, temp, by = c("NACCID","NACCVNUM"), all = TRUE)
rm(temp)

# Quick availability checks (optional QA)
colSums(is.na(NACC[, c("UDS3_SPD_EXEC", "UDS3_ATTN", "UDS3_LANG", "UDS3_MEM")]))
sum(!complete.cases(NACC[, c("UDS3_SPD_EXEC", "UDS3_ATTN", "UDS3_LANG", "UDS3_MEM")]))
colSums(!is.na(NACC[, c("UDS3_SPD_EXEC", "UDS3_ATTN", "UDS3_LANG", "UDS3_MEM")]))

# -----------------------------------------------------------------------------
# CN-referenced Z scores (anchors below are the CN means/SDs)
# -----------------------------------------------------------------------------
# Note: If you publish or share anchors elsewhere, keep these synced.

cn_means <- c(speed_executive = 0.510257918658155,
              attention       = 0.284125837550167,
              language        = 0.516029189328131,
              memory          = 0.470883050072358)

cn_sds <- c(speed_executive = 0.583395593740101,
            attention       = 0.42770429865994,
            language        = 0.568282636806374,
            memory          = 0.466114609259999)

NACC$UDS3_SPD_EXEC_Z <- (NACC$UDS3_SPD_EXEC - cn_means["speed_executive"]) / cn_sds["speed_executive"]
NACC$UDS3_ATTN_Z     <- (NACC$UDS3_ATTN     - cn_means["attention"])        / cn_sds["attention"]
NACC$UDS3_LANG_Z     <- (NACC$UDS3_LANG     - cn_means["language"])         / cn_sds["language"]
NACC$UDS3_MEM_Z      <- (NACC$UDS3_MEM      - cn_means["memory"])           / cn_sds["memory"]

# -----------------------------------------------------------------------------
# Cleaned composites (require ≥2 contributing indicators)
# -----------------------------------------------------------------------------
# Logic: count how many indicators are non-missing in each domain using raw fields
# present in NACC; keep composite only if count >= 2. Creates *_cln (raw) and *_Z_cln.

NACC <- NACC %>%
  mutate(
    spd_exec_n = rowSums(!is.na(cbind(TRAILArate, TRAILBrate, UDSVERTN))),
    attn_n     = rowSums(!is.na(cbind(DIGFORSL, DIGBACLS, UDSBENTC))),
    lang_n     = rowSums(!is.na(cbind(ANIMALS, VEG, MINTTOTS))),
    mem_n      = rowSums(!is.na(cbind(CRAFTVRS, CRAFTDVR, UDSBENTD))),
    
    # cleaned RAW composites
    UDS3_SPD_EXEC_cln = ifelse(spd_exec_n >= 2, UDS3_SPD_EXEC, NA_real_),
    UDS3_ATTN_cln     = ifelse(attn_n     >= 2, UDS3_ATTN,     NA_real_),
    UDS3_LANG_cln     = ifelse(lang_n     >= 2, UDS3_LANG,     NA_real_),
    UDS3_MEM_cln      = ifelse(mem_n      >= 2, UDS3_MEM,      NA_real_),
    
    # cleaned Z composites
    UDS3_SPD_EXEC_Z_cln = ifelse(spd_exec_n >= 2, UDS3_SPD_EXEC_Z, NA_real_),
    UDS3_ATTN_Z_cln     = ifelse(attn_n     >= 2, UDS3_ATTN_Z,     NA_real_),
    UDS3_LANG_Z_cln     = ifelse(lang_n     >= 2, UDS3_LANG_Z,     NA_real_),
    UDS3_MEM_Z_cln      = ifelse(mem_n      >= 2, UDS3_MEM_Z,      NA_real_)
  )

# Optional QA checks for cleaned counts
colSums(!is.na(NACC[, c("UDS3_SPD_EXEC_Z_cln","UDS3_ATTN_Z_cln","UDS3_LANG_Z_cln","UDS3_MEM_Z_cln")]))
colSums(is.na( NACC[, c("UDS3_SPD_EXEC_Z_cln","UDS3_ATTN_Z_cln","UDS3_LANG_Z_cln","UDS3_MEM_Z_cln")] ))
table(NACC$spd_exec_n, useNA = "ifany"); table(NACC$attn_n, useNA = "ifany")

# Raw vs Z cleaned counts should match exactly
sum(!is.na(NACC$UDS3_SPD_EXEC_cln)) == sum(!is.na(NACC$UDS3_SPD_EXEC_Z_cln))
sum(!is.na(NACC$UDS3_ATTN_cln))     == sum(!is.na(NACC$UDS3_ATTN_Z_cln))
sum(!is.na(NACC$UDS3_LANG_cln))     == sum(!is.na(NACC$UDS3_LANG_Z_cln))
sum(!is.na(NACC$UDS3_MEM_cln))      == sum(!is.na(NACC$UDS3_MEM_Z_cln))

# -----------------------------------------------------------------------------
# (Optional) Recode raw test fields per dictionary for downstream analyses
# -----------------------------------------------------------------------------
NACC <- NACC %>%
  mutate(
    TRAILALI = ifelse(TRAILALI == -4 | TRAILALI > 94,  NA, TRAILALI),
    TRAILA   = ifelse(TRAILA   == -4 | TRAILA   > 994, NA, TRAILA),
    TRAILBLI = ifelse(TRAILBLI == -4 | TRAILBLI > 94,  NA, TRAILBLI),
    TRAILB   = ifelse(TRAILB   == -4 | TRAILB   > 994, NA, TRAILB),
    UDSVERTN = ifelse(UDSVERTN == -4 | UDSVERTN > 94,  NA, UDSVERTN),
    
    DIGFORSL = ifelse(DIGFORSL == -4 | DIGFORSL > 94,  NA, DIGFORSL),
    DIGBACLS = ifelse(DIGBACLS == -4 | DIGBACLS > 94,  NA, DIGBACLS),
    UDSBENTC = ifelse(UDSBENTC == -4 | UDSBENTC > 94,  NA, UDSBENTC),
    
    ANIMALS  = ifelse(ANIMALS  == -4 | ANIMALS  > 94,  NA, ANIMALS),
    VEG      = ifelse(VEG      == -4 | VEG      > 94,  NA, VEG),
    MINTTOTS = ifelse(MINTTOTS == -4 | MINTTOTS > 94,  NA, MINTTOTS),
    
    CRAFTVRS = ifelse(CRAFTVRS == -4 | CRAFTVRS > 94,  NA, CRAFTVRS),
    CRAFTDVR = ifelse(CRAFTDVR == -4 | CRAFTDVR > 94,  NA, CRAFTDVR),
    UDSBENTD = ifelse(UDSBENTD == -4 | UDSBENTD > 94,  NA, UDSBENTD)
  )

# -----------------------------------------------------------------------------
# Save outputs
# -----------------------------------------------------------------------------

# >>>> EDIT THIS: choose where to save your outputs <<<<
# Examples:
#   "outputs/NACC_final_factor_scores.csv"
#   "outputs/NACC_final_factor_scores.rds"

write.csv(NACC, file = "<<OUTPUT_DIR>>/NACC_final_factor_scores.csv", row.names = FALSE)
saveRDS(NACC,       file = "<<OUTPUT_DIR>>/NACC_final_factor_scores.rds")



# Compare original vs cleaned counts for composites
compare_counts <- data.frame(
  Domain = c("Speed/Exec", "Attention", "Language", "Memory"),
  
  # Original = number of non-missing in uncleaned z-scores
  Original = c(
    sum(!is.na(NACC$UDS3_SPD_EXEC_Z)),
    sum(!is.na(NACC$UDS3_ATTN_Z)),
    sum(!is.na(NACC$UDS3_LANG_Z)),
    sum(!is.na(NACC$UDS3_MEM_Z))
  ),
  
  # Cleaned = number of non-missing in cleaned z-scores
  Cleaned = c(
    sum(!is.na(NACC$UDS3_SPD_EXEC_Z_cln)),
    sum(!is.na(NACC$UDS3_ATTN_Z_cln)),
    sum(!is.na(NACC$UDS3_LANG_Z_cln)),
    sum(!is.na(NACC$UDS3_MEM_Z_cln))
  )
)

# Calculate how many were dropped by the ≥2 indicator cleaning
compare_counts$Dropped_by_cleaning <- compare_counts$Original - compare_counts$Cleaned

# Print results
print(compare_counts)

