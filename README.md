# NACC-UDS3-FactorScore-Calculator

R code and crosswalks to calculate composite factor scores from the NACC UDS-3 neuropsychological battery (June 2025 data freeze).  
Generates CN-referenced z-scores and cleaned versions requiring ≥2 contributing indicators.

## Contents
- `NACC_UDS3_FactorScore_Calculator.R` – R script to generate factor scores  
- `NACC_UDS3_FactorScore_Calculator.xlsx` – Blom crosswalk lookup table  
- `LICENSE` – MIT license  

## Requirements
- R (≥ 4.0)
- Packages: `readxl`, `readr`, `dplyr`, `tidyverse`

## Usage
1. Clone or download this repository.  
2. Update file paths in the script:
   - Replace `<<PATH_TO_CROSSWALK_XLSX>>` with your local path to the Excel file  
   - Replace `<<PATH_TO_NACC_CSV>>` with your local path to the NACC dataset  
3. Run `NACC_UDS3_FactorScore_Calculator.R` in R.  

## Output
- Composite raw scores: `UDS3_SPD_EXEC`, `UDS3_ATTN`, `UDS3_LANG`, `UDS3_MEM`  
- CN-referenced z-scores: `UDS3_*_Z`  
- Cleaned versions requiring ≥2 indicators: `UDS3_*_cln` and `UDS3_*_Z_cln`  

## CN Anchor Group
Z-scores are standardized relative to a Cognitively Normal group, defined as:  
- `NACCUDSD == 1`  
- `CDRGLOB == 0`  
- Biomarker negative (`AMYLPET != 1`, `AMYLCSF != 1`, `TAUPETAD != 1`, `CSFTAU != 1`)  

## Citation
If you use this code, please cite the tool and paper outlining the methodology:  
- Matusz, E.F. (2025). NACC UDS-3 Factor Score Calculator. GitHub repository.
- Matusz, E.F., Fiala, J., Kiselica, A., Rosselli, M., Armstrong, M., Holgerson, A., Levy, S., Arias, F., Velez Uribe, I., Duara, R., Curiel Cid, R., Loewenstein, D., Smith, G., Marsiske, M., & Asken, B. (Pending Peer Review). Cognitive factor structure of the NACC UDS-3 neuropsychological battery across ethno-racial, linguistic, and cognitive status groups. 

## License
MIT License. See LICENSE for details.
