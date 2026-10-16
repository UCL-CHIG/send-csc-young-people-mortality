# 10-year cumulative risk of death in young people receiving special education or social care provision in England

This repository contains the R scripts used to generate the analyses reported in:

**Thompson-Ward IL\*, Macaulay L\*, Jay MA, Ward J, Gilbert R, Zylbersztejn A. [10-year cumulative risk of death in young people receiving special education or social care provision: a longitudinal study using linked administrative data in England.](https://ssrn.com/abstract=7209734)** SSRN preprint (not peer reviewed).

\*Joint first authors

This code is provided to support transparency and reproducibility of the published analyses.

## Study context

This study aimed to examine all-cause and cause-specific mortality over 10 years of follow-up among young people in England who received special educational needs and disability (SEND) provision and/or children's social care (CSC) support, using a national cohort constructed from linked education, social care, health and mortality records.

**Cohort and follow-up:**
- All 14-year-olds in state-funded schools in England in Year 9 (2009–2015)
- Followed for up to 10 years, with 10-year cumulative risk estimated using Kaplan-Meier methods and hazard ratios from Cox models, by sex

**Exposure groups** (recorded before the end of Year 9):
- SEND provision only
- CSC only (Child in Need or Looked After Child)
- Both SEND and CSC
- Neither (reference group)

**Cause-of-death classification:**  
Deaths were grouped as:

- **Injury-related deaths**  
  Including accidental injuries and adversity-related injuries. Herbert et al. ICD-10 code lists used to classify injury-related causes can be found [here](https://github.com/UCL-CHIG/injury-related-mortality)

- **Non-injury-related deaths**  
  All other causes of death with a recorded cause

- **Deaths with missing cause information**  
  Deaths with no recorded cause of death (examined in a sensitivity analysis)

## Repository contents

The code reflects a two-stage workflow:
1. Development of a national cohort of young people in England using linked education, social care, health and mortality records.
2. Estimation of all-cause and cause-specific mortality risk over 10 years of follow-up among young people with and without SEND and/or CSC support.

Scripts are in `r_scripts/`, are numbered in the order they should be run, and use the helper functions in `_functions.R`.

**Cohort development (scripts `01` to `07`)**

Scripts construct the cohort used in all subsequent analyses.

- `01_create_cohort`: Year 9 school census cohort (2009–2015), alternative provision and pupil referral unit pupils, demographic covariates, and linkage to hospital records
- `02_SEN_exposures`: SEND provision and type of need before the end of Year 9
- `03_CSC_exposures`: Child in Need and Looked After Child records before the end of Year 9
- `04_chronicConditions`: history of chronic conditions from hospital records. Hardelid et al. ICD-10 code lists used to define chronic conditions can be found [here](https://code.echild.ac.uk/chc_hardelid_v1)

- `05a_cohort_finalise_flow_chart`: final cohort, exposure groups and flow chart
- `05b_descriptives`: cohort characteristics by sex and exposure group
- `06_deaths_cleaning`: cleaning of ONS death records and classification of cause of death
- `07_fup_info`: follow-up time and censoring

**Mortality analyses (scripts `08` and `09`)**

Scripts implement the mortality analyses described in the paper.

- `08_all_cause_mortality`: Kaplan-Meier 10-year cumulative risk of death
- `08a_all_cause_mortality_Cox`: unadjusted and adjusted Cox models
- `08b_all_cause_mortality_Cox_PH_assumption`: assessment of proportional hazards assumptions
- `09_cause_specific_mortality_ALL`: cause-specific cumulative incidence (competing risks) and cause-specific Cox models
- `09a_missing_as_injury`: sensitivity analysis treating deaths with missing cause as injury-related

**Running the code**

File paths and database connections have been removed; set the paths (`path`, `data_dir`, `master_dir`, `outputs`, `results`) at the top of each script before running. The scripts read two lookup files from a `lookups/` folder within `outputs`: `hardelid.csv` (chronic conditions code list) and `icd10_ari_herbert_3char.csv` (injury-related code list), both derived from the code lists linked above.

## Data sources and availability

We used linked education, social care, health and mortality records for young people in England from the Education and Child Health Insights from Linked Data (ECHILD) database.
ECHILD data are not shared publicly in line with data-sharing agreements with NHS Digital and the Department for Education. ECHILD can be accessed by accredited researchers through application via the ECHILD team ([www.echild.ac.uk](https://www.echild.ac.uk/)) and the ONS Research Accreditation Panel.

## Licence

This code is released under the [MIT Licence](LICENSE).

## Contact

For questions related to this code or the analyses, please contact Ania Zylbersztejn (corresponding author) ania.zylbersztejn@ucl.ac.uk.
