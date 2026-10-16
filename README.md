# 10-year cumulative risk of death in young people receiving special education or social care provision in England

This repository contains the R scripts used for the analyses in the study:

**Thompson-Ward IL\*, Macaulay L\*, Jay MA, Ward J, Gilbert R, Zylbersztejn A. [10-year cumulative risk of death in young people receiving special education or social care provision: a longitudinal study using linked administrative data in England.](https://ssrn.com/abstract=7209734)** Preprint (not peer reviewed).

\*Joint first authors

This code is provided to support transparency and reproducibility of the published analyses.

## Study context

We followed all 14-year-olds in state-funded schools in England in Year 9 (2009–2015) for up to 10 years, using linked education, children's social care, health and mortality records. We estimated 10-year cumulative risk of all-cause, injury-related and non-injury-related death (Kaplan-Meier) and hazard ratios (Cox models), by sex.

**Exposure groups** (recorded before the end of Year 9):

- Special educational needs and disability (SEND) provision only
- Children's social care (CSC) only (Child in Need or Looked After Child)
- Both SEND and CSC
- Neither (reference group)

**Cause-of-death classification:**

- Injury-related deaths, defined using the [Herbert et al. ICD-10 code lists for accidental and adversity-related deaths](https://github.com/UCL-CHIG/injury-related-mortality)
- Non-injury-related deaths
- Deaths with missing cause information (handled in sensitivity analyses)

## Repository contents

The analysis has two stages: (1) cohort development from linked education, social care, health and mortality data; (2) estimation of mortality risk. Scripts are numbered in the order they should be run. 

File paths should be set at the top of each script before running (`path`, `data_dir`, `master_dir`, `outputs`, `results`). 

The helper functions are in `_functions.R`

The lookup files read by the scripts (`lookups/hardelid.csv` and `lookups/icd10_ari_herbert_3char.csv`) are derived from the [Herbert et al. injury-related](https://github.com/UCL-CHIG/injury-related-mortality) and [Hardelid et al. chronic conditions](https://code.echild.ac.uk/chc_hardelid_v1) code lists, and must be placed in the `lookups/` folder of `outputs`.

**Cohort development**

- `01_create_cohort`: Year 9 school census cohort (2009–2015), alternative provision and pupil referral unit pupils, demographic covariates, and linkage to hospital records
- `02_SEN_exposures`: SEND provision and type of need before the end of Year 9
- `03_CSC_exposures`: Child in Need and Looked After Child records before the end of Year 9
- `04_chronicConditions`: history of chronic conditions from hospital records
- `05a_cohort_finalise_flow_chart`: final cohort, exposure groups and flow chart
- `05b_descriptives`: cohort characteristics by sex and exposure group
- `06_deaths_cleaning`: cleaning of ONS death records and classification of cause of death (injury, non-injury, missing)
- `07_fup_info`: follow-up time and censoring

**Mortality analyses**

- `08_all_cause_mortality`: Kaplan-Meier 10-year cumulative risk of death
- `08a_all_cause_mortality_Cox`: unadjusted and adjusted Cox models
- `08b_all_cause_mortality_Cox_PH_assumption`: proportional hazards checks
- `09_cause_specific_mortality_ALL`: cause-specific cumulative incidence (competing risks) and cause-specific Cox models
- `09a_missing_as_injury`: sensitivity analysis treating deaths with missing cause as injury-related

## Data sources and availability

Data come from the [ECHILD database](https://www.echild.ac.uk/), which links National Pupil Database, Hospital Episode Statistics, children's social care and ONS mortality records for England. The data are not publicly available because of data-sharing agreements with the Department for Education and NHS England. Accredited researchers can apply for access through the ECHILD team.

## Contact

Questions about the code or analyses: Ania Zylbersztejn (corresponding author), ania.zylbersztejn@ucl.ac.uk
