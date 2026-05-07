# CAHOOTS
Spring 2026 at the Rohlfslab, University of Oregon

# Introduction

Research project on Mobile Crisis Services in Eugene OR, focusing on call outcome and MCS LC roll out.

# How to 

# Data

## Raw data

- SPD Call for Services, 2015-2025 : .xlsx document, one sheet per year
- SPD Responding Units, 2015-2025 : .xlsx document, one sheet per year
- SPD Call Outcome, 2015-2025 : .xlsx doxument, key codes and one sheet per year

- MCS LC data, 2024/08/18 - 2025/12/17 : .xlsx document, one sheet

- Eugene CAD data, 2015-2025 : one .csv per year

## Clean data

# Structure

C:.     \
├───biblio  \
├───data    \
│   ├───clean   \
│   ├───intermediate    \
│   ├───processed   \
│   │   └───samples \
│   └───raw     \
│       └───Eugene_CAD_data_noloc   \
├───draft   \
├───figures     \
├───report      \
├───results     \
└───scripts     \
    ├───1_convert_to_csv    \
    ├───2_cleaning      \
    ├───3_mapping       \
    ├───4_analysis      \
    └───5_modeling      \

# Script

## 1. Prep and merge data

Harmonize data types, merge excel sheets and convert to RDS file for cleaning.

## 2. Rough cleaning and CAHOOTS id

Rename columns for easier comparison, chose variables and convert column types.
Identify CAHOOTS calls in SPD and CAD Data

### CAHOOTS call identification CAD

To identify the agency handling each call, we created two binary columns, one for EPD, one for CAHOOTS. We assigned calls to CAHOOTS and tried identifying their unit id in `prime_unit`:
- `agency` set to "CAHE" is a CAHOOTS call.
- `service` set to "OTHR" is a CAHOOTS call.
- We used string detection to filter `nature` or `closed_as` containing "CAHOOTS" to identify units, and when in doubt, checked for the agency. If it was EPD but multiple units were dispatched, we assumed CAHOOTS were also dispatched but not as the prime unit.

We found a total of $54,546$ CAHOOTS only calls, $121,091$ CAHOOTS and EPD calls, and $1,270,414$ EPD only calls for a total of $13.81\%$ of CAHOOTS dispatches for 2015-2025 in Eugene.

### CAHOOTS call identification SPD



## 3. Mapping Call Outcomes

Identify patterns and map data to categorize call outcomes


### Categorization of call outcome

We categorize call outcome into four aggregated categories for SPD and Eugene CAD data:

- Arrests
- Medical
- Support
- Other 

**TO DO:**
- chose category for each call outcome for both datasets 

## 4. Statistical Analysis and Visualisations

**TO DO:**
- chose columns
- calculate columns of interest

### I/ Anticipated visualisations

- Call outcome proportions per agency
- Monthly arrest rate evolution for Welfare Checks in Eugene VS Springfield


## 5. Modeling (BONUS)

Prediction of outcome probability depending on call caracteristics

### Expected results 

- Measure if MCS LC compensate CAHOOTS's discontinuation in Eugene (Gap analysis Nathan)
- Effect on arrestations of CAHOOTS's stopping and MCS LC's roll out
- MCS LC's performance 


# DRAFT

- descriptive analysis of data
- trend analysis in call outcome for CAHOOTS, CAHOOTS+MCSLC, MCSLC
- Identification of best predictor for call outcome when MCS LC is dispatched
- estimate impact of MCS LC on call outcome

Anticipated results plotting:

i/ Proportion of each call outcome category when MCS LC is dispatched or not on a police intervention, and CAHOOTS

ii/ Plot trend, call outcome evolution in time with and without MCS LC, with and without CAHOOTS


add renv
add instructions to run code





