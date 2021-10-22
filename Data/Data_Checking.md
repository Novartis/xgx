# Data_Checking 

## Description of Data

Simulated PK and PD data with weekly dosing, to use for data checking

## Data specifications

Column name | description
------------|------------
ID | Unique subject id (numeric)
USUBJID | Unique subject id - same as ID column
TIME | Time relative to first drug administration
NOMTIME | Nominal time
TIMEUNIT | unit of TIME
AMT | Dosing amount (for dosing events) in mg (numeric)
LIDV | Observation on a linear scale (Observation type determined by CMT), 
 | units determined by EVENTU column (numeric)
YTYPE | Type of dependent variable
      | YTYPE 0 = Dosing Event
      | YTYPE 1 = PK concentration
      | YTYPE 2 = PD response value (continuous)
ADM | Route of administration.  1 = IV, 0 for non dosing records
CMT | Compartment number (determines observation type) (integer)
   | CMT 0 = Dosing event
   | CMT 1 = PK concentration
   | CMT 2 = PD response value (continuous)
NAME | description of event
EVENTU  | unit for observation
UNIT | unit for observation (same as EVENTU)
MDV | missing dependent variable (0 = not missing, 1 = missing)
CENS | censored values (0 = not censored, 1 = censored) (integer)
EVID | event ID  (0 = observation, 1 = dosing event) (integer)
AGEB | baseline age (years)
AGE0 | age at start of dosing (years)
WEIGHTB | baseline bodyweight (kg)
WEIGHT0 | weight at start of dosing
SEXN | sex numeric: 1 = male, 2 = female
SEX | sex character: "Male" or "Female"
TRTN | treatment numeric (mg)
TRT | treatment character
PROFDAY | Day of profile
VISNAME | Cycle and Day number of the visit
CYCLE | Starts from 1.  Every 4 weeks is a new cycle (increment by 1)
LLOQ | lower limit of quantification of the assay
