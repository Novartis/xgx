# Multiple_Ascending_Dose_Dataset2 

## Description of Data

Model generated PK and PD data to mimic an orally administered small molecule with various endpoints from continuous to ordinal response and count data. Simulated multiple dose administration ranging from 100 mg to 1600 mg, once per day. 

## Data specifications

Column name | description
------------|------------
ID | Unique subject id (numeric)
TIME | Time relative to first drug administration
NOMTIME | Nominal time
TIMEUNIT | unit of TIME
AMT | Dosing amount (for dosing events) in mg (numeric)
LIDV | Observation on a linear scale (Observation type determined by CMT), 
 | units determined by EVENTU column (numeric)
CMT | Compartment number (determines observation type) (integer)
   | CMT 1 = Dosing event
   | CMT 2 = PK concentration
   | CMT 3 = Continuous response data
   | CMT 4 = Count response data
   | CMT 5 = Ordinal response data
   | CMT 6 = Binary response data
NAME | description of event
EVENTU  | unit for observation
CENS | censored values (0 = not censored, 1 = censored) (integer)
EVID | event ID  (0 = observation, 1 = dosing event) (integer)
WEIGHTB | baseline bodyweight (kg)
SEX | sex
TRTACT | Treatment group label (string)
DOSE | randomized dose in mg (numeric)
PROFDAY | Day of profile
PROFTIME | Time within PROFDAY
CYCLE | count of drug administrations received
