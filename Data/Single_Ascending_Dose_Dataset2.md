# Single_Ascending_Dose_Dataset2 

## Description of Data

Model generated PK data to mimic an orally administered small molecule. Simulated single dose administration ranging from 100 mg to 1600 mg. 

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
NAME | description of event
EVENTU  | unit for observation
CENS | censored values (0 = not censored, 1 = censored) (integer)
EVID | event ID  (0 = observation, 1 = dosing event) (integer)
WEIGHTB | baseline bodyweight (kg)
SEX | sex
TRTACT | Treatment group label (string)
DOSE | randomized dose in mg (numeric)

