# Oncology_Efficacy_Data 

## Description of Data

Oncology efficacy data from merged RECIST (Response Evaluation Criteria in Solid Tumor) data with modeling and dosing history datasets.   

## Data specifications


Column name  |  Description
------------- | -------------
IDSHORT  | Subject ID
BOR  | Best Overall Response
BPCHG  | Best Percent Change from Baseline 
OR  | Overall Response
BORNUM  | Best Overall Response Value
psld  | Percent Change in Sum of Longest Diameters
DOSE_ABC  | Dose of Agent 1 (numeric)
DOSE_DEF  | Dose of Agent 2 (numeric)
DOSE_combo  | ARM of the study (character)
binary_BOR  | Binary Best Overall Response  
PR_rate  |  Partial Response rate
n  |   The number of total subjects in each dose group
count_cr_pr  |  Count the number of CR or PR subjects in each dose group
TIME | Evaluation time 
COMB | Combo status
TIME_OR | Overall Response evaluation time 
auc0_24 | AUC0-24