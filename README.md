# CAPTND Read Me

## CAPTND Set-Up and Reporting

The Child, Adolescent, Psychological Therapies National Dataset (CAPTND) is a new patient-level dataset designed to record the activity of outpatient mental health services in NHS Health Boards across Scotland. It is intended to provide granular detail on each stage of the patient journey from referral to treatment and discharge from the service, as well as key waiting time measures, and patient demographics. When all required data items are available, CAPTND will be a powerful tool for improving service planning and user experiences. 
This R project includes all stages of the analytical processes relating to CAPTND: loading data from the Oracle database, summarising initial data quality issues, optimising the data by joining linked records and completing missing data, reporting on usability of optimised data, and preparing data to use in PHS publications.

## How to install and run the project

On the CAPTND GitHub code page, click on the down arrow on the green ‘Code’ button, then copy the HTTPS URL for the project. Then in Posit Workbench  go to File > New Project > Version control > Git then paste the URL into the box labelled ‘Repository URL’. Next, indicate where you would like to create the project by clicking ‘Browse’ and then navigate to the ‘scripts’ folder in ‘CAPTND_shorewise’ (the full file path can be shared upon request), and then create a folder and name this after yourself. Click on your named folder and hit ‘Choose’. This should clone the project to your chosen directory. 
Please note that the code in the CAPTND project will only work for those with access to MentalHealth5 on PHS’s confi stats drive. 

## How to use the project

Make sure to open the project file and pull the latest version from ‘main’ via Git/GitHub each time you want to work on CAPTND.  A Git command cheatsheet can be found here.  
All required functions and packages are loaded on start-up via the .Rprofile file (this will require continual maintenance as the project develops).  

## Script Layout

### Control Scripts
Specific tasks have been written as functions whenever possible to make the code neat and easier to maintain. Key stages of CAPTND analysis are managed by ‘control’ scripts which organise the running order of our project functions. The key control scripts are:

-control_globalscape.R – this pulls and prepares the CAPTND data that was initially submitted to PHS via Globalscape, however as this submission method is obsolete this data will not be updated and hence this should never need rerun.
-control_swift.R – this pulls and prepares the CAPTND data that is submitted to PHS. This needs to be run each month after all Health Boards have submitted their data (the deadline is 24th of each month). 
-control_outputs.R – this loads the optimised CAPTND dataset and produces data tables of key metrics required for CAPTND reporting. 

### Function Folders
The functions for the project are saved in various folders depending on their purpose:

•	01_control – key functions to run batches of specific scripts

•	02_setup – functions to establish key elements of CAPTND analyses

•	03_globalscape_prep – functions to prepare and save old Globalscape data (seldom run as Globalscape records are no longer submitted – only updated if there are changers to setup)

•	04_check_modify – functions for evaluating fields, adding new fields, and optimising pre-existing fields

•	05_data_quality – functions to create data quality summaries

•	06_calculations – functions to generate key metrics and comparisons 

•	07_publication – functions to generate publication figures

•	08_investigations – functions for investigating potential data issues

•	09_ideas_space – experimental functions and rough drafts (must be kept tidy)

### Searching for Specific Functions
The large number of functions in different folders can become a little confusing, however a search function has been developed to pull the file path of a function that contains the keyword provided. Just use `search_for_file(‘keyword’)` to return all relevant file paths. 

## Programming Approach

Column names have been defined within objects so that all column names are defined within one script (./setup/new_colnames.R) and, if necessary, can be easily modified in the future without having to update all scripts that use that column. This approach required the use of non-standard evaluation  techniques such as `!!sym()` to call the object strings as symbols and `:=` (a.k.a. walrus case).  

## Authors

Joana Bittencourt Silvestre
Charlie Smith
Maria Gannon
