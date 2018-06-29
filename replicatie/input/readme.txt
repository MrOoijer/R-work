The Third National Health and Nutrition Examination Survey, (NHANES III),
1988-94 Series 11 No. 1A
The Third National Health and Nutrition Examination Survey (NHANES III),
1988-94, contains data for 33,994 persons ages 2 months and older who
participated in the survey.  The data and corresponding documentation for the
survey interview and examination components are found in five separate files: 
the NHANES III Household Adult Data File, NHANES III Household Youth Data File,
NHANES III Examination Data File, NHANES III Laboratory Data File, and the
NHANES III Dietary Recall Data Files.  See the documentation on for a complete description of these files.
This data contains a complete set of the five NHANES III data files and
documentation, as well as the necessary SAS code to create SAS data sets, in
ASCII format. The ADULT, YOUTH, EXAM, and LAB data each contain 3 files.  The file with the extension .DAT is the data in ASCII format. The file with the extension .DOC is the documentation for that file in ASCII format.  The documentation files are very large.  Please import them into a
word processor, using COURIER font and setting margins to zero to view the
documentation files.  The file with the .SAS extension is SAS code to create
a SAS data set for the ASCII file.
The FOODS data contains three data files and four look-up tables.  The
data files are CFF.DAT (Combination Foods File), VIF.DAT (Variable Ingredient
File), and IFF.DAT (Individual Foods File).  The corresponding documentation
files are CFF.DOC, VIF.DOC, and IFF.DOC respectively.  The four look-up
tables are IDCODE.DAT, CODEBOOK.DAT, BRANDS.DAT, and PREPD.DAT.  Documentation
for these look-up files is included in the IFF.DOC.  All foods files and
look-up tables have SAS source code for creating SAS data sets from the
ASCII versions of the files.  The SAS source code for these files ends in
the .SAS extension.
A separate CD-ROM contains data and documentation accessible through the
Statistical Export and Tabulation System (SETS) retrieval software for all
files, except for the NHANES III Dietary Recall Data Files.  
Background information on the procedures, survey components, questionnaires,
examination and laboratory methods, and statistical analysis guidelines is
available on The NHANES III Reference Manuals and Reports.  All
data users are strongly encouraged to review these reference materials
and reports before analyzing NHANES III data.
  
Guidelines for Data Users
o   NHANES III survey design and demographic variables are found on the
    Household Adult Data File, Household Youth Data File, the Laboratory
    Data File and the Examination Data File.  In preparing a data set
    for analysis, other data files should be merged with either or both
    of the Adult Household Data File or the Youth Household Data File
    to obtain many important analytic variables.
o    All of the NHANES III public use data files are linked with the common
     survey participant identification number (SEQN).  Merging information
     from multiple NHANES III data files using this variable ensures that
     the appropriate information for each survey participant is linked
     correctly.
o    NHANES III public use data files do not have the same number of records
     on each file.  The Household Questionnaire Files (divided into two files,
     Adult and Youth) contain more records than the Examination Data File
     because not everyone who was interviewed completed the examination. The
     Laboratory Data File contains data only for persons aged one year and
     older.  The Individual Foods Data File based on the dietary recall has
     multiple records for each person rather than the one record per sample
     person contained in the other data files.
o    For each data file, SAS program code with standard variable names and
          labels is provided as separate text files that contains the data files.       This SAS program code can be used to create a SAS data set from the data      file.
o    Modifications were made to items in the questionnaires, laboratory, and
     examination components over the course of the survey; as a result, data
     may not be available for certain variables for the full six years.  In
     addition, variables may differ by phase since some changes were
     implemented between phases.  Users are encouraged to read the Notes
     sections of the file documentation carefully for information about
     changes.
o    Extremely high and low values have been verified whenever possible, and
     numerous consistency checks have been performed.  Nonetheless, users
     should examine the range and frequency of values before analyzing data.
o    Some data were not ready for release at the time of this publication
     due to continued processing of the data or analysis of laboratory
     specimens.  A listing of those data are available in the general
     information section of each data file.
o    Confidential and administrative data are not being released to the
     public.  Additionally, some variables have been recoded to help 
     protect the confidentiality of the survey participants.  For example,
     all age-related variables were recoded to 90+ years for persons who
     were 90 years of age and older.
o    Some variable names may differ from those used in the Phase 1
     NHANES III Provisional Data Release and some variables included in the
     Phase 1 provisional release may not appear on these files.
o    Although the data files have been edited carefully, errors may be
     detected.  Please notify NCHS staff (301-458-4636) of any errors
     in the data file or the documentation or refer to the NCHS website
     at www.cdc.gov/nchswww/nchshome.htm for updates to these data files.
Analytic Considerations
o    NHANES III (1988-94) was designed so that the survey's first three years,
     1988-91, its last three years, 1991-94, and the entire six years were
     national probability samples.  Analysts are encouraged to use all six
     years of survey results.
o    Sample weights are available for analyzing NHANES III data.  One of the
     following three sample weights will be appropriate for nearly all
     analyses:  interviewed sample final weight (WTPFQX6), examined sample
     final weight (WTPFEX6), and mobile examination center (MEC)- and
     home-examined sample final weight (WTPFHX6).  Choosing which of these
     sample weights to use in any analysis depends on the variables being
     used.  A good rule of thumb is to use "the least common denominator"
     approach.  In this approach, the user checks the variables of
     interest.  The variable that was collected on the smallest number of
     persons is the "least common denominator," and the sample weight that
     applies to that variable is the appropriate one to use for that
     analysis.  For more detailed information, see the Analytic and 
     Reporting Guidelines for NHANES III (U.S. DHHS, 1996).
Referencing or Citing NHANES III Data
o    In publications, please acknowledge NCHS as the original data source.  For
     instance, the reference for the NHANES III Laboratory Data File is:  U.S.
     Department of Health and Human Services (DHHS).  National Center for
     Health Statistics.  Third National Health and Nutrition Examination
     Survey, 1988-1994, NHANES III Laboratory Data File.  Public Use
     Data File Documentation Number 76200.  Hyattsville, MD: Centers
     for Disease Control and Prevention, 1996.
