Data Pre-Processing
================
Kathy Trieu

12/2023

### R Setup

``` r
rm(list = ls()); gc()
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 458193 24.5     987000 52.8   644245 34.5
    ## Vcells 818394  6.3    8388608 64.0  1635327 12.5

``` r
pacman::p_load(tidyverse)
```

### Load Data

``` r
df = read.csv('data/CPL_Combined_Data.csv')
```

### Missing Values Management

#### Removed all features with 80% or more missing values

``` r
naCount = sort(colSums(is.na(df))) %>% as.data.frame(.)

missingSupervisor = df %>%
  filter(., is.na(LibraryVisits))

df = df %>%
  mutate(naProp = (apply ( X = is.na(df), MARGIN = 1, FUN = mean ) )) %>%
  filter(naProp < .2) %>%
  select(-naProp) %>%
  filter(!is.na(LibraryVisits)) 
```

#### Corrected Column Data Types

``` r
dfFixedTypes = df %>% 
  mutate(across((starts_with("Numof") | 
                   starts_with("Total") |
                   contains("Attendance") |
                   contains("Loans") |
                   contains("Books") |
                   contains("Hours") |
                   contains("Visits") |
                   contains("Reference") |
                   contains("Circulation") | 
                   contains("Population") |
                   contains("Users") |
                   contains("Uses") |
                   contains("Children") |
                   contains("Expenditures") |
                   contains("Income")
                   ), ~gsub("\\,", "", .))) %>%
  mutate(across((contains("Expenditures") | contains("Income")), ~gsub("\\$", "", .))) %>%
  mutate(CIPACompliant = ifelse(CIPACompliant == "No",0,1)) %>%
  apply(.,2, function(x) str_replace_all(string=x, pattern=" ", repl="")) %>% as.data.frame(.)

locationID = dfFixedTypes$Location
year = dfFixedTypes$FiscalYear

dfQuant = dfFixedTypes %>%
  select(., -FSCSID) %>%
  select(., PopulationofTheLegalServiceArea:NumofChildrensPrograms) %>%
  sapply(., as.numeric) %>% as.data.frame(.) %>%
  mutate(Location = locationID,
         Year = year) %>%
  select(Location, Year, everything())
```

    ## Warning in lapply(X = X, FUN = FUN, ...): NAs introduced by coercion

    ## Warning in lapply(X = X, FUN = FUN, ...): NAs introduced by coercion

    ## Warning in lapply(X = X, FUN = FUN, ...): NAs introduced by coercion

    ## Warning in lapply(X = X, FUN = FUN, ...): NAs introduced by coercion

#### Median Imputed Missing Data in Quantitative Features

``` r
dfQuant[dfQuant == "-1"] <- NA
dfQuant[dfQuant == "-2"] <- NA

dfQuantImputed = dfQuant %>%
  caret::preProcess(method="medianImpute") %>%
  predict(newdata = dfQuant)
```

#### Checked for Features Without Meaningful Variance

``` r
caret::nearZeroVar(dfQuantImputed, saveMetrics=TRUE)
```

    ##                                        freqRatio percentUnique zeroVar   nzv
    ## Location                                1.000000    17.0309654   FALSE FALSE
    ## Year                                    1.000000     0.5464481   FALSE FALSE
    ## PopulationofTheLegalServiceArea         1.500000    99.3624772   FALSE FALSE
    ## RegisteredUsersasofJune30               2.000000    99.3624772   FALSE FALSE
    ## ChildrenBorrowers                       9.000000    95.7194900   FALSE FALSE
    ## NumofCentralLibraries                   8.150000     0.1821494   FALSE FALSE
    ## NumofBranchLibraries                    2.966667     3.0054645   FALSE FALSE
    ## NumofBookmobiles                        5.147929     0.4553734   FALSE FALSE
    ## NumofOutlets                            2.202312     3.6429872   FALSE FALSE
    ## LibraryVisits                           6.444444    93.7158470   FALSE FALSE
    ## HoursOpenAllOutlets                     3.333333    68.8524590   FALSE FALSE
    ## Totalpersonsemployed                    1.000000    25.0455373   FALSE FALSE
    ## NumofLibrarianFTEs                      2.293103    28.2331512   FALSE FALSE
    ## FTEAllotherpaidstaff                    1.173913    52.8233151   FALSE FALSE
    ## NumofALAMLSLibrarianFTEs                2.169231    26.3205829   FALSE FALSE
    ## TotalOperatingIncome                    2.000000    99.6357013   FALSE FALSE
    ## LocalGovernmentIncome                   2.000000    99.3624772   FALSE FALSE
    ## StateIncome                            30.500000    71.1293260   FALSE FALSE
    ## FederalIncome                          49.230769    35.8834244   FALSE FALSE
    ## CapitalOutlayIncomefromLocalSources   110.285714    26.6848816   FALSE FALSE
    ## CapitalOutlayIncomefromStateSources   526.000000     4.0072860   FALSE  TRUE
    ## CapitalOutlayIncomefromFederalSources 532.500000     2.7322404   FALSE  TRUE
    ## CapitalOutlayIncomefromOtherSources   193.400000    11.2021858   FALSE FALSE
    ## TotalCapitalOutlayIncome              137.200000    35.5191257   FALSE FALSE
    ## OtherOperatingIncome                   27.000000    81.8761384   FALSE FALSE
    ## TotalOperatingExpenditures              2.000000    99.9089253   FALSE FALSE
    ## TotalCollectionExpenditures             1.500000    99.3624772   FALSE FALSE
    ## PrintMaterialsExpenditures              1.250000    96.6302368   FALSE FALSE
    ## PrintSerialSubscriptionExpenditures     8.500000    84.6994536   FALSE FALSE
    ## ElectronicMaterialsExpenditures         7.800000    92.2586521   FALSE FALSE
    ## OtherMaterialsExpenditures             25.000000    79.2349727   FALSE FALSE
    ## TotalPrintMaterialsExpenditures         1.250000    97.6320583   FALSE FALSE
    ## SalaryWagesExpenditures                11.500000    97.8142077   FALSE FALSE
    ## EmployeeBenefitsExpenditures            4.666667    96.8123862   FALSE FALSE
    ## TotalStaffExpenditures                 11.500000    97.6320583   FALSE FALSE
    ## TotalCapitalExpenditures              305.500000    44.2622951   FALSE FALSE
    ## AllOtherOperatingExpenditures           2.500000    98.6338798   FALSE FALSE
    ## BooksChildrenHeld                       2.000000    99.1803279   FALSE FALSE
    ## BooksYoungAdultHeld                     2.000000    95.8105647   FALSE FALSE
    ## NumofPhysicalAudioMaterials             1.333333    96.8123862   FALSE FALSE
    ## NumofPhysicalVideoMaterials             2.000000    97.8142077   FALSE FALSE
    ## NumofCurrentSerialSubscriptions         1.590909    42.3497268   FALSE FALSE
    ## NumofElectronicBooks                    6.571429    92.0765027   FALSE FALSE
    ## NumofElectronicCollections              1.114286     9.5628415   FALSE FALSE
    ## TotalPrintMaterialsHeld                 2.000000    99.4535519   FALSE FALSE
    ## NumofDownloadableAudioMaterials         4.818182    88.5245902   FALSE FALSE
    ## NumofDownloadableVideoMaterials        23.666667    54.2805100   FALSE FALSE
    ## CirculationofNonEnglishMaterials        3.818182    92.6229508   FALSE FALSE
    ## CirculationofElectronicMaterials        1.000000    93.2604736   FALSE FALSE
    ## ILLloanstoothers                       18.692308    65.0273224   FALSE FALSE
    ## ILLloansreceived                       15.250000    62.7504554   FALSE FALSE
    ## ReferenceQuestions                     41.333333    87.8870674   FALSE FALSE
    ## NumofPrograms                           1.500000    77.5956284   FALSE FALSE
    ## NumofYoungAdultPrograms                 4.142857    30.9653916   FALSE FALSE
    ## YoungAdultProgramAttendance             8.571429    71.3114754   FALSE FALSE
    ## ChildrensProgramAttendance              1.200000    96.3570128   FALSE FALSE
    ## TotalProgramAttendance                  1.833333    97.0856102   FALSE FALSE
    ## NumofAdultPrograms                      2.583333    48.4517304   FALSE FALSE
    ## AdultProgramAttendance                  3.400000    89.2531876   FALSE FALSE
    ## CIPACompliant                           1.351178     0.1821494   FALSE FALSE
    ## AnnualUsesofPublicInternetComputers     2.941176    92.8051002   FALSE FALSE
    ## NumofInternetTerminals                  1.040000    21.4936248   FALSE FALSE
    ## Websitevisits                          58.000000    78.5063752   FALSE FALSE
    ## NumofChildrensPrograms                  1.375000    65.3005464   FALSE FALSE

``` r
dfQuantImputed = dfQuantImputed %>% 
  select(-CapitalOutlayIncomefromStateSources,
         -CapitalOutlayIncomefromFederalSources)
```
