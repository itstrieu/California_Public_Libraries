Combine Data Files
================
Kathy Trieu

12/2023

### R Setup

``` r
rm(list = ls()); gc()
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 458221 24.5     987080 52.8   644245 34.5
    ## Vcells 818314  6.3    8388608 64.0  1635327 12.5

``` r
pacman::p_load(tidyverse)
```

### Combine Data Files

``` r
paths = dir("data/raw/",pattern="All-",full.names=TRUE)
names(paths) = basename(paths)
```

``` r
dfList = lapply(paths,read.csv)
df     = do.call(plyr::rbind.fill,dfList)
```

``` r
names(df) = names(df) %>% 
  str_replace_all(c("X."="Num",
                    "[.]"=""))
```

### Combine Features

#### that are the same but named differently

``` r
df = df %>%
  unite("NumofElectronicBooks",c("NumofElectronicBooks","NumofElectronicBooksinCollection","TotalElectronicBooks")
        ,na.rm=TRUE,sep="/",remove=TRUE) %>%
  unite("NumofElectronicCollections", c("NumofElectronicCollections","NumofElectronicCollectionsinCollection")
        ,na.rm=TRUE,sep="/",remove=TRUE) %>%
  unite("NumofChildrensPrograms", c("NumofChildrensPrograms","NumofChildrensProgramscalculated","ofChildrensProgramscalculated")
        ,na.rm=TRUE,sep="/",remove=TRUE) %>%
  unite("TotalProgramAttendance", c("TotalProgramAttendance", "ProgramAttendance")
        ,na.rm=TRUE,sep="/",remove=TRUE) %>%
  unite("NumofOutlets", c("NumofOutlets","TotalofOutlets")
        ,na.rm=TRUE,sep="/",remove=TRUE)

df = df %>%
  unite("NumofPhysicalAudioMaterials", c("NumofPhysicalAudioMaterials","NumofPhysicalAudioMaterialsinCollection")
        ,na.rm=TRUE,sep="/",remove=TRUE) %>%
  unite("NumofPhysicalVideoMaterials", c("NumofPhysicalVideoMaterials","NumofPhysicalVideoMaterialsinCollection")
        ,na.rm=TRUE,sep="/",remove=TRUE) %>%
  unite("NumofDownloadableAudioMaterials", c("NumofDownloadableAudioMaterials","NumofDownloadableAudioMaterialsinCollection")
        ,na.rm=TRUE,sep="/",remove=TRUE) %>%
    unite("NumofDownloadableVideoMaterials", c("NumofDownloadableVideoMaterials","NumofDownloadableVideoMaterialsinCollection")
          ,na.rm=TRUE,sep="/",remove=TRUE) %>%
  unite("Doyouchargelatefines", c("Doyouchargelatefines","Doyouchargeanypatronslatefinesforphysicalmaterials")
        ,na.rm=TRUE,sep="/",remove=TRUE)

naCountAfter = sort(colSums(is.na(df))) %>% as.data.frame(.)
```
