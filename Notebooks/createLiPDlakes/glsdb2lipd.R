#Load packages----
library(dplyr)
library(ggplot2)
library(lipdR)
library(readxl)
library(stringr)
#Function to convert age14C to age
library(oxcAAR)
quickSetupOxcal(path = "~/OxCal")
getCalMedian <- function(c14,unc=200){
  calout <- oxcAAR::oxcalCalibrate(c14,unc,)
  cp <- cumsum(calout$`1`$raw_probabilities$probabilities)
  cp <- cp/max(cp)
  calAge <- geoChronR::convertAD2BP(calout$`1`$raw_probabilities$dates[which(abs(cp-.5)==min(abs(cp-.5)))])
  return(calAge)
}
dir <- '/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/revisions/'
count<-list()
#USSR ----
#Load data
coding <- read_excel(file.path(dir,'Tarasov_GLSDB_USSR','rusdata.xls'))
calages <- c()
for (age in seq(0,18000,500)){ calages <- c(calages,getCalMedian(age))}
#Create LiPD files for USSR from Tarasov_GLSDB_USSR
count[['USSR']]<-0
for (l in 1:nrow(coding)){
  lake <- coding[l,]
  L <- list()
  L$lipdVersion <- 1.3
  L$datasetId <- paste0("glsdb",suppressWarnings(lipdverseR::createDatasetId()))
  L$createdBy <- "github.com/clhancock/DAMP21ka/Notebooks/createLiPDlakes/glsdb2lipd.R"
  L$originalDataUrl <- "https://doi.org/10.25921/49ag-ck94"
  L$archiveType <- ifelse((grepl('peat',lake$Type) | grepl('mire',lake$Type)),"Peat","LakeSediment")
  #Sitename
  sitename <- lake$Basin %>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("[^A-Za-z0-9]")
  #Publications
  pubs <- strsplit(lake$`Primary References`,';')[[1]]
  publist <- vector(mode='list',length= length(pubs))
  for (p in 1:length(pubs)){
    ref <- (strsplit(pubs[[p]],'et al.')[[1]])
    ref <- unlist(strsplit(ref,' '))
    ref <- ref[which(ref %in% c('','p.c.','&','and','in','press.','b','c','d','e')== F)]
    if(ref[1] == 'de'){
      ref2 <- c(paste0(ref[1],ref[2]))
      if (length(ref) >= 3){for (i in 3:length(ref)){ref2<-c(ref2,ref[[i]])}}
      ref <- ref2
    }
    publist[[p]] <- list()
    publist[[p]]$citation <- pubs[[p]]
    publist[[p]]$author <- list()
    if (!is.na(as.numeric(substr(ref[length(ref)],1,4)))){
      publist[[p]]$year <- ref[length(ref)]
      for (r in 1:(length(ref)-1)){
        publist[[p]]$author[[r]] <- ref[[r]]
      }
    }else{
      publist[[p]]$year <- 1111
      for (r in 1:(length(ref))){
        publist[[p]]$author[[r]] <- ref[[r]]
      }
    }
  }
  #add compilation
  p<-length(publist)+1
  publist[[p]] <- list()
  publist[[p]]$citation <- 'Tarasov et al., 1996'
  publist[[p]]$year <- 1996
  publist[[p]]$title <- 'Lake status records from the Former Soviet Union and Mongolia: documentation of the second version of the database'
  publist[[p]]$journal <- 'Paleoclimatology Publications Series Report #5'
  publist[[p]]$author <- list('Tarasov, P.E.', 'Harrison, S.P.', 'Saarse, L.', 'Pushenko, M.Ya.', 'Andreev, A.A.', 'Aleshinskaya, Z.V.', 'Davydova, N.N.', 'Dorofeyuk, N.I.', 'Efremov, Yu.V.', 'Khomutova, V.I.', 'Sevastyanov, D.V.', 'Tamosaitis, J.', 'Uspenskaya, O.N.', 'Yakushko, O.F.', 'Tarasova, I.V', 'Ya, M.', 'Elina, G.A.', 'Elovicheva, Ya.K.', 'Filimonova, L.V.', 'Gunova, V.S.', 'Kvavadze, E.V.', 'Nuestrueva, I.Yu.', 'Pisareva, V.V.', 'Shelekhova, T.S.', 'Subetto, D.A.', 'Zernitskaya, V.P.')
  L$pub <- publist
  # Add Name
  L$dataSetName <- paste(sitename,
                         publist[[1]]$author[[1]]%>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("[^A-Za-z0-9]"),
                         publist[[1]]$year,sep = ".")
  print(paste(l,'-',L$dataSetName))
  # Geo metadata
  geo <- list()
  geo$latitude <- lake$Latitude
  geo$longitude <- lake$Longitude
  geo$elevation <- lake$Elevation
  geo$geology <- lake$Geology
  geo$area <- paste(ifelse(!is.na(lake$`Lake Area`),lake$`Lake Area`,lake$`Mire Area`),'ha')
  geo$basinSize <- ifelse(!is.na(lake$`Mean Basin`),paste(lake$`Mean Basin`,'ha'),NA)
  geo$siteName <- sitename
  geo$location <- lake$`Country, Region`
  L$geo <- geo
  #Paleodata
  vals <- data.frame(age14C=seq(0,18000,500),age=calages,lakeLevel=NA,original=as.character(lake[which(names(lake)=='0'):which(names(lake)=='18')]))
  #Convert values from string to numeric
  for (v in 1:nrow(vals)){
    if(is.na(vals$original[v])){vals$lakeLevel[v] <- NA
    }else if(vals$original[v] == 'NA'){vals$lakeLevel[v] <- NA
    }else if(grepl('/',vals$original[v])){
      vals$lakeLevel[v] <- median(as.numeric(strsplit(vals$original[v],'/')[[1]]))
    }else{vals$lakeLevel[v]<-as.numeric(vals$original[v])}
  }
  pmt<- vector(mode = "list",length = 1)
  for (i in 1:3){
    pmt[[1]][[i]]<-list()
    pmt[[1]][[i]]$number <- i
    pmt[[1]][[i]]$TSid <- lipdR::createTSid("glsdb_")
    pmt[[1]][[i]]$variableName <- names(vals)[i]
    pmt[[1]][[i]]$values <- vals[,i]
    if(i == 1){
      pmt[[1]][[i]]$units <- 'yr 14C BP'
    }else if(i==2){
      pmt[[1]][[i]]$units <- 'yr BP'
      pmt[[1]][[i]]$primaryAgeColumn <- TRUE
    }else if(i==3){
      pmt[[1]][[i]]$units <- 'unitless'
      pmt[[1]][[i]]$primaryTimeseries <- TRUE
      pmt[[1]][[i]]$variableType <- 'lakeLevel@surface'
      pmt[[1]][[i]]$description <- paste('data sources:',names(lake[which(lake == 'yes')]),collapse = '; ')
      pmt[[1]][[i]]$proxy <- 'lake level'
      pmt[[1]][[i]]$proxyDetail <- 'lake level'
      pmt[[1]][[i]]$proxyGeneral <- 'stratigraphy'
      #interp
      pmt[[1]][[i]]$interpretation <- vector(mode = "list",length = 1)
      pmt[[1]][[i]]$interpretation[[1]]$scope <- "climate"
      pmt[[1]][[i]]$interpretation[[1]]$variable <- 'effectivePrecipitation'
      pmt[[1]][[i]]$interpretation[[1]]$variableDetail <- 'lakeLevel@surface'
      pmt[[1]][[i]]$interpretation[[1]]$seasonality <- 'Annual'
      pmt[[1]][[i]]$interpretation[[1]]$direction <- 'positive'
    }
  }
  L$paleoData <- vector(mode = "list",length = 1)
  L$paleoData[[1]]$measurementTable <- pmt
  #Save USSR
  L <- lipdverseR::initializeChangelog(L,notes= 'Created from De Tarasov et al. (1996) by CH July 2024')
  vals<-vals%>%filter(!is.na(lakeLevel))%>%filter(age<21000)
  if(nrow(vals)>=8 &
     length(unique(vals$lakeLevel))>=3 &
     nrow(vals%>%filter(age<12000))>=6 & 
     diff(range(vals$age)) > 10000 &
     !is.na(lake$Lithol.) &
     grepl('outflow',lake$Type)==F
     ){
    writeLipd(L,file.path(dir,'proxies','GLSDB','use'))
    count[['USSR']] <- count[['USSR']]+1
  }else{
     writeLipd(L,file.path(dir,'proxies','GLSDB','no'))
  }
}


#Europe ----

#Load data
coding <- read_excel(file.path(dir,'Yu_GLSDB_EUR','eudata.xls')) %>%
  rename(Basin='Basin...1', 'Country, Region'=Country, Latitude=Lat., Longitude=Long., Elevation=Elev.,
         'Lake Area' = Lake, 'Mire Area' = Mire, 'Mean Basin'=Basin...9)
calages <- c()
for (age in seq(0,30000,500)){ calages <- c(calages,getCalMedian(age))}

#Create LiPD files for Europe from Yu_GLSDB_EUR
count[['EU']]<-0
for (l in 1:nrow(coding)){
  lake <- coding[l,]
  L <- list()
  L$lipdVersion <- 1.3
  L$datasetId <- paste0("glsdb",suppressWarnings(lipdverseR::createDatasetId()))
  L$createdBy <- "github.com/clhancock/DAMP21ka/Notebooks/createLiPDlakes/glsdb2lipd.R"
  L$originalDataUrl <- "https://doi.org/10.25921/p5aq-0931"
  L$archiveType <- ifelse(lake$Type=='Lake',"LakeSediment","Peat") #CHANGE
  #Sitename
  sitename <- lake$Basin %>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("[^A-Za-z0-9]")
  #Publications
  pubs <- strsplit(lake$`Primary References`,';')[[1]]
  publist <- vector(mode='list',length= length(pubs))
  for (p in 1:length(pubs)){
    ref <- (strsplit(pubs[[p]],'et al.')[[1]])
    ref <- unlist(strsplit(ref,' '))
    ref <- unlist(strsplit(ref,','))
    ref <- ref[which(ref %in% c('','p.c.','&','and','in','press.','b','c','d','e')== F)]
    if(ref[1] == 'de'){
      ref2 <- c(paste0(ref[1],ref[2]))
      if (length(ref) >= 3){for (i in 3:length(ref)){ref2<-c(ref2,ref[[i]])}}
      ref <- ref2
    }
    publist[[p]] <- list()
    publist[[p]]$citation <- pubs[[p]]
    publist[[p]]$author <- list()
    if (!is.na(as.numeric(substr(ref[length(ref)],1,4)))){
      for (r in 1:(length(ref))){
        if(is.na(as.numeric(substr(ref[r],1,4)))){
          publist[[p]]$author[[r]] <- ref[[r]]
        }else{
          if (r==1){
            if (p >1){publist[[p]]$author<-publist[[p-1]]$author
            }else{publist[[p]]$author[[r]] <- 'noauthor'}
          }
          publist[[p]]$year <- ref[[r]]
        }
      }
    }else{
      publist[[p]]$year <- 1111
      for (r in 1:(length(ref))){
        publist[[p]]$author[[r]] <- ref[[r]]
      }
    }
  }
  #add compilation
  p<-length(publist)+1
  publist[[p]] <- list()
  publist[[p]]$citation <- 'Yu (1995)'
  publist[[p]]$year <- 1995
  publist[[p]]$title <- 'Lake status records from Europe: Data base documentation'
  publist[[p]]$journal <- 'Publications Series Report #3'
  publist[[p]]$author <- list('Yu, G.')
  L$pub <- publist
  # Add Name
  L$dataSetName <- paste(sitename,
                         publist[[1]]$author[[1]]%>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("[^A-Za-z0-9]"),
                         publist[[1]]$year,sep = ".")
  print(paste(l,'-',L$dataSetName))
  # Geo metadata
  geo <- list()
  geo$latitude <- as.numeric(lake$Latitude)
  geo$longitude <-  as.numeric(lake$Longitude)
  geo$elevation <-  as.numeric(lake$Elevation)
  geo$geology <- lake$Geology
  geo$area <- paste(ifelse(!is.na(lake$`Lake Area`),lake$`Lake Area`,lake$`Mire Area`),'ha')
  geo$basinSize <- ifelse(!is.na(lake$`Mean Basin`),paste(lake$`Mean Basin`,'ha'),NA)
  geo$siteName <- sitename
  geo$Location <- lake$`Country, Region`
  L$geo <- geo
  #Paleodata
  vals <- data.frame(age14C=seq(0,30000,500),age=calages,lakeLevel=NA,original=as.character(lake[which(names(lake)=='0'):which(names(lake)=='30')]))
  #Convert values from string to numeric
  for (v in 1:nrow(vals)){
    if(is.na(vals$original[v])){vals$lakeLevel[v] <- NA
    }else if(vals$original[v] == 'NA'){vals$lakeLevel[v] <- NA
    }else if(grepl('/',vals$original[v])){
      vals$lakeLevel[v] <- median(as.numeric(strsplit(vals$original[v],'/')[[1]]))
    }else{vals$lakeLevel[v]<-as.numeric(vals$original[v])}
  }
  pmt<- vector(mode = "list",length = 1)
  for (i in 1:3){
    pmt[[1]][[i]]<-list()
    pmt[[1]][[i]]$number <- i
    pmt[[1]][[i]]$TSid <- lipdR::createTSid("glsdb_")
    pmt[[1]][[i]]$variableName <- names(vals)[i]
    pmt[[1]][[i]]$values <- vals[,i]
    if(i == 1){
      pmt[[1]][[i]]$units <- 'yr 14C BP'
    }else if(i==2){
      pmt[[1]][[i]]$units <- 'yr BP'
      pmt[[1]][[i]]$primaryAgeColumn <- TRUE
    }else if(i==3){
      pmt[[1]][[i]]$units <- 'unitless'
      pmt[[1]][[i]]$primaryTimeseries <- TRUE
      pmt[[1]][[i]]$variableType <- 'lakeLevel@surface'
      pmt[[1]][[i]]$description <- paste('data sources:',names(lake[which(lake == 'yes')]),collapse = '; ')
      pmt[[1]][[i]]$proxy <- 'lake level'
      pmt[[1]][[i]]$proxyDetail <- 'lake level'
      pmt[[1]][[i]]$proxyGeneral <- 'stratigraphy'
      #interp
      pmt[[1]][[i]]$interpretation <- vector(mode = "list",length = 1)
      pmt[[1]][[i]]$interpretation[[1]]$scope <- "climate"
      pmt[[1]][[i]]$interpretation[[1]]$variable <- 'effectivePrecipitation'
      pmt[[1]][[i]]$interpretation[[1]]$variableDetail <- 'lakeLevel@surface'
      pmt[[1]][[i]]$interpretation[[1]]$seasonality <- 'Annual'
      pmt[[1]][[i]]$interpretation[[1]]$direction <- 'positive'
    }
  }
  L$paleoData <- vector(mode = "list",length = 1)
  L$paleoData[[1]]$measurementTable <- pmt
  #Save Europe
  L <- lipdverseR::initializeChangelog(L,notes= 'Created from Yu et al. (1995) by CH July 2024')
  vals<-vals%>%filter(!is.na(lakeLevel))%>%filter(age<21000)
  if(nrow(vals)>=8 &
     length(unique(vals$lakeLevel))>=3 &
     nrow(vals%>%filter(age<12000))>=6 & 
     diff(range(vals$age)) > 10000 &
     !is.na(lake$Lithol.) &
     (lake$Type %in% c('overflow lake','spill lake')==FALSE)
     ){
    count[['EU']] <- count[['EU']]+1
    writeLipd(L,file.path(dir,'proxies','GLSDB','use'))
  }else{
    writeLipd(L,file.path(dir,'proxies','GLSDB','no'))
  }
}
print(paste('Europe:',count,'records identified matching criteria'))

#China ----

#Load data
coding <- read_excel(file.path(dir,'chdata.xlsx'))
calages <- c()
for (age in seq(0,24000,500)){ calages <- c(calages,getCalMedian(age))}

#Create LiPD files for China from chdata.xlsx
count[['CH']]<-0
for (l in 1:nrow(coding)){
  lake <- coding[l,]
  L <- list()
  L$lipdVersion <- 1.3
  L$datasetId <- paste0("glsdb",suppressWarnings(lipdverseR::createDatasetId()))
  L$createdBy <- "github.com/clhancock/DAMP21ka/Notebooks/createLiPDlakes/glsdb2lipd.R"
  L$originalDataUrl <- "https://www.bgc-jena.mpg.de/5362440/tech_report04.pdf"
  L$archiveType <- "LakeSediment"
  #Sitename
  sitename <- lake$Basin %>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("[^A-Za-z0-9]")
  #Publications
  pubs <- strsplit(lake$`Primary References`,';')[[1]]
  publist <- vector(mode='list',length= length(pubs))
  for (p in 1:length(pubs)){
    ref <- (strsplit(pubs[[p]],'et al.')[[1]])
    ref <- unlist(strsplit(ref,' '))
    ref <- unlist(strsplit(ref,','))
    ref <- ref[which(ref %in% c('','p.c.','&','and','in','press.','b','c','d','e')== F)]
    if(ref[1] == 'de'){
      ref2 <- c(paste0(ref[1],ref[2]))
      if (length(ref) >= 3){for (i in 3:length(ref)){ref2<-c(ref2,ref[[i]])}}
      ref <- ref2
    }
    publist[[p]] <- list()
    publist[[p]]$citation <- pubs[[p]]
    publist[[p]]$author <- list()
    if (!is.na(as.numeric(substr(ref[length(ref)],1,4)))){
      for (r in 1:(length(ref))){
        if(is.na(as.numeric(substr(ref[r],1,4)))){
          publist[[p]]$author[[r]] <- ref[[r]]
        }else{
          if (r==1){
            if (p >1){publist[[p]]$author<-publist[[p-1]]$author
            }else{publist[[p]]$author[[r]] <- 'noauthor'}
          }
          publist[[p]]$year <- ref[[r]]
        }
      }
    }else{
      publist[[p]]$year <- 1111
      for (r in 1:(length(ref))){
        publist[[p]]$author[[r]] <- ref[[r]]
      }
    }
  }
  #add compilation
  p<-length(publist)+1
  publist[[p]] <- list()
  publist[[p]]$citation <- 'Yu et al., 2001'
  publist[[p]]$year <- 2001 
  publist[[p]]$title <- 'Lake status records from China: data base documentation'
  publist[[p]]$journal <- 'MPI-BGC Tech Rep 4'
  publist[[p]]$author <- list('Yu, G.','Harrison, S.P.','Xue, B.')
  L$pub <- publist
  # Add Name
  L$dataSetName <- paste(sitename,
                         publist[[1]]$author[[1]]%>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("[^A-Za-z0-9]"),
                         publist[[1]]$year,sep = ".")
  print(paste(l,'-',L$dataSetName))
  # Geo metadata
  geo <- list()
  geo$latitude <- as.numeric(lake$Latitude)
  geo$longitude <-  as.numeric(lake$Longitude)
  geo$elevation <-  as.numeric(lake$Elevation)
  geo$area <- ifelse(!is.na(lake$`Lake Area`),paste(lake$`Lake Area`,'km2'),NA)
  geo$basinSize <- ifelse(!is.na(lake$`Mean Basin`),paste(lake$`Mean Basin`,'km2'),NA)
  geo$siteName <- sitename
  L$geo <- geo
  #Paleodata
  vals <- data.frame(age14C=seq(0,24000,500),age=calages,lakeLevel=NA,original=as.character(lake[which(names(lake)=='0'):which(names(lake)=='24')]))
  #Convert values from string to numeric
  for (v in 1:nrow(vals)){
    if(is.na(vals$original[v])){vals$lakeLevel[v] <- NA
    }else if(vals$original[v] == 'NA'){vals$lakeLevel[v] <- NA
    }else if(grepl('/',vals$original[v])){
      vals$lakeLevel[v] <- median(as.numeric(strsplit(vals$original[v],'/')[[1]]))
    }else{vals$lakeLevel[v]<-as.numeric(vals$original[v])}
  }
  pmt<- vector(mode = "list",length = 1)
  for (i in 1:3){
    pmt[[1]][[i]]<-list()
    pmt[[1]][[i]]$number <- i
    pmt[[1]][[i]]$TSid <- lipdR::createTSid("glsdb_")
    pmt[[1]][[i]]$variableName <- names(vals)[i]
    pmt[[1]][[i]]$values <- vals[,i]
    if(i == 1){
      pmt[[1]][[i]]$units <- 'yr 14C BP'
    }else if(i==2){
      pmt[[1]][[i]]$units <- 'yr BP'
      pmt[[1]][[i]]$primaryAgeColumn <- TRUE
    }else if(i==3){
      pmt[[1]][[i]]$units <- 'unitless'
      pmt[[1]][[i]]$primaryTimeseries <- TRUE
      pmt[[1]][[i]]$variableType <- 'lakeLevel@surface'
      pmt[[1]][[i]]$description <- paste('data sources:',names(lake[which(lake == 'yes')]),collapse = '; ')
      pmt[[1]][[i]]$proxy <- 'lake level'
      pmt[[1]][[i]]$proxyDetail <- 'lake level'
      pmt[[1]][[i]]$proxyGeneral <- 'stratigraphy'
      #interp
      pmt[[1]][[i]]$interpretation <- vector(mode = "list",length = 1)
      pmt[[1]][[i]]$interpretation[[1]]$scope <- "climate"
      pmt[[1]][[i]]$interpretation[[1]]$variable <- 'effectivePrecipitation'
      pmt[[1]][[i]]$interpretation[[1]]$variableDetail <- 'lakeLevel@surface'
      pmt[[1]][[i]]$interpretation[[1]]$seasonality <- 'Annual'
      pmt[[1]][[i]]$interpretation[[1]]$direction <- 'positive'
    }
  }
  L$paleoData <- vector(mode = "list",length = 1)
  L$paleoData[[1]]$measurementTable <- pmt
  #Save de CH
  L <- lipdverseR::initializeChangelog(L,notes= 'Created from Yu et al. (1995) by CH July 2024')
  vals<-vals%>%filter(!is.na(lakeLevel))%>%filter(age<21000)
  if(nrow(vals)>=8 &
     length(unique(vals$lakeLevel))>=3 &
     nrow(vals%>%filter(age<12000))>=6 & 
     diff(range(vals$age)) > 10000 &
     !is.na(lake$Lithol.) & 
     lake$Type=='closed'){
    writeLipd(L,file.path(dir,'proxies','GLSDB','use'))
    count[['CH']] <- count[['CH']]+1
  }else{
    writeLipd(L,file.path(dir,'proxies','GLSDB','no'))
  }
}
print(count)
