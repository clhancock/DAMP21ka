#Load packages----
library(dplyr)
library(ggplot2)
library(lipdR)
library(readxl)
library(stringr)

dir <- '/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/revisions/'

##Look at cort
coding        <- read_excel(file.path(dir,'ESA database - spreadsheets','coding.xlsx'), sheet = "Sheet1")
codingsources <- read_excel(file.path(dir,'ESA database - spreadsheets','codingsources.xlsx'), sheet = "Sheet1")
codingbases   <- read_excel(file.path(dir,'ESA database - spreadsheets','codingbases.xlsx'), sheet = "Sheet1")
coding <- coding %>% left_join(codingbases, by = c("lakeid", "status"))
#Refs
lakerefs      <- read_excel(file.path(dir,'ESA database - spreadsheets','lakerefs.xlsx'), sheet = "Sheet1")
refs          <- read_excel(file.path(dir,'ESA database - spreadsheets','refs.xlsx'), sheet = "Sheet1")
refs <-  refs %>% filter(abbrev != 'Google Earth') %>% mutate(
  FirstAuthor = str_extract(abbrev, "^[^ ]+"),
  year = str_replace_all(str_extract(abbrev, "\\(([^)]+)\\)"), "[()]", ""),
  author = str_extract(reference, "^[^\\d]+(?=\\d{4})") %>% str_trim(),
  title = str_extract(reference, "(?<=\\d{4}\\. ).*?(?=\\.)") %>% str_trim(),
  ) %>% arrange(year) %>% rename(citation = reference)
refs$entry <- refs$pages <-  refs$volume <- refs$publisher <- refs$journal <- NA
for(r in which(!is.na(refs$doi))){
  print(r)
  bib <- suppressWarnings(rcrossref::cr_cn(refs$doi[r],'bibentry'))
  if (is.null(bib)){next}
  for (name in c('title','author','journal','publisher','volume','pages','entry')){
    if(!is.null(bib[[name]])){refs[r,name] <- bib[[name]]}
  }
}

#metadata
lakes         <- read_excel(file.path(dir,'ESA database - spreadsheets','lakes.xlsx'), sheet = "Sheet1")
namesAlt      <- read_excel(file.path(dir,'ESA database - spreadsheets','alternativenames.xlsx'), sheet = "Sheet1")
#chronology
dating        <- read_excel(file.path(dir,'ESA database - spreadsheets','dating.xlsx'), sheet = "Sheet1")
dating <- dating %>% mutate(
  site =  paste(dateid,lakeid,sep='_'),
  sampleID = featurename,
  depth = as.numeric(depth),
  thickness = as.numeric(thickness),
  age14C = as.numeric(rc_age),
  uncertaintyHigh=as.numeric(rc_uncertainty_positive),
  uncertaintyLow=as.numeric(rc_uncertainty_negative),
  reservoir = as.numeric(reservoir_age),
  material = paste(materialdated,featuretype,sep='_'),
  age=as.numeric(ifelse(new_isused==1,new_weightedmean,published_cal)),
  uncertaintyHigh95 = as.numeric(ifelse(new_isused==1,new_oldestuncertainty,published_cal_upper2sigmar1)),
  uncertaintyLow95 =  as.numeric(ifelse(new_isused==1,new_youngestuncertainty,published_cal_lower2sigmar1)),
  correction = ifelse(new_isused==1,new_calibration,published_calibration),
  notes =  paste(ifelse(is.na(datetype),'',datetype),
                 paste('agemodel:',     ifelse(new_isused==1,new_agemodeltype,published_agemodeltype)),
                 paste('timereference:',ifelse(new_isused==1,new_timereference,published_timereference)),
                 ifelse(is.na(notes),'',notes),sep='; ')
)

#Which relevent to DAMP21ka
damp21k <- coding %>%
  left_join(lakes, by = "lakeid") %>%
  filter(enddate < 21000) %>%
  #filter(basintype.x != 'open')%>% #none are open all the time
  group_by(lakeid) %>%
  mutate(count_x = n()) %>%
  mutate(count_unique = n_distinct(status)) %>%
  mutate(basintypes =  paste(basintype.x,collapse = ',')) %>% 
  ungroup() %>%
  filter(count_x >= 8 & count_unique>=3,grepl('closed',basintypes)) 

damp21k <- damp21k %>%
  filter(lakeid %in% ((damp21k %>% filter(enddate < 12000)  %>% group_by(lakeid) %>% mutate(count_x = n()) %>%  ungroup() %>% filter(count_x >= 6))$lakeid))


D<-list()
#Create LiPD files for De Cort et al. (2021)
for (l_id in unique(coding$lakeid)){
  L <- list()
  L$lipdVersion <- 1.3
  L$datasetId <- paste0("dc",suppressWarnings(lipdverseR::createDatasetId()))
  L$createdBy <- "github.com/clhancock/DAMP21ka/Notebooks/createLiPDlakes/deCort2lipd.R"
  L$originalDataUrl <- "https://doi.org/10.5281/zenodo.4494804"
  L$deCortSiteId <- l_id
  L$archiveType <- ifelse(sum((codingsources %>% filter(lakeid == l_id))$datasource %in% c('Stratigraphy','Shoreline features','Non-shoreline geomorphology','Instrumental/gauge data','Archaeological data','Historical data'))>0,
                          "Shoreline","LakeSediment")
  #Sitename
  metaRow <- lakes %>% filter(lakeid == l_id)
  sitename <- metaRow$lakename %>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("[^A-Za-z0-9]")
  metaRowAlt <- namesAlt %>% filter(lakeid == l_id)
  if(nrow(metaRowAlt)>0){
    sitenameAlt <- paste0(metaRowAlt$alternativename %>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("[^A-Za-z0-9]"),collapse='_')
    sitename <- paste(sitename,sitenameAlt,sep='_')
  }
  #Publications
  dc_ref <- tibble(referenceid=0,abbrev= 'De Cort et al. (2021)',
                 citation = 'De Cort, G., Chevalier, M., Burrough, S.L., Chen, C.Y., Harrison, S.P., 2021. An uncertainty-focused database approach to extract spatiotemporal trends from qualitative and discontinuous lake-status histories. Quat. Sci. Rev. 258.',
                 doi = '10.1016/j.quascirev.2021.106870', FirstAuthor='De Cort', year = 2021,
                 author = list('De Cort, G.', 'Chevalier, M.', 'Burrough, S.L.', 'Chen, C.Y.', 'Harrison, S.P.'),
                 title = 'An uncertainty-focused database approach to extract spatiotemporal trends from qualitative and discontinuous lake-status histories',
                 journal='Quaternary Science Reviews',publisher='Elsevier',volume='258',pages='106870',entry='article')
  pubs <- rbind((refs %>% filter(referenceid %in% (lakerefs %>% filter(lakeid == l_id))$referenceid)),dc_ref)
  publist <- list()
  for (r in 1:nrow(pubs)){
    publist[[r]] <- list()
    for (name in names(pubs)){
      publist[[r]][[name]] <- as.character(pubs[r,name])
    }
  }
  L$pub <- publist# rbind(pubs,dc_ref)
  # Add Name
  L$dataSetName <- paste(sitename,publist[[1]]$FirstAuthor,publist[[1]]$year,sep = ".")
  print(paste(l_id,'-',L$dataSetName))
  # Geo metadata
  geo <- list()
  geo$latitude <- metaRow$latitude
  geo$longitude <- metaRow$longitude
  geo$elevation <- metaRow$elevation
  geo$basintype <- metaRow$basintype
  geo$catchment <- metaRow$catchment
  geo$area <- metaRow$area
  geo$volume <- metaRow$volume
  geo$siteName <- sitename
  L$geo <- geo
  #Paleodata
  thisPmt <-  coding %>% filter(lakeid==l_id) %>%
    mutate(medianage = (startdate+enddate)/2)%>%
    tidyr::uncount(ifelse(startdate!=enddate, 2, 1)) %>%
    rename(lakeLevel=status, uncertaintyHigh=maxstatus, uncertaintyLow=minstatus, sampleID=shorelineid,notes=evidence,
           startdate_uncertaintyHigh95=oldeststartdate,startdate_uncertaintyLow95=youngeststartdate,enddate_uncertaintyHigh95=oldestenddate,enddate_uncertaintyLow95=youngestenddate) %>%
    mutate(age = ifelse(row_number() %% 2 == 1, startdate, enddate))%>%
    select(-lakeid) %>% arrange(age)
  an <- names(thisPmt)
  pmt <-vector(mode = "list",length = 1)
  for(coll in 1:length(an)){
    cname <- an[coll]
    thisCol <- list()
    thisCol$number <- coll
    thisCol$TSid <- lipdR::createTSid("dc_")
    thisCol$values <- as.matrix(thisPmt[,coll])
    thisCol$variableName <- cname #conv$variableName[cr]
    thisCol$units <- ifelse(grepl('date',cname) | grepl('age',cname),'yr BP','unitless')
    thisCol$primaryTimeseries <- ifelse(cname == 'lakeLevel',TRUE,FALSE)
    thisCol$primaryAgeColumn <- ifelse(cname == 'age',TRUE,FALSE)
    if(an[coll] == 'lakeLevel'){
      thisCol$variableType <- 'lakeLevel@surface'
      thisCol$description <- 'Best-estimate relative lake- status class'
      thisCol$proxyDetail <- paste((codingsources %>% filter(lakeid == l_id))$datasource,collapse='; ')
      thisCol$proxy <- 'lake level'
      thisCol$proxyGeneral <- ifelse(L$archiveType=='Shoreline','stratigraphy','lake level')
      #interp
      thisCol$interpretation <- vector(mode = "list",length = 1)
      thisCol$interpretation[[1]]$scope <- "climate"
      thisCol$interpretation[[1]]$variable <- 'effectivePrecipitation'
      thisCol$interpretation[[1]]$variableDetail <- 'lakeLevel@surface'
      thisCol$interpretation[[1]]$seasonality <- 'Annual'
      thisCol$interpretation[[1]]$direction <- 'positive'
    }
    pmt[[1]][[cname]] <- thisCol
  }
  L$paleoData <- vector(mode = "list",length = 1)
  L$paleoData[[1]]$measurementTable <- pmt
  # #Chron table
  # dates <- dating %>% filter(lakeid == l_id)
  # an <-  c('sampleID','depth','thickness','age14C','uncertaintyHigh','uncertaintyLow','reservoir','material','age','uncertaintyHigh95','uncertaintyLow95','correction','notes')
  # cmt <- vector(mode = "list",length = 1)
  # for(coll in 1:length(an)){
  #   cname <- an[coll]
  #   thisCol <- list()
  #   thisCol$number <- coll
  #   thisCol$TSid <- lipdR::createTSid("dc_")
  #   thisCol$values <- as.matrix(dates[,cname])
  #   thisCol$variableName <- cname #conv$variableName[cr]
  #   if (cname %in% c('depth','thickness')){                                           thisCol$units <- 'cm'
  #   } else if(cname %in% c('age','uncertaintyHigh95','uncertaintyLow95')){            thisCol$units <- 'yr BP'
  #   } else if(cname %in% c('age14C','uncertaintyHigh','uncertaintyLow','reservoir')){ thisCol$units <- 'yr 14C BP'
  #   }else{                                                                            thisCol$units <- 'unitless'
  #   }
  #   cmt[[1]][[cname]] <- thisCol
  # }
  # L$chronData <- vector(mode = "list",length = 1)
  # L$chronData[[1]]$measurementTable <- cmt
  #Save
  L <- lipdverseR::initializeChangelog(L,notes= 'Created from De Cort et al. (2021) by CH July 2024')
  if (l_id %in% damp21k$lakeid){
    writeLipd(L,file.path(dir,'proxies','DeCort','use'))
  }else{
    writeLipd(L,file.path(dir,'proxies','DeCort','no'))
  }
  D[[l_id]] <- L
}

