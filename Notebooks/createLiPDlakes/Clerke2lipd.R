#Load packages----
library(dplyr)
library(ggplot2)
library(lipdR)
library(readxl)
library(stringr)

dir <- '/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/revisions/'

##Load data
coding        <- read_excel(file.path(dir,'ALSDB_Clerke23.xlsx'), sheet = "values") %>% filter(!is.na(age))
#Refs
lakerefs      <- tibble(read_excel(file.path(dir,'ALSDB_Clerke23.xlsx'), sheet = "refs")) %>% mutate(author=ifelse(is.na(author),NA,strsplit(author,';')))
#metadata
lakes         <- read_excel(file.path(dir,'ALSDB_Clerke23.xlsx'), sheet = "metadata") %>% filter(grepl('created',note))

D<-list()
#Create LiPD files for Clerke (2023)
for (l_id in unique(lakes$lakeid)){
  L <- list()
  L$lipdVersion <- 1.3
  L$datasetId <- paste0("alsdb",suppressWarnings(lipdverseR::createDatasetId()))
  L$createdBy <- "github.com/clhancock/DAMP21ka/Notebooks/createLiPDlakes/Clerke2lipd.R"
  L$originalDataUrl <- "https://doi.org/10.25949/22662253.v1"
  L$deCortSiteId <- l_id
  metaRow <- lakes %>% filter(lakeid == l_id)
  L$archiveType <- ifelse(sum(strsplit(metaRow$basis,';')[[1]]>0),"Shoreline","LakeSediment")
  #Sitename
  sitename <- metaRow$name %>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("[^A-Za-z0-9]")
  #Publications
  clerke_ref <- tibble(refID=0,
                 year = 2023, doi = '10.25949/22662253.v1',
                 title = 'Hydrological regime of Australian lakes over the Late-Quaternary and Holocene',
                 author = list('Clerke, L.'),journal='PhD Thesis',
                 citation = 'Clerke, L., 2023. Hydrological regime of Australian lakes over the Late-Quaternary and Holocene. PhD Thesis, Macquarie University, Sydney, 10.25949/22662253.v1'
                 )
  pubs <- rbind(lakerefs %>% filter(refID %in% strsplit(metaRow$reference,';')[[1]]),clerke_ref)
  publist <- list()
  for (r in 1:nrow(pubs)){
    publist[[r]] <- list()
    for (name in names(pubs)){
      if (name == 'author'){
        publist[[r]][[name]] <- pubs[r,name][[1]]
      }else{publist[[r]][[name]] <- as.character(pubs[r,name])}
    }
  }
  L$pub <- publist# rbind(pubs,dc_ref)
  # Add Name
  L$dataSetName <- paste(sitename,strsplit(publist[[1]]$author[[1]],',')[[1]][1]  %>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("[^A-Za-z0-9]"),publist[[1]]$year,sep = ".")
  print(paste(l_id,'-',L$dataSetName))
  # Geo metadata
  geo <- list()
  geo$latitude <- metaRow$lat
  geo$longitude <- metaRow$lon
  geo$elevation <- metaRow$elevation
  geo$basintype <- metaRow$type
  geo$catchment <- metaRow$basin.area
  geo$area <- metaRow$lake.area
  geo$siteName <- sitename
  L$geo <- geo
  #Paleodata
  vals <- coding%>%filter(lakeid==l_id) %>% select(c(age,value))%>%rename(lakeLevel=value)
  pmt<- vector(mode = "list",length = 1)
  for (i in 1:2){
    pmt[[1]][[i]]<-list()
    pmt[[1]][[i]]$number <- i
    pmt[[1]][[i]]$TSid <- lipdR::createTSid("alsdb_")
    pmt[[1]][[i]]$variableName <- names(vals)[i]
    pmt[[1]][[i]]$values <- vals[,i]
    if(i==1){
      pmt[[1]][[i]]$units <- 'yr BP'
      pmt[[1]][[i]]$primaryAgeColumn <- TRUE
    }else if(i==2){
      print(pmt[[1]][[i]]$TSid)
      pmt[[1]][[i]]$units <- 'unitless'
      pmt[[1]][[i]]$primaryTimeseries <- TRUE
      pmt[[1]][[i]]$variableType <- 'lakeLevel@surface'
      pmt[[1]][[i]]$description <- paste('data sources:',metaRow$basis)
      pmt[[1]][[i]]$proxy <- 'lake level'
      pmt[[1]][[i]]$proxyDetail <- 'lake level'
      pmt[[1]][[i]]$proxyGeneral <- 'stratigraphy'
      #interp
      pmt[[1]][[i]]$interpretation <- vector(mode = "list",length = 1)
      pmt[[1]][[i]]$interpretation[[1]]$scope <- "climate"
      pmt[[1]][[i]]$interpretation[[1]]$variable <- 'effectivePrecipitation'
      pmt[[1]][[i]]$interpretation[[1]]$variableDetail <- 'lakeLevel@surface'
      pmt[[1]][[i]]$interpretation[[1]]$seasonality <- 'Annual'
      pmt[[1]][[i]]$interpretation[[1]]$direction <- 'positive' # lake-status class of 1 was assigned to the lowest inferred lake level phase of a sequence, while higher lake-status classes were representative of higher lake levels
    }
  }
  L$paleoData <- vector(mode = "list",length = 1)
  L$paleoData[[1]]$measurementTable <- pmt
  #Save
  L <- lipdverseR::initializeChangelog(L,notes= 'Created from ALSDB (Clerke et al., 2023)) by CH July 2024')
  if (metaRow$note %in% c("created","created; OLS")){
    writeLipd(L,file.path(dir,'proxies','ALSDB','use'))
  }else{
    writeLipd(L,file.path(dir,'proxies','ALSDB','no'))
  }
  D[[l_id]] <- L
}

