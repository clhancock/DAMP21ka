library(dplyr)
library(ggplot2)
library(lipdR)
library(geoChronR)
wd <- '/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/Data/proxies/Lakes21k/'
D_all <- readLipd(wd)
ts_all <- extractTs(D_all)
is <- c()
for (i in 1:4){is <- c(is,which(grepl('DAMP',pullTsVariable(ts_all,paste0('inCompilationBeta',i,'_compilationName')),ignore.case = TRUE)))}
ts_all <- ts_all[is]

df <- data.frame(
  dsn  = pullTsVariable(ts_all,'dataSetName'),
  Site.Name = pullTsVariable(ts_all,'geo_siteName'),
  TSid  = pullTsVariable(ts_all,'paleoData_TSid'),
  Lat  = as.character(round(pullTsVariable(ts_all,'geo_latitude'),2)),
  Lon  = as.character(round(pullTsVariable(ts_all,'geo_longitude'),2)),
  Elev.m = round(pullTsVariable(ts_all,'geo_elevation'),0),
  Duration.kyr=NA,
  Source = pullTsVariable(ts_all,'originalDataURL'),
  Publication = NA,
  pub1 = pullTsVariable(ts_all,'pub1_doi'),
  pub2 = pullTsVariable(ts_all,'pub2_doi'),
  pub1yr = pullTsVariable(ts_all,'pub1_year')
) #%>% rename('Site Name' = site)

#Standardize source column
for (i in 1:nrow(df)){
  if(substr(df$TSid[i],1,2) == 'pd'){
    df$Source[i] <- ("NALDB ('20)") #'Liefert and Shuman (2020)'
  } else if(substr(df$TSid[i],1,2) == 'dc'){
    df$Source[i] <- ("ESADB ('21)")#'De Cort et al. (2021)
  } else if(substr(df$TSid[i],1,5) == 'alsdb'){
    df$Source[i] <- ("ALSDB ('23)")#'Clerke (2023)'
  } else if(substr(df$TSid[i],1,5) == 'glsdb'){
    if (df$Source[i] == 'https://doi.org/10.25921/49ag-ck94'){
      df$Source[i] <- ("FSUDB ('98)")#'Tarasov et al., 1996'
    }else if (df$Source[i] == 'https://doi.org/10.25921/p5aq-0931'){
      df$Source[i] <- ("ELSDB ('95)")#'Yu and Harrison, 1995
    }else if (df$Source[i] == 'https://www.bgc-jena.mpg.de/5362440/tech_report04.pdf'){
      df$Source[i] <- ("CLSDB ('01)")#'Yu, 2001' 
    }
  } else if(substr(df$TSid[i],1,3) == 'OLS'){
    df$Source[i] <- ("OLSDB ('89)")#'Street-Perrott et al. (1989)'
  } else{
    df$Source[i] <- 'Original Pub'
  }
}
print(table(df$Source))

#Standardize duration column
for (i in 1:nrow(df)){
  ages <- ts_all[[i]]$age[which(!is.na(ts_all[[i]]$paleoData_values))]
  duration <- round(diff(range(ages))/1000,1)
  df$Duration.kyr[i] <- min(duration,21)
}
print(paste('Duration spans between:',paste(range(df$Duration.kyr),collapse='-'),'kyr'))

df$Publication <- NA
referencelist = list()
#Standardize pub info
pb <- progress::progress_bar$new(format = "  assigning refs [:bar] :percent eta: :eta", total = nrow(df), clear = FALSE, width = 90)
for (i in 1:nrow(df)){
  cite <- ''
  pubs <- D_all[[df$dsn[[i]]]]$pub
  for (p in 1:length(pubs)){
    if (p > 3){next}
    doi <- pubs[[p]]$doi
    if (is.null(doi)){doi <- ''}
    if (doi == '10.1029/2019GL086412'){next} #Don't add compilation ref
    if (doi == '10.2172/5609291'){next} #Don't add compilation ref
    if (doi == '10.25949/22662253.v1'){next} #Don't add compilation ref
    if (substr(doi,1,2) == '10'){ #10
      ref <- rcrossref::cr_works(doi)
      author <- ref$data$author[[1]]$family[[1]]
      year <- substring(ref$data$published.print,1,4)
      if (nrow(ref$data$author[[1]]) > 1){
        ref <- paste0(author,' et al. (',year,')')
      } else{
        ref <- paste0(author,' (',year,')')
      }
    } else if(df$Source[i] %in% c("NALDB ('20)","CLSDB ('01)","FSUDB ('98)","ELSDB ('95)")){
      if(pubs[[p]]$citation=="Yu et al., 2001"){next} #Don't add compilation ref
      if(pubs[[p]]$citation=="Tarasov et al., 1996"){next} #Don't add compilation ref
      if(pubs[[p]]$citation=="Yu (1995)"){next} #Don't add compilation ref
      ref <- pubs[[p]]$citation
    } else if(df$Source[i] %in% c("ESADB ('21)")){ 
      if(pubs[[p]]$abbrev=="De Cort et al. (2021)"){next} #Don't add compilation ref
      ref <- pubs[[p]]$abbrev
    } else{
      if ('name' %in% names(pubs[[p]]$author[[1]])){      author <- pubs[[p]]$author[[1]]$name
      }else{ author <- pubs[[p]]$author[[1]]
      }
      if (author != ''){author<- strsplit(author,c(','))[[1]][[1]]}
      year <- pubs[[1]]$year
      if (length(pubs[[p]]$author) > 1){
        ref <- paste0(author,' et al. (',year,')')
      } else{
        ref <- paste0(author,' (',year,')')
      }
    }
    if (cite == ''){cite <- ref
    }else{cite <- paste0(cite,'; ',ref)}
    if (ref %in% names(referencelist)){
      next
    }
    if (substr(doi,1,2) == '10'){
      referencelist[[ref]] <- rcrossref::cr_cn(doi,'text','copernicus-publications')
    }else if(df$Source[i] %in% c("ALSDB ('23)","ESADB ('21)")){
      referencelist[[ref]] <- pubs[[p]]$citation
    }else{
      if(is.null(pubs[[p]]$title)){
        referencelist[[ref]] <- doi
      }else{
        referencelist[[ref]] <- paste0(pubs[[p]]$author[[p]],': ',pubs[[p]]$title,', ',pubs[[p]]$journal,', ',pubs[[p]]$year)
      }
    }
  }
  df$Publication[i] <- cite
  pb$tick()
}
#TO DO ADD CITATION LIST
#"ALSDB ('23)" and ESADB have full citations
#print(cite)
#fn = "holocene_recon_2024-02-20_23:40:51.950519_annual_DAMP21ka.8000loc.5000window.500."
#tsids <- read.csv(file.path('/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/Data/results',fn,'Fig4_precip_CorrelationDF.csv'))








#Create similar table for pollen -----

wd <- 'https://lipdverse.org/HoloceneHydroclimate/0_9_1/HoloceneHydroclimate0_9_1.zip'
D_poll <- readLipd(wd)
ts_poll <- extractTs(D_poll)
ts_poll <- ts_poll[pullTsVariable(ts_poll,'paleoData_TSid') %in% tsids$TSid]

df_poll <- data.frame(
  dsn  = pullTsVariable(ts_poll,'dataSetName'),
  tsid  = pullTsVariable(ts_poll,'paleoData_TSid'),
  site = pullTsVariable(ts_poll,'geo_siteName'),
  lat  = round(pullTsVariable(ts_poll,'geo_latitude'),2),
  lon  = round(pullTsVariable(ts_poll,'geo_longitude'),2),
  elev = pullTsVariable(ts_poll,'geo_elevation'),
  source = pullTsVariable(ts_poll,'originalDataSource'),
  pub1 = pullTsVariable(ts_poll,'pub1_doi'),
  pub2 = pullTsVariable(ts_poll,'pub2_doi'),
  pub1yr = pullTsVariable(ts_poll,'pub1_year'),
  duration = round((unlist(lapply(pullTsVariable(ts_poll,'age'),max,na.rm=T))-unlist(lapply(pullTsVariable(ts_poll,'age'),min,na.rm=T)))/1000,1)
)
df_poll$duration[df_poll$duration>21] <- 21.0

df_poll$cite <- NA
#Standardize pub info
for (i in 1:nrow(df_poll)){
  cite <- ''
  pubs <- D_poll[[df_poll$dsn[[i]]]]$pub
  for (p in 1:length(pubs)){
    #
    #
    #
    i<-147
    pubs <- D_poll[[df_poll$dsn[[i]]]]$pub
    doi <- pubs[[1]]$doi
    if (substr(doi,1,2) == '10'){
      ref <- rcrossref::cr_works(doi)
    }
    print(doi)
    print(ref)
    #
    #
    #
      author <- ref$data$author[[1]]$family[[1]]
      year <- substring(ref$data$published.print,1,4)
      if (nrow(ref$data$author[[1]]) > 1){
        ref <- paste0(author,' et al. (',year,')')
      } else{
        ref <- paste0(author,' (',year,')')
      }
    } else if(doi == 'noPubOnRecord'){
      ref <- ""
    } else{
      author <- pubs[[p]]$author[[1]]$name
      if (is.null(author)){if (is.null(doi)){author<- strsplit(doi,c(','))[[1]][[1]]}}
      else if (author != ''){author<- strsplit(author,c(','))[[1]][[1]]}
      year <- pubs[[1]]$year
      if (length(pubs[[p]]$author) > 1){
        ref <- paste0(author,' et al. (',year,')')
      } else{
        ref <- paste0(author,' (',year,')')
      }
    }
    if (cite == ''){cite <- ref
    }else{cite <- paste0(cite,'; ',ref)}
  }
  df_poll$cite[i] <- cite
}
write.csv(df_poll,'/Users/chrishancock/Desktop/TableS2.csv')
