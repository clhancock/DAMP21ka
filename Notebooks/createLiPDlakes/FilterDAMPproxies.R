#Load packages----
library(dplyr)
library(ggplot2)
library(lipdR)

#Load data----
#Load df of all possible lipd records
df_raw <- queryLipdverse()
#sort(unique(df_raw$paleoData_proxy))

#Filter for lake level with good temporal coverage
df_lakes <- df_raw %>%
  filter(tolower(gsub(" ","",archiveType)) %in% c("lakesediment","shoreline")) %>%
  filter(tolower(gsub(" ","",paleoData_proxy)) %in% c("lakelevel",'stratigraphy')) %>%
  filter(maxAge > 12000 & minAge < 2000)
print(paste(nrow(df_lakes),'potential lake level records found'))

#Load lipd files
D_lipdverse <- readLipd(df_lakes)
D_dc <- readLipd('/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/revisions/proxies/DeCort/use')
D_glsdb <- readLipd('/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/revisions/proxies/GLSDB/use')
D_alsdb <- readLipd('/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/revisions/proxies/ALSDB/use')
D_lipdverse2 <- D_lipdverse[names(D_lipdverse)[which(names(D_lipdverse) %in% names(D_glsdb)==F)]]
L <- readLipd('/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/revisions/proxies/UyuniBasin.Placzek.2006.lpd')

D_all <- c(D_lipdverse2,D_dc,D_glsdb,D_alsdb)
D_all[[L$dataSetName]] <- L
print(paste(as.character(length(D_all)),'datasets loaded'))

#Filter Data----

ts <- extractTs(D_all)
ts <- ts[!is.na(pullTsVariable(ts,"paleoData_proxy"))]
ts <- ts[!grepl('uncertainty',pullTsVariable(ts,"paleoData_VariableName"))]
ts <- ts[which(is.na(pullTsVariable(ts,'paleoData_primaryTimeseries')) | pullTsVariable(ts,'paleoData_primaryTimeseries')==TRUE)]

filters <- read.csv('/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/Data/proxies/LakeFilters.csv')
# z <- c()
# for (i in 1:nrow(filters)){
#   if (filters$tsid[i]==''){next}
#   L <- (ts[[which(pullTsVariable(ts,"paleoData_TSid") == filters$tsid[i])]])
#   #z <- c(z,L$dataSetName)
#   z <- c(z,L$geo_latitude)
#   #z <- c(z,L$geo_longitude)
# }
#View(as.data.frame(z))
ts <- ts[which(pullTsVariable(ts,"paleoData_TSid")%in%filters$tsid==F)]

#Set up df to check
df <- data.frame(name=pullTsVariable(ts,"dataSetName"),
                 TSid=pullTsVariable(ts,"paleoData_TSid"),
                 lat=pullTsVariable(ts,"geo_latitude"),
                 lon=pullTsVariable(ts,"geo_longitude"),
                 source=pullTsVariable(ts,'createdBy'),
                 valid = FALSE,
                 duration = NA,
                 count=NA)
#Remove based on age resolution/duration
verbose=F
for (i in 1:nrow(df)){
  idx <- which(pullTsVariable(ts,"paleoData_TSid")==df$TSid[i])
  vals <- ts[[idx]]$paleoData_values[which(ts[[idx]]$age <= 21000)]
  ages <- ts[[idx]]$age[             which(ts[[idx]]$age <= 21000)]
  agesValid <- ages[!is.na(vals)]
  df$duration[i] <- diff(range(agesValid))
  #
  if (length(unique(agesValid)) >= 8){
    if (length(unique(agesValid[which(agesValid<12000)])) >= 6){
      if (df$duration[i] > 10000){
        if (length(unique(vals[!is.na(vals)])) >= 3){
          if (median(diff(ages)) < 1500){
            #if (max(diff(ages)) < 5000){
              df$valid[i] <- TRUE
            #}
          }else{
            if(verbose){print(paste(i,ts[[i]]$dataSetName,'median(diff)>1500',sep=' - '))}
          }
        }else{
          if(verbose){print(paste(i,ts[[i]]$dataSetName,'need 3 unique values',sep=' - '))}
        }
      }else{
        if(verbose){print(paste(i,ts[[i]]$dataSetName,'duration<10kry',sep=' - '))}
      }
    }else{
      if(verbose){print(paste(i,ts[[i]]$dataSetName,'need 6 Holocene dates',sep=' - '))}
    }
  }else{
    if(verbose){print(paste(i,ts[[i]]$dataSetName,'need 8 total dates',sep=' - '))}
  }
}

df <- df %>% filter(valid) %>% arrange(lat)
print(paste(nrow(df),' lake records found'))

# Map anomalies----
#Set Projections
PROJ     <- "+proj=robin"
#Countries for basemap
countries  <- rworldmap::getMap("li")
#BaseMap
basemap <- ggplot() +
  geom_map(data=countries,  map=fortify(countries),
           aes(group=group, map_id=id), fill ="whitesmoke",color="grey30",size=0.03) +
  coord_sf(crs=PROJ,default_crs=countries@proj4string@projargs,xlim=c(-180,180),ylim=c(-90,90))+
  scale_y_continuous(breaks = c(-89.9,-60,-30,0,30,60,89.9))+
  scale_x_continuous(breaks = seq(-180, 180, 60))+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_line(color='black',linewidth = 0.05),
        panel.ontop = TRUE)
#plot available records
basemap + geom_point(aes(df$lon,df$lat,color=df$duration,shape=df$source))+
  scale_color_continuous(type='viridis',name='Duration (kyr)',breaks=seq(8000,21000,3000),labels=seq(8,21,3),limits=c(7000,21000))+
  scale_shape_discrete(name='Source',labels=c('Liefert and Shuman (2020)', 'Street-Perrott et al. (1989)','Other'))+
  guides(shape=guide_legend(ncol=1,title.position = 'top',override.aes = list(size = 3)))+
  guides(color=guide_colourbar(title.position = 'top'))+
  labs(title='DAMP21k - Lake Level Availability',
       subtitle=paste(nrow(df),'records from',length(unique(df$name)),'datasets'),
       caption='[>10,000 duration & median resolution <1,500&\n>8 values & >=3 unique values]')+
  theme(legend.position = 'right',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

#Save ----
#Add metadata
count <- 0
tsids<-dsns<-c()
for (i in 1:nrow(df)){
  for (pd in 1:length(D_all[[df$name[i]]][["paleoData"]])){
    for (mt in 1:length(D_all[[df$name[i]]][["paleoData"]][[pd]][["measurementTable"]])){
      for (name in 1: length(D_all[[df$name[i]]][["paleoData"]][[pd]][["measurementTable"]][[mt]])){
        if ('TSid' %in% names(D_all[[df$name[i]]][["paleoData"]][[pd]][["measurementTable"]][[mt]][[name]])){
          print(D_all[[df$name[i]]][["paleoData"]][[pd]][["measurementTable"]][[mt]][[name]]$TSid)
          if (df$TSid[i] == D_all[[df$name[i]]][["paleoData"]][[pd]][["measurementTable"]][[mt]][[name]]$TSid){
            n <- length(D_all[[df$name[i]]][["paleoData"]][[pd]][["measurementTable"]][[mt]][[name]]$inCompilationBeta)
            if (n == 0){D_all[[df$name[i]]][["paleoData"]][[pd]][["measurementTable"]][[mt]][[name]]$inCompilationBeta <- vector(mode='list')}
            D_all[[df$name[i]]][["paleoData"]][[pd]][["measurementTable"]][[mt]][[name]]$inCompilationBeta[[n+1]] <- vector(mode='list')
            D_all[[df$name[i]]][["paleoData"]][[pd]][["measurementTable"]][[mt]][[name]]$inCompilationBeta[[n+1]]$compilationName <- c('DAMP21k_Lakes')
            D_all[[df$name[i]]][["paleoData"]][[pd]][["measurementTable"]][[mt]][[name]]$inCompilationBeta[[n+1]]$compilationVersion <- c("0_1_0")
            tsids <- c(tsids,df$TSid[i])
            dsns<-c(dsns,df$name[i])
            count <- count+1
          }
        }
      }
    }
  }
}
if(count!=nrow(df)){print(paste0('WARNING!!!! Mismatch between number of records selected (',count,') and number of records expected (',nrow(df),') !!!!!'))
}else{print(paste(count,'records found'))}

#Check for duplicates
# #ID duplicates
for (i in 2:nrow(df)){
  #df$count[i] <- length(which(pullTsVariable(ts,"dataSetName")== df$name[i]))
  if (abs(df$lat[i] - df$lat[(i-1)]) < 0.12){
    if (abs(df$lon[i] - df$lon[(i-1)]) < 0.12){
      df$count[i] <- 2
      df$count[(i-1)] <- 2
    }
  }
}
#View(df %>% filter(!is.na(count)))

#Save results
save <- TRUE
if (save){
  D_Lakes <- D_all[df$name]
  writeLipd(D_Lakes,path ='/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/Data/proxies/Lakes21k')
}

