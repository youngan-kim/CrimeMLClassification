#######################################################################
##  Made by: Dr. Keungoui Kim & Dr. Young-an Kim
##  Title: ML-based Crime Classification
##  goal : train & test set by regions
##  Data set:  
##  Time Span:
##  Variables
##      Input: 
##      Output: 
##  Time-stamp: #  "Sun Jan 26 01:47:34 2020":  edited by awe kim ; code
##  Notice :

#######################################################################
### Import packages
#######################################################################

library(haven)
library(stringr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(mltools)
library(reshape2)
library(lsa)
library(corrplot)
library(proxy)
library(factoextra)
library(tidyverse)  
library(cluster)    
library(factoextra)
library(caret)

'%ni%' <- Negate('%in%')

####################################################################
##### Data Preparation
#####################################################################

### Description of variables
# gage29  "% aged 15 to 29 in block group, 2010"
# gasian "% Asian"
# gblack  "% Black"
# gethhet "racial heterogeneity in block group, 2010"
# ghowlng  "avg length of residence in block group"
# glatino "% Latino"
# gocc  "% occupied houses"
# gowner "% homeowners"
# gpop  "population"
# fdis "Concentrated disadvantage index"
# ProResArea   "Res proportion of building area"
# ProOfficeArea  "Office proportion of building area"
# ProRetailArea  "Retail proportion of building area"
# ProFactryArea  "Factry proportion of building area"

### Data Import
# read .dta file and save .RData
# ny_data <- 0
# for(i in c(2010:2015)){
#   temp <-
#     read_dta(file=paste0('../data/NYC_data_bg2010_',i,'.dta'))
#   ny_data <- rbind(ny_data, temp)
# }
# ny_data <- ny_data[2:nrow(ny_data),]
# save(ny_data, file="../R file/ny_data.RData")

load(file="../R file/ny_data.RData")

### Data Check
ny_data %>% data.frame %>% head
nrow(ny_data) # 166669
ny_data$year %>% unique # 2010 2011 2012 2013 2014 2015
ny_data %>% summary
names(ny_data)
names(ny_data)[ny_data %>% is.na %>% colSums > 0]

# How to handling missing values?
ny_data %>% filter(is.na(gagebld) == TRUE) %>% data.frame %>%
  head
# Remove missing values 
# (later when conducting socio-economic analysis)
# ny_data %<>% 
#   select(-names(ny_data)[ny_data %>% is.na %>% colSums > 0])

ny_data_yr <- list()
for(i in 1:length(unique(ny_data$year))){
  ny_data_yr[[i]] <- ny_data %>%
    filter(year==unique(ny_data$year)[i])
}
lapply(ny_data_yr, head)

# Normalization ~ Scaling
nor_minmax <- function(x){
  result <- (x-min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE)- min(x, na.rm=TRUE))
  return(result)
}

# Normalize all variables (both socioecon + crime)
ny_data_yr_nor <- list()
for(i in 1:length(ny_data_yr)){
  ny_data_yr_nor[[i]] <- 
    apply(ny_data_yr[[i]] %>% select(-c("bgidfp10","year")), 
          MARGIN=2, FUN="nor_minmax") %>% data.frame 
  ny_data_yr_nor[[i]] %<>% 
    cbind(ny_data_yr[[i]] %>% 
            select(c("bgidfp10","year")))
}
lapply(ny_data_yr_nor, head)

row.names(ny_data_yr_nor[[1]]) <- ny_data_yr_nor[[1]]$bgidfp10
row.names(ny_data_yr_nor[[2]]) <- ny_data_yr_nor[[2]]$bgidfp10
row.names(ny_data_yr_nor[[3]]) <- ny_data_yr_nor[[3]]$bgidfp10
row.names(ny_data_yr_nor[[4]]) <- ny_data_yr_nor[[4]]$bgidfp10
row.names(ny_data_yr_nor[[5]]) <- ny_data_yr_nor[[5]]$bgidfp10
row.names(ny_data_yr_nor[[6]]) <- ny_data_yr_nor[[6]]$bgidfp10

save(ny_data_yr_nor, file="../R file/ny_data_yr_nor.RData")

#####################################################################
##### Top Crime Regions
#####################################################################

temp.df.all <- ny_data_yr[[1]] %>%
  rbind(ny_data_yr[[2]]) %>% rbind(ny_data_yr[[3]]) %>%
  rbind(ny_data_yr[[4]]) %>% rbind(ny_data_yr[[5]]) %>%
  rbind(ny_data_yr[[6]])

# normalized version
temp.df.all.nor <- ny_data_yr_nor[[1]] %>%
  rbind(ny_data_yr_nor[[2]]) %>% rbind(ny_data_yr_nor[[3]]) %>%
  rbind(ny_data_yr_nor[[4]]) %>% rbind(ny_data_yr_nor[[5]]) %>%
  rbind(ny_data_yr_nor[[6]])

# yearly rank 
# To prove that Top crime region's crime rank does not change often.
i<-1
temp.df.all <- 0
for (i in 1:length(ny_data_yr)){
  temp <- ny_data_yr[[i]] %>%
    # temp.df.all %>% filter(year == unique(temp.df.all$year)[[i]]) %>% 
    mutate(crime=assaul+burglr+larcen+motveh+robber) %>%
    arrange(desc(crime)) %>% 
    mutate(rank=paste0(unique(ny_data_yr[[i]]$year),"-",dense_rank(desc(crime))))
  temp.df.all <- temp.df.all %>% rbind(temp)
  rm(temp)
}
temp.df.all %<>% filter(bgidfp10 != 0)

### Create different top 30s
# crime ==> top 30 
# Violent crime = assaul + robber ==> top 30 
# Property crime = burglr + larcen + motveh ==> top 30 

### Descriptive statistics of each cluster (socio-economic variables)
top10.crime <- 
  temp.df.all %>% # temp.df.all.nor
  mutate(crime=assaul+burglr+larcen+motveh+robber) %>%
  mutate(viol = assaul + robber, property = burglr + larcen + motveh) %>%
  group_by(bgidfp10) %>% 
  summarize(crime.sum=sum(crime), crime.avg=mean(crime), 
            viol.sum=sum(viol), viol.avg=mean(viol), property.sum=sum(property), property.avg=mean(property)) %>% 
  mutate(crime.prop=crime.sum/sum(crime.sum), 
         viol.prop=viol.sum/sum(viol.sum), property.prop=property.sum/sum(property.sum)) %>%
  arrange(desc(crime.prop)) %>% 
  mutate(rank=dense_rank(desc(crime.prop)))

#####################################################################
##### k-means Clustering with Crime variables
#####################################################################

ny.cluster.all <- 0
ny.kmeans.df.all <- 0

for(i in 1:length(ny_data_yr_nor)){
  temp.df <- ny_data_yr_nor[[i]] %>%
    select("bgidfp10","assaul","burglr","larcen","motveh","robber") 

  # Elbow method
  wss <- function(k) {
    kmeans(temp.df,
           k, nstart = 10 )$tot.withinss}
  k.values <- 1:15
  wss_values <-
    map_dbl(k.values, wss)
  k_df <- data.frame(k=k.values,
                     wss = wss_values)
  png(file=paste0("../R file/elbow.",unique(ny_data_yr_nor[[i]]$year),".png"), 
      width=600, height=350)
  plot(k_df$k, k_df$wss, main = unique(ny_data_yr_nor[[i]]$year),
       type="b", pch = 19, frame = FALSE,
       xlab="Number of clusters K",ylab="Total within-clusters sum of squares")
  dev.off()
  
  rm(wss, k.values, wss_values, k_df)
  
  ny.kmeans.df.tt <- 0
  ny.cluster.tt <- 0
  
  for(x in c(3:12)){
    
    temp <- temp.df %>% select("assaul","burglr","larcen","motveh","robber") %>%
      kmeans(centers = x, nstart = 25)
    ny.cluster.tt <- rbind(ny.cluster.tt, temp$centers %>% data.frame %>%
                             mutate(cluster=row.names(.), k= x))
    temp.1 <- temp.df
    temp.1$cluster <- temp$cluster
    
    temp.1.merge <- temp.1 %>%
      left_join(top10.crime %>% select(bgidfp10, crime.sum, crime.avg, rank, viol.sum, viol.avg, property.sum, property.avg)) %>%
      left_join(ny_data_yr_nor[[1]] %>% 
                  select(-c("assaul","burglr","larcen","motveh","robber","prpevt",
                            "vioevt","fdis","ethhet","year"))) %>%
      arrange(rank) 
    
    ny.kmeans.df.tt <- rbind(ny.kmeans.df.tt, temp.1.merge %>% mutate(k = x))
    
    print(x)
    
    rm(temp, temp.1)
    
    ny.cluster.tt <- ny.cluster.tt %>% filter(cluster!=0) 
    ny.kmeans.df.tt <- ny.kmeans.df.tt %>% filter(cluster!=0) 
    
    print(paste("Cluter",x,"completed."))
  }
  ny.cluster.all <- rbind(ny.cluster.all, 
                          ny.cluster.tt %>% mutate(year=unique(ny_data_yr_nor[[i]]$year)))
  ny.kmeans.df.all <- rbind(ny.kmeans.df.all, 
                            ny.kmeans.df.tt %>% mutate(year=unique(ny_data_yr_nor[[i]]$year)))
  
}
ny.cluster.all %<>% filter(year !=0)
ny.kmeans.df.all %<>% filter(bgidfp10 !=0)

save(ny.cluster.all, 
     file=paste0("../R file/ny.cluster.all.RData"))
save(ny.kmeans.df.all, 
     file=paste0("../R file/ny.kmeans.df.all.RData"))

load(file=paste0("../R file/ny.kmeans.df.all.RData"))

# Determine Crime High regions
top.cluster.crime.df <- 0
for(x in c(3:12)){
  load(file=paste0("../R file/ny.cluster.all.RData"))
  
  temp <- ny.kmeans.df.all %>% filter(k==x)
  
  top.cluster.crime <- 0
  for (i in 1:length(unique(temp$year))){
    top.cluster.crime.df <- 
      rbind(top.cluster.crime.df,
            temp %>%
              ### Filter year & top 30
              filter(year == unique(temp$year)[[i]] & rank <= 60) %>%
              ## Cluster selection
              group_by(cluster) %>% 
              summarize(cluster.count=length(unique(bgidfp10))) %>%
              mutate(k = x, year=unique(temp$year)[[i]]) %>%
              select(k, year, cluster, cluster.count))
    
    temp.1 <- temp %>%
      ### Filter year & top 30
      filter(year == unique(temp$year)[[i]] & rank <= 60) %>%
      ## Cluster selection
      group_by(cluster) %>% 
      summarize(cluster.count=length(unique(bgidfp10))) %>%
      filter(cluster.count==max(cluster.count)) %>%
      mutate(k = x, year = unique(temp$year)[[i]]) %>%
      select(k, year, cluster, cluster.count) %>%
      mutate(crime.cluster.d=1)
    top.cluster.crime <- rbind(top.cluster.crime, temp.1)
  }
  top.cluster.crime %<>% filter(year != 0)
  top.cluster.crime.df %<>% filter(year != 0)
  
  temp %<>% left_join(top.cluster.crime, by=c("k","year","cluster")) 
  temp$crime.cluster.d[is.na(temp$crime.cluster.d)] <- 0
  temp %<>% 
    mutate(crime.cluster=ifelse(crime.cluster.d==1, "CrimeCluster", cluster)) %>%
    mutate(crime.cluster.d=ifelse(crime.cluster.d==1, "CrimeCluster", "NormalCluster")) 
  
  temp %>% filter(year == unique(temp$year)[[i]]) %>%
    group_by(cluster.count, crime.cluster) %>%
    summarize_if(is.numeric, mean, na.rm=TRUE) %>%
    gather(var, value, gage29:ProOtherArea) %>%
    ggplot(aes(x=crime.cluster, y=value, fill=var)) +
    geom_bar(stat="identity", position=position_dodge())
  
  
  ny.kmeans.df.all.p <- temp
  
  save(top.cluster.crime, file=paste0("../R file/ny.",x,".top.cluster.crime.RData"))
  save(ny.kmeans.df.all.p, file=paste0("../R file/ny.",x,".kmeans.df.all.RData"))
  write.csv(ny.kmeans.df.all.p %>% select(bgidfp10, cluster, crime.cluster) %>% unique,
            file=paste0("../R file/ny.",x,"top_cluster_list_all.csv"))
  
  print(x)
}

top.cluster.crime.df %>% data.frame

# Measure descriptive statistics
# Must add time!
for(x in c(3:12)){
  load(file=paste0("../R file/ny.",x,".kmeans.df.all.RData"))

  ny.kmeans.df.all.p %>% mutate(cluster.count=x) %>% select(-c("cluster","rank")) %>%
    group_by(cluster.count, crime.cluster) %>%
    summarize_if(is.numeric, mean, na.rm=TRUE) %>%
    gather(var, value, gage29:ProOtherArea) %>%
    ggplot(aes(x=crime.cluster, y=value, fill=var)) +
    geom_bar(stat="identity", position=position_dodge())
  ggsave(paste0("../R figures/ny_cluster",x,"_summary.png"))

  print(x)
}

# Check how top 60 regions behave 
x <- 3
for(x in 3:12){
  load(file=paste0("../R file/ny.",x,".cluster.all.RData"))
  load(file=paste0("../R file/ny.",x,".kmeans.df.all.RData"))

  print(x)
  ny.kmeans.df.all.p %>% #filter(year==2015) %>%
    group_by(k, year) %>% select(bgidfp10,year,crime.cluster) %>%
    slice(1:60) %>% data.frame %>% print
}

ny.kmeans.df.tt$bgidfp10 %>% unique %>% length
ny.kmeans.df.tt %>% head(30)
hist(ny.kmeans.df.tt$cluster)

# Comparison of high and low group
temp.df.all %>% mutate(crime=assaul+burglr+larcen+motveh+robber) %>%
  group_by(bgidfp10) %>% 
  summarize(crime.sum=sum(crime), crime.avg=mean(crime)) %>% 
  mutate(crime.sum.sum=sum(crime.sum)) %>% 
  mutate(crime.prop=crime.sum/(crime.sum.sum)) %>%
  arrange(desc(crime.sum)) %>% 
  mutate(rank=dense_rank(desc(crime.sum))) %>%
  mutate(high=ifelse(rank<=30,"High","Low")) %>%
  ungroup %>% group_by(high) %>%
  summarize_if(is.numeric, mean)

#### Data for Logistic Regression
library(plm)
library(pglm)
library('pscl') # zero-inflated nb

load(file="../R file/ny_data.RData")

for (x in 3:12){
  load(file=paste0("../R file/ny.",x,".kmeans.df.all.RData"))

  top.reg <- ny.kmeans.df.all.p %>% 
    filter(rank <=60) %>%
    select(bgidfp10, year, k) %>%
    mutate(top.60 = 1)
    
  ### top column 추가
  ny.reg.sample <- ny.kmeans.df.all.p %>% 
    select(bgidfp10, year, crime.cluster) %>% unique %>% 
    left_join(ny_data, by=c("bgidfp10","year")) %>%
    mutate(crime.d = case_when(crime.cluster=="CrimeCluster" ~ "Crime",
                               crime.cluster!="CrimeCluster" ~ "noCrime")) %>%
    mutate(crime.d=as.factor(crime.d)) %>%
    mutate(crime.count = assaul+burglr+larcen+motveh+robber) %>%
    left_join(top.reg) %>% mutate(top.60 = ifelse(is.na(top.60), 0, top.60))
  write.csv(ny.reg.sample, 
            file=paste0("../R file/newset/ny.reg.sample.cluster",x,".all.csv"))
}