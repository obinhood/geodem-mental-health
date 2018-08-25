getwd()

#---- 1 Loading Libraries -----
install.packages("tidyverse")
library(tidyverse)
install.packages("corrplot")
library(corrplot)
install.packages("maptools")
install.packages("V8")
install.packages(c("classint", "OpenStreetMap", "RColorBrewer", "Sp", "rgeos", "tmap", "tmaptools", "sf", "downloader", "rgdal", "geojsonio"))
install.packages("rgdal")
install.packages("highcharter")
install.packages("rgeos")
library(maptools)
library(RColorBrewer)
library(classInt)
library(OpenStreetMap)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(rJava)
library(lattice)
library(ggplot2)
library("spgwr")
library(tidyr)
library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(highcharter)

install.packages("spgwr")

install.packages("OpenStreetMap")

#----- 2.1 Census Data Manipulation -----
######### Census 
data<- read.csv("census_norm.csv", header=TRUE, sep=",")
names(data)

mh_lsoa <-read.csv("mh_total_checked.csv",header=T)

DataLondon <- data
DataLondon <- merge(data, mh_lsoa, by.x="lsoa01cd",by.y = "lsoa01" )

names(lsoa.map@data)[79] <- c("diagnosis")




install.packages("Johnson")
library(Johnson)
# normalisation 

geodemographic3 <- geodemographic

geodemographic4 <- RE.Johnson(geodemographic3)[74]

geodemographic %>% gather() %>% head()

geodemographic3<- subset(geodemographic3, select =-c(X))

ggplot(gather(geodemographic), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

geodemographic2 <- geodemographic

geodemographic2 <- log(geodemographic2)
geodemographic2 %>% gather() %>% head()

write.csv(geodemographic2, file="census_log.csv")
geodemographic2 <- read.csv("census_log1.csv",header=T,sep=',')

ggplot(gather(geodemographics2), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

stan_data2 <- stand_data
stan_data2 <- log(stan_data2)

ggplot(gather(stand_data), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

# 2.2 Mental Health -------

lsoa_mh <- read.csv("stand_data_mhTotal.csv", header=T, sep=",")
lsoa_mh<- subset(lsoa_mh, select =-c(lsoa01nm))


# binding mh + shapefile
lsoa_test@data <- data.frame(lsoa_test@data,mh_lsoa
                            [match(lsoa_test@data[,"LSOA_CODE"],lsoa_mh[,"lsoa01cd"]),])

# change name for 
names(lsoa.map@data)[79] <- c("diagnosis")

write.csv(DataLondon, file="data_london.csv")


# read england file 
lsoa.map <- readOGR("LSOA/Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")



# binding data together 
lsoa.map@data <- data.frame(lsoa.map@data,mh_lsoa
                             [match(lsoa.map@data[,"lsoa11cd"],mh_lsoa[,"lsoa01"]),])

lsoa.map@data <-data.frame(lsoa.map@data,DataLondon
                           [match(lsoa.map@data[,"lsoa11cd"],DataLondon[,"lsoa01cd"]),])
#chaning class of variables
class(lsoa.map@data$greenspace_m2_2005) <- "integer"
class(lsoa.map@data$house_mean.price) <- "integer"
class(lsoa.map@data$X..child.tax.credit.in.lone.parent) <- "integer"
class(lsoa.map@data$Unauthorised..Absence.in.All.Schools....)<- "integer"
class(lsoa.map@data$Average.Level.3..QCDA.Point.Score.Per.Student.)<- "integer"
# getting London data only by removing NA 
lsoa.map <- lsoa.map[!is.na(lsoa.map@data$diagnosis),]

names(lsoa.map@data)
names(lsoa.map@data)[8] <- c("diagnosis")

#class
class(lsoa.map)

# save shapefile polygon for further use 
#new folder
dir.create("London_Polygon")
writeOGR(obj=lsoa.map, dsn="London_Polygon", layer="lsoa.map", driver="ESRI Shapefile")


log_mg <- log(mh_lsoa$total)
class(log_mg)
histogram(mh_lsoa$total)
histogram(log_mg)
library(shinyjs)
#it's possible to explicitly tell R which package to get the function from with the :: operator...
tmaptools::palette_explorer()

tmap_mode("view")



tm_shape(lsoa_test1) + tm_fill("mh_tot_log", palette = "Reds",
                               style = "pretty", title = "Mental Health") +tm_borders(alpha=.1)+
  tm_layout(frame = F)

tm_shape(lsoa_test1) + tm_fill("mh_tot_log", palette = "RdBu",style = "jenks",n=7) +
  tm_borders(alpha=0.1)+tm_layout(frame = F, main.title ="Mental Health Distribution", title="Log-Transformed Values",
                                  title.size=1)+tm_legend(show=T)

?tm_fill

  ?tm_polygons

palette_explorer()
#----- 3.0 moran trial model -----

# read stuff 
lsoa.imd <- readOGR(".", "LSOA_2011_London_gen_MHW")

lsoa.imd@data <- data.frame(lsoa.imd@data,DataLondon
                            [match(lsoa.imd@data[,"LSOA11CD"],DataLondon[,"lsoa11cd"]),])

# OMIT NAs !!! autocorrelatiion doesnt read 
sum(is.na(lsoa.imd@data$Index.of.Multiple.Deprivation))
orm(lsoa.imd2)
lsoa.imd2 <- lsoa.imd

lsoa.imd2 <- lsoa.imd2[!is.na(lsoa.imd2@data$Index.of.Multiple.Deprivation),]

# merging IMD + choropleth map


lsoa.deprivation <- lsoa.imd$Index.of.Multiple.Deprivation
choro.deprivation <- auto.shading(lsoa.deprivation,n=6,cols=brewer.pal(5,'Blues'))

?auto.shading

choropleth(lsoa.imd2,lsoa.deprivation,choro.deprivation)


# Crime Domain LSOA Map 

tm_shape(lsoa.imd2) + tm_fill("Crime.Domain", 
                              palette = "Blues", style = "quantile", 
                              title = "IMD Crime Domain") + tm_borders(alpha=.4)  


#neighborhood lsoa 1

lsoa.imd.nb <- poly2nb(lsoa.imd2)
lsoa.imd.nb

#plot 

plot(lsoa.imd2, border='lightgrey')
plot(lsoa.imd.nb,coordinates(lsoa.imd2),add=TRUE,col='red')


lsoa.imd.nb2 <- poly2nb(lsoa.imd2,queen=FALSE)

plot(lsoa.imd2, border='lightgrey')

# Queen Case neighborhood info as networks 

plot(lsoa.imd.nb,coordinates(lsoa.imd2),add=TRUE,col='blue',lwd=2)

# Rook's case neighborrs 

plot(lsoa.imd.nb2,coordinates(lsoa.imd2),add=TRUE,col='yellow')


# neighbor list

lsoa.imd.lw <- nb2listw(lsoa.imd.nb, zero.policy=TRUE)
lsoa.imd.lw

?nb2listw
# moran tetst 

moran.test(lsoa.imd2@data$Index.of.Multiple.Deprivation, lsoa.imd.lw)

#IMD lagged means 
crime.lagged.means <- lag.listw(lsoa.imd,lw,crime)
choropleth(lsoa1,crime.lagged.means,chorolsoa)

#choropleth 
choropleth(lsoa1,crime.lagged.means,chorolsoa)



#plot something cool



plot(crime, crime.lagged.means,asp=1,xlim=range(crime),ylim=range(crime))
abline(a=0,b=1)
abline(v=mean(crime,lty=2))
abline(h=mean(crime.lagged.means),lty=2)

moran.plot(crime,lsoa.lw)

moran.test(crime,lsoa.lw)

lsoa.imd.nb <- poly2nb(lsoa.imd)
lsoa.imd.nb


#------------3.1 Moran's Test for MH  ----- 

library(spdep)

library(tidyverse)
library(SpatialEpi)
library(GISTools)
library(rgdal)


# read england file 
lsoa_test <- readOGR("LSOA/Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")

lsoa_test <- readOGR("LSOA_2004_London_Low_Resolution.shp")  




# binding data Census + shape together 
lsoa_test@data <- data.frame(lsoa_test@data,DataLondon
                             [match(lsoa_test@data[,"LSOA_CODE"],DataLondon[,"lsoa01cd"]),])

# binding shape + mh 
lsoa_test@data <- data.frame(lsoa_test@data,lsoa_mh
                             [match(lsoa_test@data[,"LSOA_CODE"],lsoa_mh[,"lsoa01cd"]),])

lsoa_test@data<- subset(lsoa_test@data, select =-c(lsoa01cd.1))

names(lsoa_test@data)
names(lsoa_test@data)[87] <- c("diagnosis")

# getting London data only by removing NA 
lsoa.map <- lsoa.map[!is.na(lsoa.map@data$diagnosis),]




#chaning class of variables
class(lsoa_test@data$greenspace_m2_2005) <- "integer"
class(lsoa_test@data$house_mean.price) <- "integer"
class(lsoa_test@data$X..child.tax.credit.in.lone.parent) <- "integer"
class(lsoa_test@data$Unauthorised..Absence.in.All.Schools....)<- "integer"
class(lsoa_test@data$Average.Level.3..QCDA.Point.Score.Per.Student.)<- "integer"

# getting London data only by removing NA 
lsoa_test <- lsoa_test[!is.na(lsoa_test@data$diagnosis),]

#class
class(lsoa_test)

# save shapefile polygon for further use 
#new folder
dir.create("London_Polygon")
writeOGR(obj=lsoa_test, dsn="London_Polygon", layer="lsoa_test", driver="ESRI Shapefile")

# checking NA 
sum(is.na(lsoa_test@data$diagnosis))

# neigborhood check 
lsoa.test.nb <- poly2nb(lsoa_test1)
lsoa.test.nb

#removing rows with no links


lsoa.test.nb2 <- poly2nb(lsoa_test1,queen=FALSE)
lsoa.test.nb2

#neighborhood plot 
plot(lsoa_test1, border='lightgrey')
plot(lsoa.test.nb,coordinates(lsoa_test1),add=TRUE,col='red')

# list of moran 
lsoa.test.lw <- nb2listw(lsoa.test.nb, zero.policy=TRUE)
lsoa.test.lw2 <-nb2listw(lsoa.test.nb2,zero.policy=T)
?nb2listw
print(lsoa.test.lw, zero.policy=TRUE)
print(lsoa.test.lw2, zero.policy=TRUE)

diag.lagged.means <- lag.listw(lsoa.imd,lw,crime)

# moran tetst 

moran.test(lsoa_test1@data$mh_tot_log, lsoa.test.lw, zero.policy = TRUE)
moran.test(lsoa_test1@data$mh_tot_log, lsoa.test.lw2, zero.policy = TRUE)
geary.test(lsoa_test1@data$mh_tot_log, lsoa.test.lw, zero.policy = TRUE)
geary.test(lsoa_test1@data$mh_tot_log, lsoa.test.lw2, zero.policy = TRUE)

# creates a moran plot
moran.test <- moran.plot(lsoa_test1@data$mh_tot_log, 
                         listw = nb2listw(lsoa.test.nb, style = "W", zero.policy = TRUE),
                         labels=F,xlab="mh_tot_log",ylab="spatially laffed mh_tot_log",main="Moran's I plot")
?moran.plot
# local moran 
local.test <- localmoran(x = lsoa_test1@data$mh_tot_log, 
                         listw = nb2listw(lsoa.test.nb, style = "W", zero.policy = TRUE))


localg.test
dev.off()

# moran map for IMD 
moran.map <- cbind(lsoa_test1, local.test)
# ploot map 
tm_shape(moran.map) + tm_fill(col = "Ii", palette = "RdBu", style = "jenks", title = "Local Moran's Statistic")+
tm_layout(frame = F, main.title ="Local Moran's I Test")

?tm_fill
#spplot(lsoa_test, "localg", main="Local Geary's G", at=c(-4, -3,-2,-1,0,1,2,3, 4), 
#       col.regions=brewer.pal(n=8, "RdBu"))

# -----3.2 LISA Cluster Map ----
### to create LISA cluster map ### 
quadrant <- vector(mode="numeric",length=nrow(local.test))

# centers the variable of interest around its mean
mh_quandrant <- lsoa_test1@data$mh_tot_log - mean(lsoa_test1@data$mh_tot_log)

# centers the local Moran's around the mean
m.local <- local.test[,1] - mean(local.test[,1])

# significance threshold
signif <- 0.2

# builds a data quadrant
quadrant[mh_quandrant >0 & m.local>0] <- 4  
quadrant[mh_quandrant <0 & m.local<0] <- 1      
quadrant[mh_quandrant <0 & m.local>0] <- 2
quadrant[mh_quandrant >0 & m.local<0] <- 3
quadrant[local.test[,5]>signif] <- 0   

dev.off()

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(lsoa_test1,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=F)],main="Local Indicators of Spatial Association")
box()
legend("bottomright",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n",cex=0.8)

?legend
?plot

#------ 3.3 Getis-ord test ----- 

# creates centroid and joins neighbours within 0 and 800 units
nb <- dnearneigh(coordinates(lsoa_test1),0,2000)
# creates listw
nb_lw <- nb2listw(nb, style = 'B',zero.policy = TRUE)

# plot the data and neighbours
plot(lsoa_test, border = 'lightgrey')
plot(nb, coordinates(lsoa_test), add=TRUE, col = 'red')

# compute Getis-Ord Gi statistic
local_g <- localG(lsoa_test1@data$mh_tot, nb_lw)
local_g <- cbind(lsoa_test1, as.matrix(local_g))
names(local_g)
names(local_g)[85] <- "gstat"

# map the results

tm_shape(local_g) + tm_fill("gstat", palette = "RdBu", style = "jenks",title="z-scores") + tm_borders(alpha=.2)+
  tm_layout(frame = F, main.title ="Getis Ord Test",title.size=1.3,main.title.size = 1.7)+
  tm_legend(show=T)




#--------- 3.4 K- means cluster and classificarion ----- 

setwd()

geodemographic <- DataLondon

geodemographic <- geodemographic[!is.na(geodemographic$pop2011),]
#remove column
geodemographic<- subset(geodemographic, select =-c(LSOA11NM,LAD11CD,LAD11NM))


# lists the column names and positions
names(geodemographic)


# summary statistics
summary(geodemographic$ed1)

# boxplot
boxplot(geodemographic$dm1)

# histogram - renamed the x axis and turned of the title
hist(geodemographic$age0_15, xlab = "Age", main = NULL)


#standardization with z value

stand_data1 <- scale(geodemographic$pop, center = TRUE, scale = TRUE)

# or simply...
stand_data <- scale(geodemographic$pop)

#standradizet all columns
value <- colnames(geodemographic)
value
# creates a new data frame
geodemographic <- geodemographic[3:75]
stand_data <- geodemographic

install.packages("vegan")

library(cluster)
library(vegan)

dis <- vegdist(geodemographic)
res <- pam(dis,5) #choice of clustering algorithm 
sil <- silhouette (res$clustering,dis) #  cluster vector

# Summary of silhouette analysis
si.sum <- summary(sil)
# Average silhouette width of each cluster
si.sum$clus.avg.widths

# K-means clustering
km.res <- eclust(geodemographic3, "kmeans", k = 6,
                 nstart = 20, graph = F)

fviz_silhouette(km.res)

# loops columns from position 1 : the last column

for(i in 1: ncol (geodemographic)){
  stand_data[, value[i]] <- scale(as.numeric(geodemographic[, value[i]]))
}

# creates a pairwise correlation matrix for the dataframe
cor_mh <- cor(stand_data, method = "pearson")
library(corrplot)
corrplot(stand_data)

set.seed(20)
# runs k-means clustering
Km <- kmeans(stand_data, 6, nstart = 20, iter.max = 1000)
Km

?kmeans

# the cluster membership for each case
KmClusters <- as.matrix(Km$cluster)
KmClusters <- as.data.frame(KmClusters)
table(KmClusters)
# the cluster centres
KmCenters <- as.matrix(Km$centers)
KmCenters <- as.data.frame(KmCenters)

# the frequency of cases in each cluster
table(KmClusters)

# function to compute total within-cluster sum of square # creates an empty data object first
wss <- NULL

# finding the tot.withinss for 15 kmeans models
for (i in 1:14) wss[i] <- kmeans(stand_data,centers = i,iter.max = 2000)$tot.withinss

ggplot(Km, aes(1:14, tot.withinss)) +geom_line()

plot(1:14, wss, type = "b", pch = 19, xlab = "Number of Clusters", 
     ylab = "Total within-cluster sum of squares",col="#90a0c7",main="Homogeneity within Cluster Groups")
#we can also look at the betweenss values to observe how distinctive the clusters are
bss <- NULL


for (i in 1:14) bss[i] <- kmeans(stand_data,centers = i,iter.max = 2000)$betweenss

plot(1:14, bss, type = "b", pch = 19, xlab = "Number of Clusters", 
     ylab = "The between-cluster sum of squares", col="#90a0c7", main="Heterogeneity between Cluster Groups")
?plot
# Cluster Plot against 1st 2 principal components
library(cluster)

clusplot(stand_data,Km$cluster, color = TRUE, shade = FALSE,
         labels = 4, lines = 0, plotchar = FALSE)
dev.off()
library(ggplot2)
library(factoextra)
install.packages("factoextra")
install.packages("plotrix")
# this may require you to install additional packages 

fviz_cluster(Km, data = stand_data, geom = "point", ellipse = F, pointsize = 0.5, 
             ggtheme = theme_gray(),main="Cluster Plot")
?fviz_cluster


library(plotrix)
# RADIAlS SAMPLE


radial.plot(KmCenters[1,], labels = colnames(KmCenters),
            boxed.radial = FALSE, show.radial.grid = FALSE,
            line.col = "blue", radlab = TRUE )


# Creats a polygon(p) plot
radial.plot(KmCenters[1,], labels = colnames(KmCenters), boxed.radial = FALSE, show.radial.grid = TRUE,
            line.col = "blue",  radlab = TRUE,
            rp.type = "p")

# creates a object of six zeros (remember we have 6 groups)
KmCenters[7,]<- c(0)

# this reduces the size of grid and axis labels in upcoming plots

par(cex.axis = 0.5, cex.lab = 0.7)
# creates a radial plot for the two columns

# moves the grid labels to position 3 (centre to top)



# -----3.4.0 Cluster Map Overall and Individual Cluster----- 


names(DataLondon)
#Extract join the cluster labels to the first column 
Classification <- as.data.frame(cbind(as.character(DataLondon[,1]), KmClusters[,1]))

#w rename the column headers
names(Classification) <- c("lsoa11cd", "Classification")

#selection values for each cluster 

Class1 <- Classification[grep("^1",Classification[,2]),]
Class2 <- Classification[grep("^2",Classification[,2]),]
Class3 <- Classification[grep("^3",Classification[,2]),]
Class4 <- Classification[grep("^4",Classification[,2]),]
Class5 <- Classification[grep("^5",Classification[,2]),]
Class6 <- Classification[grep("^6",Classification[,2]),]

# export as a CSV
write.csv(Classification, "Classification.csv")

library("sp") 
library("rgdal") 
library("rgeos")


#load LSOA 
lsoa_classification<- lsoa_test

#Load the output area shapefiles
LSOA.Class<- merge(lsoa_classification, Classification, by.x = "LSOA_CODE", by.y = "lsoa11cd")

library(tmap)

# create overall map  map 
tm_shape(LSOA.Class) + tm_fill("Classification", palette = "Set2",
                               style = "kmeans", title = "Cluster Groups") +tm_borders(alpha=.1)+
  tm_layout(frame = T, main.title ="Geodemographic Classification",
            legend.title.size = 0.9,legend.text.size=0.5,legend.bg.alpha = 1,legend.bg.color = "white",
            main.title.size=1.2,fontfamily="Helvetica") 

tm_shape(LSOA.Class) + tm_fill("Classification", palette = "Set2",
                               style = "kmeans", title = "Geodemographics") +tm_borders(alpha=.1)+
  tm_layout(frame = F)+tm_legend(show=T)




library(leaflet)
# turns view map on, enter tmap_mode("plot") to turn it off 
tmap_mode("view") 



# ----- 3.4.1  Cluster 1 --------
par(cex.axis = 0.95, cex.lab = 0.9)
# radial 
radial.plot(KmCenters[c(1,7),], labels = colnames(KmCenters), 
            boxed.radial = FALSE, show.radial.grid = TRUE, line.col = c("#7dc0a5","red"),rad.col="grey",
            radlab = TRUE, rp.type = "p", grid.bg="#f2f2f2",lwd=3,
            show.grid.labels = 3)

?radial.plot
  # map 
cluster_map1 <- merge(lsoa_classification, Class1, by.x = "LSOA_CODE", by.y = "lsoa11cd")

# creates map 

tm1 <- tm_shape(cluster_map1) + 
  tm_fill("Classification", palette = "Set2",
                               style = "kmeans", title = "Cluster 1") +
  tm_borders(alpha=0.1)+
  tm_layout(frame = F, main.title ="Cluster 1:", title="Cluster 1",title.size=1)+
  tm_legend(show=FALSE)

tm_shape(cluster_map1) + 
  tm_fill("Classification", palette = "Set2",
          style = "kmeans", title = "Cluster 1") +
  tm_borders(alpha=0.1)+
  tm_layout(frame = F, main.title ="Cluster 1:", title="Aging Outer City",title.size=1.3,main.title.size = 1.7)+
  tm_legend(show=FALSE)


# ----- 3.4.2 Cluster 2 --------

# radial

radial.plot(KmCenters[c(2,7),], labels = colnames(KmCenters), 
            boxed.radial = FALSE, show.radial.grid = TRUE, line.col = c("#ee926b","red"),rad.col="grey",
            radlab = TRUE, rp.type = "p",grid.bg="#f2f2f2",lwd=3,
            show.grid.labels = 1)
  dev.off()
# map 

# map 
cluster_map2 <- merge(lsoa_classification, Class2, by.x = "LSOA_CODE", by.y = "lsoa11cd")

# creates map 

tm2 <- tm_shape(cluster_map2) + 
  tm_fill("Classification", palette = "Set2",
          style = "kmeans") +
  tm_borders(alpha=0.1)+
  tm_layout(frame = F, main.title ="Cluster 2", title="Cluster 2",title.size=1.3,main.title.size = 1.7,fontfamily="Helvetica")+
  tm_legend(show=FALSE)

tm_shape(cluster_map2) + 
  tm_fill("Classification", palette = "Set2",
          style = "kmeans") +
  tm_borders(alpha=0.1)+
  tm_layout(frame = F, main.title ="Cluster 2", title="Metropolitan Liberal Workers",main.title.size =1.7,title.size=1.3,fontfamily="Helvetica")+
  tm_legend(show=FALSE)
# ----- 3.4.3 Cluster 3 --------

# radial 
radial.plot(KmCenters[c(3,7),], labels = colnames(KmCenters), 
            boxed.radial = FALSE, show.radial.grid = TRUE, line.col = c("#90a0c7","red"),rad.col="grey",
            radlab = TRUE, rp.type = "p",grid.bg="#f2f2f2",lwd=3,
            show.grid.labels = 1)

  # map 
# map 
cluster_map3 <- merge(lsoa_classification, Class3, by.x = "LSOA_CODE", by.y = "lsoa11cd")

# creates map 

tm3 <-tm_shape(cluster_map3) + 
  tm_fill("Classification", palette = "Set2",
          style = "kmeans") +
  tm_borders(alpha=0.1)+
  tm_layout(frame = F, main.title ="Cluster 3", title="Cluster 3",title.size=1)+
  tm_legend(show=FALSE)

tm_shape(cluster_map3) + 
  tm_fill("Classification", palette = "Set2",
          style = "kmeans") +
  tm_borders(alpha=0.1)+
  tm_layout(frame = F, main.title ="Cluster 3", title="Urban Elites",title.size=1)+
  tm_legend(show=FALSE)

# ----- 3.4.4  Cluster 4 --------

# radial 



radial.plot(KmCenters[c(4,7),], labels = colnames(KmCenters), 
            boxed.radial = FALSE, show.radial.grid = TRUE, line.col = c("#db90c0","red"),rad.col="grey",poly.col = ,
            radlab = TRUE, rp.type = "p",grid.bg="#f2f2f2",lwd=3,
            show.grid.labels = 1)

# map 

# map 
cluster_map4 <- merge(lsoa_classification, Class4, by.x = "LSOA_CODE", by.y = "lsoa11cd")

# creates map 

tm4 <-tm_shape(cluster_map4) + 
  tm_fill("Classification", palette = "Set2",
          style = "kmeans") +
  tm_borders(alpha=0.1)+
  tm_layout(frame = F, main.title ="Cluster 4", title="Cluster 4",title.size=1)+
  tm_legend(show=FALSE)

tm_shape(cluster_map4) + 
  tm_fill("Classification", palette = "Set2",
          style = "kmeans") +
  tm_borders(alpha=0.1)+
  tm_layout(frame = F, main.title ="Cluster 4", title="Deprived Suburbs UK Born",title.size=1)+
  tm_legend(show=FALSE)

# ----- 3.4.5 Cluster 5 --------

# radial 



radial.plot(KmCenters[c(5,7),], labels = colnames(KmCenters), 
            boxed.radial = FALSE, show.radial.grid = TRUE, line.col = c("#b0d667","red"),rad.col="grey",
            radlab = TRUE, rp.type = "p",grid.bg="#f2f2f2",lwd=3,
            show.grid.labels = 1)
# map 

# map 
cluster_map5 <- merge(lsoa_classification, Class5, by.x = "LSOA_CODE", by.y = "lsoa11cd")

# creates map 

tm_shape(cluster_map5) + 
  tm_fill("Classification", palette = "Set2",
          style = "kmeans") +
  tm_borders(alpha=0.1)+
  tm_layout(frame = F, main.title ="Cluster 5", title="Multi-Ethnic Suburbs",title.size=1)+
  tm_legend(show=FALSE)

tm_shape(cluster_map5) + 
  tm_fill("Classification", palette = "Set2",
          style = "kmeans") +
  tm_borders(alpha=0.1)+
  tm_layout(frame = F, main.title ="Cluster 5", title="Cluster 5",title.size=1)+
  tm_legend(show=FALSE)

# ----- 3.4.6 Cluster 6 --------

# radial 


radial.plot(KmCenters[c(6,7),], labels = colnames(KmCenters), 
            boxed.radial = FALSE, show.radial.grid = TRUE, line.col = c("#fad956","red"),rad.col="grey",
            radlab = TRUE, rp.type = "p",grid.bg="#f2f2f2",lwd=3,
            show.grid.labels = 1)

# map 

# map 
cluster_map6 <- merge(lsoa_classification, Class6, by.x = "LSOA_CODE", by.y = "lsoa11cd")

# creates map 

tm6 <-tm_shape(cluster_map6) + tm_fill("Classification", palette = "Set2",style = "kmeans") + 
  tm_borders(alpha=0.1)+tm_layout(frame = F, main.title ="Cluster 6", 
                                  title="Cluster 6",title.size=1)+tm_legend(show=FALSE)

 
tm_shape(cluster_map6) + tm_fill("Classification", palette = "Set2",style = "kmeans") + 
  tm_borders(alpha=0.1)+tm_layout(frame = F, main.title ="Cluster 6", 
                                  title="High Density Deprived Mixed Ethnicity",title.size=1)+tm_legend(show=FALSE)


#------- 3.4.7 Cluster Maps ======
tmap_arrange(tm1, tm2, tm3, tm4, tm5, tm6)
tmap_arrange(tm0, tm1, tm2)


#---- 3.4.8. testing -----

hist(stand_data$ed2)

# ----- 4.0.1 Linear Model + Residuals   ------ 

# runs a linear model 

# acquire data 
lsoa_test1 <- readOGR("LSOA_2004_London_Low_Resolution.shp")  
lsoa_test3 <- lsoa_test1
gwr_lsoa <- as.data.frame(cbind(DataLondon[,1],stand_data))

names(lsoa_test1@data)                   
names(gwr_lsoa)[1] <- c("lsoa01cd")

gwr_lsoa <- merge(gwr_lsoa,lsoa_mh,by="lsoa01cd")
gwr_lsoa <-merge(gwr_lsoa,Classification,by.x="lsoa01cd",by.y="lsoa11cd")

lsoa_test1@data <- cbind(lsoa_test1@data,geodemographics2)
lsoa_test1@data <-cbind(lsoa_test1@data,lsoa_mh)
lsoa_test1@data <-cbind(lsoa_test1@data,log_lsoa_mh)
# linear model regression 



model_geod0 <- lm(lsoa_test1$mh_tot_log~
                 lsoa_test1$age0_15
                 +lsoa_test1$age16_29
                 +lsoa_test1$age30_44
                 +lsoa_test1$age45_64
                 +lsoa_test1$age65_
                 +lsoa_test1$pop_per_h
                 +lsoa_test1$dm1
                 +lsoa_test1$dm2
                 +lsoa_test1$dm3
                 +lsoa_test1$dm4
                 +lsoa_test1$dm5
                 +lsoa_test1$eth1
                 +lsoa_test1$eth2
                 +lsoa_test1$eth3
                 +lsoa_test1$eth4
                 +lsoa_test1$eth5
                 +lsoa_test1$birth1
                 +lsoa_test1$birth2
                 +lsoa_test1$rel1
                 +lsoa_test1$rel2
                 +lsoa_test1$rel3
                 +lsoa_test1$rel4
                 +lsoa_test1$rel5
                 +lsoa_test1$rel6
                 +lsoa_test1$rel7
                 +lsoa_test1$rel8
                 +lsoa_test1$rel9
                 +lsoa_test1$ten1
                 +lsoa_test1$ten2
                 +lsoa_test1$ten3
                 +lsoa_test1$ten4
                 +lsoa_test1$house_price
                 +lsoa_test1$crim_tot
                 +lsoa_test1$crim1
                 +lsoa_test1$crim2
                 +lsoa_test1$crim3
                 +lsoa_test1$crim4
                 +lsoa_test1$crim5
                 +lsoa_test1$crim6
                 +lsoa_test1$em_n_w_child
                 +lsoa_test1$em_n_l
                 +lsoa_test1$em1
                 +lsoa_test1$em2
                 +lsoa_test1$em3
                 +lsoa_test1$in1
                 +lsoa_test1$in2
                 +lsoa_test1$in3
                 +lsoa_test1$bad_health
                 +lsoa_test1$green_space
                 +lsoa_test1$poll_PM10
                 +lsoa_test1$poll_Nox
                 +lsoa_test1$poll_NO2
                 +lsoa_test1$poll_index
                 +lsoa_test1$acc1
                 +lsoa_test1$acc2
                 +lsoa_test1$acc3
                 +lsoa_test1$acc4
                 +lsoa_test1$acc5
                 +lsoa_test1$acc6
                 +lsoa_test1$acc7
                 +lsoa_test1$acc8
                 +lsoa_test1$in4
                 +lsoa_test1$in5
                 +lsoa_test1$ed_ab
                 +lsoa_test1$ed0
                 +lsoa_test1$ed1
                 +lsoa_test1$ed2
                 +lsoa_test1$ed2a
                 +lsoa_test1$ed3
                 +lsoa_test1$ed4
                 +lsoa_test1$ed5
                 )
                

plot(model_geod0,v)

summary(model_geod0)
# use the par function if we want to plot them in a 2x2 frame
par(mfrow=c(2,2))
dev.off()
par(mfrow)
plot(model_geod)



hist(lsoa_mh$mh_tot)
lsoa_mh2 <- read.csv("stand_data_mhTotal.csv",header=T,sep=",")
log_lsoa_mh <-as.data.frame(log(lsoa_mh2$mh_crude_tot))
names(log_lsoa_mh)[1] <- "mh_tot_log"

hist(lsoa_mh2$mh_crude_tot)
hist(lsoa_mh$mh_tot)
hist(log_lsoa_mh$mh_tot_log)
dev.off()

# mapping
resids_geod0 <-residuals(model_geod0)

map.resids_geod0 <- cbind(lsoa_test1, resids_geod0) 
# rename the column header from the resids file - in this case its the 21th column of map.resids

names(map.resids_geod0)
names(map.resids_geod0)[85] <- "resids"

# maps the residuals using the quickmap function from tmap
qtm(map.resids_geod0, fill = "resids")

gwr <- tm_shape(map.resids_geod) + tm_fill("resids", palette = "Set2",
                                      style = "quantile", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)

tm_shape(map.resids_geod0) + tm_fill("resids", palette = "Reds",
                                    style = "quantile", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


 #----- 4.0.2 Geographcail Weighted Regression -----

GWRbandwidth_geod0 <- gwr.sel(lsoa_test1$mh_tot_log~
                               lsoa_test1$age0_15
                             +lsoa_test1$age16_29
                             +lsoa_test1$age30_44
                             +lsoa_test1$age45_64
                             +lsoa_test1$age65_
                             +lsoa_test1$pop_per_h
                             +lsoa_test1$dm1
                             +lsoa_test1$dm2
                             +lsoa_test1$dm3
                             +lsoa_test1$dm4
                             +lsoa_test1$dm5
                             +lsoa_test1$eth1
                             +lsoa_test1$eth2
                             +lsoa_test1$eth3
                             +lsoa_test1$eth4
                             +lsoa_test1$eth5
                             +lsoa_test1$birth1
                             +lsoa_test1$birth2
                             +lsoa_test1$rel1
                             +lsoa_test1$rel2
                             +lsoa_test1$rel3
                             +lsoa_test1$rel4
                             +lsoa_test1$rel5
                             +lsoa_test1$rel6
                             +lsoa_test1$rel7
                             +lsoa_test1$rel8
                             +lsoa_test1$rel9
                             +lsoa_test1$ten1
                             +lsoa_test1$ten2
                             +lsoa_test1$ten3
                             +lsoa_test1$ten4
                             +lsoa_test1$house_price
                             +lsoa_test1$crim_tot
                             +lsoa_test1$crim1
                             +lsoa_test1$crim2
                             +lsoa_test1$crim3
                             +lsoa_test1$crim4
                             +lsoa_test1$crim5
                             +lsoa_test1$crim6
                             +lsoa_test1$em_n_w_child
                             +lsoa_test1$em_n_l
                             +lsoa_test1$em1
                             +lsoa_test1$em2
                             +lsoa_test1$em3
                             +lsoa_test1$in1
                             +lsoa_test1$in2
                             +lsoa_test1$in3
                             +lsoa_test1$bad_health
                             +lsoa_test1$green_space
                             +lsoa_test1$poll_PM10
                             +lsoa_test1$poll_Nox
                             +lsoa_test1$poll_NO2
                             +lsoa_test1$poll_index
                             +lsoa_test1$acc1
                             +lsoa_test1$acc2
                             +lsoa_test1$acc3
                             +lsoa_test1$acc4
                             +lsoa_test1$acc5
                             +lsoa_test1$acc6
                             +lsoa_test1$acc7
                             +lsoa_test1$acc8
                             +lsoa_test1$in4
                             +lsoa_test1$in5
                             +lsoa_test1$ed_ab
                             +lsoa_test1$ed0
                             +lsoa_test1$ed1
                             +lsoa_test1$ed2
                             +lsoa_test1$ed2a
                             +lsoa_test1$ed3
                             +lsoa_test1$ed4
                             +lsoa_test1$ed5,
                            data=lsoa_test1,adapt=T)
  
  #run the gwr model
gwr.model0 = gwr(lsoa_test1$mh_tot~
                  lsoa_test1$age0_15
                +lsoa_test1$age16_29
                +lsoa_test1$age30_44
                +lsoa_test1$age45_64
                +lsoa_test1$age65_
                +lsoa_test1$pop_per_h
                +lsoa_test1$dm1
                +lsoa_test1$dm2
                +lsoa_test1$dm3
                +lsoa_test1$dm4
                +lsoa_test1$dm5
                +lsoa_test1$eth1
                +lsoa_test1$eth2
                +lsoa_test1$eth3
                +lsoa_test1$eth4
                +lsoa_test1$eth5
                +lsoa_test1$birth1
                +lsoa_test1$birth2
                +lsoa_test1$rel1
                +lsoa_test1$rel2
                +lsoa_test1$rel3
                +lsoa_test1$rel4
                +lsoa_test1$rel5
                +lsoa_test1$rel6
                +lsoa_test1$rel7
                +lsoa_test1$rel8
                +lsoa_test1$rel9
                +lsoa_test1$ten1
                +lsoa_test1$ten2
                +lsoa_test1$ten3
                +lsoa_test1$ten4
                +lsoa_test1$house_price
                +lsoa_test1$crim_tot
                +lsoa_test1$crim1
                +lsoa_test1$crim2
                +lsoa_test1$crim3
                +lsoa_test1$crim4
                +lsoa_test1$crim5
                +lsoa_test1$crim6
                +lsoa_test1$em_n_w_child
                +lsoa_test1$em_n_l
                +lsoa_test1$em1
                +lsoa_test1$em2
                +lsoa_test1$em3
                +lsoa_test1$in1
                +lsoa_test1$in2
                +lsoa_test1$in3
                +lsoa_test1$bad_health
                +lsoa_test1$green_space
                +lsoa_test1$poll_PM10
                +lsoa_test1$poll_Nox
                +lsoa_test1$poll_NO2
                +lsoa_test1$poll_index
                +lsoa_test1$acc1
                +lsoa_test1$acc2
                +lsoa_test1$acc3
                +lsoa_test1$acc4
                +lsoa_test1$acc5
                +lsoa_test1$acc6
                +lsoa_test1$acc7
                +lsoa_test1$acc8
                +lsoa_test1$in4
                +lsoa_test1$in5
                +lsoa_test1$ed_ab
                +lsoa_test1$ed0
                +lsoa_test1$ed1
                +lsoa_test1$ed2
                +lsoa_test1$ed2a
                +lsoa_test1$ed3
                +lsoa_test1$ed4
                +lsoa_test1$ed5,
                data=lsoa_test1, adapt=GWRbandwidth_geod0, hatmatrix=TRUE, se.fit=TRUE)

  #print the results of the model
gwr.model0


results_geod0 <-as.data.frame(gwr.model0$SDF)

names(results_geod)
head(results_geod)

gwr.map0 <- cbind(lsoa_test1, as.matrix(results_geod0))

qtm(gwr.map0, fill = "localR2")


tm_shape(gwr.map0) + tm_fill("localR2", palette = "Reds",
                             style = "quantile", title = "localR2") +tm_borders(alpha=.1)+
  tm_layout(frame = T)



# 4.1.1 GWE + Reg Cluster 1 -----


gwr_cluster1 <- lsoa_test1

gwr_cluster1@data <- data.frame(gwr_cluster1@data,Class1
                             [match(gwr_cluster1@data[,"LSOA_CODE"],Class1[,"lsoa11cd"]),])
?match
gwr_cluster1 <- gwr_cluster1[!is.na(gwr_cluster1@data$Classification),]

options(scipen = 999)

model_geod1 <- lm(gwr_cluster1$mh_tot_log~
                   gwr_cluster1$age0_15
                 +gwr_cluster1$age16_29
                 +gwr_cluster1$age30_44
                 +gwr_cluster1$age45_64
                 +gwr_cluster1$age65_
                 +gwr_cluster1$pop_per_h
                 +gwr_cluster1$dm1
                 +gwr_cluster1$dm2
                 +gwr_cluster1$dm3
                 +gwr_cluster1$dm4
                 +gwr_cluster1$dm5
                 +gwr_cluster1$eth1
                 +gwr_cluster1$eth2
                 +gwr_cluster1$eth3
                 +gwr_cluster1$eth4
                 +gwr_cluster1$eth5
                 +gwr_cluster1$birth1
                 +gwr_cluster1$birth2
                 +gwr_cluster1$rel1
                 +gwr_cluster1$rel2
                 +gwr_cluster1$rel3
                 +gwr_cluster1$rel4
                 +gwr_cluster1$rel5
                 +gwr_cluster1$rel6
                 +gwr_cluster1$rel7
                 +gwr_cluster1$rel8
                 +gwr_cluster1$rel9
                 +gwr_cluster1$ten1
                 +gwr_cluster1$ten2
                 +gwr_cluster1$ten3
                 +gwr_cluster1$ten4
                 +gwr_cluster1$house_price
                 +gwr_cluster1$crim_tot
                 +gwr_cluster1$crim1
                 +gwr_cluster1$crim2
                 +gwr_cluster1$crim3
                 +gwr_cluster1$crim4
                 +gwr_cluster1$crim5
                 +gwr_cluster1$crim6
                 +gwr_cluster1$em_n_w_child
                 +gwr_cluster1$em_n_l
                 +gwr_cluster1$em1
                 +gwr_cluster1$em2
                 +gwr_cluster1$em3
                 +gwr_cluster1$in1
                 +gwr_cluster1$in2
                 +gwr_cluster1$in3
                 +gwr_cluster1$bad_health
                 +gwr_cluster1$green_space
                 +gwr_cluster1$poll_PM10
                 +gwr_cluster1$poll_Nox
                 +gwr_cluster1$poll_NO2
                 +gwr_cluster1$poll_index
                 +gwr_cluster1$acc1
                 +gwr_cluster1$acc2
                 +gwr_cluster1$acc3
                 +gwr_cluster1$acc4
                 +gwr_cluster1$acc5
                 +gwr_cluster1$acc6
                 +gwr_cluster1$acc7
                 +gwr_cluster1$acc8
                 +gwr_cluster1$in4
                 +gwr_cluster1$in5
                 +gwr_cluster1$ed_ab
                 +gwr_cluster1$ed0
                 +gwr_cluster1$ed1
                 +gwr_cluster1$ed2
                 +gwr_cluster1$ed2a
                 +gwr_cluster1$ed3
                 +gwr_cluster1$ed4
                 +gwr_cluster1$ed5
)


plot(model_geod)


# use the par function if we want to plot them in a 2x2 frame
par(mfrow=c(2,2))
par(mfrow)
plot(model_geod)



# mapping
resids_geod1 <-residuals(model_geod1)

map.resids_geod1 <- cbind(gwr_cluster1, resids_geod1) 
# rename the column header from the resids file - in this case its the 21th column of map.resids

names(map.resids_geod1)
names(map.resids_geod1)[88] <- "resids"

# maps the residuals using the quickmap function from tmap
qtm(map.resids_geod1, fill = "resids")

gwr1 <- tm_shape(map.resids_geod1) + tm_fill("resids", palette = "Reds",
                                           style = "quantile", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)

tm_shape(map.resids_geod1) + tm_fill("resids", palette = "Reds",
                                    style = "pretty", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


#----- 4.1.2 Geographcail Weighted Regression -----

GWRbandwidth_geod1 <- gwr.sel(gwr_cluster1$mh_tot_log~
                                gwr_cluster1$age0_15
                              +gwr_cluster1$age16_29
                              +gwr_cluster1$age30_44
                              +gwr_cluster1$age45_64
                              +gwr_cluster1$age65_
                              +gwr_cluster1$pop_per_h
                              +gwr_cluster1$dm1
                              +gwr_cluster1$dm2
                              +gwr_cluster1$dm3
                              +gwr_cluster1$dm4
                              +gwr_cluster1$dm5
                              +gwr_cluster1$eth1
                              +gwr_cluster1$eth2
                              +gwr_cluster1$eth3
                              +gwr_cluster1$eth4
                              +gwr_cluster1$eth5
                              +gwr_cluster1$birth1
                              +gwr_cluster1$birth2
                              +gwr_cluster1$rel1
                              +gwr_cluster1$rel2
                              +gwr_cluster1$rel3
                              +gwr_cluster1$rel4
                              +gwr_cluster1$rel5
                              +gwr_cluster1$rel6
                              +gwr_cluster1$rel7
                              +gwr_cluster1$rel8
                              +gwr_cluster1$rel9
                              +gwr_cluster1$ten1
                              +gwr_cluster1$ten2
                              +gwr_cluster1$ten3
                              +gwr_cluster1$ten4
                              +gwr_cluster1$house_price
                              +gwr_cluster1$crim_tot
                              +gwr_cluster1$crim1
                              +gwr_cluster1$crim2
                              +gwr_cluster1$crim3
                              +gwr_cluster1$crim4
                              +gwr_cluster1$crim5
                              +gwr_cluster1$crim6
                              +gwr_cluster1$em_n_w_child
                              +gwr_cluster1$em_n_l
                              +gwr_cluster1$em1
                              +gwr_cluster1$em2
                              +gwr_cluster1$em3
                              +gwr_cluster1$in1
                              +gwr_cluster1$in2
                              +gwr_cluster1$in3
                              +gwr_cluster1$bad_health
                              +gwr_cluster1$green_space
                              +gwr_cluster1$poll_PM10
                              +gwr_cluster1$poll_Nox
                              +gwr_cluster1$poll_NO2
                              +gwr_cluster1$poll_index
                              +gwr_cluster1$acc1
                              +gwr_cluster1$acc2
                              +gwr_cluster1$acc3
                              +gwr_cluster1$acc4
                              +gwr_cluster1$acc5
                              +gwr_cluster1$acc6
                              +gwr_cluster1$acc7
                              +gwr_cluster1$acc8
                              +gwr_cluster1$in4
                              +gwr_cluster1$in5
                              +gwr_cluster1$ed_ab
                              +gwr_cluster1$ed0
                              +gwr_cluster1$ed1
                              +gwr_cluster1$ed2
                              +gwr_cluster1$ed2a
                              +gwr_cluster1$ed3
                              +gwr_cluster1$ed4
                              +gwr_cluster1$ed5,
                             data=gwr_cluster1,adapt=T)

#run the gwr model
gwr.model1 = gwr(gwr_cluster1$mh_tot_log~
                   gwr_cluster1$age0_15
                 +gwr_cluster1$age16_29
                 +gwr_cluster1$age30_44
                 +gwr_cluster1$age45_64
                 +gwr_cluster1$age65_
                 +gwr_cluster1$pop_per_h
                 +gwr_cluster1$dm1
                 +gwr_cluster1$dm2
                 +gwr_cluster1$dm3
                 +gwr_cluster1$dm4
                 +gwr_cluster1$dm5
                 +gwr_cluster1$eth1
                 +gwr_cluster1$eth2
                 +gwr_cluster1$eth3
                 +gwr_cluster1$eth4
                 +gwr_cluster1$eth5
                 +gwr_cluster1$birth1
                 +gwr_cluster1$birth2
                 +gwr_cluster1$rel1
                 +gwr_cluster1$rel2
                 +gwr_cluster1$rel3
                 +gwr_cluster1$rel4
                 +gwr_cluster1$rel5
                 +gwr_cluster1$rel6
                 +gwr_cluster1$rel7
                 +gwr_cluster1$rel8
                 +gwr_cluster1$rel9
                 +gwr_cluster1$ten1
                 +gwr_cluster1$ten2
                 +gwr_cluster1$ten3
                 +gwr_cluster1$ten4
                 +gwr_cluster1$house_price
                 +gwr_cluster1$crim_tot
                 +gwr_cluster1$crim1
                 +gwr_cluster1$crim2
                 +gwr_cluster1$crim3
                 +gwr_cluster1$crim4
                 +gwr_cluster1$crim5
                 +gwr_cluster1$crim6
                 +gwr_cluster1$em_n_w_child
                 +gwr_cluster1$em_n_l
                 +gwr_cluster1$em1
                 +gwr_cluster1$em2
                 +gwr_cluster1$em3
                 +gwr_cluster1$in1
                 +gwr_cluster1$in2
                 +gwr_cluster1$in3
                 +gwr_cluster1$bad_health
                 +gwr_cluster1$green_space
                 +gwr_cluster1$poll_PM10
                 +gwr_cluster1$poll_Nox
                 +gwr_cluster1$poll_NO2
                 +gwr_cluster1$poll_index
                 +gwr_cluster1$acc1
                 +gwr_cluster1$acc2
                 +gwr_cluster1$acc3
                 +gwr_cluster1$acc4
                 +gwr_cluster1$acc5
                 +gwr_cluster1$acc6
                 +gwr_cluster1$acc7
                 +gwr_cluster1$acc8
                 +gwr_cluster1$in4
                 +gwr_cluster1$in5
                 +gwr_cluster1$ed_ab
                 +gwr_cluster1$ed0
                 +gwr_cluster1$ed1
                 +gwr_cluster1$ed2
                 +gwr_cluster1$ed2a
                 +gwr_cluster1$ed3
                 +gwr_cluster1$ed4
                 +gwr_cluster1$ed5,
                data=gwr_cluster1, adapt=GWRbandwidth_geod1, hatmatrix=TRUE, se.fit=TRUE)

#print the results of the model
gwr.model1


results_geod1 <-as.data.frame(gwr.model1$SDF)

names(results_geod1)
head(results_geod1)

gwr.map1 <- cbind(gwr_cluster1, as.matrix(results_geod1))

qtm(gwr.map1, fill = "localR2")

tm_shape(gwr.map1) + tm_fill("localR2", palette = "Reds",
                                     style = "quantile", title = "localR2") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


# 4.2.1 GWE + Reg Cluster 2 -----


gwr_cluster2 <- lsoa_test1

gwr_cluster2@data <- data.frame(gwr_cluster2@data,Class2
                                [match(gwr_cluster2@data[,"LSOA_CODE"],Class2[,"lsoa11cd"]),])
?match
gwr_cluster2 <- gwr_cluster2[!is.na(gwr_cluster2@data$Classification),]

names(gwr_cluster2)

model_geod2 <- lm(gwr_cluster2$mh_tot_log~
                    gwr_cluster2$age0_15
                  +gwr_cluster2$age16_29
                  +gwr_cluster2$age30_44
                  +gwr_cluster2$age45_64
                  +gwr_cluster2$age65_
                  +gwr_cluster2$pop_per_h
                  +gwr_cluster2$dm1
                  +gwr_cluster2$dm2
                  +gwr_cluster2$dm3
                  +gwr_cluster2$dm4
                  +gwr_cluster2$dm5
                  +gwr_cluster2$eth1
                  +gwr_cluster2$eth2
                  +gwr_cluster2$eth3
                  +gwr_cluster2$eth4
                  +gwr_cluster2$eth5
                  +gwr_cluster2$birth1
                  +gwr_cluster2$birth2
                  +gwr_cluster2$rel1
                  +gwr_cluster2$rel2
                  +gwr_cluster2$rel3
                  +gwr_cluster2$rel4
                  +gwr_cluster2$rel5
                  +gwr_cluster2$rel6
                  +gwr_cluster2$rel7
                  +gwr_cluster2$rel8
                  +gwr_cluster2$rel9
                  +gwr_cluster2$ten1
                  +gwr_cluster2$ten2
                  +gwr_cluster2$ten3
                  +gwr_cluster2$ten4
                  +gwr_cluster2$house_price
                  +gwr_cluster2$crim_tot
                  +gwr_cluster2$crim1
                  +gwr_cluster2$crim2
                  +gwr_cluster2$crim3
                  +gwr_cluster2$crim4
                  +gwr_cluster2$crim5
                  +gwr_cluster2$crim6
                  +gwr_cluster2$em_n_w_child
                  +gwr_cluster2$em_n_l
                  +gwr_cluster2$em1
                  +gwr_cluster2$em2
                  +gwr_cluster2$em3
                  +gwr_cluster2$in1
                  +gwr_cluster2$in2
                  +gwr_cluster2$in3
                  +gwr_cluster2$bad_health
                  +gwr_cluster2$green_space
                  +gwr_cluster2$poll_PM10
                  +gwr_cluster2$poll_Nox
                  +gwr_cluster2$poll_NO2
                  +gwr_cluster2$poll_index
                  +gwr_cluster2$acc1
                  +gwr_cluster2$acc2
                  +gwr_cluster2$acc3
                  +gwr_cluster2$acc4
                  +gwr_cluster2$acc5
                  +gwr_cluster2$acc6
                  +gwr_cluster2$acc7
                  +gwr_cluster2$acc8
                  +gwr_cluster2$in4
                  +gwr_cluster2$in5
                  +gwr_cluster2$ed_ab
                  +gwr_cluster2$ed0
                  +gwr_cluster2$ed1
                  +gwr_cluster2$ed2
                  +gwr_cluster2$ed2a
                  +gwr_cluster2$ed3
                  +gwr_cluster2$ed4
                  +gwr_cluster2$ed5
)


plot(model_geod2)


# use the par function if we want to plot them in a 2x2 frame
par(mfrow=c(2,2))
par(mfrow)
plot(model_geod)



# mapping
resids_geod2 <-residuals(model_geod2)

map.resids_geod2 <- cbind(gwr_cluster2, resids_geod2) 
# rename the column header from the resids file - in this case its the 21th column of map.resids

names(map.resids_geod2)
names(map.resids_geod2)[88] <- "resids"

# maps the residuals using the quickmap function from tmap
qtm(map.resids_geod2, fill = "resids")

gwr2 <- tm_shape(map.resids_geod2) + tm_fill("resids", palette = "Reds",
                                             style = "quantile", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)

tm_shape(map.resids_geod2) + tm_fill("resids", palette = "Reds",
                                     style = "pretty", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


#----- 4.2.2 Geographcail Weighted Regression -----

GWRbandwidth_geod2 <- gwr.sel(gwr_cluster2$mh_tot_log~
                                gwr_cluster2$age0_15
                              +gwr_cluster2$age16_29
                              +gwr_cluster2$age30_44
                              +gwr_cluster2$age45_64
                              +gwr_cluster2$age65_
                              +gwr_cluster2$pop_per_h
                              +gwr_cluster2$dm1
                              +gwr_cluster2$dm2
                              +gwr_cluster2$dm3
                              +gwr_cluster2$dm4
                              +gwr_cluster2$dm5
                              +gwr_cluster2$eth1
                              +gwr_cluster2$eth2
                              +gwr_cluster2$eth3
                              +gwr_cluster2$eth4
                              +gwr_cluster2$eth5
                              +gwr_cluster2$birth1
                              +gwr_cluster2$birth2
                              +gwr_cluster2$rel1
                              +gwr_cluster2$rel2
                              +gwr_cluster2$rel3
                              +gwr_cluster2$rel4
                              +gwr_cluster2$rel5
                              +gwr_cluster2$rel6
                              +gwr_cluster2$rel7
                              +gwr_cluster2$rel8
                              +gwr_cluster2$rel9
                              +gwr_cluster2$ten1
                              +gwr_cluster2$ten2
                              +gwr_cluster2$ten3
                              +gwr_cluster2$ten4
                              +gwr_cluster2$house_price
                              +gwr_cluster2$crim_tot
                              +gwr_cluster2$crim1
                              +gwr_cluster2$crim2
                              +gwr_cluster2$crim3
                              +gwr_cluster2$crim4
                              +gwr_cluster2$crim5
                              +gwr_cluster2$crim6
                              +gwr_cluster2$em_n_w_child
                              +gwr_cluster2$em_n_l
                              +gwr_cluster2$em1
                              +gwr_cluster2$em2
                              +gwr_cluster2$em3
                              +gwr_cluster2$in1
                              +gwr_cluster2$in2
                              +gwr_cluster2$in3
                              +gwr_cluster2$bad_health
                              +gwr_cluster2$green_space
                              +gwr_cluster2$poll_PM10
                              +gwr_cluster2$poll_Nox
                              +gwr_cluster2$poll_NO2
                              +gwr_cluster2$poll_index
                              +gwr_cluster2$acc1
                              +gwr_cluster2$acc2
                              +gwr_cluster2$acc3
                              +gwr_cluster2$acc4
                              +gwr_cluster2$acc5
                              +gwr_cluster2$acc6
                              +gwr_cluster2$acc7
                              +gwr_cluster2$acc8
                              +gwr_cluster2$in4
                              +gwr_cluster2$in5
                              +gwr_cluster2$ed_ab
                              +gwr_cluster2$ed0
                              +gwr_cluster2$ed1
                              +gwr_cluster2$ed2
                              +gwr_cluster2$ed2a
                              +gwr_cluster2$ed3
                              +gwr_cluster2$ed4
                              +gwr_cluster2$ed5,
                              data=gwr_cluster2,adapt=T)

#run the gwr model
gwr.model2 = gwr(gwr_cluster2$mh_tot_log~
                   gwr_cluster2$age0_15
                 +gwr_cluster2$age16_29
                 +gwr_cluster2$age30_44
                 +gwr_cluster2$age45_64
                 +gwr_cluster2$age65_
                 +gwr_cluster2$pop_per_h
                 +gwr_cluster2$dm1
                 +gwr_cluster2$dm2
                 +gwr_cluster2$dm3
                 +gwr_cluster2$dm4
                 +gwr_cluster2$dm5
                 +gwr_cluster2$eth1
                 +gwr_cluster2$eth2
                 +gwr_cluster2$eth3
                 +gwr_cluster2$eth4
                 +gwr_cluster2$eth5
                 +gwr_cluster2$birth1
                 +gwr_cluster2$birth2
                 +gwr_cluster2$rel1
                 +gwr_cluster2$rel2
                 +gwr_cluster2$rel3
                 +gwr_cluster2$rel4
                 +gwr_cluster2$rel5
                 +gwr_cluster2$rel6
                 +gwr_cluster2$rel7
                 +gwr_cluster2$rel8
                 +gwr_cluster2$rel9
                 +gwr_cluster2$ten1
                 +gwr_cluster2$ten2
                 +gwr_cluster2$ten3
                 +gwr_cluster2$ten4
                 +gwr_cluster2$house_price
                 +gwr_cluster2$crim_tot
                 +gwr_cluster2$crim1
                 +gwr_cluster2$crim2
                 +gwr_cluster2$crim3
                 +gwr_cluster2$crim4
                 +gwr_cluster2$crim5
                 +gwr_cluster2$crim6
                 +gwr_cluster2$em_n_w_child
                 +gwr_cluster2$em_n_l
                 +gwr_cluster2$em1
                 +gwr_cluster2$em2
                 +gwr_cluster2$em3
                 +gwr_cluster2$in1
                 +gwr_cluster2$in2
                 +gwr_cluster2$in3
                 +gwr_cluster2$bad_health
                 +gwr_cluster2$green_space
                 +gwr_cluster2$poll_PM10
                 +gwr_cluster2$poll_Nox
                 +gwr_cluster2$poll_NO2
                 +gwr_cluster2$poll_index
                 +gwr_cluster2$acc1
                 +gwr_cluster2$acc2
                 +gwr_cluster2$acc3
                 +gwr_cluster2$acc4
                 +gwr_cluster2$acc5
                 +gwr_cluster2$acc6
                 +gwr_cluster2$acc7
                 +gwr_cluster2$acc8
                 +gwr_cluster2$in4
                 +gwr_cluster2$in5
                 +gwr_cluster2$ed_ab
                 +gwr_cluster2$ed0
                 +gwr_cluster2$ed1
                 +gwr_cluster2$ed2
                 +gwr_cluster2$ed2a
                 +gwr_cluster2$ed3
                 +gwr_cluster2$ed4
                 +gwr_cluster2$ed5,
                 data=gwr_cluster2, adapt=GWRbandwidth_geod2, hatmatrix=TRUE, se.fit=TRUE)

#print the results of the model
gwr.model2


results_geod2 <-as.data.frame(gwr.model2$SDF)

names(results_geod1)
head(results_geod1)

gwr.map2 <- cbind(gwr_cluster2, as.matrix(results_geod2))

qtm(gwr.map2, fill = "localR2")

tm_shape(gwr.map2) + tm_fill("localR2", palette = "Reds",
                             style = "quantile", title = "localR2") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


# 4.3.1 GWE + Reg Cluster 3 -----


gwr_cluster3 <- lsoa_test1

gwr_cluster3@data <- data.frame(gwr_cluster3@data,Class3
                                [match(gwr_cluster3@data[,"LSOA_CODE"],Class3[,"lsoa11cd"]),])
?match
gwr_cluster3 <- gwr_cluster3[!is.na(gwr_cluster3@data$Classification),]
names(gwr_cluster3)


model_geod3 <- lm(gwr_cluster3$mh_tot_log~
                    gwr_cluster3$age0_15
                  +gwr_cluster3$age16_29
                  +gwr_cluster3$age30_44
                  +gwr_cluster3$age45_64
                  +gwr_cluster3$age65_
                  +gwr_cluster3$pop_per_h
                  +gwr_cluster3$dm1
                  +gwr_cluster3$dm2
                  +gwr_cluster3$dm3
                  +gwr_cluster3$dm4
                  +gwr_cluster3$dm5
                  +gwr_cluster3$eth1
                  +gwr_cluster3$eth2
                  +gwr_cluster3$eth3
                  +gwr_cluster3$eth4
                  +gwr_cluster3$eth5
                  +gwr_cluster3$birth1
                  +gwr_cluster3$birth2
                  +gwr_cluster3$rel1
                  +gwr_cluster3$rel2
                  +gwr_cluster3$rel3
                  +gwr_cluster3$rel4
                  +gwr_cluster3$rel5
                  +gwr_cluster3$rel6
                  +gwr_cluster3$rel7
                  +gwr_cluster3$rel8
                  +gwr_cluster3$rel9
                  +gwr_cluster3$ten1
                  +gwr_cluster3$ten2
                  +gwr_cluster3$ten3
                  +gwr_cluster3$ten4
                  +gwr_cluster3$house_price
                  +gwr_cluster3$crim_tot
                  +gwr_cluster3$crim1
                  +gwr_cluster3$crim2
                  +gwr_cluster3$crim3
                  +gwr_cluster3$crim4
                  +gwr_cluster3$crim5
                  +gwr_cluster3$crim6
                  +gwr_cluster3$em_n_w_child
                  +gwr_cluster3$em_n_l
                  +gwr_cluster3$em1
                  +gwr_cluster3$em2
                  +gwr_cluster3$em3
                  +gwr_cluster3$in1
                  +gwr_cluster3$in2
                  +gwr_cluster3$in3
                  +gwr_cluster3$bad_health
                  +gwr_cluster3$green_space
                  +gwr_cluster3$poll_PM10
                  +gwr_cluster3$poll_Nox
                  +gwr_cluster3$poll_NO2
                  +gwr_cluster3$poll_index
                  +gwr_cluster3$acc1
                  +gwr_cluster3$acc2
                  +gwr_cluster3$acc3
                  +gwr_cluster3$acc4
                  +gwr_cluster3$acc5
                  +gwr_cluster3$acc6
                  +gwr_cluster3$acc7
                  +gwr_cluster3$acc8
                  +gwr_cluster3$in4
                  +gwr_cluster3$in5
                  +gwr_cluster3$ed_ab
                  +gwr_cluster3$ed0
                  +gwr_cluster3$ed1
                  +gwr_cluster3$ed2
                  +gwr_cluster3$ed2a
                  +gwr_cluster3$ed3
                  +gwr_cluster3$ed4
                  +gwr_cluster3$ed5
)


plot(model_geod3)


# use the par function if we want to plot them in a 2x2 frame
par(mfrow=c(2,2))
par(mfrow)
plot(model_geod)



# mapping
resids_geod3 <-residuals(model_geod3)

map.resids_geod3 <- cbind(gwr_cluster3, resids_geod3) 
# rename the column header from the resids file - in this case its the 21th column of map.resids

names(map.resids_geod3)
names(map.resids_geod3)[88] <- "resids"

# maps the residuals using the quickmap function from tmap
qtm(map.resids_geod3, fill = "resids")

gwr1 <- tm_shape(map.resids_geod3) + tm_fill("resids", palette = "Reds",
                                             style = "quantile", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)

tm_shape(map.resids_geod3) + tm_fill("resids", palette = "Reds",
                                     style = "pretty", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


#----- 4.3.2 Geographcail Weighted Regression -----

GWRbandwidth_geod3 <- gwr.sel(gwr_cluster3$mh_tot_log~
                                gwr_cluster3$age0_15
                              +gwr_cluster3$age16_29
                              +gwr_cluster3$age30_44
                              +gwr_cluster3$age45_64
                              +gwr_cluster3$age65_
                              +gwr_cluster3$pop_per_h
                              +gwr_cluster3$dm1
                              +gwr_cluster3$dm2
                              +gwr_cluster3$dm3
                              +gwr_cluster3$dm4
                              +gwr_cluster3$dm5
                              +gwr_cluster3$eth1
                              +gwr_cluster3$eth2
                              +gwr_cluster3$eth3
                              +gwr_cluster3$eth4
                              +gwr_cluster3$eth5
                              +gwr_cluster3$birth1
                              +gwr_cluster3$birth2
                              +gwr_cluster3$rel1
                              +gwr_cluster3$rel2
                              +gwr_cluster3$rel3
                              +gwr_cluster3$rel4
                              +gwr_cluster3$rel5
                              +gwr_cluster3$rel6
                              +gwr_cluster3$rel7
                              +gwr_cluster3$rel8
                              +gwr_cluster3$rel9
                              +gwr_cluster3$ten1
                              +gwr_cluster3$ten2
                              +gwr_cluster3$ten3
                              +gwr_cluster3$ten4
                              +gwr_cluster3$house_price
                              +gwr_cluster3$crim_tot
                              +gwr_cluster3$crim1
                              +gwr_cluster3$crim2
                              +gwr_cluster3$crim3
                              +gwr_cluster3$crim4
                              +gwr_cluster3$crim5
                              +gwr_cluster3$crim6
                              +gwr_cluster3$em_n_w_child
                              +gwr_cluster3$em_n_l
                              +gwr_cluster3$em1
                              +gwr_cluster3$em2
                              +gwr_cluster3$em3
                              +gwr_cluster3$in1
                              +gwr_cluster3$in2
                              +gwr_cluster3$in3
                              +gwr_cluster3$bad_health
                              +gwr_cluster3$green_space
                              +gwr_cluster3$poll_PM10
                              +gwr_cluster3$poll_Nox
                              +gwr_cluster3$poll_NO2
                              +gwr_cluster3$poll_index
                              +gwr_cluster3$acc1
                              +gwr_cluster3$acc2
                              +gwr_cluster3$acc3
                              +gwr_cluster3$acc4
                              +gwr_cluster3$acc5
                              +gwr_cluster3$acc6
                              +gwr_cluster3$acc7
                              +gwr_cluster3$acc8
                              +gwr_cluster3$in4
                              +gwr_cluster3$in5
                              +gwr_cluster3$ed_ab
                              +gwr_cluster3$ed0
                              +gwr_cluster3$ed1
                              +gwr_cluster3$ed2
                              +gwr_cluster3$ed2a
                              +gwr_cluster3$ed3
                              +gwr_cluster3$ed4
                              +gwr_cluster3$ed5,
                              data=gwr_cluster3,adapt=T)

#run the gwr model
gwr.model3 = gwr(gwr_cluster3$mh_tot_log~
                   gwr_cluster3$age0_15
                 +gwr_cluster3$age16_29
                 +gwr_cluster3$age30_44
                 +gwr_cluster3$age45_64
                 +gwr_cluster3$age65_
                 +gwr_cluster3$pop_per_h
                 +gwr_cluster3$dm1
                 +gwr_cluster3$dm2
                 +gwr_cluster3$dm3
                 +gwr_cluster3$dm4
                 +gwr_cluster3$dm5
                 +gwr_cluster3$eth1
                 +gwr_cluster3$eth2
                 +gwr_cluster3$eth3
                 +gwr_cluster3$eth4
                 +gwr_cluster3$eth5
                 +gwr_cluster3$birth1
                 +gwr_cluster3$birth2
                 +gwr_cluster3$rel1
                 +gwr_cluster3$rel2
                 +gwr_cluster3$rel3
                 +gwr_cluster3$rel4
                 +gwr_cluster3$rel5
                 +gwr_cluster3$rel6
                 +gwr_cluster3$rel7
                 +gwr_cluster3$rel8
                 +gwr_cluster3$rel9
                 +gwr_cluster3$ten1
                 +gwr_cluster3$ten2
                 +gwr_cluster3$ten3
                 +gwr_cluster3$ten4
                 +gwr_cluster3$house_price
                 +gwr_cluster3$crim_tot
                 +gwr_cluster3$crim1
                 +gwr_cluster3$crim2
                 +gwr_cluster3$crim3
                 +gwr_cluster3$crim4
                 +gwr_cluster3$crim5
                 +gwr_cluster3$crim6
                 +gwr_cluster3$em_n_w_child
                 +gwr_cluster3$em_n_l
                 +gwr_cluster3$em1
                 +gwr_cluster3$em2
                 +gwr_cluster3$em3
                 +gwr_cluster3$in1
                 +gwr_cluster3$in2
                 +gwr_cluster3$in3
                 +gwr_cluster3$bad_health
                 +gwr_cluster3$green_space
                 +gwr_cluster3$poll_PM10
                 +gwr_cluster3$poll_Nox
                 +gwr_cluster3$poll_NO2
                 +gwr_cluster3$poll_index
                 +gwr_cluster3$acc1
                 +gwr_cluster3$acc2
                 +gwr_cluster3$acc3
                 +gwr_cluster3$acc4
                 +gwr_cluster3$acc5
                 +gwr_cluster3$acc6
                 +gwr_cluster3$acc7
                 +gwr_cluster3$acc8
                 +gwr_cluster3$in4
                 +gwr_cluster3$in5
                 +gwr_cluster3$ed_ab
                 +gwr_cluster3$ed0
                 +gwr_cluster3$ed1
                 +gwr_cluster3$ed2
                 +gwr_cluster3$ed2a
                 +gwr_cluster3$ed3
                 +gwr_cluster3$ed4
                 +gwr_cluster3$ed5,
                 data=gwr_cluster3, adapt=GWRbandwidth_geod3, hatmatrix=TRUE, se.fit=TRUE)

#print the results of the model
gwr.model3


results_geod3 <-as.data.frame(gwr.model3$SDF)

names(results_geod3)
head(results_geod3)

gwr.map3 <- cbind(gwr_cluster3, as.matrix(results_geod3))

qtm(gwr.map3, fill = "localR2")

tm_shape(gwr.map3) + tm_fill("localR2", palette = "Reds",
                             style = "quantile", title = "localR2") +tm_borders(alpha=.1)+
  tm_layout(frame = T)



# 4.4.1 GWE + Reg Cluster 4 -----


gwr_cluster4 <- lsoa_test1

gwr_cluster4@data <- data.frame(gwr_cluster4@data,Class4
                                [match(gwr_cluster4@data[,"LSOA_CODE"],Class4[,"lsoa11cd"]),])
?match
gwr_cluster4 <- gwr_cluster4[!is.na(gwr_cluster4@data$Classification),]



model_geod4 <- lm(gwr_cluster4$mh_tot_log~
                    gwr_cluster4$age0_15
                  +gwr_cluster4$age16_29
                  +gwr_cluster4$age30_44
                  +gwr_cluster4$age45_64
                  +gwr_cluster4$age65_
                  +gwr_cluster4$pop_per_h
                  +gwr_cluster4$dm1
                  +gwr_cluster4$dm2
                  +gwr_cluster4$dm3
                  +gwr_cluster4$dm4
                  +gwr_cluster4$dm5
                  +gwr_cluster4$eth1
                  +gwr_cluster4$eth2
                  +gwr_cluster4$eth3
                  +gwr_cluster4$eth4
                  +gwr_cluster4$eth5
                  +gwr_cluster4$birth1
                  +gwr_cluster4$birth2
                  +gwr_cluster4$rel1
                  +gwr_cluster4$rel2
                  +gwr_cluster4$rel3
                  +gwr_cluster4$rel4
                  +gwr_cluster4$rel5
                  +gwr_cluster4$rel6
                  +gwr_cluster4$rel7
                  +gwr_cluster4$rel8
                  +gwr_cluster4$rel9
                  +gwr_cluster4$ten1
                  +gwr_cluster4$ten2
                  +gwr_cluster4$ten3
                  +gwr_cluster4$ten4
                  +gwr_cluster4$house_price
                  +gwr_cluster4$crim_tot
                  +gwr_cluster4$crim1
                  +gwr_cluster4$crim2
                  +gwr_cluster4$crim3
                  +gwr_cluster4$crim4
                  +gwr_cluster4$crim5
                  +gwr_cluster4$crim6
                  +gwr_cluster4$em_n_w_child
                  +gwr_cluster4$em_n_l
                  +gwr_cluster4$em1
                  +gwr_cluster4$em2
                  +gwr_cluster4$em3
                  +gwr_cluster4$in1
                  +gwr_cluster4$in2
                  +gwr_cluster4$in3
                  +gwr_cluster4$bad_health
                  +gwr_cluster4$green_space
                  +gwr_cluster4$poll_PM10
                  +gwr_cluster4$poll_Nox
                  +gwr_cluster4$poll_NO2
                  +gwr_cluster4$poll_index
                  +gwr_cluster4$acc1
                  +gwr_cluster4$acc2
                  +gwr_cluster4$acc3
                  +gwr_cluster4$acc4
                  +gwr_cluster4$acc5
                  +gwr_cluster4$acc6
                  +gwr_cluster4$acc7
                  +gwr_cluster4$acc8
                  +gwr_cluster4$in4
                  +gwr_cluster4$in5
                  +gwr_cluster4$ed_ab
                  +gwr_cluster4$ed0
                  +gwr_cluster4$ed1
                  +gwr_cluster4$ed2
                  +gwr_cluster4$ed2a
                  +gwr_cluster4$ed3
                  +gwr_cluster4$ed4
                  +gwr_cluster4$ed5
)


plot(model_geod4)


# use the par function if we want to plot them in a 2x2 frame
par(mfrow=c(2,2))
par(mfrow)
plot(model_geod)



# mapping
resids_geod4 <-residuals(model_geod4)

map.resids_geod4 <- cbind(gwr_cluster4, resids_geod4) 
# rename the column header from the resids file - in this case its the 21th column of map.resids

names(map.resids_geod4)
names(map.resids_geod4)[88] <- "resids"

# maps the residuals using the quickmap function from tmap
qtm(map.resids_geod4, fill = "resids")

gwr1 <- tm_shape(map.resids_geod4) + tm_fill("resids", palette = "Reds",
                                             style = "quantile", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)

tm_shape(map.resids_geod4) + tm_fill("resids", palette = "Reds",
                                     style = "pretty", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


#----- 4.4.2 Geographcail Weighted Regression -----

GWRbandwidth_geod4 <- gwr.sel(gwr_cluster4$mh_tot_log~
                                gwr_cluster4$age0_15
                              +gwr_cluster4$age16_29
                              +gwr_cluster4$age30_44
                              +gwr_cluster4$age45_64
                              +gwr_cluster4$age65_
                              +gwr_cluster4$pop_per_h
                              +gwr_cluster4$dm1
                              +gwr_cluster4$dm2
                              +gwr_cluster4$dm3
                              +gwr_cluster4$dm4
                              +gwr_cluster4$dm5
                              +gwr_cluster4$eth1
                              +gwr_cluster4$eth2
                              +gwr_cluster4$eth3
                              +gwr_cluster4$eth4
                              +gwr_cluster4$eth5
                              +gwr_cluster4$birth1
                              +gwr_cluster4$birth2
                              +gwr_cluster4$rel1
                              +gwr_cluster4$rel2
                              +gwr_cluster4$rel3
                              +gwr_cluster4$rel4
                              +gwr_cluster4$rel5
                              +gwr_cluster4$rel6
                              +gwr_cluster4$rel7
                              +gwr_cluster4$rel8
                              +gwr_cluster4$rel9
                              +gwr_cluster4$ten1
                              +gwr_cluster4$ten2
                              +gwr_cluster4$ten3
                              +gwr_cluster4$ten4
                              +gwr_cluster4$house_price
                              +gwr_cluster4$crim_tot
                              +gwr_cluster4$crim1
                              +gwr_cluster4$crim2
                              +gwr_cluster4$crim3
                              +gwr_cluster4$crim4
                              +gwr_cluster4$crim5
                              +gwr_cluster4$crim6
                              +gwr_cluster4$em_n_w_child
                              +gwr_cluster4$em_n_l
                              +gwr_cluster4$em1
                              +gwr_cluster4$em2
                              +gwr_cluster4$em3
                              +gwr_cluster4$in1
                              +gwr_cluster4$in2
                              +gwr_cluster4$in3
                              +gwr_cluster4$bad_health
                              +gwr_cluster4$green_space
                              +gwr_cluster4$poll_PM10
                              +gwr_cluster4$poll_Nox
                              +gwr_cluster4$poll_NO2
                              +gwr_cluster4$poll_index
                              +gwr_cluster4$acc1
                              +gwr_cluster4$acc2
                              +gwr_cluster4$acc3
                              +gwr_cluster4$acc4
                              +gwr_cluster4$acc5
                              +gwr_cluster4$acc6
                              +gwr_cluster4$acc7
                              +gwr_cluster4$acc8
                              +gwr_cluster4$in4
                              +gwr_cluster4$in5
                              +gwr_cluster4$ed_ab
                              +gwr_cluster4$ed0
                              +gwr_cluster4$ed1
                              +gwr_cluster4$ed2
                              +gwr_cluster4$ed2a
                              +gwr_cluster4$ed3
                              +gwr_cluster4$ed4
                              +gwr_cluster4$ed5,
                              data=gwr_cluster4,adapt=T)

#run the gwr model
gwr.model4 = gwr(gwr_cluster4$mh_tot_log~
                   gwr_cluster4$age0_15
                 +gwr_cluster4$age16_29
                 +gwr_cluster4$age30_44
                 +gwr_cluster4$age45_64
                 +gwr_cluster4$age65_
                 +gwr_cluster4$pop_per_h
                 +gwr_cluster4$dm1
                 +gwr_cluster4$dm2
                 +gwr_cluster4$dm3
                 +gwr_cluster4$dm4
                 +gwr_cluster4$dm5
                 +gwr_cluster4$eth1
                 +gwr_cluster4$eth2
                 +gwr_cluster4$eth3
                 +gwr_cluster4$eth4
                 +gwr_cluster4$eth5
                 +gwr_cluster4$birth1
                 +gwr_cluster4$birth2
                 +gwr_cluster4$rel1
                 +gwr_cluster4$rel2
                 +gwr_cluster4$rel3
                 +gwr_cluster4$rel4
                 +gwr_cluster4$rel5
                 +gwr_cluster4$rel6
                 +gwr_cluster4$rel7
                 +gwr_cluster4$rel8
                 +gwr_cluster4$rel9
                 +gwr_cluster4$ten1
                 +gwr_cluster4$ten2
                 +gwr_cluster4$ten3
                 +gwr_cluster4$ten4
                 +gwr_cluster4$house_price
                 +gwr_cluster4$crim_tot
                 +gwr_cluster4$crim1
                 +gwr_cluster4$crim2
                 +gwr_cluster4$crim3
                 +gwr_cluster4$crim4
                 +gwr_cluster4$crim5
                 +gwr_cluster4$crim6
                 +gwr_cluster4$em_n_w_child
                 +gwr_cluster4$em_n_l
                 +gwr_cluster4$em1
                 +gwr_cluster4$em2
                 +gwr_cluster4$em3
                 +gwr_cluster4$in1
                 +gwr_cluster4$in2
                 +gwr_cluster4$in3
                 +gwr_cluster4$bad_health
                 +gwr_cluster4$green_space
                 +gwr_cluster4$poll_PM10
                 +gwr_cluster4$poll_Nox
                 +gwr_cluster4$poll_NO2
                 +gwr_cluster4$poll_index
                 +gwr_cluster4$acc1
                 +gwr_cluster4$acc2
                 +gwr_cluster4$acc3
                 +gwr_cluster4$acc4
                 +gwr_cluster4$acc5
                 +gwr_cluster4$acc6
                 +gwr_cluster4$acc7
                 +gwr_cluster4$acc8
                 +gwr_cluster4$in4
                 +gwr_cluster4$in5
                 +gwr_cluster4$ed_ab
                 +gwr_cluster4$ed0
                 +gwr_cluster4$ed1
                 +gwr_cluster4$ed2
                 +gwr_cluster4$ed2a
                 +gwr_cluster4$ed3
                 +gwr_cluster4$ed4
                 +gwr_cluster4$ed5,
                 data=gwr_cluster4, adapt=GWRbandwidth_geod4, hatmatrix=TRUE, se.fit=TRUE)

#print the results of the model
gwr.model4


results_geod4 <-as.data.frame(gwr.model4$SDF)

names(results_geod4)
head(results_geod4)

gwr.map4 <- cbind(gwr_cluster4, as.matrix(results_geod4))

qtm(gwr.map4, fill = "localR2")

tm_shape(gwr.map4) + tm_fill("localR2", palette = "Reds",
                             style = "quantile", title = "localR2") +tm_borders(alpha=.1)+
  tm_layout(frame = T)

# 4.5.1 GWE + Reg Cluster 5 -----

gwr_cluster5 <- lsoa_test1

gwr_cluster5@data <- data.frame(gwr_cluster5@data,Class5
                                [match(gwr_cluster5@data[,"LSOA_CODE"],Class5[,"lsoa11cd"]),])
?match
gwr_cluster5 <- gwr_cluster5[!is.na(gwr_cluster5@data$Classification),]



model_geod5 <- lm(gwr_cluster5$mh_tot_log~
                    gwr_cluster5$age0_15
                  +gwr_cluster5$age16_29
                  +gwr_cluster5$age30_44
                  +gwr_cluster5$age45_64
                  +gwr_cluster5$age65_
                  +gwr_cluster5$pop_per_h
                  +gwr_cluster5$dm1
                  +gwr_cluster5$dm2
                  +gwr_cluster5$dm3
                  +gwr_cluster5$dm4
                  +gwr_cluster5$dm5
                  +gwr_cluster5$eth1
                  +gwr_cluster5$eth2
                  +gwr_cluster5$eth3
                  +gwr_cluster5$eth4
                  +gwr_cluster5$eth5
                  +gwr_cluster5$birth1
                  +gwr_cluster5$birth2
                  +gwr_cluster5$rel1
                  +gwr_cluster5$rel2
                  +gwr_cluster5$rel3
                  +gwr_cluster5$rel4
                  +gwr_cluster5$rel5
                  +gwr_cluster5$rel6
                  +gwr_cluster5$rel7
                  +gwr_cluster5$rel8
                  +gwr_cluster5$rel9
                  +gwr_cluster5$ten1
                  +gwr_cluster5$ten2
                  +gwr_cluster5$ten3
                  +gwr_cluster5$ten4
                  +gwr_cluster5$house_price
                  +gwr_cluster5$crim_tot
                  +gwr_cluster5$crim1
                  +gwr_cluster5$crim2
                  +gwr_cluster5$crim3
                  +gwr_cluster5$crim4
                  +gwr_cluster5$crim5
                  +gwr_cluster5$crim6
                  +gwr_cluster5$em_n_w_child
                  +gwr_cluster5$em_n_l
                  +gwr_cluster5$em1
                  +gwr_cluster5$em2
                  +gwr_cluster5$em3
                  +gwr_cluster5$in1
                  +gwr_cluster5$in2
                  +gwr_cluster5$in3
                  +gwr_cluster5$bad_health
                  +gwr_cluster5$green_space
                  +gwr_cluster5$poll_PM10
                  +gwr_cluster5$poll_Nox
                  +gwr_cluster5$poll_NO2
                  +gwr_cluster5$poll_index
                  +gwr_cluster5$acc1
                  +gwr_cluster5$acc2
                  +gwr_cluster5$acc3
                  +gwr_cluster5$acc4
                  +gwr_cluster5$acc5
                  +gwr_cluster5$acc6
                  +gwr_cluster5$acc7
                  +gwr_cluster5$acc8
                  +gwr_cluster5$in4
                  +gwr_cluster5$in5
                  +gwr_cluster5$ed_ab
                  +gwr_cluster5$ed0
                  +gwr_cluster5$ed1
                  +gwr_cluster5$ed2
                  +gwr_cluster5$ed2a
                  +gwr_cluster5$ed3
                  +gwr_cluster5$ed4
                  +gwr_cluster5$ed5
)


plot(model_geod5)


# use the par function if we want to plot them in a 2x2 frame
par(mfrow=c(2,2))
par(mfrow)
plot(model_geod)



# mapping
resids_geod5 <-residuals(model_geod5)

map.resids_geod5 <- cbind(gwr_cluster5, resids_geod5)
# rename the column header from the resids file - in this case its the 21th column of map.resids

names(map.resids_geod5)
names(map.resids_geod5)[88] <- "resids"

# maps the residuals using the quickmap function from tmap
qtm(map.resids_geod1, fill = "resids")

gwr1 <- tm_shape(map.resids_geod1) + tm_fill("resids", palette = "Reds",
                                             style = "quantile", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)

tm_shape(map.resids_geod5) + tm_fill("resids", palette = "Reds",
                                     style = "pretty", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


#----- 4.5.2 Geographcail Weighted Regression -----

GWRbandwidth_geod5 <- gwr.sel(gwr_cluster5$mh_tot_log~
                                gwr_cluster5$age0_15
                              +gwr_cluster5$age16_29
                              +gwr_cluster5$age30_44
                              +gwr_cluster5$age45_64
                              +gwr_cluster5$age65_
                              +gwr_cluster5$pop_per_h
                              +gwr_cluster5$dm1
                              +gwr_cluster5$dm2
                              +gwr_cluster5$dm3
                              +gwr_cluster5$dm4
                              +gwr_cluster5$dm5
                              +gwr_cluster5$eth1
                              +gwr_cluster5$eth2
                              +gwr_cluster5$eth3
                              +gwr_cluster5$eth4
                              +gwr_cluster5$eth5
                              +gwr_cluster5$birth1
                              +gwr_cluster5$birth2
                              +gwr_cluster5$rel1
                              +gwr_cluster5$rel2
                              +gwr_cluster5$rel3
                              +gwr_cluster5$rel4
                              +gwr_cluster5$rel5
                              +gwr_cluster5$rel6
                              +gwr_cluster5$rel7
                              +gwr_cluster5$rel8
                              +gwr_cluster5$rel9
                              +gwr_cluster5$ten1
                              +gwr_cluster5$ten2
                              +gwr_cluster5$ten3
                              +gwr_cluster5$ten4
                              +gwr_cluster5$house_price
                              +gwr_cluster5$crim_tot
                              +gwr_cluster5$crim1
                              +gwr_cluster5$crim2
                              +gwr_cluster5$crim3
                              +gwr_cluster5$crim4
                              +gwr_cluster5$crim5
                              +gwr_cluster5$crim6
                              +gwr_cluster5$em_n_w_child
                              +gwr_cluster5$em_n_l
                              +gwr_cluster5$em1
                              +gwr_cluster5$em2
                              +gwr_cluster5$em3
                              +gwr_cluster5$in1
                              +gwr_cluster5$in2
                              +gwr_cluster5$in3
                              +gwr_cluster5$bad_health
                              +gwr_cluster5$green_space
                              +gwr_cluster5$poll_PM10
                              +gwr_cluster5$poll_Nox
                              +gwr_cluster5$poll_NO2
                              +gwr_cluster5$poll_index
                              +gwr_cluster5$acc1
                              +gwr_cluster5$acc2
                              +gwr_cluster5$acc3
                              +gwr_cluster5$acc4
                              +gwr_cluster5$acc5
                              +gwr_cluster5$acc6
                              +gwr_cluster5$acc7
                              +gwr_cluster5$acc8
                              +gwr_cluster5$in4
                              +gwr_cluster5$in5
                              +gwr_cluster5$ed_ab
                              +gwr_cluster5$ed0
                              +gwr_cluster5$ed1
                              +gwr_cluster5$ed2
                              +gwr_cluster5$ed2a
                              +gwr_cluster5$ed3
                              +gwr_cluster5$ed4
                              +gwr_cluster5$ed5,
                              data=gwr_cluster5,adapt=T)

#run the gwr model
gwr.model5 = gwr(gwr_cluster5$mh_tot_log~
                   gwr_cluster5$age0_15
                 +gwr_cluster5$age16_29
                 +gwr_cluster5$age30_44
                 +gwr_cluster5$age45_64
                 +gwr_cluster5$age65_
                 +gwr_cluster5$pop_per_h
                 +gwr_cluster5$dm1
                 +gwr_cluster5$dm2
                 +gwr_cluster5$dm3
                 +gwr_cluster5$dm4
                 +gwr_cluster5$dm5
                 +gwr_cluster5$eth1
                 +gwr_cluster5$eth2
                 +gwr_cluster5$eth3
                 +gwr_cluster5$eth4
                 +gwr_cluster5$eth5
                 +gwr_cluster5$birth1
                 +gwr_cluster5$birth2
                 +gwr_cluster5$rel1
                 +gwr_cluster5$rel2
                 +gwr_cluster5$rel3
                 +gwr_cluster5$rel4
                 +gwr_cluster5$rel5
                 +gwr_cluster5$rel6
                 +gwr_cluster5$rel7
                 +gwr_cluster5$rel8
                 +gwr_cluster5$rel9
                 +gwr_cluster5$ten1
                 +gwr_cluster5$ten2
                 +gwr_cluster5$ten3
                 +gwr_cluster5$ten4
                 +gwr_cluster5$house_price
                 +gwr_cluster5$crim_tot
                 +gwr_cluster5$crim1
                 +gwr_cluster5$crim2
                 +gwr_cluster5$crim3
                 +gwr_cluster5$crim4
                 +gwr_cluster5$crim5
                 +gwr_cluster5$crim6
                 +gwr_cluster5$em_n_w_child
                 +gwr_cluster5$em_n_l
                 +gwr_cluster5$em1
                 +gwr_cluster5$em2
                 +gwr_cluster5$em3
                 +gwr_cluster5$in1
                 +gwr_cluster5$in2
                 +gwr_cluster5$in3
                 +gwr_cluster5$bad_health
                 +gwr_cluster5$green_space
                 +gwr_cluster5$poll_PM10
                 +gwr_cluster5$poll_Nox
                 +gwr_cluster5$poll_NO2
                 +gwr_cluster5$poll_index
                 +gwr_cluster5$acc1
                 +gwr_cluster5$acc2
                 +gwr_cluster5$acc3
                 +gwr_cluster5$acc4
                 +gwr_cluster5$acc5
                 +gwr_cluster5$acc6
                 +gwr_cluster5$acc7
                 +gwr_cluster5$acc8
                 +gwr_cluster5$in4
                 +gwr_cluster5$in5
                 +gwr_cluster5$ed_ab
                 +gwr_cluster5$ed0
                 +gwr_cluster5$ed1
                 +gwr_cluster5$ed2
                 +gwr_cluster5$ed2a
                 +gwr_cluster5$ed3
                 +gwr_cluster5$ed4
                 +gwr_cluster5$ed5,
                 data=gwr_cluster5, adapt=GWRbandwidth_geod5, hatmatrix=TRUE, se.fit=TRUE)

#print the results of the model
gwr.model5


results_geod5 <-as.data.frame(gwr.model5$SDF)

names(results_geod1)
head(results_geod1)

gwr.map5 <- cbind(gwr_cluster5, as.matrix(results_geod5))

qtm(gwr.map1, fill = "localR2")

tm_shape(gwr.map5) + tm_fill("localR2", palette = "Reds",
                             style = "quantile", title = "localR2") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


# 4.6.1 GWE + Reg Cluster 6 -----


gwr_cluster6 <- lsoa_test1

gwr_cluster6@data <- data.frame(gwr_cluster6@data,Class6
                                [match(gwr_cluster6@data[,"LSOA_CODE"],Class6[,"lsoa11cd"]),])
?match
gwr_cluster6 <- gwr_cluster6[!is.na(gwr_cluster6@data$Classification),]



model_geod6 <- lm(gwr_cluster6$mh_tot_log~
                    gwr_cluster6$age0_15
                  +gwr_cluster6$age16_29
                  +gwr_cluster6$age30_44
                  +gwr_cluster6$age45_64
                  +gwr_cluster6$age65_
                  +gwr_cluster6$pop_per_h
                  +gwr_cluster6$dm1
                  +gwr_cluster6$dm2
                  +gwr_cluster6$dm3
                  +gwr_cluster6$dm4
                  +gwr_cluster6$dm5
                  +gwr_cluster6$eth1
                  +gwr_cluster6$eth2
                  +gwr_cluster6$eth3
                  +gwr_cluster6$eth4
                  +gwr_cluster6$eth5
                  +gwr_cluster6$birth1
                  +gwr_cluster6$birth2
                  +gwr_cluster6$rel1
                  +gwr_cluster6$rel2
                  +gwr_cluster6$rel3
                  +gwr_cluster6$rel4
                  +gwr_cluster6$rel5
                  +gwr_cluster6$rel6
                  +gwr_cluster6$rel7
                  +gwr_cluster6$rel8
                  +gwr_cluster6$rel9
                  +gwr_cluster6$ten1
                  +gwr_cluster6$ten2
                  +gwr_cluster6$ten3
                  +gwr_cluster6$ten4
                  +gwr_cluster6$house_price
                  +gwr_cluster6$crim_tot
                  +gwr_cluster6$crim1
                  +gwr_cluster6$crim2
                  +gwr_cluster6$crim3
                  +gwr_cluster6$crim4
                  +gwr_cluster6$crim5
                  +gwr_cluster6$crim6
                  +gwr_cluster6$em_n_w_child
                  +gwr_cluster6$em_n_l
                  +gwr_cluster6$em1
                  +gwr_cluster6$em2
                  +gwr_cluster6$em3
                  +gwr_cluster6$in1
                  +gwr_cluster6$in2
                  +gwr_cluster6$in3
                  +gwr_cluster6$bad_health
                  +gwr_cluster6$green_space
                  +gwr_cluster6$poll_PM10
                  +gwr_cluster6$poll_Nox
                  +gwr_cluster6$poll_NO2
                  +gwr_cluster6$poll_index
                  +gwr_cluster6$acc1
                  +gwr_cluster6$acc2
                  +gwr_cluster6$acc3
                  +gwr_cluster6$acc4
                  +gwr_cluster6$acc5
                  +gwr_cluster6$acc6
                  +gwr_cluster6$acc7
                  +gwr_cluster6$acc8
                  +gwr_cluster6$in4
                  +gwr_cluster6$in5
                  +gwr_cluster6$ed_ab
                  +gwr_cluster6$ed0
                  +gwr_cluster6$ed1
                  +gwr_cluster6$ed2
                  +gwr_cluster6$ed2a
                  +gwr_cluster6$ed3
                  +gwr_cluster6$ed4
                  +gwr_cluster6$ed5
)


plot(model_geod)


# use the par function if we want to plot them in a 2x2 frame
par(mfrow=c(2,2))
par(mfrow)
plot(model_geod)



# mapping
resids_geod6 <-residuals(model_geod6)

map.resids_geod6 <- cbind(gwr_cluster6, resids_geod6) 
# rename the column header from the resids file - in this case its the 21th column of map.resids

names(map.resids_geod6)
names(map.resids_geod6)[88] <- "resids"

# maps the residuals using the quickmap function from tmap
qtm(map.resids_geod1, fill = "resids")

gwr1 <- tm_shape(map.resids_geod1) + tm_fill("resids", palette = "Reds",
                                             style = "quantile", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)

tm_shape(map.resids_geod6) + tm_fill("resids", palette = "Reds",
                                     style = "pretty", title = "Residuals") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


#----- 4.6.2 Geographcail Weighted Regression -----

GWRbandwidth_geod6 <- gwr.sel(gwr_cluster6$mh_tot_log~
                                gwr_cluster6$age0_15
                              +gwr_cluster6$age16_29
                              +gwr_cluster6$age30_44
                              +gwr_cluster6$age45_64
                              +gwr_cluster6$age65_
                              +gwr_cluster6$pop_per_h
                              +gwr_cluster6$dm1
                              +gwr_cluster6$dm2
                              +gwr_cluster6$dm3
                              +gwr_cluster6$dm4
                              +gwr_cluster6$dm5
                              +gwr_cluster6$eth1
                              +gwr_cluster6$eth2
                              +gwr_cluster6$eth3
                              +gwr_cluster6$eth4
                              +gwr_cluster6$eth5
                              +gwr_cluster6$birth1
                              +gwr_cluster6$birth2
                              +gwr_cluster6$rel1
                              +gwr_cluster6$rel2
                              +gwr_cluster6$rel3
                              +gwr_cluster6$rel4
                              +gwr_cluster6$rel5
                              +gwr_cluster6$rel6
                              +gwr_cluster6$rel7
                              +gwr_cluster6$rel8
                              +gwr_cluster6$rel9
                              +gwr_cluster6$ten1
                              +gwr_cluster6$ten2
                              +gwr_cluster6$ten3
                              +gwr_cluster6$ten4
                              +gwr_cluster6$house_price
                              +gwr_cluster6$crim_tot
                              +gwr_cluster6$crim1
                              +gwr_cluster6$crim2
                              +gwr_cluster6$crim3
                              +gwr_cluster6$crim4
                              +gwr_cluster6$crim5
                              +gwr_cluster6$crim6
                              +gwr_cluster6$em_n_w_child
                              +gwr_cluster6$em_n_l
                              +gwr_cluster6$em1
                              +gwr_cluster6$em2
                              +gwr_cluster6$em3
                              +gwr_cluster6$in1
                              +gwr_cluster6$in2
                              +gwr_cluster6$in3
                              +gwr_cluster6$bad_health
                              +gwr_cluster6$green_space
                              +gwr_cluster6$poll_PM10
                              +gwr_cluster6$poll_Nox
                              +gwr_cluster6$poll_NO2
                              +gwr_cluster6$poll_index
                              +gwr_cluster6$acc1
                              +gwr_cluster6$acc2
                              +gwr_cluster6$acc3
                              +gwr_cluster6$acc4
                              +gwr_cluster6$acc5
                              +gwr_cluster6$acc6
                              +gwr_cluster6$acc7
                              +gwr_cluster6$acc8
                              +gwr_cluster6$in4
                              +gwr_cluster6$in5
                              +gwr_cluster6$ed_ab
                              +gwr_cluster6$ed0
                              +gwr_cluster6$ed1
                              +gwr_cluster6$ed2
                              +gwr_cluster6$ed2a
                              +gwr_cluster6$ed3
                              +gwr_cluster6$ed4
                              +gwr_cluster6$ed5,
                              data=gwr_cluster6,adapt=T)

#run the gwr model
gwr.model6 = gwr(gwr_cluster6$mh_tot_log~
                   gwr_cluster6$age0_15
                 +gwr_cluster6$age16_29
                 +gwr_cluster6$age30_44
                 +gwr_cluster6$age45_64
                 +gwr_cluster6$age65_
                 +gwr_cluster6$pop_per_h
                 +gwr_cluster6$dm1
                 +gwr_cluster6$dm2
                 +gwr_cluster6$dm3
                 +gwr_cluster6$dm4
                 +gwr_cluster6$dm5
                 +gwr_cluster6$eth1
                 +gwr_cluster6$eth2
                 +gwr_cluster6$eth3
                 +gwr_cluster6$eth4
                 +gwr_cluster6$eth5
                 +gwr_cluster6$birth1
                 +gwr_cluster6$birth2
                 +gwr_cluster6$rel1
                 +gwr_cluster6$rel2
                 +gwr_cluster6$rel3
                 +gwr_cluster6$rel4
                 +gwr_cluster6$rel5
                 +gwr_cluster6$rel6
                 +gwr_cluster6$rel7
                 +gwr_cluster6$rel8
                 +gwr_cluster6$rel9
                 +gwr_cluster6$ten1
                 +gwr_cluster6$ten2
                 +gwr_cluster6$ten3
                 +gwr_cluster6$ten4
                 +gwr_cluster6$house_price
                 +gwr_cluster6$crim_tot
                 +gwr_cluster6$crim1
                 +gwr_cluster6$crim2
                 +gwr_cluster6$crim3
                 +gwr_cluster6$crim4
                 +gwr_cluster6$crim5
                 +gwr_cluster6$crim6
                 +gwr_cluster6$em_n_w_child
                 +gwr_cluster6$em_n_l
                 +gwr_cluster6$em1
                 +gwr_cluster6$em2
                 +gwr_cluster6$em3
                 +gwr_cluster6$in1
                 +gwr_cluster6$in2
                 +gwr_cluster6$in3
                 +gwr_cluster6$bad_health
                 +gwr_cluster6$green_space
                 +gwr_cluster6$poll_PM10
                 +gwr_cluster6$poll_Nox
                 +gwr_cluster6$poll_NO2
                 +gwr_cluster6$poll_index
                 +gwr_cluster6$acc1
                 +gwr_cluster6$acc2
                 +gwr_cluster6$acc3
                 +gwr_cluster6$acc4
                 +gwr_cluster6$acc5
                 +gwr_cluster6$acc6
                 +gwr_cluster6$acc7
                 +gwr_cluster6$acc8
                 +gwr_cluster6$in4
                 +gwr_cluster6$in5
                 +gwr_cluster6$ed_ab
                 +gwr_cluster6$ed0
                 +gwr_cluster6$ed1
                 +gwr_cluster6$ed2
                 +gwr_cluster6$ed2a
                 +gwr_cluster6$ed3
                 +gwr_cluster6$ed4
                 +gwr_cluster6$ed5,
                 data=gwr_cluster6, adapt=GWRbandwidth_geod6, hatmatrix=TRUE, se.fit=TRUE)

#print the results of the model
gwr.model6


results_geod6 <-as.data.frame(gwr.model6$SDF)

names(results_geod6)
head(results_geod6)

gwr.map6 <- cbind(gwr_cluster6, as.matrix(results_geod6))

qtm(gwr.map6, fill = "localR2")

tm_shape(gwr.map6) + tm_fill("localR2", palette = "Reds",
                             style = "quantile", title = "localR2") +tm_borders(alpha=.1)+
  tm_layout(frame = T)


#----- 5.0 Correlation ---- 

correlation <- lsoa_test1@data

correlation<- subset(correlation, select =-c(LSOA_CODE, LSOA_NAME,MSOA_CODE,STWARDCODE,
                                             STWARDNAME,LA_CODE,LA_NAME,lsoa01cd,pop,tot_house))

correlation<- subset(correlation, select =-c(MSOA_NAME,Classification))
                     
cormat <- round(cor(correlation),2)

library(reshape2)
library(ggplot2)
melted_cormat <- melt(cormat)

ggplot(data=melted_cormat, aes(x=Var1,y=Var2,fill=value))+geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 5, hjust = 1),axis.text.y =element_text(size=5,vjust=1,hjust=1) )+
  coord_fixed()+ggtitle("Pearson's Correlation Matrix")







################# ---------------------- END OF MAIN ANALYSIS --------------------------



# ---- EXPERIMENTAL PART ----- 

#------- 6.0 Artificial Neural Networking ----- 

install.packages("nnet")
install.packages("mlbench")

library("nnet")
library(mlbench)

# data 
ann <- correlation
names(ann)
# number of repeating results 
set.seed(10)

# selecting what variables 
ann.nnet<- nnet(mh_tot_log~ ,age0_15, age16_29, age30_44, age45_64,age65_,pop_per_h,dm1,
                dm2,dm3,dm4,dm5,eth1,eth2,eth3        
                ,eth4,eth5,birth1,birth2,rel1,rel2,rel3,rel4,rel5,rel6,rel7,rel8,rel9,ten1,        
                ten2,ten3,ten4,house_price,crim_tot, crim1,crim2,data=ann, size=2)



#crim3        crim4        crim5        crim6        em_n_w_child em_n_l       em1         
#em2          em3          in1          in2          in3          bad_health   green_space 
#poll_PM10    poll_Nox     poll_NO2     poll_index   acc1         acc2         acc3        
#acc4         acc5         acc6         acc7         acc8         in4          in5         
#ed_ab        ed0          ed1          ed2          ed2a         ed3          ed4         
#ed5          mh_tot

# predict 
ann.predict <- predict(ann.nnet) * 50

mean((ann.predict - ann$mh_tot)^2)

install.packages("caret")
install.packages("devtools")
require(caret)
library("devtools")
# test decay - learning rate 

mygrid <- expand.grid(.decay=c(0.5, 0.1), .size=c(4,5,6))

mh.nnet.parameters <- train(mh_tot/50 ~ ten2, data=ann, 
                             method="nnet", metric = "Rsquared", maxit=10, tuneGrid=mygrid, trace=F)

dev.off()

print(mh.nnet.parameters)
  # cross validation 
fitControl <- trainControl("repeatedcv", number = 10, repeats = 3, returnResamp = "all")
mh.nnet.cv <- train(mh_tot/50 ~ ten2, data = ann, "nnet", metric = "Rsquared", 
                    trControl = fitControl, tuneGrid=mygrid)

# decay result 
mh.nnet.cv$best

#training NN with optimal values 
mh.nnet.final <- nnet(mh_tot/50 ~ ten2, data=ann, size=6, decay=0.5)

plot.nnet <- function(mod.in,nid=T,all.out=T,all.in=T,bias=T,wts.only=F,rel.rsc=5,circle.cex=5,
                      node.labs=T,var.labs=T,x.lab=NULL,y.lab=NULL,line.stag=NULL,struct=NULL,cex.val=1,
                      alpha.val=1,circle.col='lightblue',pos.col='black',neg.col='grey', max.sp = F, ...){
  
  require(scales)
  
  #sanity checks
  if('mlp' %in% class(mod.in)) warning('Bias layer not applicable for rsnns object')
  if('numeric' %in% class(mod.in)){
    if(is.null(struct)) stop('Three-element vector required for struct')
    if(length(mod.in) != ((struct[1]*struct[2]+struct[2]*struct[3])+(struct[3]+struct[2])))
      stop('Incorrect length of weight matrix for given network structure')
  }
  if('train' %in% class(mod.in)){
    if('nnet' %in% class(mod.in$finalModel)){
      mod.in<-mod.in$finalModel
      warning('Using best nnet model from train output')
    }
    else stop('Only nnet method can be used with train object')
  }
  
  #gets weights for neural network, output is list
  #if rescaled argument is true, weights are returned but rescaled based on abs value
  nnet.vals<-function(mod.in,nid,rel.rsc,struct.out=struct){
    
    require(scales)
    require(reshape)
    
    if('numeric' %in% class(mod.in)){
      struct.out<-struct
      wts<-mod.in
    }
    
    #neuralnet package
    if('nn' %in% class(mod.in)){
      struct.out<-unlist(lapply(mod.in$weights[[1]],ncol))
      struct.out<-struct.out[-length(struct.out)]
      struct.out<-c(
        length(mod.in$model.list$variables),
        struct.out,
        length(mod.in$model.list$response)
      )    		
      wts<-unlist(mod.in$weights[[1]])   
    }
    
    #nnet package
    if('nnet' %in% class(mod.in)){
      struct.out<-mod.in$n
      wts<-mod.in$wts
    }
    
    #RSNNS package
    if('mlp' %in% class(mod.in)){
      struct.out<-c(mod.in$nInputs,mod.in$archParams$size,mod.in$nOutputs)
      hid.num<-length(struct.out)-2
      wts<-mod.in$snnsObject$getCompleteWeightMatrix()
      
      #get all input-hidden and hidden-hidden wts
      inps<-wts[grep('Input',row.names(wts)),grep('Hidden_2',colnames(wts)),drop=F]
      inps<-melt(rbind(rep(NA,ncol(inps)),inps))$value
      uni.hids<-paste0('Hidden_',1+seq(1,hid.num))
      for(i in 1:length(uni.hids)){
        if(is.na(uni.hids[i+1])) break
        tmp<-wts[grep(uni.hids[i],rownames(wts)),grep(uni.hids[i+1],colnames(wts)),drop=F]
        inps<-c(inps,melt(rbind(rep(NA,ncol(tmp)),tmp))$value)
      }
      
      #get connections from last hidden to output layers
      outs<-wts[grep(paste0('Hidden_',hid.num+1),row.names(wts)),grep('Output',colnames(wts)),drop=F]
      outs<-rbind(rep(NA,ncol(outs)),outs)
      
      #weight vector for all
      wts<-c(inps,melt(outs)$value)
      assign('bias',F,envir=environment(nnet.vals))
    }
    
    if(nid) wts<-rescale(abs(wts),c(1,rel.rsc))
    
    #convert wts to list with appropriate names 
    hid.struct<-struct.out[-c(length(struct.out))]
    row.nms<-NULL
    for(i in 1:length(hid.struct)){
      if(is.na(hid.struct[i+1])) break
      row.nms<-c(row.nms,rep(paste('hidden',i,seq(1:hid.struct[i+1])),each=1+hid.struct[i]))
    }
    row.nms<-c(
      row.nms,
      rep(paste('out',seq(1:struct.out[length(struct.out)])),each=1+struct.out[length(struct.out)-1])
    )
    out.ls<-data.frame(wts,row.nms)
    out.ls$row.nms<-factor(row.nms,levels=unique(row.nms),labels=unique(row.nms))
    out.ls<-split(out.ls$wts,f=out.ls$row.nms)
    
    assign('struct',struct.out,envir=environment(nnet.vals))
    
    out.ls
    
  }
  
  wts<-nnet.vals(mod.in,nid=F)
  
  if(wts.only) return(wts)
  
  #circle colors for input, if desired, must be two-vector list, first vector is for input layer
  if(is.list(circle.col)){
    circle.col.inp<-circle.col[[1]]
    circle.col<-circle.col[[2]]
  }
  else circle.col.inp<-circle.col
  
  #initiate plotting
  x.range<-c(0,100)
  y.range<-c(0,100)
  #these are all proportions from 0-1
  if(is.null(line.stag)) line.stag<-0.011*circle.cex/2
  layer.x<-seq(0.17,0.9,length=length(struct))
  bias.x<-layer.x[-length(layer.x)]+diff(layer.x)/2
  bias.y<-0.95
  circle.cex<-circle.cex
  
  #get variable names from mod.in object
  #change to user input if supplied
  if('numeric' %in% class(mod.in)){
    x.names<-paste0(rep('X',struct[1]),seq(1:struct[1]))
    y.names<-paste0(rep('Y',struct[3]),seq(1:struct[3]))
  }
  if('mlp' %in% class(mod.in)){
    all.names<-mod.in$snnsObject$getUnitDefinitions()
    x.names<-all.names[grep('Input',all.names$unitName),'unitName']
    y.names<-all.names[grep('Output',all.names$unitName),'unitName']
  }
  if('nn' %in% class(mod.in)){
    x.names<-mod.in$model.list$variables
    y.names<-mod.in$model.list$respons
  }
  if('xNames' %in% names(mod.in)){
    x.names<-mod.in$xNames
    y.names<-attr(terms(mod.in),'factor')
    y.names<-row.names(y.names)[!row.names(y.names) %in% x.names]
  }
  if(!'xNames' %in% names(mod.in) & 'nnet' %in% class(mod.in)){
    if(is.null(mod.in$call$formula)){
      x.names<-colnames(eval(mod.in$call$x))
      y.names<-colnames(eval(mod.in$call$y))
    }
    else{
      forms<-eval(mod.in$call$formula)
      x.names<-mod.in$coefnames
      facts<-attr(terms(mod.in),'factors')
      y.check<-mod.in$fitted
      if(ncol(y.check)>1) y.names<-colnames(y.check)
      else y.names<-as.character(forms)[2]
    } 
  }
  #change variables names to user sub 
  if(!is.null(x.lab)){
    if(length(x.names) != length(x.lab)) stop('x.lab length not equal to number of input variables')
    else x.names<-x.lab
  }
  if(!is.null(y.lab)){
    if(length(y.names) != length(y.lab)) stop('y.lab length not equal to number of output variables')
    else y.names<-y.lab
  }
  
  #initiate plot
  plot(x.range,y.range,type='n',axes=F,ylab='',xlab='',...)
  
  #function for getting y locations for input, hidden, output layers
  #input is integer value from 'struct'
  get.ys<-function(lyr, max_space = max.sp){
    if(max_space){ 
      spacing <- diff(c(0*diff(y.range),0.9*diff(y.range)))/lyr
    } else {
      spacing<-diff(c(0*diff(y.range),0.9*diff(y.range)))/max(struct)
    }
    
    seq(0.5*(diff(y.range)+spacing*(lyr-1)),0.5*(diff(y.range)-spacing*(lyr-1)),
        length=lyr)
  }
  
  #function for plotting nodes
  #'layer' specifies which layer, integer from 'struct'
  #'x.loc' indicates x location for layer, integer from 'layer.x'
  #'layer.name' is string indicating text to put in node
  layer.points<-function(layer,x.loc,layer.name,cex=cex.val){
    x<-rep(x.loc*diff(x.range),layer)
    y<-get.ys(layer)
    points(x,y,pch=21,cex=circle.cex,col=in.col,bg=bord.col)
    if(node.labs) text(x,y,paste(layer.name,1:layer,sep=''),cex=cex.val)
    if(layer.name=='I' & var.labs) text(x-line.stag*diff(x.range),y,x.names,pos=2,cex=cex.val)      
    if(layer.name=='O' & var.labs) text(x+line.stag*diff(x.range),y,y.names,pos=4,cex=cex.val)
  }
  
  #function for plotting bias points
  #'bias.x' is vector of values for x locations
  #'bias.y' is vector for y location
  #'layer.name' is  string indicating text to put in node
  bias.points<-function(bias.x,bias.y,layer.name,cex,...){
    for(val in 1:length(bias.x)){
      points(
        diff(x.range)*bias.x[val],
        bias.y*diff(y.range),
        pch=21,col=in.col,bg=bord.col,cex=circle.cex
      )
      if(node.labs)
        text(
          diff(x.range)*bias.x[val],
          bias.y*diff(y.range),
          paste(layer.name,val,sep=''),
          cex=cex.val
        )
    }
  }
  
  #function creates lines colored by direction and width as proportion of magnitude
  #use 'all.in' argument if you want to plot connection lines for only a single input node
  layer.lines<-function(mod.in,h.layer,layer1=1,layer2=2,out.layer=F,nid,rel.rsc,all.in,pos.col,
                        neg.col,...){
    
    x0<-rep(layer.x[layer1]*diff(x.range)+line.stag*diff(x.range),struct[layer1])
    x1<-rep(layer.x[layer2]*diff(x.range)-line.stag*diff(x.range),struct[layer1])
    
    if(out.layer==T){
      
      y0<-get.ys(struct[layer1])
      y1<-rep(get.ys(struct[layer2])[h.layer],struct[layer1])
      src.str<-paste('out',h.layer)
      
      wts<-nnet.vals(mod.in,nid=F,rel.rsc)
      wts<-wts[grep(src.str,names(wts))][[1]][-1]
      wts.rs<-nnet.vals(mod.in,nid=T,rel.rsc)
      wts.rs<-wts.rs[grep(src.str,names(wts.rs))][[1]][-1]
      
      cols<-rep(pos.col,struct[layer1])
      cols[wts<0]<-neg.col
      
      if(nid) segments(x0,y0,x1,y1,col=cols,lwd=wts.rs)
      else segments(x0,y0,x1,y1)
      
    }
    
    else{
      
      if(is.logical(all.in)) all.in<-h.layer
      else all.in<-which(x.names==all.in)
      
      y0<-rep(get.ys(struct[layer1])[all.in],struct[2])
      y1<-get.ys(struct[layer2])
      src.str<-paste('hidden',layer1)
      
      wts<-nnet.vals(mod.in,nid=F,rel.rsc)
      wts<-unlist(lapply(wts[grep(src.str,names(wts))],function(x) x[all.in+1]))
      wts.rs<-nnet.vals(mod.in,nid=T,rel.rsc)
      wts.rs<-unlist(lapply(wts.rs[grep(src.str,names(wts.rs))],function(x) x[all.in+1]))
      
      cols<-rep(pos.col,struct[layer2])
      cols[wts<0]<-neg.col
      
      if(nid) segments(x0,y0,x1,y1,col=cols,lwd=wts.rs)
      else segments(x0,y0,x1,y1)
      
    }
    
  }
  
  bias.lines<-function(bias.x,mod.in,nid,rel.rsc,all.out,pos.col,neg.col,...){
    
    if(is.logical(all.out)) all.out<-1:struct[length(struct)]
    else all.out<-which(y.names==all.out)
    
    for(val in 1:length(bias.x)){
      
      wts<-nnet.vals(mod.in,nid=F,rel.rsc)
      wts.rs<-nnet.vals(mod.in,nid=T,rel.rsc)
      
      if(val != length(bias.x)){
        wts<-wts[grep('out',names(wts),invert=T)]
        wts.rs<-wts.rs[grep('out',names(wts.rs),invert=T)]
        sel.val<-grep(val,substr(names(wts.rs),8,8))
        wts<-wts[sel.val]
        wts.rs<-wts.rs[sel.val]
      }
      
      else{
        wts<-wts[grep('out',names(wts))]
        wts.rs<-wts.rs[grep('out',names(wts.rs))]
      }
      
      cols<-rep(pos.col,length(wts))
      cols[unlist(lapply(wts,function(x) x[1]))<0]<-neg.col
      wts.rs<-unlist(lapply(wts.rs,function(x) x[1]))
      
      if(nid==F){
        wts.rs<-rep(1,struct[val+1])
        cols<-rep('black',struct[val+1])
      }
      
      if(val != length(bias.x)){
        segments(
          rep(diff(x.range)*bias.x[val]+diff(x.range)*line.stag,struct[val+1]),
          rep(bias.y*diff(y.range),struct[val+1]),
          rep(diff(x.range)*layer.x[val+1]-diff(x.range)*line.stag,struct[val+1]),
          get.ys(struct[val+1]),
          lwd=wts.rs,
          col=cols
        )
      }
      
      else{
        segments(
          rep(diff(x.range)*bias.x[val]+diff(x.range)*line.stag,struct[val+1]),
          rep(bias.y*diff(y.range),struct[val+1]),
          rep(diff(x.range)*layer.x[val+1]-diff(x.range)*line.stag,struct[val+1]),
          get.ys(struct[val+1])[all.out],
          lwd=wts.rs[all.out],
          col=cols[all.out]
        )
      }
      
    }
  }
  
  #use functions to plot connections between layers
  #bias lines
  if(bias) bias.lines(bias.x,mod.in,nid=nid,rel.rsc=rel.rsc,all.out=all.out,pos.col=alpha(pos.col,alpha.val),
                      neg.col=alpha(neg.col,alpha.val))
  
  #layer lines, makes use of arguments to plot all or for individual layers
  #starts with input-hidden
  #uses 'all.in' argument to plot connection lines for all input nodes or a single node
  if(is.logical(all.in)){  
    mapply(
      function(x) layer.lines(mod.in,x,layer1=1,layer2=2,nid=nid,rel.rsc=rel.rsc,
                              all.in=all.in,pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val)),
      1:struct[1]
    )
  }
  else{
    node.in<-which(x.names==all.in)
    layer.lines(mod.in,node.in,layer1=1,layer2=2,nid=nid,rel.rsc=rel.rsc,all.in=all.in,
                pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val))
  }
  #connections between hidden layers
  lays<-split(c(1,rep(2:(length(struct)-1),each=2),length(struct)),
              f=rep(1:(length(struct)-1),each=2))
  lays<-lays[-c(1,(length(struct)-1))]
  for(lay in lays){
    for(node in 1:struct[lay[1]]){
      layer.lines(mod.in,node,layer1=lay[1],layer2=lay[2],nid=nid,rel.rsc=rel.rsc,all.in=T,
                  pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val))
    }
  }
  #lines for hidden-output
  #uses 'all.out' argument to plot connection lines for all output nodes or a single node
  if(is.logical(all.out))
    mapply(
      function(x) layer.lines(mod.in,x,layer1=length(struct)-1,layer2=length(struct),out.layer=T,nid=nid,rel.rsc=rel.rsc,
                              all.in=all.in,pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val)),
      1:struct[length(struct)]
    )
  else{
    node.in<-which(y.names==all.out)
    layer.lines(mod.in,node.in,layer1=length(struct)-1,layer2=length(struct),out.layer=T,nid=nid,rel.rsc=rel.rsc,
                pos.col=pos.col,neg.col=neg.col,all.out=all.out)
  }
  
  #use functions to plot nodes
  for(i in 1:length(struct)){
    in.col<-bord.col<-circle.col
    layer.name<-'H'
    if(i==1) { layer.name<-'I'; in.col<-bord.col<-circle.col.inp}
    if(i==length(struct)) layer.name<-'O'
    layer.points(struct[i],layer.x[i],layer.name)
  }
  
  if(bias) bias.points(bias.x,bias.y,'B')
  
}

# plot model 
plot.nnet(mh.nnet.final)


nn_mh_test <- (ann[,-14])
pred <- predict(mh.nnet.final, nn_mh_test)
#Now plot the predicted vs observed data (figure 4):

x <- pred*50
y <- ann$mh_tot
plot(x,y, main="Neural network predictions vs observed", xlab="Observed", ylab="Predicted")






