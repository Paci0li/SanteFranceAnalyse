#dossier de travial
setwd("/home/clement/Projet/R/santeVIenne/")

# chargement des Packages
library("ggplot2")
library("plyr")
library("maptools")
library("mapproj")
library("gpclib")

#chargement des données
temps <- read.csv("data/tempsSoin.csv")
france <- readShapePoly("data/departements-metropole.shp")

#renommage des colonnes
names(temps) <- c("commune", "minutes")

#suppression des valeurs aberrantes et travail sur les données
#temps.communes$minutes[temps$minutes>180] <- NA
temps$dep <- floor(temps$commune/1000)
temps$dep <- as.character(temps$dep)
temps$dep <- revalue(temps$dep, c("1"="01", "2"="02", "3"="03", "4"="04", "5"="05", "6"="06", "7"="07", "8"="08", "9"="09"))

#résumé des données par département
temps.dep <- ddply(temps, .(dep), summarise, moy = mean(minutes, na.rm=TRUE))
temps.dep <- data.frame(dep=c(temps.dep$dep, "2A", "2B"), moy=c(temps.dep$moy, NA, NA))

#création de la carte
gpclibPermit()

#création de la carte de france
temps.dep$moy <- as.integer(round(temps.dep$moy))
france@data$id <- rownames(france@data)
france.point <- fortify(france, region="NUMERO")
france.point <- merge(france.point, temps.dep, by.x="id", by.y="dep", all.x=TRUE)
france.map <- ggplot(france.point, aes(x=long, y=lat, group=group, fill=moy)) + geom_polygon() + geom_path(colour="grey", size=0.1) + coord_map()
france.map