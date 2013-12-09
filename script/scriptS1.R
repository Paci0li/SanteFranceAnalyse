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
vienne <- readShapePoly("data/86-Vienne.shp")
france <- readShapePoly("data/departements-metropole.shp")

#renommage des colonnes
names(temps) <- c("commune", "minutes")

#création de la table consolidée
temps.communes <- temps[,]

#suppression des valeurs aberrantes et travail sur les données
#temps.communes$minutes[temps$minutes>180] <- NA
temps.communes$dep <- floor(temps.communes$commune/1000)
temps.communes$dep <- as.character(temps.communes$dep)
temps.communes$dep <- revalue(temps.communes$dep, c("1"="01", "2"="02", "3"="03", "4"="04", "5"="05", "6"="06", "7"="07", "8"="08", "9"="09"))

#analyses univariés sur minutes
summary(temps.communes$minutes)

#résumé des données par département
temps.dep <- ddply(temps.communes, .(dep), summarise, moy = mean(minutes, na.rm=TRUE))
temps.dep <- data.frame(dep=c(temps.dep$dep, "2A", "2B"), moy=c(temps.dep$moy, NA, NA))

#création d'une variable pour ordonner la data frame
temps.dep$order <- reorder(temps.dep$dep, temps.dep$moy)

#création de l'histogramme
p <- ggplot(temps.dep, aes(y=moy))
p + geom_bar(aes(x=order), data=temps.dep, stat="identity", width=.5) + labs(x="départements", y="temps moyen (mn)", title="temps pour se faire soigner par département") + geom_hline(yintercept=33.35, color="red3") + annotate("text", x=6, y=35, label="moyenne", colour="red3")

#création de la carte
gpclibPermit()

#création de la carte de france
temps.dep$moy <- as.integer(round(temps.dep$moy))
france@data$id <- rownames(france@data)
france.point <- fortify(france, region="NUMERO")
france.point <- merge(france.point, temps.dep, by.x="id", by.y="dep", all.x=TRUE)
france.map <- ggplot(france.point, aes(x=long, y=lat, group=group, fill=moy)) + geom_polygon() + geom_path(colour="grey", size=0.1) + coord_map() + labs(title="temps pour se faire soigner par département en France")
france.map

#on rajoute un colonne id à carte@data
vienne@data$id <- rownames(vienne@data)

#pour pouvoir transformer un format lisible par ggplot2
vienne.point <- fortify(vienne, region="CODE_INSEE")
vienne.point <- merge(vienne.point, temps, by.x="id", by.y="commune")

#création du graph
#attention à l'ordre des fonctions
vienne.map <- ggplot(data=vienne.point, aes(x=long, y=lat, group=group, fill=minutes) ) + geom_polygon() + geom_path(colour="grey", size=0.1) + coord_map() + labs(title="temps pour se faire soigner par communes de la vienne")


#affichage du graph
vienne.map


