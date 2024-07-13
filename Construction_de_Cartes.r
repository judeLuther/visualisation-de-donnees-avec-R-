#### Construction de cartes ####
# Référence : https://lrouviere.github.io/TUTO_VISU/correction/faire-des-cartes-avec-r.html
# Document : "Tutoriel_VisualisationDesDonnees.pdf" à télécharger sur Moodle

library(dplyr)
library(tidyverse)

#### page 73 ####
library(ggmap)

# Register your Stadia Maps API key

register_stadiamaps("9a994b9e-9412-4568-bf72-095e322d68bc", write = TRUE)

# Pour tracer la carte des Etats Unis :
## On renseigne les coordonnées géographiques : les longitudes et les latitudes
us <- c(left = -125, bottom = 25.75, right = -67, top = 49) 
map <- get_stadiamap(us, zoom = 5) # la carte US
ggmap(map) # trace la carte US

##### page 74  ####
# Pour tracer la carte de l'Europe :
europe <- c(left = -12, bottom = 35, right = 30, top = 63)
get_stadiamap(europe, zoom = 5) %>% ggmap()
##### page 75 ####
# On peut également changer le fond de carte :
get_stamenmap(europe, zoom = 5, maptype = "toner-background") %>% ggmap()

# Pour tracer la carte de la France :
fr <- c(left = -6, bottom = 41, right = 10, top = 52)
get_stadiamap(fr, zoom = 5) %>% ggmap()

# Pour tracer la carte de l'Afrique :
Africa <- c(left=-18.5, bottom= -35 , right = 52, top = 38)
get_stadiamap(Africa, zoom = 4) %>% ggmap()

##### page 76  ####
# La fonction "geocode()" de "ggmap" qui permettait de récupérer des longitudes et latitudes 
# nécessite désormais une API, ce qui contraint son utilisation. 
# Nous proposons d’utiliser la fonction suivante :

if (!(require(jsonlite))) install.packages("jsonlite")
mygeocode <- function(adresses){
  # adresses est un vecteur contenant toutes les adresses sous forme de chaine de caracteres
  nominatim_osm <- function(address = NULL){
    ## details: http://wiki.openstreetmap.org/wiki/Nominatim
    ## fonction nominatim_osm proposée par D.Kisler
    if(suppressWarnings(is.null(address)))  return(data.frame())
    tryCatch(
      d <- jsonlite::fromJSON(
        gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
             'http://nominatim.openstreetmap.org/search?q=@addr@&format=json&addressdetails=0&limit=1')
      ), error = function(c) return(data.frame())
    )
    if(length(d) == 0) return(data.frame())
    return(c(as.numeric(d$lon), as.numeric(d$lat)))
  }
  tableau <- t(sapply(adresses,nominatim_osm))
  colnames(tableau) <- c("lon","lat")
  return(tableau)
}

##### page 77 ####
mygeocode("the white house")
# on obtient le résultat suivant :
#                    lon     lat
# the white housse -77.03655 38.8977

mygeocode("Paris")
# on obtient le résultat suivant :
#          lon     lat
# Paris 2.351462 48.8567

mygeocode("Rennes")
# on obtient le résultat suivant :
#           lon     lat
# Rennes -1.68002 48.11134

mygeocode("51100 Reims, France")
# on obtient le résultat suivant :
#                         lon     lat
# 51100 Reims, France  4.026549 49.26027


#### Exercice 2.1 (Populations des grandes villes de France) ####
# 1. Récupérer les latitudes et longitudes de Paris, Lyon et Marseille et 
# représenter ces 3 villes sur une carte de la France.
V <- c("Paris","Lyon","Marseille")
A <- mygeocode(V)
A

library(dplyr)
A %>% as_tibble() %>% mutate(Villes=V) -> A
A
fr <- c(left = -6, bottom = 41, right = 10, top = 52)
fond <- get_stadiamap(fr, zoom = 5) 
ggmap(fond)+geom_point(data=A, aes(x=lon, y=lat), color="red")

##### page 78 ####
# importer le fichier de données  "villes_fr.csv"
library(tidyverse)
df <- read_csv(file.choose())

df$Commune[10]
df$Commune[10] <- "Lille"

coord <- mygeocode(as.character(df$Commune)) %>% as_tibble()
df1 <- bind_cols(df,coord)

View(df1)
  
ggmap(fond)+geom_point(data=df1, aes(x=lon, y=lat), color="red")
ggmap(fond)+geom_point(data=df1, aes(x=lon, y=lat, size=`2014`), color="red")

##### page 79 #####
library(sf)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
class(nc)
nc
##### p 80 ####
plot(st_geometry(nc))

ggplot(nc)+geom_sf()

##### p 81 ####
ggplot(nc[1:3,]) +
  geom_sf(aes(fill = AREA)) + 
   geom_sf_label(aes(label = NAME))

##### p 82 ####
coord.ville.nc <- mygeocode(paste(as.character(nc$NAME),"NC"))
coord.ville.nc <- as.data.frame(coord.ville.nc)
names(coord.ville.nc) <- c("lon","lat")

# importer le fichier de données "coord_ville_nc.csv"
 coord.ville.nc <- read_csv(file.choose())

coord.ville1.nc <- coord.ville.nc %>%  
       filter(lon<=-77 & lon>=-85 & lat>=33 & lat<=37) %>% 
        as.matrix() %>% st_multipoint() %>% st_geometry() %>% st_cast(to="POINT")

coord.ville1.nc

st_crs(coord.ville1.nc) <- 4326 

ggplot(nc)+geom_sf()+geom_sf(data=coord.ville1.nc)

nc2 <- nc %>% mutate(centre=st_centroid(nc)$geometry)
       
ggplot(nc2)+geom_sf()+geom_sf(aes(geometry=centre))

##### page 83 ####
# Exercice 2.2
# Importer les fichiers contenus dans le répertoire "dpt" 
dpt <- read_sf("/Users/amorkeziou/Documents/DisqueExterne/Enseignement/2022_2023/INFO0808_VisualisationDeDonnees/Data/dpt")
ggplot(dpt) + geom_sf()

##### page 84 ####
coord.ville1 <- data.frame(df1[, c("lon", "lat")]) %>%
                  as.matrix() %>% st_multipoint() %>% st_geometry()

coord.ville2 <- st_cast(coord.ville1, to = "POINT")
coord.ville1

st_geometry(df1) <- coord.ville2
st_crs(df1) <- 4326
ggplot(dpt)+geom_sf(fill="white")+
  geom_sf(data=df1, aes(size=`2014`), color="red")+theme_void()

##### page 85 ####
# Exercice 2.3
# importer le fichier de données "tauxchomage.csv"
chomage <- read_delim(file.choose(), delim=";")
dpt <- read_sf("/Users/amorkeziou/Documents/DisqueExterne/Enseignement/2022_2023/INFO0808_VisualisationDeDonnees/Data/dpt")
dpt2 <- inner_join(dpt, chomage, by="CODE_DEPT")

dpt3 <- dpt2 %>% select(A2006=TCHOMB1T06, A2011=TCHOMB1T11, geometry) %>%
      pivot_longer(-geometry, names_to="Annee", values_to="TxChomage") %>% st_as_sf()

ggplot(dpt3) + aes(fill = TxChomage)+geom_sf() +
  facet_wrap(~Annee, nrow = 1)+
  scale_fill_gradient(low="white", high="brown")+theme_bw()

##### page 86 ####
# importer le fichier de données "synop.2021031912.csv"
donnees <- read_delim(file.choose(), 
                       delim=";", col_types = cols(t=col_double()))

# importer le fichier de données "postesSynop.csv"
station <- read_delim(file.choose(), delim=";")

donnees$t <- donnees$t-273.15 #on passe en degrés celcius
temp <- donnees %>% select(numer_sta,t)
names(temp)[1] <- c("ID")
D <- inner_join(temp, station, by = c("ID"))

station1 <- D %>% filter(Longitude<25 & Longitude>-20) %>% na.omit()
station4326 <- st_multipoint(as.matrix(station1[,5:4])) %>% st_geometry()
st_crs(station4326) <- 4326
ggplot(dpt) + geom_sf()+geom_sf(data=station4326)

station2 <- station1 %>% select(Longitude,Latitude) %>%
             as.matrix() %>% st_multipoint() %>% st_geometry()
st_crs(station2) <- 4326
station2 <- st_cast(station2, to = "POINT")

df <- data.frame(temp=station1$t)
st_geometry(df) <- station2

ggplot(dpt) + geom_sf(fill="white")+
  geom_sf(data=df,aes(color=temp),size=2)+
  scale_color_continuous(low="yellow",high="red")

centro <- st_centroid(dpt$geometry)
centro <- st_transform(centro,crs=4326)

library(lwgeom)
DD <- st_distance(df,centro)

NN <- apply(DD,2,order)[1,]
t_prev <- station1[NN,2]

dpt1 <- dpt %>% mutate(t_prev=as.matrix(t_prev))
ggplot(dpt1) + geom_sf(aes(fill=t_prev)) +
  scale_fill_continuous(low="yellow",high="red")+theme_void()

ggplot(dpt1) + geom_sf(aes(fill=t_prev,color=t_prev)) +
  scale_fill_continuous(low="yellow",high="red") +
  scale_color_continuous(low="yellow",high="red")+theme_void()

##### page 89 ####
library(rgeos)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")+theme_void()

##### page 90 ####
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

regions <- read_sf("/Users/amorkeziou/Documents/DisqueExterne/Enseignement/2022_2023/INFO0808_VisualisationDeDonnees/Data/regions-20180101-shp/")

##### page 91 ####
format(object.size(regions),units="Mb")

library(rmapshaper)
regions1 <- ms_simplify(regions)
format(object.size(regions1),units="Mb")

ggplot(regions1)+geom_sf()+
  coord_sf(xlim = c(-5.5,10),ylim=c(41,51))+theme_void()

##### page 91 ####
library(leaflet)
leaflet() %>% addTiles()

##### p 92 ####
Paris <- mygeocode("paris")
#          lon     lat
# Paris 2.351462 48.8567

Paris <- matrix(c(2.351462, 48.8567), nrow=1, ncol=2)
colnames(Paris) <- c("lon", "lat")
row.names(Paris) <- "Paris"
Paris

m2 <- leaflet() %>% setView(lng = Paris[1], lat = Paris[2], zoom = 12) %>%
       addTiles()
m2 %>% addProviderTiles("Stamen.Toner")

##### p 93 ####
m2 %>% addProviderTiles("Wikimedia")

##### p 94 ####
m2 %>% addProviderTiles("Esri.NatGeoWorldMap")

##### p 95 ####
m2 %>%
  addProviderTiles("Stamen.Watercolor") %>%
  addProviderTiles("Stamen.TonerHybrid")

##### p 96 ####
data(quakes)
str(quakes)
leaflet(data = quakes[1:20,]) %>% addTiles() %>% 
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))

##### P 97 ####
content <- paste(sep = "<br/>",
                 "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                 "606 5th Ave. S",
                 "Seattle, WA 98138")

leaflet() %>% addTiles() %>%
  addPopups(-122.327298, 47.597131, content,
            options = popupOptions(closeButton = FALSE))

##### P 98 ####
# Exercice 2.4

R2 <- mygeocode("Universite Rennes 2 Villejean") %>% as_tibble()
# On obtient 
#                                  lon      lat 
# Universite Rennes 2 Villejean -1.701487  48.11938

R2 <- matrix(c(-1.701487, 48.11938), nrow = 1, ncol = 2)
colnames(R2) <- c("lon", "lat")
row.names(R2) <- "Universite Rennes 2 Villejean"
R2 <- as_tibble(R2)

info <- paste(sep = "<br/>",
              "<b><a href='https://www.univ-rennes2.fr'>Universite Rennes 2</a></b>",
              "Campus Villejean")
leaflet() %>% addTiles() %>%
  addPopups(R2[1]$lon, R2[2]$lat, info, options = popupOptions(closeButton = FALSE))


##### P 99 ####
#lien <- "https://opendata.paris.fr/explore/dataset/velib-disponibilite-en-temps-reel/export/?disjunctive.name&disjunctive.is_installed&disjunctive.is_renting&disjunctive.is_returning&disjunctive.nom_arrondissement_communes/velib-disponibilite-en-temps-reel"

# importer le fichier de données "velib-disponibilite-en-temps-reel.csv"
sta.Paris <- read_delim(file.choose(), delim=";")

sta.Paris1 <- sta.Paris %>% separate(`Coordonnées géographiques`,
                                     into=c("lat","lon"),sep=",") %>%
                             mutate(lat=as.numeric(lat),lon=as.numeric(lon))

##### P 100 ####
map.velib1 <- leaflet(data = sta.Paris1) %>%
  addTiles() %>%
  addCircleMarkers(~ lon, ~ lat,radius=3,
                   stroke = FALSE, fillOpacity = 0.5,color="red"
  )

map.velib1

map.velib2 <- leaflet(data = sta.Paris1) %>%
              addTiles() %>%
               addCircleMarkers(~ lon, ~ lat,radius=3,stroke = FALSE,
                   fillOpacity = 0.7,color="red",
                   popup = ~ sprintf("<b> Vélos dispos: %s</b>",
                            as.character(`Nombre total vélos disponibles`)))
#ou sans sprintf
map.velib2 <- leaflet(data = sta.Paris1) %>%
              addTiles() %>%
              addCircleMarkers(~ lon, ~ lat,radius=3,stroke = FALSE, fillOpacity = 0.7,color="red",
              popup = ~ paste("Vélos dispos :", as.character(`Nombre total vélos disponibles`)))

map.velib2

map.velib3 <- leaflet(data = sta.Paris1) %>%
              addTiles() %>%
              addCircleMarkers(~ lon, ~ lat,radius=3,stroke = FALSE,
                   fillOpacity = 0.7,color="red",
                   popup = ~ paste(as.character(`Nom station`),", Vélos dispos :",
                                   as.character(`Nombre total vélos disponibles`),
                                   sep=""))
map.velib3

ColorPal1 <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7",
                                                   space = "Lab"), domain = c(0,1))
ColorPal2 <- colorNumeric(scales::seq_gradient_pal(low = "red", high = "black",
                                                   space = "Lab"), domain = c(0,1))

map.velib4 <- leaflet(data = sta.Paris1) %>%
           addTiles() %>%
            addCircleMarkers(~ lon, ~ lat,radius=3,stroke = FALSE, fillOpacity = 0.7,
                   color=~ColorPal1(`Nombre total vélos disponibles`/
                                      `Capacité de la station`),
                   popup = ~ paste(as.character(`Nom station`),", Vélos dispos :",
                                   as.character(`Nombre total vélos disponibles`),
                                   sep=""))
map.velib4

map.velib5 <- leaflet(data = sta.Paris1) %>%
  addTiles() %>%
  addCircleMarkers(~ lon, ~ lat,stroke = FALSE, fillOpacity = 0.7,
                   color=~ColorPal2(`Nombre total vélos disponibles`/
                                      `Capacité de la station`),
                   radius=~(`Nombre total vélos disponibles`/
                              `Capacité de la station`)*8,
                   popup = ~ paste(as.character(`Nom station`),", Vélos dispos :",
                                   as.character(`Nombre total vélos disponibles`),
                                   sep=""))
map.velib5

nom.station <- "Jussieu - Fossés Saint-Bernard"
local.station <- function(nom.station){
  df <- sta.Paris1 %>% filter(`Nom station`==nom.station)
  leaflet(data = sta.Paris1) %>% setView(lng=df$lon,lat=df$lat,zoom=15) %>%
    addTiles() %>%
    addCircleMarkers(~ lon, ~ lat,stroke = FALSE, fillOpacity = 0.7,
                     popup = ~ paste(as.character(`Nom station`),", Vélos dispos :",
                                     as.character(`Nombre total vélos disponibles`),
                                     sep="")) %>%
    addMarkers(lng=df$lon,lat=df$lat,
               popup = ~ paste(nom.station,", Vélos dispos :",
                               as.character(df$`Nombre total vélos disponibles`),
                               sep=""),
               popupOptions = popupOptions(noHide = T))
}

local.station("Jussieu - Fossés Saint-Bernard")

local.station("Gare Montparnasse - Arrivée")

##### P 106 ####
dpt2 <- st_transform(dpt1, crs = 4326)
dpt2$t_prev <- round(dpt2$t_prev)
pal <- colorNumeric(scales::seq_gradient_pal(low = "yellow", high = "red",
                                             space = "Lab"), domain = dpt2$t_prev)
m <- leaflet() %>% addTiles() %>%
  addPolygons(data = dpt2,color=~pal(t_prev),fillOpacity = 0.6,
              stroke = TRUE,weight=1,
              popup=~paste(as.character(NOM_DEPT),as.character(t_prev),sep=" : ")) %>%
  addLayersControl(options=layersControlOptions(collapsed = FALSE))
m

pal1 <- colorNumeric(palette = c("inferno"),domain = dpt2$t_prev)
m1 <- leaflet() %>% addTiles() %>%
  addPolygons(data = dpt2,color=~pal1(t_prev),fillOpacity = 0.6,
              stroke = TRUE,weight=1,
              popup=~paste(as.character(NOM_DEPT),as.character(t_prev),sep=" : ")) %>%
  addLayersControl(options=layersControlOptions(collapsed = FALSE))
m1





