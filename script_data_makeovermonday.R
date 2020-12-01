#Libraries inladen
library(tidyverse)
library(lubridate)
library(sf)
library(httr)    

##Bronnen inladen
#Geodata gemeenten downloaden
url <- parse_url("https://geodata.nationaalgeoregister.nl/bestuurlijkegrenzen/wfs")
url$query <- list(service = "WFS",
                  version = "2.0.0",
                  request = "GetFeature",
                  typename = "bestuurlijkegrenzen:gemeenten",
                  output = "application/json")
request <- build_url(url)
gemeenten <- st_read(request, 
                     crs = 28992)

#CBS vierkantstatistiek (voor aantal inwoners)
cbs_vk500 <- st_read("~/Persoonlijke_projecten/Covid_dataviz_wedstrijd/bron_cbs_vk500_2019.gpkg",
                     "bron_cbs_vk500_2019",
                     crs = 28992)

#Cijfers RIVM (voor aantal sterfgevallen)
bron_rivm <- read.csv2("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv",
                       stringsAsFactors = F)

##Data verwerken
#Gemeenten toewijzen aan vierkantstatistieken
cbs_vk100 <- cbs_vk100[ , c("c28992r100", "aantal_inwoners", "geom")]  %>%
  filter(cbs_vk100$aantal_inwoners > 0) #overbodige kolommen en inwonerloze rijen verwijderen

cbs_vk500_join <- st_join(cbs_vk500, 
                     gemeenten[ , c("code", "gemeentenaam", "geom")],
                     join = st_intersects,
                     largest = T) #gemeentenamen aan vierkantstatistieken hangen

#Meest recente statistiek maken
rivm_overleden_dag <- bron_rivm %>%
  group_by(Date_of_report, Municipality_name) %>%
  summarize(n_overleden = sum(Deceased)) %>%
  as.data.frame() %>%
  filter(Date_of_report == as_date((today()-1)))

##Input ruimtelijke functie genereren
#Functie definieren voor aantal toe te wijzen punten
nsplit = function(X,n){
  p = X/sum(X)
  diff(round(n*cumsum(c(0,p))))
}

#Leeg dataframe maken voor for loop
cbs_vk500_output <- data.frame(
  c28992r500 = NA,
  aantal_inwoners = NA,
  aantal_mannen = NA,
  aantal_vrouwen = NA,
  code = NA,
  gemeentenaam = NA,
  n_points = NA
)

#Punten verdelen o.b.v. inwoners
for (i in rivm_overleden_dag$Municipality_name){
  temp_cbs <- filter(cbs_vk500_join, cbs_vk500_join$gemeentenaam == i) %>% st_drop_geometry()
  temp_n_overleden <- rivm_overleden_dag[rivm_overleden_dag$Municipality_name == i, 3]
  temp_n_points <- nsplit(temp_cbs$aantal_inwoners, temp_n_overleden)
  temp_cbs$n_points <- temp_n_points
  
  cbs_vk500_output <- rbind(cbs_vk500_output, temp_cbs)
}

#Geometrie van vierkantstatistieken toevoegen, lege eerste rij verwijderen
cbs_vk500_output <- cbs_vk500_output[-1, ] %>%
  left_join(cbs_vk500_join[ , c("c28992r500", "geom")], by = "c28992r500") %>%
  st_sf(crs = 28992)

#Punten zetten obv gewogen inwoneraantal per 500x500 vierkant
points <- st_sample(cbs_vk500_output, size = cbs_vk500_output$n_points,
                    type = 'random', exact = T) %>%
  st_sf('ID' = seq(length(.)), 'geometry' = .) %>%
  st_intersection(., cbs_vk500_output)


#Wegschrijven voor laden in flexdashboard en visuele check in qgis
st_write(points, "points_20201129_output.gpkg", "points_20201129_output.gpkg")


#Gemeenten ruimtelijk maken met n_overledenen voor flexdashboard
gemeenten <- left_join(gemeenten,
                       rivm_overleden_dag,
                       by = c("gemeentenaam" = "Municipality_name"))
gemeenten <- gemeenten[ , c(3, 5, 6)]
st_write(gemeenten, "~/Persoonlijke_projecten/Covid_dataviz_wedstrijd/points_20201129_output.gpkg", "gemeenten_sterfgevallen", append = F)


