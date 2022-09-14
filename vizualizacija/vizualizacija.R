# 3. faza: Vizualizacija podatkov

library(tmap)
library(digest)
library(ggplot2)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(reshape2)
library(scales)


# Uvoz zemljevida:
world_map <- ne_countries(scale = 50, returnclass = 'sf')
european_union <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                    "Czech Rep.","Denmark","Estonia","Finland","France",
                    "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                    "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                    "Portugal","Romania","Slovakia","Slovenia","Spain",
                    "Sweden", "Norway")
european_union[6] <- "Czechia"
european_union_map <- world_map %>% 
  filter(name %in% european_union)
bbox_europe <- st_bbox(c(xmin = -10, ymin = 20, xmax = 50, ymax = 80), crs = st_crs(european_union_map))
european_union_map_cropped <- st_crop(european_union_map, bbox_europe)

#1. zemljevid: sprememba ocene dobro
drzave <- intersect(skupaj$Država,european_union)
df <- tibble(country = drzave, some_value = skupaj$OCENA_DOBRO)

map <- european_union_map_cropped %>% 
  left_join(df, by = c("name" = "country"))

zemljevid1 <- ggplot(data = map) +
  geom_sf(mapping = aes(fill = some_value)) +
  scale_fill_gradient2(name = "Sprememba ", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50") +
  labs(title = "Sprememba deleža samocene 'dobro'") +
  theme(plot.title.position = "plot",axis.text = element_blank())


#2. zemljevid: narast pričakovanih let življenja
df2 <- tibble(country = drzave, some_value = skupaj$PRIČAKOVANA_LETA)

map2 <- european_union_map_cropped %>% 
  left_join(df, by = c("name" = "country"))

zemljevid2 <- ggplot(data = map2) +
  geom_sf(mapping = aes(fill = some_value)) +
  scale_fill_gradient2(name = "Sprememba", na.value = "grey50") +
  labs(title = "Sprememba v pričakovanih letih") +
  theme(plot.title.position = "plot",axis.text = element_blank())


# 1. graf: sprememba v fizični aktivnosti po državah (kolesarjenje)

graf1 <- ggplot(aktivnost[,c(1,2)], aes(Država, Razlika_kolo, fill = Država)) + geom_bar(stat = "identity", show.legend = FALSE) +  labs(title = "Sprememba aktivnosti", subtitle = "Kolesarjenje") + theme(axis.text = element_text(angle = 90)) + scale_y_continuous(limits = c(-10,10)) 

#2. graf: sprememba v fizični aktivnosti po državah (aerobični športi)

graf2 <- ggplot(aktivnost[,c(1,3)], aes(Država, Razlika_aero, fill = Država)) + geom_bar(stat = "identity", show.legend = FALSE) +  labs(title = "Sprememba aktivnosti", subtitle = "Aerobični športi") + theme(axis.text = element_text(angle = 90)) + scale_y_continuous(limits = c(-27,20))


# 3. graf: sprememba v deležu bolniške po državah
df3 <- skupaj[,c("Država","BOLNISKA")]
graf3 <- ggplot(df3, aes(Država,BOLNISKA)) + geom_bar(stat = "identity") + ggtitle("Sprememba bolniške") + theme(axis.text = element_text(angle = 90))


# 4. graf: sprememba v deležu ocene zdravja "slabo" po državah

graf4 <- ggplot(ocena.slabo,aes(x=Leto,y=Delež,group=Država, color = Država)) + geom_line() + scale_y_continuous(limits = c(3,20)) + labs(title = "Ocena zdravja", subtitle = "slabo")


# 5. graf: sprememba v deležu ocene zdravja "normalno" po državah
graf5 <- ggplot(ocena.normalno,aes(x=Leto,y=Delež,group=Država, color = Država)) + geom_line() + scale_y_continuous(limits = c(14,43)) + labs(title = "Ocena zdravja", subtitle = "normalno")


# 6. graf: sprememba v deležu ocene zdravja "dobro" po državah

graf6 <- ggplot(ocena.dobro,aes(x=Leto,y=Delež,group=Država, color = Država)) + geom_line() + scale_y_continuous(limits = c(40,82)) + labs(title = "Ocena zdravja", subtitle = "dobro")


# 7. graf: sprememba v pričakovanih letih življenja po državah
df7 <- skupaj[,c("Država","PRIČAKOVANA_LETA")]
graf7 <- ggplot(df7, aes(Država,PRIČAKOVANA_LETA)) + geom_bar(stat = "identity") + ggtitle("Sprememba pričakovanih let") + theme(axis.text = element_text(angle = 90))


# 8. graf: odstopanja od povprečij pri pričakovanih letih, kolesarjenju in aerobični vadbi
odstopanja <- data.frame(Odstop.pric.leta = round(skupaj$PRIČAKOVANA_LETA - as.numeric(mean(skupaj$PRIČAKOVANA_LETA)),2),
                         Odstop.kolo = round(skupaj$KOLO - as.numeric(mean(skupaj$KOLO)),2),
                         Odstop.aero.sporti = round(skupaj$AEROBIČNI_ŠPORTI - as.numeric(mean(skupaj$AEROBIČNI_ŠPORTI)),2),
                         Država = skupaj$Država)

mdfr <- melt(odstopanja, id.vars = "Država") %>% filter(Država != "Finland")

graf8 <- ggplot(mdfr, aes(Država, value, fill = variable)) + geom_bar(position = "dodge", stat = "identity") + labs(title = "Odstopanja od povprečja razlik v pričakovanih letih, deležu kolesarjev in prebivalstva v aerobičnih športih") + ylab("Odstop od povprečja") + scale_fill_discrete(labels=c("Odstop pričakovanih let", "Odstop kolesarjenja", "Odstop pri aerobičnih športih")) + theme(legend.title = element_blank(),axis.text = element_text(angle = 90))

