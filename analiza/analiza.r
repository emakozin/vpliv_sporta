library(scales)
library(reshape2)

# 4. faza: Napredna analiza podatkov

  
odstopanja <- data.frame(Odstop.pric.leta =  round(skupaj$PRIČAKOVANA_LETA - as.numeric(mean(skupaj$PRIČAKOVANA_LETA)),2), 
                         Odstop.kolo = round(skupaj$KOLO - as.numeric(mean(skupaj$KOLO)),2), 
                         Odstop.aero.sporti = round(skupaj$AEROBIČNI_ŠPORTI - as.numeric(mean(skupaj$AEROBIČNI_ŠPORTI)),2), 
                         Odstop.dobro = round(skupaj$OCENA_DOBRO- as.numeric(mean(skupaj$OCENA_DOBRO)),2), 
                         Odstop.slabo = round(skupaj$OCENA_SLABO - as.numeric(mean(skupaj$OCENA_SLABO)),2), 
                         Država = skupaj$Država)

mdfr <- melt(odstopanja, id.vars = "Država") %>% filter(!Država %in% c("Finland", "Norway"))


graf9 <- ggplot(mdfr, aes(Država, value, fill = variable)) +  
  geom_bar(position = "dodge", stat = "identity") + 
  labs(title = "Odstopanja od povprečja razlik v pričakovanih letih, deležu kolesarjev in prebivalstva v aerobičnih športih") + 
  ylab("Odstop od povprečja") + 
  scale_fill_discrete(labels=c("Odstop pričakovanih let", "Odstop kolesarjenja", "Odstop pri aerobičnih športih", "Odstop ocene dobro", "Odstop ocene slabo")) + 
  theme(legend.title = element_blank(),axis.text = element_text(angle = 90))
  