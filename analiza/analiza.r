library(scales)
library(reshape2)

# 4. faza: Napredna analiza podatkov



test <- tibble(ocena.dobro %>% group_by(Država) %>% summarise(skupaj = sum(Delež),povprečje = mean(Delež))) %>% inner_join(ocena.dobro, by = "Država")
names(test) <- c("Država", "skupaj","povprečje", "Leto","Delež")

graf9 <- ggplot(test, aes(Leto, povprečje - Delež)) + 
  geom_boxplot(fill = "lightyellow") + 
  geom_point() + 
  geom_label(aes(label = Država, fill = Država),show.legend = FALSE, size = 3) +
  labs(title = "Odstopanja od povprečnih deležev pri oceni dobro") + ylab("Odstopanje")

