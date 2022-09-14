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


podatki.model <- skupaj[,c(1,2,3,6,7)]
colnames(podatki.model) <- c("drzava","kolo","aero","pric.leta","ocena.dobro")

model <- lm(pric.leta ~ kolo+aero+ocena.dobro,data = podatki.model)
print(model)


podatki.model %>% ggplot(mapping = aes(x = drzava, y = pric.leta)) +
  geom_point() +
  theme(axis.text = element_text(angle = 90))

novi.podatki <- data.frame(kolo=-0.030112, aero = -0.003102, ocena.dobro = 0.006593)
napovedi = predict(model,newdata = novi.podatki)


diagram_podatki_napovedi = function(podatki, model) {
  podatki %>%
    bind_cols(napovedana.leta = predict(model, podatki)) %>%
    ggplot() +
    geom_point(mapping = aes(x = drzava, y = napovedana.leta), color = "red") +
    geom_point(mapping = aes(x = drzava, y = pric.leta)) +
    theme(axis.text.x = element_text(angle = 90), axis.title.y = element_text(colour = "red")) + labs(title = "Napovedne in dejanske vrednosti razlike v pričakovanih letih", x = "Drzava", y= "Napovedi") } 
graf.napovedi <- diagram_podatki_napovedi(podatki.model,model)
