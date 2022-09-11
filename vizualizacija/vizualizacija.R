# 3. faza: Vizualizacija podatkov

library(tmap)
library(digest)
library(ggplot2)

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


# 8. graf: sprememba v deležu normalnega indeksa telesne mase po državah
df8 <- skupaj[,c("Država","BMI")]
graf8 <- ggplot(df8, aes(Država,BMI)) + geom_bar(stat = "identity") + ggtitle("Sprememba BMI") + theme(axis.text = element_text(angle = 90))


