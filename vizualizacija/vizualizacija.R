# 3. faza: Vizualizacija podatkov

library(tmap)
library(digest)

# 1. graf: sprememba v fizični aktivnosti po državah
df1 <- skupaj[,c("Država","AKTIVNOST")]
graf1 <- ggplot(df1, aes(Država,AKTIVNOST)) + geom_bar(stat = "identity") ggtitle("Sprememba aktivnosti")



# 2. graf: sprememba v deležu normalnega indeksa telesne mase po državah
df2 <- skupaj[,c("Država","BMI")]
graf2 <- ggplot(df2, aes(Država,BMI)) + geom_bar(stat = "identity") + ggtitle("Sprememba BMI")


# 3. graf: sprememba v deležu bolniške po državah
df3 <- skupaj[,c("Država","BOLNISKA")]
graf3 <- ggplot(df3, aes(Država,BOLNISKA)) + geom_bar(stat = "identity") + ggtitle("Sprememba bolniške")



# 4. graf: sprememba v deležu ocene zdravja "slabo" po državah
df4 <- skupaj[,c("Država","OCENA.SLABO")]
graf4 <- ggplot(df4, aes(Država,OCENA.SLABO)) + geom_bar(stat = "identity") + ggtitle("Sprememba ocene slabo")


# 5. graf: sprememba v deležu ocene zdravja "normalno" po državah
df5 <- skupaj[,c("Država","OCENA.NORMALNO")]
graf5 <- ggplot(df5, aes(Država,OCENA.NORMALNO)) + geom_bar(stat = "identity") + ggtitle("Sprememba ocene normalno")


# 6. graf: sprememba v deležu ocene zdravja "dobro" po državah
df6 <- skupaj[,c("Država","OCENA.DOBRO")]
graf6 <- ggplot(df6, aes(Država,OCENA.DOBRO)) + geom_bar(stat = "identity") + ggtitle("Sprememba ocene dobro")


# 7. graf: sprememba v pričakovanih letih življenja po državah
df7 <- skupaj[,c("Država","PRIČAKOVANA.LETA")]
graf7 <- ggplot(df7, aes(Država,PRIČAKOVANA.LETA)) + geom_bar(stat = "identity") + ggtitle("Sprememba pričakovana leta")



