
install.packages('readxl')
library(readxl)
library(dplyr)
library(tidyr)





# indeks telesne mase (bmi) v letih 2014 in 2019


bmi.2014 <- data.frame(read_excel('podatki/bmi.xlsx', sheet = 'Sheet 1', skip = 11)) %>% 
  rename( Normalen_2014 = "Normal", Država = "BMI..Labels.") %>%  
  select(-2,-3,-5,-6,-7) %>% 
  filter(!row_number() %in% c(1,28)) %>% 
  na.omit() 

bmi.2019 <- data.frame(read_excel('podatki/bmi.xlsx', sheet = 'Sheet 2', skip = 11)) %>% 
  rename( Normalen_2019 = "Normal", Država = "BMI..Labels.") %>%  
  select(-2,-3,-5,-6,-7) %>% 
  filter(!row_number() %in% c(1,28)) %>% 
  na.omit()


bmi <- bmi.2014 %>% inner_join(bmi.2019, by = "Država")
bmi$Razlika_bmi <- bmi$Normalen_2019 - bmi$Normalen_2014
razlika.bmi <- bmi %>% select(-2,-3)


# delež prebivalstva, ki je bil na 'bolniški'

bolniska.2014 <- data.frame(read_excel('podatki/bolniska.xlsx', sheet = 'Sheet 1', skip = 6)) %>% 
  select(-3) %>% 
  filter(!row_number() %in% c(1)) %>% 
  rename(Država = "ISCED11..Labels.", Delež_2014 = "All.ISCED.2011.levels") %>% 
  na.omit()

bolniska.2019 <- data.frame(read_excel('podatki/bolniska.xlsx', sheet = 'Sheet 2', skip = 6)) %>% 
  select(-3) %>% 
  filter(!row_number() %in% c(1)) %>% 
  rename(Država = "ISCED11..Labels.", Delež_2019 = "All.ISCED.2011.levels") %>% 
  na.omit()

bolniska <- bolniska.2014 %>% inner_join(bolniska.2019, by = "Država")
bolniska$Razlika_bolniska <- bolniska$Delež_2019-bolniska$Delež_2014
razlika.bolniska <- bolniska %>% select(-2,-3)


# pričakovana leta življenja

pricakovana_leta <- data.frame(read_excel('podatki/pricakovana_leta.xlsx', sheet = 'Sheet 3', skip = 5)) %>% 
  rename(Država = "TIME", leto_2014 = "X2014", leto_2019 = "X2019") %>% 
  filter(!row_number() %in% c(1,29)) %>% 
  na.omit()
pricakovana_leta$leto_2014 <- as.numeric(pricakovana_leta$leto_2014)
pricakovana_leta$leto_2019 <- as.numeric(pricakovana_leta$leto_2019)

pricakovana_leta$Razlika_pricakovana_leta <- pricakovana_leta$leto_2019 - pricakovana_leta$leto_2014

razlika.pricakovana_leta <- pricakovana_leta %>% select(-2,-3)


# ocena zdravja za Slovenijo (samoocenjevanje)


ocenazdravja.2019 <- data.frame(read.csv('podatki/sursocena2019.csv',sep = ';',header = TRUE, skip = 2)) %>% 
  rename(Stanje = "ZDRAVSTVENO.STANJE", leto_2019 = "X2019")

ocenazdravja.2014 <- data.frame(read.csv('podatki/sursocena2014.csv',sep = ';',header = TRUE, skip = 2)) %>% 
  rename(Stanje = "ZDRAVSTVENO.STANJE", leto_2014 = "X2014")

ocenazdravja <- ocenazdravja.2014 %>% inner_join(ocenazdravja.2019, by = "Stanje")
ocenazdravja$Razlika_ocena_slo <- ocenazdravja$leto_2019 -ocenazdravja$leto_2014


# ocena zdravja za Evropo

ocenazdravjaEU.2014 <- data.frame(read_excel('podatki/ocenezdravja.xlsx', sheet = 'Sheet 1', skip = 6)) %>% 
  rename(Država = "LEVELS..Labels.", Dobro = "Very.good.or.good", Normalno = "Fair", Slabo = "Bad.or.very.bad") %>% 
  na.omit()
  

ocenazdravjaEU.2015 <- data.frame(read_excel('podatki/ocenezdravja.xlsx', sheet = 'Sheet 2', skip = 6)) %>% 
  rename(Država = "LEVELS..Labels.", Dobro = "Very.good.or.good", Normalno = "Fair", Slabo = "Bad.or.very.bad") %>% 
  na.omit()

ocenazdravjaEU.2016 <- data.frame(read_excel('podatki/ocenezdravja.xlsx', sheet = 'Sheet 3', skip = 6)) %>% 
  rename(Država = "LEVELS..Labels.", Dobro = "Very.good.or.good", Normalno = "Fair", Slabo = "Bad.or.very.bad") %>% 
  na.omit()
  
ocenazdravjaEU.2017 <- data.frame(read_excel('podatki/ocenezdravja.xlsx', sheet = 'Sheet 4', skip = 6)) %>% 
  rename(Država = "LEVELS..Labels.", Dobro = "Very.good.or.good", Normalno = "Fair", Slabo = "Bad.or.very.bad")  %>% 
  na.omit()
  
ocenazdravjaEU.2018 <- data.frame(read_excel('podatki/ocenezdravja.xlsx', sheet = 'Sheet 5', skip = 6)) %>% 
  rename(Država = "LEVELS..Labels.", Dobro = "Very.good.or.good", Normalno = "Fair", Slabo = "Bad.or.very.bad")  %>% 
  na.omit()
  
ocenazdravjaEU.2019 <- data.frame(read_excel('podatki/ocenezdravja.xlsx', sheet = 'Sheet 6', skip = 6, na = c(":"))) %>% 
  rename(Država = "LEVELS..Labels.", Dobro = "Very.good.or.good", Normalno = "Fair", Slabo = "Bad.or.very.bad") %>% 
  na.omit()
ocenazdravjaEU.2019[,2] <- as.numeric(ocenazdravjaEU.2019[,2])

ocena.dobro <- ocenazdravjaEU.2014[,c(1,2)] %>% 
  inner_join(ocenazdravjaEU.2015[,c(1,2)], by = "Država") %>% 
  inner_join(ocenazdravjaEU.2016[,c(1,2)], by = "Država") %>% 
  inner_join(ocenazdravjaEU.2017[,c(1,2)], by = "Država") %>% 
  inner_join(ocenazdravjaEU.2018[,c(1,2)], by = "Država") %>%
  inner_join(ocenazdravjaEU.2019[,c(1,2)], by = "Država")
names(ocena.dobro) <- c("Država", "2014","2015","2016","2017","2018","2019")

razlika.ocena.dobro <- cbind.data.frame(ocena.dobro$Država, ocena.dobro$`2019`- ocena.dobro$`2014`)
names(razlika.ocena.dobro) <- c("Država", "Razlika_dobro")

ocena.dobro <- ocena.dobro %>% pivot_longer(-Država, names_to = "Leto", values_to = "Delež")


ocena.normalno <- ocenazdravjaEU.2014[,c(1,3)] %>% 
  inner_join(ocenazdravjaEU.2015[,c(1,3)], by = "Država") %>% 
  inner_join(ocenazdravjaEU.2016[,c(1,3)], by = "Država") %>% 
  inner_join(ocenazdravjaEU.2017[,c(1,3)], by = "Država") %>% 
  inner_join(ocenazdravjaEU.2018[,c(1,3)], by = "Država") %>%
  inner_join(ocenazdravjaEU.2019[,c(1,3)], by = "Država") 
names(ocena.normalno) <- c("Država", "2014","2015","2016","2017","2018","2019")
ocena.normalno$`2019`<- as.numeric(ocena.normalno$`2019`)

razlika.ocena.normalno <- cbind.data.frame(ocena.normalno$Država, ocena.normalno$`2019`- ocena.normalno$`2014`)
names(razlika.ocena.normalno) <- c("Država", "Razlika_normalno")

ocena.normalno <- ocena.normalno %>% pivot_longer(-Država, names_to = "Leto", values_to = "Delež")

ocena.slabo <- ocenazdravjaEU.2014[,c(1,4)] %>% 
  inner_join(ocenazdravjaEU.2015[,c(1,4)], by = "Država") %>%
  inner_join(ocenazdravjaEU.2016[,c(1,4)], by = "Država") %>% 
  inner_join(ocenazdravjaEU.2017[,c(1,4)], by = "Država") %>% 
  inner_join(ocenazdravjaEU.2018[,c(1,4)], by = "Država") %>%
  inner_join(ocenazdravjaEU.2019[,c(1,4)], by = "Država") 
names(ocena.slabo) <- c("Država", "2014","2015","2016","2017","2018","2019")
ocena.slabo$`2019`<- as.numeric(ocena.slabo$`2019`)

razlika.ocena.slabo <- cbind.data.frame(ocena.slabo$Država, ocena.slabo$`2019`-ocena.slabo$`2014`)
names(razlika.ocena.slabo) <- c("Država", "Razlika_slabo")

ocena.slabo <- ocena.slabo %>% pivot_longer(-Država, names_to = "Leto", values_to = "Delež")


 
# delež prebivalstva, ki se ukvarja s fizičnimi aktvinosti (hoja, kolesarstvo, aerobični športi in trening za moč)

aktivnost.2014<-data.frame(read_excel('podatki/aktivnost.xlsx', sheet = 'Sheet 1', skip = 6)) %>%
  select(-2,-3,-5,-7,-8,-9) %>%
  filter(!row_number() %in% c(1,23)) %>% 
  rename(Država = "PHYSACT..Labels.", Kolesarjenje = "Cycling.to.get.to.and.from.place", Aerobicnisporti = "Aerobic.sports")
aktivnost.2014$Kolesarjenje <- as.numeric(aktivnost.2014$Kolesarjenje)
aktivnost.2014$Aerobicnisporti <- as.numeric(aktivnost.2014$Aerobicnisporti)


aktivnost.2019 <-data.frame(read_excel('podatki/aktivnost.xlsx', sheet = 'Sheet 2', skip = 6, na = c(":"))) %>%
  filter(!row_number() %in% c(1)) %>% 
  select(-2,-3,-5,-7,-8,-9) %>% 
  rename(Država = "PHYSACT..Labels.", Kolesarjenje = "Cycling.to.get.to.and.from.place", Aerobicnisporti = "Aerobic.sports") %>%
  na.omit()

aktivnost.2019$Kolesarjenje<- as.numeric(aktivnost.2019$Kolesarjenje)
aktivnost.2019$Aerobicnisporti<- as.numeric(aktivnost.2019$Aerobicnisporti)

aktivnost <- as.data.frame(aktivnost.2014$Država) %>% rename(Država = "aktivnost.2014$Država")
aktivnost$Razlika_kolo <- aktivnost.2019$Kolesarjenje - aktivnost.2014$Kolesarjenje
aktivnost$Razlika_aero <- aktivnost.2019$Aerobicnisporti - aktivnost.2014$Aerobicnisporti
aktivnost <- aktivnost %>% na.omit()





skupaj <- aktivnost %>% inner_join(razlika.bmi, by = "Država") %>% 
       inner_join(razlika.bolniska, by = "Država") %>% 
      inner_join(razlika.pricakovana_leta, by = "Država")  %>% 
       inner_join(razlika.ocena.dobro, by = "Država") %>%
       inner_join(razlika.ocena.normalno, by = "Država") %>%
       inner_join(razlika.ocena.slabo, by = "Država")
    

skupaj <- as.data.frame(skupaj %>% 
                        rename(BMI = "Razlika_bmi",KOLO = "Razlika_kolo", AEROBIČNI_ŠPORTI = "Razlika_aero",BOLNISKA="Razlika_bolniska",
                               OCENA_DOBRO = "Razlika_dobro", OCENA_NORMALNO = "Razlika_normalno",
                               OCENA_SLABO = "Razlika_slabo", PRIČAKOVANA_LETA = "Razlika_pricakovana_leta"))

pokazatelji <- list(colnames(skupaj)[colnames(skupaj) != "Država"])

