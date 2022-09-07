
install.packages('readxl')
library(readxl)
library(dplyr)










# indeks telesne mase (bmi) v letih 2014 in 2019


bmi.2014 <- data.frame(read_excel('podatki/bmi.xlsx', sheet = 'Sheet 1', skip = 11)) %>% 
  rename( Normalen_2014 = "Normal", Država = "BMI..Labels.") %>%  
  select(-2,-3,-5,-6,-7) %>% 
  filter(!row_number() %in% c(1,28))

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
  rename(Država = "ISCED11..Labels.", Delež_2014 = "All.ISCED.2011.levels")

bolniska.2019 <- data.frame(read_excel('podatki/bolniska.xlsx', sheet = 'Sheet 2', skip = 6)) %>% 
  select(-3) %>% 
  filter(!row_number() %in% c(1)) %>% 
  rename(Država = "ISCED11..Labels.", Delež_2019 = "All.ISCED.2011.levels")

bolniska <- bolniska.2014 %>% inner_join(bolniska.2019, by = "Država")
bolniska$Razlika_bolniska <- bolniska$Delež_2019-bolniska$Delež_2014
razlika.bolniska <- bolniska %>% select(-2,-3)


# pričakovana leta življenja

pricakovana_leta <- data.frame(read_excel('podatki/pricakovana_leta.xlsx', sheet = 'Sheet 3', skip = 5)) %>% 
  rename(Država = "TIME", leto_2014 = "X2014", leto_2019 = "X2019") %>% 
  filter(!row_number() %in% c(1,29))
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


ocenazdravjaEU.2014 <- data.frame(read_excel('podatki/ocena_zdravja.xlsx', sheet = 'Sheet 1', skip = 7)) %>% 
  rename(Država = "LEVELS..Labels.", Dobro_14 = "Very.good.or.good", Normalno_14 = "Fair", Slabo_14 = "Bad.or.very.bad") %>%
  filter(!row_number() %in% c(1))


ocenazdravjaEU.2019 <- data.frame(read_excel('podatki/ocena_zdravja.xlsx', sheet = 'Sheet 2', skip = 7)) %>% 
  rename(Država = "LEVELS..Labels.", Dobro_19 = "Very.good.or.good", Normalno_19 = "Fair", Slabo_19 = "Bad.or.very.bad") %>% 
  filter(!row_number() %in% c(1))

ocenazdravjaEU.2019$Dobro_19 <- as.numeric(ocenazdravjaEU.2019$Dobro_19)
ocenazdravjaEU.2019$Normalno_19 <- as.numeric(ocenazdravjaEU.2019$Normalno_19)
ocenazdravjaEU.2019$Slabo_19 <- as.numeric(ocenazdravjaEU.2019$Slabo_19)


ocenazdravjaEU <- ocenazdravjaEU.2014 %>% inner_join(ocenazdravjaEU.2019, by = "Država")
ocenazdravjaEU$Razlika_dobro <- ocenazdravjaEU$Dobro_19 - ocenazdravjaEU$Dobro_14
ocenazdravjaEU$Razlika_normalno <- ocenazdravjaEU$Normalno_19 - ocenazdravjaEU$Normalno_14
ocenazdravjaEU$Razlika_slabo <- ocenazdravjaEU$Slabo_19 - ocenazdravjaEU$Slabo_14

razlika.ocenaEU <- ocenazdravjaEU %>% select(-2,-3,-4,-5,-6,-7)
 
# delež prebivalstva, ki se ukvarja s fizičnimi aktvinosti (hoja, kolesarstvo, aerobični športi in trening za moč)

aktivnost.2014<-data.frame(read_excel('podatki/aktivnost.xlsx', sheet = 'Sheet 1', skip = 6)) %>%
  select(-3,-5,-7,-9) %>%
  filter(!row_number() %in% c(1,28)) %>% 
  rename(Država = "PHYSACT..Labels." )
aktivnost.2014$povp14 <- rowMeans(aktivnost.2014[-1], na.rm = TRUE) 



aktivnost.2019 <-data.frame(read_excel('podatki/aktivnost.xlsx', sheet = 'Sheet 2', skip = 6)) %>%
  select(-3,-5,-7,-9) %>%
  filter(!row_number() %in% c(1,28)) %>% 
  rename(Država = "PHYSACT..Labels.", Hoja_2019 = "Walking.to.get.to.and.from.place", 
    Kolesarjenje_2019 = "Cycling.to.get.to.and.from.place", Aerobicnisporti_2019 = "Aerobic.sports",     
    Trening_za_moč_2019 = "Muscle.strengthening")

aktivnost.2019$Hoja_2019<- as.numeric(aktivnost.2019$Hoja_2019)
aktivnost.2019$Kolesarjenje_2019<- as.numeric(aktivnost.2019$Kolesarjenje_2019)
aktivnost.2019$Aerobicnisporti_2019<- as.numeric(aktivnost.2019$Aerobicnisporti_2019)
aktivnost.2019$Trening_za_moč_2019<- as.numeric(aktivnost.2019$Trening_za_moč_2019)
aktivnost.2019$povp19 <- as.numeric(rowMeans(aktivnost.2019[-1], na.rm = TRUE))


aktivnost.2019 <- aktivnost.2019 %>% select(-2,-3,-4,-5)
aktivnost.2014 <- aktivnost.2014 %>% select(-2,-3,-4,-5)

aktivnost <- aktivnost.2014 %>% inner_join(aktivnost.2019, by = "Država")
aktivnost$Razlika_aktivnost <- aktivnost$povp19 - aktivnost$povp14

razlika.aktivnost <- aktivnost %>% select(-2,-3)

skupaj <- razlika.bmi %>% inner_join(razlika.aktivnost, by = "Država") %>% 
  inner_join(razlika.bolniska, by = "Država") %>% 
  inner_join(razlika.ocenaEU, by = "Država") %>% 
  inner_join(razlika.pricakovana_leta, by = "Država")
 
pokazatelji <- list(bmi,bolniska,ocenazdravjaEU,aktivnost,pricakovana_leta)

skupaj <- as.data.frame(skupaj %>% rename(BMI = "Razlika_bmi",AKTIVNOST = "Razlika_aktivnost",
                            BOLNISKA = "Razlika_bolniska", OCENA.SLABO = "Razlika_slabo", 
                            OCENA.DOBRO = "Razlika_dobro", OCENA.NORMALNO = "Razlika_normalno", 
                            PRIČAKOVANA.LETA = "Razlika_pricakovana_leta"))
