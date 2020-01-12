library(rvest)
library(archivist)
library(lubridate)
library(eeptools)
library(stringi)
library(tidyr)
library(xml2)
library(plyr)
library(ggplot2)
library(dplyr)
# SCRAPING
poslowie <- list()
for (posel in 1:460) {
  strona <- read_html(paste0("http://sejm.gov.pl/sejm8.nsf/posel.xsp?id=",posel%/%100, (posel%/%10)%%10, posel %% 10))
  nazwisko <- html_text(html_nodes(strona, "h1"))
  
  pola <- html_text(html_nodes(strona, ".data p"))
  if (any(pola == "Wygaśnięcie mandatu:")) {
    ind <- which(pola == "Wygaśnięcie mandatu:")
    pola <- pola[-ind+c(0,-1)]
    if (!grepl(pola[ind], pattern=":$"))
      pola <- pola[-ind]
  }
  wartosci <- pola[seq(2, length(pola), 2)]
  names(wartosci) <- pola[seq(1, length(pola), 2)]
  wartosci[["Nazwisko:"]] <- nazwisko
  id <- as.character(posel)
  poslowie[[id]] <- wartosci
  print(poslowie[[id]])
}

pola <- c("Liczba głosów:", "Klub/koło:", "Data i miejsce urodzenia:", "Wykształcenie:", "Zawód:", "Nazwisko:", "Okręg wyborczy:")

dane <- list()
for(pole in pola) {
  dane[[pole]] <- sapply(poslowie, function(posel) posel[pole])
}

df <- as.data.frame(dane)
colnames(df) <- c("Liczba.Glosow", "Klub", "Data", "Wyksztalcenie", "Zawod", "ImieNazwisko", "OkregWyborczy")
rownames(df) <- names(poslowie)
df$Klub <- gsub(df$Klub, pattern = "Klub Parlamentarny |Klub Poselski ", replacement="")
df$DataUrodzenia <- gsub(df$Data, pattern = ",.*$", replacement = "")
df$MiejsceUrodzenia <- gsub(df$Data, pattern = "^.*,.", replacement = "")

get_gender <- function(name) {
  if (all(stri_sub(unlist(strsplit(toString(name), " "))[1], -1, -1) == "a")) {
    return("K")
  } else {
    return("M")
  }
}

poslowie <- df[,-3]
poslowie$Liczba.Glosow <- as.numeric(as.character(poslowie$Liczba.Glosow))
poslowie$Klub <- factor(poslowie$Klub)
poslowie$DataUrodzenia <- dmy(poslowie$DataUrodzenia)
poslowie$MiejsceUrodzenia <- factor(poslowie$MiejsceUrodzenia)
poslowie$Wiek <- floor(age_calc(poslowie$DataUrodzenia, units = "years"))
poslowie$Plec <- sapply(poslowie$ImieNazwisko,get_gender)
write.csv(poslowie,'deputies.csv', row.names = TRUE)

# ANALYSIS
poslowie_data <- read.table(file = "deputies.csv", sep=",", header=TRUE)
head(poslowie_data)
summary(poslowie_data)

mean(as.numeric(poslowie_data$Liczba.głosów))
table(poslowie_data$Wyksztalcenie)
factor(poslowie_data$MiejsceUrodzenia)
poslowie_data$Klub

#CHARTS

#klub poselski - liczba członków -> kolumnowy
table_1 = table(poslowie_data$Klub)
table_1
df_1 = as.data.frame(table_1)
df_1
names(df_1)[1] = 'klub'
names(df_1)[2] = 'liczba_poslow'
df_1 <-df_1[order(-df_1$liczba_poslow),]
df_1
par(mar=c(6,4,4,4))
barplot(df_1$liczba_poslow,names.arg=c("PiS","PO-KO","PSL", "Kukiz'15", "PN", "Konfederacja", "UPR", "PO", "KPPP", "Teraz!","","WiS","PSL-UED"),ylab="liczba posłów",col=rainbow(length(df$klub)),
        main="Wielkość klubów poselskich",border="black",las=2)

#klub poselski - plec
df_5 <- data.frame(
  plec=poslowie_data$Plec ,  
  klub=poslowie_data$Klub
)
df_5 <- ddply(df_5, .(df_5$plec, df_5$klub), nrow)
names(df_5) <- c("plec", "klub", "liczba_poslow")

df_5 <-df_5[order(-df_5$plec),]
df_5 <-df_5[order(-df_5$klub),]
head(df_5)

par(mar=c(13,4,4,4))
ggplot(data=df_5, aes(x=klub, y=liczba_poslow, fill=plec)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#wyksztalcenie
table_2 = table(poslowie_data$Wyksztalcenie)
table_2
df_2 = as.data.frame(table_2)
df_2
names(df_2)[1] = 'wyksztlcenie'
names(df_2)[2] = 'liczba_poslow'
df_2 <-df_2[order(-df_2$liczba_poslow),]
df_2
par(mar=c(13,4,4,4))
barplot(df_2$liczba_poslow,names.arg=df_2$wyksztlcenie,ylab="liczba posłów",col=rainbow(length(df$klub)),
        main="Wykształcenie posłów",border="black",las=2)

#histogram wiek 
df_3 = as.data.frame(poslowie_data$Wiek)
head(df_3)
names(df_3)[1] = 'wiek'
df_3

ggplot(df_3, aes(x=wiek)) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(wiek)),
             color="blue", linetype="dashed", size=1)

#histogram wiek płeć
df_4 <- data.frame(
  plec=poslowie_data$Plec ,  
  wiek=poslowie_data$Wiek
)
head(df_4)

summary(df_4)

mu <- ddply(df_4, "plec", summarise, grp.mean=mean(plec))
print(mu)

ggplot(df_4, aes(x=wiek, fill=plec, color=plec)) +
  geom_histogram( alpha=0.2, position="identity", binwidth=2) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=plec),
              linetype="dashed")

#poslowie 9
poslowie9 <- list()
for (posel in 371:460) {
  strona <- read_html(paste0("http://sejm.gov.pl/sejm9.nsf/posel.xsp?id=",posel%/%100, (posel%/%10)%%10, posel %% 10))
  nazwisko <- html_text(html_nodes(strona, "h1"))
  print(nazwisko)
  print(id)
  
  pola <- html_text(html_nodes(strona, ".data p"))
  if (any(pola == "Wygaśnięcie mandatu:")) {
    ind <- which(pola == "Wygaśnięcie mandatu:")
    pola <- pola[-ind+c(0,-1)]
    if (!grepl(pola[ind], pattern=":$"))
      pola <- pola[-ind]
  }
  wartosci <- pola[seq(2, length(pola), 2)]
  names(wartosci) <- pola[seq(1, length(pola), 2)]
  wartosci[["Nazwisko:"]] <- nazwisko
  id <- as.character(posel)
  poslowie9[[id]] <- wartosci
}

pola <- c("Wybrany dnia", "Lista", "Okręg wyborczy", "Liczba głosów:", "Staż parlamentarny", "Klub/koło:", "Data i miejsce urodzenia:", "Wykształcenie:", "Ukończona szkoła:",  "Zawód:", "Email:", "Nazwisko:")

dane <- list()
for(pole in pola) {
  dane[[pole]] <- sapply(poslowie9, function(posel) posel[pole])
}

head(poslowie9)

df <- as.data.frame(dane)
colnames(df) <- c("Liczba.Glosow", "Klub", "Data", "Wyksztalcenie", "Zawod", "ImieNazwisko", "OkregWyborczy")
rownames(df) <- names(poslowie9)
df$Klub <- gsub(df$Klub, pattern = "Klub Parlamentarny |Klub Poselski ", replacement="")
df$DataUrodzenia <- gsub(df$Data, pattern = ",.*$", replacement = "")
df$MiejsceUrodzenia <- gsub(df$Data, pattern = "^.*,.", replacement = "")


poslowie9 <- df[,-3]
poslowie9$Liczba.Glosow <- as.numeric(as.character(poslowie9$Liczba.Glosow))
poslowie9$Klub <- factor(poslowie9$Klub)
poslowie9$DataUrodzenia <- dmy(poslowie9$DataUrodzenia)
poslowie9$MiejsceUrodzenia <- factor(poslowie9$MiejsceUrodzenia)
poslowie9$Wiek <- floor(age_calc(poslowie9$DataUrodzenia, units = "years"))
poslowie9$Plec <- sapply(poslowie9$ImieNazwisko,get_gender)
write.csv(poslowie9,'deputies2.csv', row.names = TRUE)