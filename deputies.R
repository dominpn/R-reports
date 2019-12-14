library(rvest)
library(archivist)
library(lubridate)
library(eeptools)
library(stringi)
library(tidyr)

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
}

pola <- c("Liczba głosów:", "Klub/koło:", "Data i miejsce urodzenia:", "Wykształcenie:", "Zawód:", "Nazwisko:")

dane <- list()
for(pole in pola) {
  dane[[pole]] <- sapply(poslowie, function(posel) posel[pole])
}

df <- as.data.frame(dane)
colnames(df) <- c("Glosow", "Klub", "Data", "Wyksztalcenie", "Zawod", "ImieNazwisko")
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
poslowie$Glosow <- as.numeric(as.character(poslowie$Glosow))
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
