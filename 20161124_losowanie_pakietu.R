rm(list=ls())
library(rvest)
library(data.table)
library(sqldf)

#zaladowanie srodowiska, w przypadku braki dodanie go
if (file.exists("srodowisko_biblioteki.RData")==F){
  wylosowane_biblioteki<-c()
  save.image("srodowisko_biblioteki.RData")
} else {
  load("srodowisko_biblioteki.RData")
}


#scrapowanie listy bibliotek i ich krotkiego opisu
web<-"https://cran.r-project.org/web/packages/available_packages_by_name.html"
web<-read_html(web)

web %>% 
  html_nodes("td a")%>%
  html_text() -> biblioteka

web %>% 
  html_nodes("td+ td")%>%
  html_text() -> opisy
rm(web)

biblioteki<-data.frame(biblioteka,opisy)
rm(opisy,biblioteka)


# Pobranie danych na temat pobran -----------------------------------------

#ustawienie dni za ktorych maja byc pobrane dane
ldni<-2 #chcemy tylko za 2 ostatnie dni (duze pliki)
wczoraj <- as.Date(Sys.Date()-0)
start <- as.Date(wczoraj-ldni)
sekw <- seq(start, wczoraj, by = 'day')
rok <- as.POSIXlt(sekw)$year + 1900
url <- paste0('http://cran-logs.rstudio.com/', rok, '/', sekw, '.csv.gz')


dir.create("CRANlogs")
do.call(file.remove, list(list.files("CRANlogs", full.names = TRUE)))
braki<-c()
#pobranie logow pobrania bibliotek
for (i in 1:length(sekw)) {
  tryCatch({download.file(url[i], paste0('CRANlogs/', sekw[i], '.csv.gz'))},
              error=function(cond){
                warning(paste("Nie ma danych dla:", sekw[i]))
                braki[length(braki)+1]<<-i
              })
}


file_list <- list.files("CRANlogs", full.names=TRUE)
#usuniecie plikow dla ktorych nie bylo danych
if (length(braki)>0){
    file.remove(file_list[braki])
    file_list<<-file_list[-braki]
}

#wczytanie logow pobran
logs <- list()
for (file in file_list) {
  logs[[file]] <- read.table(file, header = TRUE, sep = ",", quote = "\"",
                             dec = ".", fill = TRUE, comment.char = "", as.is=TRUE)
}

#polaczenie plikow w jedna liste
dat <- rbindlist(logs)

#zmiana typow kolumn
dat[, date:=as.Date(date)]
dat[, package:=factor(package)]
dat[, country:=factor(country)]
dat[, weekday:=weekdays(date)]
dat[, week:=strftime(as.POSIXlt(date),format="%Y-%W")]

setkey(dat, package, date, week, country)

#zagregowanie danych
d1 <- dat[, length(week), by=package]
colnames(d1)<-c("biblioteka","suma")
rm(dat,rok,url,start,wczoraj,logs,i,sekw,braki,file,file_list,ldni)

#join ze wszystkimi bibliotekami
biblioteki<-merge(biblioteki,d1,by="biblioteka",all.x = T)
biblioteki[is.na(biblioteki$suma),3]<-0

#usuniecie wylosowanych juz bibliotek
if (length(wylosowane_biblioteki)>0){
  biblioteki<-biblioteki[-which(biblioteki$biblioteka %in% wylosowane_biblioteki),]
}

#dodanie prawdopodobienstw wylosowania danej biblioteki w zaleznosci od jej ilosci pobran
biblioteki$prob<-biblioteki$suma/sum(biblioteki$suma)

#dodanie biblioteki do wektora
wylosowane_biblioteki<-append(wylosowane_biblioteki,as.character(biblioteki[sample(seq(nrow(biblioteki)),1,prob=biblioteki$prob),1]))

