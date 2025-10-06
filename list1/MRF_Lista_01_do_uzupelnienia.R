
#-----------------*************** Lista 1 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 1.1--------------------------------------
#
#_______________________________________________________________________________
# Zainstaluj pakiet FinCal w R i sprawdź polecenia 
# library(FinCal), ls("package:FinCal"), help(pv).
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#install.packages("FinCal")
library(FinCal)
ls("package:FinCal")
help(pv)

#_______________________________________________________________________________
#
#------------------------------Zadanie 1.2--------------------------------------
#
#_______________________________________________________________________________


#-----------------------------   a  --------------------------------------------
#     Wczytaj dane Notowania.csv do ramki danych df.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
data <- read.csv("C:\\Users\\Tomek\\modeling_of_financial_markets_5SEM\\list1\\Notowania.csv")


#-----------------------------   b  --------------------------------------------
#     Sprawdź liczbę kolumn i ich nazwy, liczbę wierszy 
#     oraz wyświetl pięć pierwszych i ostatnich wierszy.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

ncol(data)
colnames(data)
nrow(data)

print('pierwsze 5:')
head(data,5)
print('ostatnie 5:')
tail(data,5)


#-----------------------------   c  --------------------------------------------
#     Z ramki danych df wybierz dane odpowiadające PKOBP oraz PKN z roku 2015 
#     i zapisz je do do pliku Notowania_2015.csv.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(dplyr)
str(data)

data %>% 
  select('Data', 'PKOBP','PKN') %>%
  filter(substr(data$Data,nchar(data$Data)-1, nchar(data$Data)) == '15')



#_______________________________________________________________________________
#
#------------------------------Zadanie 1.3--------------------------------------
#
#_______________________________________________________________________________


#-----------------------------   a  --------------------------------------------
#     Pobierz dane dotyczące notowań PZU ze strony stooq.pl 
#     i zapisz je do ramki danych pzu.df.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
pzu.df <- read.csv("C:\\Users\\Tomek\\modeling_of_financial_markets_5SEM\\list1\\pzu_d.csv")




#-----------------------------   b  --------------------------------------------
#     Dołącz pakiet xts a nastepnie zamień pzu.df na pzu.xts
#     - obiekt klasy xts - za pomocą xts oraz as.Date.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(xts)
pzu.xts <- xts(pzu.df[-1], as.Date(pzu.df$Data))

str(pzu.xts)

#-----------------------------   c  --------------------------------------------
#     Z pzu.xts wybierz dane z roku 2023 jako pzu.xts.2023
#     i utwórz dla nich wykresy wszystkich zmiennych.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

pzu.xts.2023 <- pzu.xts['2023']
str(pzu.xts)
plot.xts(pzu.xts.2023[,-5], multi.panel = TRUE)
# osobno dla wolumenu z powodu innej skali, moim zdaniem nawet lepiej wyswietlac te dane osobno  
plot.xts(pzu.xts.2023[,5])


# dodatkowo wykres swiecowy, wraz z wolumenem
library(quantmod)
pzu.ohlc <- pzu.xts.2023
colnames(pzu.ohlc) <- c("Open", "High", "Low", "Close", "Volume")
chartSeries(pzu.ohlc, type = "candlesticks", name = "PZU 2025", theme = chartTheme("white"))

#-----------------------------   d  --------------------------------------------
#   Wyznacz podstawowe charakterystyki rozkładu dla wszystkich zmiennych
#   za pomocą summary() oraz współczynniki korelacji pomiędzy zmiennymi 
#   dla pzu.xts.2023.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

summary(pzu.xts.2023)

cor(pzu.xts.2023)





#_______________________________________________________________________________
#
#------------------------------Zadanie 1.4--------------------------------------
#
#_______________________________________________________________________________


#-----------------------------   a  --------------------------------------------
#     Dołącz pakiet quantmod i pobierz dane AAPL z yahoo za pomocą getSymbols.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#install.packages('quantmod')
library(quantmod)

getSymbols('AAPL', src = 'yahoo')



#-----------------------------   b  --------------------------------------------
#     Z pobranych danych AAPL wybierz te z roku 2025 i wyznacz stopy zwrotu "Rt"
#
#           R_t = (P_t-P_{t-1}) / P_{t-1]}
#
#     oraz logarytmiczne stopy zwrotu "log.Rt"
#
#           \tilde{R}_t = log(1+R_t),
#
# gdzie P_t jest ceną zamknięcia w chwili t.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
aapl.2025 <- AAPL['2025']
R_t = (aapl.2025$AAPL.Close - lag(aapl.2025$AAPL.Close, 1)/lag(aapl.2025$AAPL.Close, 1))
log.Rt = log(1+aapl.2025$AAPL.Close) 
colnames(R_t) <- c('R_t')
colnames(log.Rt) <- c('log.Rt')

#-----------------------------   c  --------------------------------------------
#     Utwórz wykresy zwrotów oraz logarytmicznych zwrotów a także ich 
#     histogramy i wykresy pudełkowe.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
par(mfrow = c(2,1))
plot.xts(R_t)
plot.xts(log.Rt)
par(mfrow = c(1,2))
hist(R_t)
hist(log.Rt)
boxplot(R_t)
boxplot(log.Rt)



# dodatkowy wykres interaktywny 
library(dygraphs)
dygraph(R_t, main = "R_t 2025") %>%
  dySeries("R_t", label = "R_t", col = "blue") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),
            drawPoints = TRUE, pointSize = 2) %>%
  dyRangeSelector()




