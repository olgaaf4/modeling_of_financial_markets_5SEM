
#-----------------*************** Lista 3 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 3.1--------------------------------------
#
#_______________________________________________________________________________
# Ile wyniosła cena "brudna" obligacji PS0123 kupionej na GPW w dniu 
# D=12.02.2019 po kursie 102.40? 
# Przyjmujemy tutaj, że dzień rozliczenia transakcji to D+2.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# od 25.01.2019 do 14.02.2019 = 20 dni
  # wartosc kuponu = 25 zl 
  odsetki <- 25*20/365
brudna <- 1000*1.0240 + odsetki 
brudna


#_______________________________________________________________________________
#
#------------------------------Zadanie 3.2--------------------------------------
#
#_______________________________________________________________________________
# Jedna sztuka obligacji OK0521 została kupiona na Giełdzie Papierów 
# Wartościowych (GPW) w dniu D=20.02.2019 po kursie 96,90. 
# Jaka jest rentowność tej inwestycji dla  kupującego 
# (zakładając, że będzie trzymał obligację do dnia wykupu), 
# jeśli prowizja maklerska jaką płaci kupujący wynosi 0,12% 
# wartości transakcji? Przyjmujemy tutaj, że dzień rozliczenia transakcji 
# to D+2.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# obligacje zerokuponowe 
# liczba dni: 822

PV <- 1000* 0.9690*1.0012 
FV <- 1000 

YTM <- (FV/PV)^(365/822) -1
YTM
#_______________________________________________________________________________
#
#------------------------------Zadanie 3.3--------------------------------------
#
#_______________________________________________________________________________
# Dana jest obligacja zerokuponowa, w przypadku której do terminu wykupu 
# pozostały dwa lata i 115 dni. Jej wartość nominalna wynosi 100 PLN,
# a wymagana stopa dochodu inwestora 10%. Dokonaj wyceny obligacji. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

## YTM=(FV/PV)^(1/t)-1
## PV = FV*(1/(YTM+1)^t)

t = 2 + 115/365
YTM=0.1
FV = 100
PV=FV*(1/(YTM+1)^t)
PV


#_______________________________________________________________________________
#
#------------------------------Zadanie 3.4--------------------------------------
#
#_______________________________________________________________________________
# Pewien inwestor zakupił obligację zerokuponową w przypadku której do
# terminu wykupu pozostało 10 lat. Rynkowa stopa rentowności przy zakupie
# wynosiła 7,25%. Inwestor ten, po 25 miesiącach sprzedał obligację,
# przy stopie rentowności 6,75%. Jaką stopę zwrotu uzyskał inwestor 
# na tej obligacji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  t1 = 25/12
  t2 = 10-t1
  r1 = 0.0725
  r2 = 0.0675
  # do obliczen przyjmujemy standardowa wart nominalu, moze ona jednak byc dowolna
  FV = 100
  
  PV0 = FV/(1+r1)^10
  PV1 = FV/(1+r2)^t2
  
  stopa = PV1/PV0-1 
  stopa
#_______________________________________________________________________________
#
#------------------------------Zadanie 3.5--------------------------------------
#
#_______________________________________________________________________________
# Inwestor rozważa zakup obligacji zerokuponowych o terminie do wykupu jeden,
# trzy lub pięć lat. W tym momencie rentowności obligacji zerokuponowych 
# 
# 1-, 2-, 3-, 4-, 5-letnich
# 
# to odpowiednio 
#
# 3,1%, 3,8%, 4%, 4,2%  4,3%
#
# przy czym rentowność jest wyrażona w skali roku z kapitalizacją co pół roku.
#
# Inwestor planuje sprzedaż kupowanych dziś obligacji za rok. 
# Rozważa przy tym dwa następujące scenariusze:
# 
# I. Stopy procentowe za rok będą takie sama jak dziś i dlatego rentowności 
# obligacji 1-, 2-, 3-, 4- i 5-letnich wyniosą odpowiednio 
# 
# 3,1%, 3,8%, 4%, 4,2%  4,3%
#
# II. Stopy procentowe (dla wszystkich okresów) za rok będą o $0,5$ p.p. 
# (punktu procentowego) wyższe niż dziś tzn. 
# rentowność obligacji 1-rocznych wyniesie 3,6%,
# dwuletnich 4,3% itd, przy czym rentowność jest tu również wyrażona 
# w skali roku z kapitalizacją co pół roku.
#
# Które z obligacji powinien zakupić inwestor w każdym z tych dwóch scenariuszy,
# aby uzyskać jak najwyższą stopę zwrotu z tej rocznej inwestycji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# I scenariusz:




# II scenariusz:








#_______________________________________________________________________________
#
#------------------------------Zadanie 3.6--------------------------------------
#
#_______________________________________________________________________________
# Znajdź YTM dla 30-letniej obligacji o wartości nominalnej 1000 PLN 
# i kuponach 40 PLN płaconych na koniec każdego roku. 
# Cena obligacji wynosi 1200 PLN.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

t0=30
PV=1200
kupon=40
FV=1000
C=40
n=30

rownanie <- function(YTM){
  sum(C/(1 + YTM)^(1:n)) + FV/(1 + YTM)^t0 - PV
}
uniroot(rownanie, c(0,1))

#_______________________________________________________________________________
#
#------------------------------Zadanie 3.7--------------------------------------
#
#_______________________________________________________________________________
# Rozpatrujemy obligacje skarbowe serii PS0728. 
# Oblicz YTM tej obligacji kupionej na przetargu, 
# który był rozliczany 25.01.2023.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 25.07.2022 - 25.01.2023 184 dni

ytm <- function(y){
odsetki = 1000*0.075*184/365
odsetki
brudna = 1070+odsetki
tpoz = (365-184)/365
suma = 1000/(1+y)^(5+tpoz)
for (i in 0:5)
  suma = suma + 75/(1+y)^(i+tpoz)
return(suma-brudna)
}
uniroot(ytm,c(0,0.1))


#_______________________________________________________________________________
#
#------------------------------Zadanie 3.8--------------------------------------
#
#_______________________________________________________________________________
# Rozpatrzmy obligację z dwuletnim terminem wykupu, o wartości nominalnej 
# 1000 PLN, 20% kuponie płatnym co pół roku i YTM równym 15%. 
#
# (a) Jaka jest jej bieżąca wartość (cena)?
# (b) Jaki jest czas trwania obligacji?
# (c) Jaki jest zmodyfikowany czas trwania obligacji?
# (d) Jak i w przybliżeniu o ile zmieni się cena obligacji, gdy YTM 
#     spadnie do 14%?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


# (a)


# (b)      


# (c)  


# (d)   


 


