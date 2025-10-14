
#-----------------*************** Lista 2 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 2.1--------------------------------------
#
#_______________________________________________________________________________
# Dłużnik spłaca pierwsza ratę długu 
# na koniec pierwszego roku w wysokości 8 000 PLN oraz 
# drugą na koniec drugiego roku w wysokości 6 000$ PLN. 
# Jaka jest obecna wartość długu przy założeniu oprocentowania R = 4% ?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
R=0.04
C1=8000
C2=6000
PV = C1/(1+R)^1+C2/(1+R)^2
PV
#_______________________________________________________________________________
#
#------------------------------Zadanie 2.2--------------------------------------
#
#_______________________________________________________________________________
# Wpłacamy na konto dzisiaj 5000 PLN oraz przez kolejne 11 miesiecy po 
# 1 000 PLN. Jaką kwotę na koncie ujrzymy na koniec roku, jeśli oprocentowanie 
# w skali roku to R = 11% ? Zakładamy kapitalizację miesięczną i wpłaty 
# na początku każdego kolejnego miesiąca.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
poczatek <- 5000
wplata <- 1000
r = 0.11/12

FV = poczatek * (1+r)^(12)

for(i in 0:10){
  FV=FV+wplata*((1+r)^(11-i))
} 

## 2 METODA
fv = poczatek*(1+r)
for (i in 0:10){
  fv = (fv+wplata)*(1+r)
}
fv
#_______________________________________________________________________________
#
#------------------------------Zadanie 2.3--------------------------------------
#
#_______________________________________________________________________________
# Firma zainwestowała 5.0 mln PLN PLN w pewien projekt inwestycyjny, który 
# przynosi następujące przepływy pieniezne: 1.0 mln PLN na koniec trzeciego 
# roku, 2.0 mln PLN na koniec siódmego roku, 3.0 mln PLN na koniec 
# dwunastego roku. Wyznacz IRR tej inwestycji.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C1=-5000000 #inwestycja
C2=1000000 #na koniec 3 roku
C3=2000000 #na koniec 7
C4=3000000 #na koniec 12

rownanie<- function(IRR){
  C1/(1+IRR)^0 +  C2/(1+IRR)^3 + C3/(1+IRR)^7 + C4/(1+IRR)^12}

uniroot(rownanie,c(-1,100))
#_______________________________________________________________________________
#
#------------------------------Zadanie 2.4--------------------------------------
#
#_______________________________________________________________________________
# Efektywna roczna stopa procentowa wynosi $15\%$. Podaj nominalna równoważną 
# stope procentową przy kapitalizacji 
#
# a) kwartalnej,
# b) miesięcznej,
# c) dziennej,
# d) ciągłej.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#stopa efektywna=stopa równoważnej lokaty z kapitalizacją roczną
# podpunkt (a)

m=4
reff=0.15
rnom = (((reff + 1)^(1/m)-1)*m)
rnom*100

# podpunkt (b)

m=12
reff=0.15
rnom = (((reff + 1)^(1/m)-1)*m)
rnom*100

# podpunkt (c)

m=365
reff=0.15
rnom = (((reff + 1)^(1/m)-1)*m)
rnom*100

# podpunkt (d)

reff=0.15
rnom = log(1+reff)
rnom*100



#_______________________________________________________________________________
#
#------------------------------Zadanie 2.5--------------------------------------
#
#_______________________________________________________________________________
# Na rachunek (z kapitalizacja miesięczną) chcemy wpłacic 7000 PLN teraz oraz
# co miesiac przez pół roku kwoty w wysokości 800 PLN. Jakie najniższe 
# oprocentowanie w skali roku pozwoli nam zgromadzić po pół roku kwotę 
# w wysokości 12000 PLN?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# FV = 12000


FV_function <- function(r){
  wplaty <- 800
  FV <- 7000*(1+r)^6
  for (i in 1:6){
    FV <- FV + wplaty*(1+r)^(6-i)}
  return(FV - 12000)
}
wynik <- uniroot(FV_function, interval = c(0, 0.2))

stopa_mies = wynik$root*12
stopa_mies
#_______________________________________________________________________________
#
#------------------------------Zadanie 2.6--------------------------------------
#
#_______________________________________________________________________________
# Każda z osób A,B,C,D zaciąga kredyt w wysokości K na n okresów. 
# Spłaty rat kredytu następują na koniec każdego z okresów. 
# Oprocentowanie kredytu w skali okresu wynosi r, przy czym
#
# - A spłaca kredyt w jednakowych ratach,
#
# - B spłaca kredyt metodą "jednakowych rat kapitałowych"
#   tzn. każda rata spłaty jest równa K/n plus odsetki od kwoty pozostałej 
#   do spłaty na początku okresu,
#
# - C w pierwszych n-1 ratach spłaca jedynie odsetki za kończący się okres,
#   a w ostatniej spłaca cały kapitał K plus odsetki za n-ty okres,
#
# - D spłaca całość zobowiązań na koniec n-tego okresu, przy czym 
#   odsetki są kapitalizowane na koniec każdego okresu.
#
#   Napisz funkcję KalkulatorRatalny(TypeFlag = c("A", "B", "C", "D"), K, n, r)}
#   zwracającą wektor wszystkich rat spłaty kredytu dla każdej z osób A,B,C,D.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


KalkulatorRatalny <- function(TypeFlag = c("A", "B", "C", "D"), K, n, r) {
  raty <- numeric(n)
  if (TypeFlag == 'A') {
    # renta platna z dolu wzor na present value (suma zdyskontowanych rat)
    # PV = C/r(1-(1+r)^-n)
    C <- K*r*(1+r)^n/((1+r)^n-1)
    for (i in 1:n){
      raty[i] <- C
    }
  } else if (TypeFlag == 'B'){
    # kapital <- K/n
    # odsetki <-(K-(i-1)*K/n) * r
    for (i in 1:n){
      raty[i] <- K/n + (K-(i-1)*K/n) * r
    }
  } else if (TypeFlag == 'C'){
    # najpierw odsetki za kazdy okres pozniej caly kapital + odsetki za ten okres
    for (i in 1:n-1){
      raty[i] <- K*r
    }
    raty[n] <- K + K*r
  } else {
    # nie splacamy nic przez caly okres, jedna platnosc na koncu
    # odsetki sie kapitalizuja, czyli traktujemy jak lokate 
    for (i in 1:n-1){
      raty[i] <- 0
    }
    raty[n] <- K*(1+r)^n
  }
  return(raty)
  
}

KalkulatorRatalny("C",K=1000, n=4,r=0.2)


#_______________________________________________________________________________
#
#------------------------------Zadanie 2.7--------------------------------------
#
#_______________________________________________________________________________
# Bank może nam udzielić kredytu w wysokości 16 000 PLN na okres 4 lat.
# Możemy wybrac pomiędzy jednym z dwóch sposobów spłaty kredytu:
#
# - spłata kredytu w dwóch ratach po 10000 PLN każda 
#   (na koniec drugiego i czwartego roku) 
#
# lub
#
# - spłata kredytu w czterech ratach po 5000 PLN każda.
#
# Ile wyniesie RRSO w obydwu propozycjach i która z propozycji spłaty 
# jest dla nas korzystniejsza?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


# K = 16000
# c1,c2 = 10000
# c1,2,3,4 = 5000

# wzor z wykladu, u nas mamy jedna wyplate wiec lewa strona = K 
# prawa strona zdyskontowane spłaty rat kredytu 

rrso_1 <- function(i) {
  K <- 16000
  ci <- 10000
  
  return(ci/(1+i)^2 + ci/(1+i)^4 - K) 
}

rrso_a <- uniroot(rrso_1, c(0,0.2))$root
rrso_a

## 2 propozycja

rrso_2 <- function(RRSO){
  S1 = 5000/(1+RRSO)^1 
  S2 = 5000/(1+RRSO)^2 
  S3 = 5000/(1+RRSO)^3
  S4 = 5000/(1+RRSO)^4
  return(S1+S2+S3+S4 - 16000 )
}

uniroot(rrso_2,c(0,0.1))
## Odp: Bardziej opłaca się 1 propozycja

#_______________________________________________________________________________
#
#------------------------------Zadanie 2.8--------------------------------------
#
#_______________________________________________________________________________
# Bank może nam udzielić kredytu w wysokości 200 000 PLN na okres 5 lat.
# Kredyt będzie spłacany metodą równych rat kapitałowych tj. 
# każda w wysokości 40 000 PLN, płatne na początku roku.
#
# Stopa procentowa wynosi 4%.
#
# W chwili t=0 pobierane są następujące opłaty
#
  # - prowizja 6000 PLN,
  # - opłata przygotowawcza 2000 PLN,
  # - wycena zabezpiecenia 1000 PLN.
# 
# Ponadto na początku każdego okresu płacone jest również 
# ubezpieczenie spłaty w wysokości 2% kwoty kredytu pozostałej do spłaty.
#
# Ile wynosi RRSO tego kredytu?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# wzor z wykladu, u nas mamy jedna wyplate wiec lewa strona = K 
  # prawa strona zdyskontowane spłaty rat kredytu
  # platnosci sa w chwilach t0,1,2,3,4
# placimy tam: K/n kredytu + 4% odsetki z pozostalego kredytu do splaty + 2% ubezpieczenie z kwoty pozostalej
# dodatkowo w chwili t0 placimy dodatkowo 



rrso_8 <- function(irr){
  K <- 200000
  n <- 5
  dodatek_t0 <- 6000+2000+1000
  suma <- 0
  for (i in 0:4){
    kwota_pozostala <- K - (i+1)*K/n
    odsetki <- kwota_pozostala*0.04
    ubez <- kwota_pozostala*0.02
    ci = K/n + odsetki + ubez
    if (i == 0){ci <- ci+dodatek_t0}
    suma <- suma +  -ci/(1+irr)^i
    
  }
  return(suma + K)
}
wyn <- uniroot(rrso_8, c(0,0.1))
wyn






