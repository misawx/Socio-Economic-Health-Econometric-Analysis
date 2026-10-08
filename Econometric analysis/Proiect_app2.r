
# Eliberare workspace
rm(list = ls()) 
# !!! EDITATI CALEA CATRE DIRECTORUL UNDE ESTE FISIERUL .R ASA CUM ESTE IN CALCULATORUL VOSTRU
setwd(dirname(here::here()))
setwd("C:/Users/danie/OneDrive/Documente/An III/Econometrie/Proiect_v7_app2_ -Andreea/Analiza_econometrica")
setwd("D:/Coding/Econometrie/Proiect")
# Instalare si activare pachete
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", 
                  "olsrr", "moments","whitestrap","tseries","dplyr","here")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

install.packages("lmtest")
library(lmtest)

install.packages("sandwich")
library(sandwich)

install.packages("magrittr")
library(magrittr)

install.packages("dplyr")
library(dplyr)

install.packages("tseries")
library(tseries)

install.packages("glmnet")
library(glmnet)

install.packages("plm")
library(plm)

install.packages("stats")
library(stats)


# Importarea librariilor necesare in analiza
library(readxl)
library(tidyverse)
library(gplots)
library(foreign)
library(panelr)

# Vom studia speranta de viata in functie de rata somajului(Unemployment) 
# si procent (%) din PIB cheltuit cu Educatia (Education Expenditure)

#DATE SI FILTRARI----------------------------------------
# !!! EDITATI CALEA CATRE DATE ASA CUM ESTE IN CALCULATORUL VOSTRU
  LIFE_EXPECTANCY<-read.csv("C:/Users/danie/OneDrive/Documente/An III/Econometrie/Proiect_v7_app2_ -Andreea/Date utilizate/panel_country_data.csv",
                     sep=",")
  #Eliminare coloane care nu sunt de interes
  LIFE_EXPECTANCY %<>% select(Country_Name,Region,IncomeGroup,Life_Expectancy_World_Bank,Unemployment,Year,
                              Health_Expenditure_.)
  # Health_Expenditure_. - cheltuieli guvernamentale cu Sanatatea, ca procent din PIB
  # Unemployment - % procentul de someri din totalul fortei de munca
  # Life_Expectancy_World_Bank - speranta de viata in ani, confrom WorldBank
  # Year - vom filtra a.i. sa avem anii intre 2009 si 2018 inclusiv(10 ani)
  # IncomeGroup: am pastrat decat tarile High_Income
  
  
  # Eliminare inregistrari cu valori lipsa
  LIFE_EXPECTANCY <- na.omit(LIFE_EXPECTANCY)
  head(LIFE_EXPECTANCY,20)
  
  # Folosim doar intervalul de timp 2009-2018
  LIFE_EXPECTANCY_filtrat<- LIFE_EXPECTANCY %>%
    filter(Year >= 2009 & Year <= 2018)
  
  # Sortam datele ca sa fie datale anuale despre o tara consecutive (Romania-2009, Romania-2018,...)
  LIFE_EXPECTANCY_filtrat <- LIFE_EXPECTANCY_filtrat %>%
    arrange(Country_Name)
  head(LIFE_EXPECTANCY_filtrat,20)
  
  # Statistici descriptive
  summary(LIFE_EXPECTANCY_filtrat)
  
  # Declararea setului de date de tip panel
  pdata_df <- pdata.frame(LIFE_EXPECTANCY_filtrat,index = c("Country_Name", "Year"))
  head(panelData_df)
  
  # Aflam cate tari intra in analiza
  unique(pdata_df$Country_Name) # 46 tari 
  
  # Gaseste randul cu valoarea minima in coloana x
  index_min <- which.min(pdata_df$Life_Expectancy_World_Bank)
  index_min <- which.min(pdata_df$Unemployment)
  index_min <- which.min(pdata_df$Health_Expenditure_.)
  
  # Afiseaza randul cu valoarea minima
 pdata_df[index_min, ]
 
 # Gaseste randul cu valoarea maxima pentru fiecare coloana
 max_life_expectancy <- pdata_df[which.max(pdata_df$Life_Expectancy_World_Bank), ]
 max_unemployment <- pdata_df[which.max(pdata_df$Unemployment), ]
 max_health_expenditure <- pdata_df[which.max(pdata_df$Health_Expenditure_), ]
 
 # Afiseaza rezultatele
 max_life_expectancy
 max_unemployment
 max_health_expenditure
 
 
  
  #GRAFICE-------------------------------------------------
  # !!! Fiecare punct colorat diferit din grafice reprezinta o REGIUNE
  
  # Corelatia dintre Unemployment/spatiu/timp
  coplot(Unemployment ~ Year | Region, 
         data = LIFE_EXPECTANCY_filtrat,               # Setul de date
         type = "b",                           # Linie si puncte
         col = rainbow(length(unique(LIFE_EXPECTANCY_filtrat$Region))),  # Culori distincte pentru fiecare regiune
         pch = 16,                              # Tipul punctelor (puncte pline)
         lwd = 2,                               # Grosimea liniei
         cex = 1.5,                             # Marimea punctelor
         xlab = "Anul",                        # Eticheta pentru axa X
         ylab = "Rata somajului (%)",           # Eticheta pentru axa Y
         main = "Evolutia ratei somajului per regiune",
         cex.axis = 1.2,                       # Marimea fontului pentru axele X si Y
         cex.lab = 1.3,                        # Marimea fontului pentru etichete
         panel = panel.smooth)                  # Linia de tendinta pentru a arata directia datelor
  
  # Corelatia dintre life expectancy/spatiu/timp
  coplot(Life_Expectancy_World_Bank ~ Year | Region, 
         type = "l", data = LIFE_EXPECTANCY_filtrat, show.given = TRUE,               
         col = rainbow(length(unique(LIFE_EXPECTANCY_filtrat$Region))),
         pch = 16, cex = 1.5,lwd = 2,                         
         xlab = "Anul",ylab = "Speranta de viata (ani)", main = "Evolutia Sperantei de Viata in functie de regiune",
         cex.axis = 0.8,cex.lab = 1.2,panel = panel.smooth) 
  
  # Corelatia dintre Health expenditure/spatiu/timp
  coplot(Health_Expenditure_. ~ Year | Region, 
         type = "l", data = LIFE_EXPECTANCY_filtrat, show.given = TRUE,               
         col = rainbow(length(unique(LIFE_EXPECTANCY_filtrat$Region))),
         pch = 16, cex = 1.5,lwd = 2,                         
         xlab = "Anul",ylab = "Health Expenditure (%)", main = "Evolutia Health Expenditure in functie de regiune",
         cex.axis = 0.8,cex.lab = 1.2,panel = panel.smooth)    
  
  # Legenda pt a explica fiecare culoare (se va folosi aceeasi legenda pt toate graficele de mai sus)
  legend(x = 1,  # Poziție mai spre dreapta (0 - stânga, 1 - dreapta)
         y = 1, # Pozitie jos (0 - jos, 1 - sus)
         legend = unique(LIFE_EXPECTANCY_filtrat$Region),  # regiunile unice
         col = rainbow(length(unique(LIFE_EXPECTANCY_filtrat$Region))), 
         lwd = 2, 
         pch = 16, 
         title = "Regiuni", 
         cex = 0.8)  # Dimensiunea fontului legendei mai mica
  
  
  #HETEROGENEITATEA DATELOR---------------------------------------------------------------
  
  # Analizam distributia sperantei de viata - histograma
  # Histograma pentru variabila Life Expectancy
  hist(LIFE_EXPECTANCY_filtrat$Life_Expectancy_World_Bank, 
       main = "Distributia Sperantei de viata", 
       xlab = "Speranta de viata", 
       col = "lightblue", 
       border = "black")
       # Intepretare: 
  # Histograma ilustreaza ca majoritatea tarilor au o speranta de viata concentrata in jurul valorii de 80-82 de ani, 
  # cu mai putine tari la extremitatile distributiei. Acest lucru sugereaza ca speranta de viata este relativ ridicata pentru majoritatea
  # tarilor reprezentate in date. 
  # Histograma arata o distributie inclinata spre dreapta a sperantei de viata intre tari, cu varful in intervalul 80-82 de ani.
  # Majoritatea tarilor sunt concentrate in intervalele superioare (78-82 de ani), in timp ce mai putine tari au 
  # speranta de viata sub 74 sau peste 82 de ani.
  
  #MODEL FE VS. RE---------------------------------------------------------------------------------------
  
  # Model OLS - model clasic de regresie liniara
  ols <- plm(Life_Expectancy_World_Bank~ Health_Expenditure_. + Unemployment, 
             pdata_df, 
             index = c("Region","Year"),
             model = 'pooling')
  summary(ols) #R-squared= 28.2%
               #Health_Expenditure are efect pozitiv si semnificativ asupra Life_expectancy
  
  # Model cu efecte fixe
  model_fe <- plm(Life_Expectancy_World_Bank~ Health_Expenditure_. + Unemployment,
                  data = pdata_df, model = "within")
  summary(model_fe) #R-squared: 8.85%
                    # coeficientii sunt semnificativi la prag de seminficatie de 0.001
                    # Desi nu explica un procent mare din variatia Life_Expectancy, 
                    # rezultatele sunt robuste si semnificative
  
  # Model cu efecte aleatoare
  model_re <- plm(Life_Expectancy_World_Bank~ Health_Expenditure_. + Unemployment,
                  data = pdata_df, model = "random")
  summary(model_re)
  
  #Testul Hausmann il utilizam pentru a decide intre FE si RE
  # H0: model cu efecte random 
  # H1: model cu efecte fixe
      # p-value < 0.05 => model cu efecte fixe
      # p-value > 0.05 => model cu efecte random
  
  hausman_test <- phtest(model_fe, model_re)
  # rezultat:
  hausman_test # p-value = 0.0004035 <0.05 => FOLOSIM MODELUL CU EFECTE FIXE
  
  # Test Breusch-Pagan LM si test Parasan CD 
  pcdtest(model_fe,test='lm') # p-value < 2.2e-16 <0.05 => exista dependenta transversala
  pcdtest(model_fe,test='cd') # p-value < 2.2e-16 <0.05 =>exista dependenta transversala
   #Dependenta transversala indica faptul ca erorile pt o unitate (o tara)
  # pot influenta erorile pt alte unitati
  
  #Testul Breusch-Pagan
  bptest(model_fe)# p-value = 0.1481 > 0.5 => nu avem heteroschedasticitate -ok!
                 # => Variabilitatea reziduurilor este constanta
  
  #Testul Breusch-Godfrey/Wooldridge
  pbgtest(model_fe)# p-value < 2.2e-16 <0.5 => avem autocorelare (reziduurile
  # NU sunt independente intre ele)
  