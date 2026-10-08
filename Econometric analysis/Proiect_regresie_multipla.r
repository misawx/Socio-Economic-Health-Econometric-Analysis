
# Eliberare workspace
rm(list = ls()) 
# !!! EDITATI CALEA CATRE DIRECTORUL UNDE ESTE FISIERUL .R ASA CUM ESTE IN CALCULATORUL VOSTRU
setwd(dirname(here::here()))
setwd("C:/Users/danie/OneDrive/Documente/An III/Econometrie/Proiect_v9-Andreea/Analiza_econometrica")
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

install.packages("car") #pt verificare multicolinearitate
library(car)

install.packages("ggplot2")
library(ggplot2)

install.packages("glmnet")
library(glmnet)


# !!! EDITATI CALEA CATRE DATE ASA CUM ESTE IN CALCULATORUL VOSTRU
GDP_GROWTH<-read.csv("C:/Users/danie/OneDrive/Documente/An III/Econometrie/Proiect_v9-Andreea/Date utilizate/country_data.csv",
                     sep=",")

head(GDP_GROWTH,20)
# Eliminam tarile cu gdp_per_capita<5.000 USD => setul va contine doar tari mediu si superior dezvoltate economic
GDP_GROWTH <- GDP_GROWTH %>% 
  filter(gdp_per_capita >= 5000)

# Verificam primele randuri din setul filtrat
head(GDP_GROWTH, 20)

GDP_GROWTH %<>% select(name,gdp_growth,homicide_rate,internet_users,infant_mortality,
                       urban_population,co2_emissions,unemployment,exports)
# Eliminare inregistrari cu valori lipsa
GDP_GROWTH <- na.omit(GDP_GROWTH)
head(GDP_GROWTH,20)


#Regresie multipla ---------------------------------------

# Regresie gdp_growth = beta0 + beta1*homicide_rate + beta2*urban_population + u
model_multiple <- lm(gdp_growth ~ homicide_rate+urban_population,GDP_GROWTH)
summary(model_multiple)


# Grafic 1: GDP growth vs Homicide Rate
ggplot(GDP_GROWTH, aes(x = homicide_rate, y = gdp_growth)) +
  geom_point(aes(color = urban_population), size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + #Linie de regresie
  labs(title = "GDP Growth vs Homicide Rate",
       x = "Homicide Rate", y = "GDP Growth") +
  theme_minimal()

# Grafic 2: GDP growth vs Urban Population
ggplot(GDP_GROWTH, aes(x = urban_population, y = gdp_growth)) +
  geom_point(aes(color = homicide_rate), size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Linie de regresie
  labs(title = "GDP Growth vs Urban Population",
       x = "Urban Population", y = "GDP Growth") +
  theme_minimal()


#IPOTEZE-----------------------------
#Ipoteza 1.Forma este liniara (mai sus)
      # Grafic reziduuri vs. valori ajustate
plot(fitted(model_multiple), resid(model_multiple),
     main = "Reziduuri vs Valori Ajustate model initial", xlab = "Valori Ajustate", ylab = "Reziduuri")
     abline(h = 0, col = "red")

#Ipoteza 2. Erorile trb sa aiba media -> 0
mean(model_multiple$residuals) # -1.439621e-17 => tinde spre 0 (sunt 17 de 0 dupa virgula)

#Ipoteza 3. Homoschedasticitatea reziduurilor
bptest(model_multiple) # p-value = 0.0001566 <0.1 => reziduuri heteroscedastice!!! (nu e bine) 
                                               # =>vom corecta modelul prin logaritmare 
# white_test(model_multiple)

robust_se <- sqrt(diag(vcovHC(model_multiple)))  #erorile standard robuste
summary(model_multiple, robust = TRUE)

#Ipoteza 4. Reziduurile au distributie normala
 # Test Jarque-Bera
jarque.bera.test(model_multiple$residuals) #p-value < 2.2e-16 <<< 0.05 => REZ. NU SUNT DISTRIBUITE NORMAL
  # Test Shapiro-Wilk
  shapiro.test(resid(model_multiple)) # p-value = 4.399e-09 <<< 0.05 => REZ. NU SUNT DISTRIBUITE NORMAL

  hist(model_multiple$residuals, col = "blue", main = "Histograma reziduurilor initiale")
  
#NU SUNT DELOC DISTRIBUITE NORMAL, DECI VA TREBUI SA
#CORECTAM MODELUL!! 

#Ipoteza 5. Lipsa multicolinearitatii
   # Daca VIF este mai mare de 10 pentru o variabila, aceasta sugereaza multicolinearitate puternica
vif(model_multiple)#homicide_rate urban_population 
                 #     1.013543         1.013543 
                # =>nu avem multicolinearitate deloc (minimul este 1; peste 10 inseamna MC semnificativa)

#Ipoteza 6. Reziduurile nu sunt autocorelate
  # H0 : reziduurile nu sunt autocorelate
  # H1 : reziduurile sunt autocorelate

  #Testul Durbin-Watson
  dwtest(model_multiple) # DW = 1.7109 -apropiata de 2, p-value = 0.08>0.01 =>nu avem autocorelare
  #Testul Breusch-Godfrey (BG)
  bgtest(model_multiple) # p-value = 0.5098 >0.05 >0.01 => nu avem autocorelare
  bgtest(model_multiple,order = 2) # p-value = 0.3483 >0.05>0.01 => nu avem autocorelare
  

#CORECTARE MODEL----------------------------------------------------
qqnorm(model_multiple$residuals)
qqline(model_multiple$residuals, col = "red")


#1) CORECTARE HETEROSCEDASTICITATE----------------------
# Transformarea logaritmica a variabilelor
# ! NU logaritmam urban_population pentru ca e deja exprimata in procente
GDP_GROWTH$log_homicide_rate <- log(GDP_GROWTH$homicide_rate)

# Crearea unui nou model cu variabila logaritmata
model_log <- lm(gdp_growth ~ log_homicide_rate + urban_population, data = GDP_GROWTH)
plot(fitted(model_log),resid(model_log),main = "Reziduuri vs Valori Ajustate dupa logaritmare", xlab = "Valori Ajustate", ylab = "Reziduuri")
abline(h = 0, col = "red")


# Verificarea reziduurilor modelului pt homoscedasticitate - ok!
summary(model_log)


#Verificare ipoteze pe modelul nou model_log
jarque.bera.test(model_log$residuals) #p-value < 2.2e-16 <<<0.05 => MAI CORECTAM O DATA!!! 
shapiro.test(resid(model_log)) # p-value = 1.57e-10 <<< 0.05 => rez. NU sunt distribuite normal (confirmat de jarque-bera)
bptest(model_log) # p-value = 0.01392 >0.01 => reziduuri homoscedastice =>AM CORECTAT HETEROSCEDASTICITATEA! -ok!
vif(model_log)
dwtest(model_log) # DW = 1.6091, p-value = 0.03415>0.01 => nu avem autocorelare -ok!
bgtest(model_log) # p-value = 0.5665 >0.05> =>nu avem autocorelare -ok!
bgtest(model_log,order = 2) # p-value = 0.2016 -ok!
bgtest(model_log,order = 3) # p-value = 0.3171 -ok!


#2) CORECTARE REZIDUURI PT A AVEA DSITRIBUTIE NORMALA A REZIDUURILOR-----------------------

# Calcularea reziduurilor standardizate
std_residuals <- rstandard(model_log)

# Crearea unui grafic de reziduuri standardizate
plot(std_residuals, main = "Reziduuri standardizate", ylab = "Reziduuri standardizate", pch = 19)
abline(h = c(-3, 3), col = "red", lty = 2)  # linia de referinta pentru outlieri

# Calcularea COOK'S DISTANCE
cooks_distance <- cooks.distance(model_log)

# Crearea unui grafic pentru Cook's Distance
plot(cooks_distance, main = "Cook's Distance", ylab = "Cook's Distance", pch = 19)
abline(h = 4 / length(cooks_distance), col = "red", lty = 2)  # linia de referinta

# Excluderea observatiilor influente pe baza Cook's Distance
influent_obs <- which(cooks_distance > 4 / length(cooks_distance))
GDP_GROWTH_clean <- GDP_GROWTH[-influent_obs, ]

# Recalculeaza modelul fara observatiile influente
model_clean <- lm(gdp_growth ~ homicide_rate + urban_population, data = GDP_GROWTH_clean)
summary(model_clean) # r^2 = 0.1373 / adj. r^2 = 0.1126

cooks_clean<-cooks.distance(model_clean)

plot(cooks_clean, main = "Cook's Distance dupa excluderea observatiilor influente", ylab = "Cook's Distance", pch = 19)
abline(h = 4 / length(cooks_distance), col = "red", lty = 2)


#Verificare ipoteze pe modelul nou model_clean
jarque.bera.test(model_clean$residuals) #p-value < 2.296e-12 <<< 0.05 => rez. NU sunt distribuite normal -nu e ok
shapiro.test(resid(model_clean)) # p-value = 0.0002011 <<< 0.05 => rez. NU sunt distribuite normal -nu e ok
bptest(model_clean) # p-value = 0.9727 >0.05 => reziduuri homoscedastice =>AM CORECTAT HETEROSCEDASTICITATEA! -ok!
dwtest(model_clean) # DW = 1.6091, p-value = 0.4988 > 0.05 => nu avem autocorelare -ok!
bgtest(model_clean) # p-value = 0.9826 >0.05 =>nu avem autocorelare -ok!
bgtest(model_clean,order = 2) # p-value = 0.06054 > 0.05 -ok!
bgtest(model_clean,order = 3) # p-value = 0.1251 > 0.05 -ok!

hist(model_clean$residuals, col="blue", main="Histograma reziduurilor dupa corectarea modelului")

#CONCLUZII MODEL LINEAR OPTIM---------------------
# Singura ipoteza pe care modelul nostru final, model_clean, nu o indeplineste este cea de distributie
# normala  a reziduurilor, asadar aceasta va fi o limitare modelului/datelor.


#VARIABILE DUMMY-------------
head(GDP_GROWTH_clean)

GDP_GROWTH_clean$high_unemployment_dummy<-ifelse(GDP_GROWTH_clean$unemployment>6,1,0) #sursa - media ratelor istorice globale: https://www.macrotrends.net/global-metrics/countries/wld/world/unemployment-rate#:~:text=Unemployment%20refers%20to%20the%20share,a%200.77%25%20decline%20from%202021.
GDP_GROWTH_clean$low_co2_dummy<-ifelse(GDP_GROWTH_clean$co2_emissions<quantile(GDP_GROWTH_clean$co2_emissions,0.5),1,0) # <50% dintre valori

model_dummy<-lm(gdp_growth ~ homicide_rate+urban_population+high_unemployment_dummy+low_co2_dummy, GDP_GROWTH_clean)

summary(model_dummy) # r^2 = 0.2972 / adj. r^2 = 0.262 => imbunatatit fata de model_clean
# Estimate high_unemployment_dummy = -1.26 => o tara cu rata de somaj peste 6% are, in medie, crestere economica anuala
#cu 1.26% mai mica decat o tara cu rata de somaj mai mica de 6%

# Estimare low_co2_dummy = 0.997 => o tara cu valori mici de emisii de co2 per capita are, in medie, crestere economica anuala
#cu 0.997% mai mare decat o tara cu emisii mai mari de co2 per capita

#termen de interactiune - unemployment * homicide_rate
model_dummy_interact<-lm(gdp_growth ~ high_unemployment_dummy+low_co2_dummy+
                             low_co2_dummy*high_unemployment_dummy, GDP_GROWTH_clean)
summary(model_dummy_interact)

#pentru tari cu rata ridicata de somaj, cresterea economica e cu 2% mai mica
#pentru tari cu emisii scazute de co2, cresterea economica e cu 0.74% mai mare (nu e semnificant statistic)
#efectul combinat al variabilelor dummy (rata ridicata de somaj si emisii scazute de co2) => crestere economica cu 1.27% (nu e semnificant statistic)

#RELUARE IPOTEZE

#1. Forma liniara
plot(fitted(model_dummy), resid(model_dummy),
     main = "Reziduuri vs Valori Ajustate model_dummy", xlab = "Valori Ajustate", ylab = "Reziduuri")
abline(h = 0, col = "red")

#2. medie erori->0
mean(model_dummy$residuals) # -8.404416e-17 - OK

#3. Homoschedasticitatea reziduurilor
bptest(model_dummy) # p-value = 0.5909 > 0.1 - OK

#4. reziduurile au distributie normala
hist(model_dummy$residuals, col = "blue", main="Histograma reziduurilor dupa introducerea variabilelor dummy")
jarque.bera.test(model_dummy$residuals) # p-value = 0.0001814 < 0.05
shapiro.test(model_dummy$residuals) # p-value = 0.02096 < 0.05
#in continuare nu sunt distribuite normal

#5. lipsa multicoliniaritatii
vif(model_dummy)# homicide_rate        urban_population    high_unemployment_dummy 
                # 1.091853             1.087496            1.084937 
                # low_co2_dummy 
                # 1.068301 
                #->1 => nu exista deloc multicolinearitate

#6. Reziduurile nu sunt autocorelate

dwtest(model_dummy) # DW = 1.9356 -> 2 / p-value = 0.3866>0.01 =>nu avem autocorelare
#Testul Breusch-Godfrey (BG)
bgtest(model_dummy) # p-value = 0.7813 >0.05 >0.01 => nu avem autocorelare
bgtest(model_dummy,order = 2) # p-value = 0.3194 > 0.05 >0.01 => nu avem autocorelare

#modelul s-a imbunatatit (au crescut r2 si adj r2)

#TEHNICI DE REGULARIZARE------------------------

#prediction initial
pred_data<-data.frame(homicide_rate=5,
                      urban_population=60,
                      high_unemployment_dummy=1,
                      low_co2_dummy=0)
predict(model_dummy,newdata=pred_data) #gdp growth 2.06

#predictori
x<-model.matrix(gdp_growth ~ homicide_rate+urban_population
                         +low_co2_dummy+high_unemployment_dummy, GDP_GROWTH_clean)[,-1]
#variabila raspuns
y<-GDP_GROWTH_clean$gdp_growth

#REGRESIA RIDGE -------------------------------
 # Utila pentru ca VIF a crescut la ultimul model

#modelul ridge (alpha=0)
model_ridge<-glmnet(x,y,alpha=0)
summary(model_ridge)

cv_model_ridge<-cv.glmnet(x,y,alpha=0) # Validare incrucisata pentru selectarea lambda optim
best_lambda_ridge<-cv_model_ridge$lambda.min
best_lambda_ridge # 0.4103953

plot(cv_model_ridge)

best_model_ridge<-glmnet(x,y,alpha=0,lambda = best_lambda_ridge) #modelul cu lambda optim
coef(best_model_ridge)
#homicide rate creste cu o unitate => gdp growth scade cu 0.04%
#urban population creste cu un procent => gdp growth scade cu 0.04%
#o tara cu emisii scazude de co2 are crestere economica mai mare cu 0.84% decat o tara cu emisii ridicate
#o tara cu rata mare de somaj are crestere economica mai mica cu 1.03% decat o tara cu rata mica de somaj

#diagrama Trace
plot(model_ridge,xvar="lambda")
legend("bottomright",lwd=1,col=1:4,legend=colnames(x),cex=.7)

#prognoza
y_pred_ridge<-predict(model_ridge,s=best_lambda_ridge,newx=x)

new<-matrix(c(5,60,1,0),nrow=1,ncol=4)
predict(best_model_ridge,s=best_lambda_ridge,newx=new) #gdp growth 4.021246


#r2
sstR<-sum((y-mean(y))^2)
sseR<-sum((y_pred_ridge-y)^2)
rsqR<-1-sseR/sstR
rsqR #28.82%

#REGRESIA LASSO----------------------------

model_lasso<-glmnet(x,y,alpha=1)
cv_model_lasso<-cv.glmnet(x,y,alpha=1)

best_lambda_lasso<-cv_model_lasso$lambda.min
best_lambda_lasso # 0.01545112

plot(cv_model_lasso)

best_model_lasso<-glmnet(x,y,alpha=1,lambda = best_lambda_lasso)
coef(best_model_lasso)
#homicide rate creste cu o unitate => gdp growth scade cu 0.04%
#urban population creste cu un procent => gdp growth scade cu 0.04%
#o tara cu emisii scazude de co2 are crestere economica mai mare cu 0.97% decat o tara cu emisii ridicate
#o tara cu rata mare de somaj are crestere economica mai mica cu 1.23% decat o tara cu rata mica de somaj



plot(model_lasso,xvar="lambda",label=T)
legend("bottomright", lwd = 1, col = 1:4, legend = colnames(x), cex = .7)

y_pred_lasso<-predict(best_model_lasso,s=best_lambda_lasso,newx=x)

new<-matrix(c(5,60,1,0),nrow=1,ncol=4)
predict(best_model_lasso,s=best_lambda_lasso,newx=new) #gdp growth 4.271111%
sstL<-sum((y-mean(y))^2)
sseL<-sum((y_pred_lasso-y)^2)
rsqL<-1-sseL/sstL
rsqL #29.67%


#ELASTIC NET REGRESSION-------------------

model_en<-glmnet(x,y,alpha=0.5)
cv_model_en<-cv.glmnet(x,y,alpha=0.5)

best_lambda_en<-cv_model_en$lambda.min
best_lambda_en # 0.06504626

plot(cv_model_en)

best_model_en<-glmnet(x,y,alpha=0.5,lambda=best_lambda_en)
coef(best_model_en)
#homicide rate creste cu o unitate => gdp growth scade cu 0.04%
#urban population creste cu un procent => gdp growth scade cu 0.04%
#o tara cu emisii scazude de co2 are crestere economica mai mare cu 0.93% decat o tara cu emisii ridicate
#o tara cu rata mare de somaj are crestere economica mai mica cu 1.18% decat o tara cu rata mica de somaj


plot(model_en,xvar="lambda",label=T)
legend("bottomright", lwd = 1, col = 1:4, legend = colnames(x), cex = .7)

y_pred_en<-predict(best_model_en,s=best_lambda_en,newx=x)

new<-matrix(c(5,60,1,0),nrow=1,ncol=4)
predict(best_model_en,s=best_lambda_en,newx=new) #gdp growth 4.205318

sstE <- sum((y - mean(y))^2)
sseE <- sum((y_pred_en - y)^2)
rsqE <- 1 - sseE/sstE
rsqE # 29.54%

#COMPARARE-------------

#RIDGE
coef(best_model_ridge)
best_lambda_ridge # 0.5425185
rsqR # 28.3%

#LASSO
coef(best_model_lasso)
best_lambda_lasso # 0.01545112
rsqL # 29.7%

#ELASTIC NET
coef(best_model_en)
best_lambda_en # 0.06504626
rsqE # 29.54%

#CONCLUZIE - LASSO REGRESSION ARE BONITATEA MAXIMA, coeficientii variabilelor independente nu sunt seminificativi
