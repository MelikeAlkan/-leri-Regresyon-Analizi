#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(olsrr)
library(lmtest)
library(readr)
veri <- read.csv("C:/Users/Melike/Desktop/CarPrice_Assignment.csv",header = T)
veri$car_ID  <-factor(veri$car_ID)
set.seed(2)
index<-sample(1:nrow(veri),round(nrow(veri)*0.85)) 
veritrain<-veri[index,] 
veritest<-veri[-index,] 
wmod<-lm(residuals(lmod)^2~fitted(lmod)+fitted(lmod)^2,veritrain)
library(ggplot2) 
veritrain$artik <-residuals(lmod) #EKK modelinden elde edilen artiklar (residuals)
veritrain$tahmin<-predict(lmod) #EKK modelinden elde edilen tahminler (prediction)
model1<-lm(abs(veritrain$artik)~compressionratio,data=veritrain)
weights1<-1/(predict(model1))^2 
veritrain<-veritrain[, -c(27,28)]
weightedleastsquaremod1<-lm(price~wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg, data= veritrain, weights = weights1)
model2<-lm(abs(residuals(lmod))~predict(lmod))
weights2<-1/(predict(model2))^2 
weightedleastsquaremod2<-lm(price~wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg, data= veritrain, weights = weights2)
kareresid<-((residuals(lmod))^2)
model3<-lm(kareresid~highwaympg,data=veritrain)
weights3<-1/(predict(model3))^2 
weightedleastsquaremod3<-lm(price~wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg,data=veritrain, weights = weights3)
library(dplyr)
X<-model.matrix(lmod) 
y<-veritrain$price #verimizdeki yanit degiskeni
W<-diag(weights2) #kosegenlerinde agirliklar olan matris
Z<-sqrt(W)%*%X #w matrisinin karekoku ile x in carpimi
yyildiz<-sqrt(W) %*% y
Betawls<-solve(t(Z)%*%Z,t(Z)%*%yyildiz) 
cbind(Betawls,coef(weightedleastsquaremod2))
donart<-yyildiz-Z%*%Betawls #donusturulmus model artiklari
bpmod<-lm(donart^2~ wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg,data=veritrain) 
golddata=read.csv("C:/Users/Melike/Desktop/BSE-BOM590111.csv", header=T)
GoldData <- golddata%>%select(c("Total.Turnover","Open","High","Low","Close","WAP","No..of.Shares","No..of.Trades","Deliverable.Quantity","X..Deli..Qty.to.Traded.Qty"))
set.seed(2)
index<-sample(1:nrow(GoldData),round(nrow(GoldData)*0.85)) 
veritrain1<-GoldData[index,] 
veritest1<-GoldData[-index,] 
lmod1<- lm(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty,data=veritrain1)
library(corrplot)
x <- model.matrix(lmod1)[,-1] #yanit degiskeni Total.Turnover i cikardik
e <- eigen(t(x)%*%x)$values
k <- sqrt(max(e)/min(e))
library(car)
lambda<- 10^seq(-15, 9, length.out = 200)
x1<-as.matrix(veritrain1[,-1])
library(glmnet)
ridgemodel<-glmnet(x1,veritrain1$Total.Turnover,alpha = 0,lambda = lambda)
cv.fitridge<-cv.glmnet(x1,veritrain1$Total.Turnover,alpha=0,lambda = lambda) #yukarida olusturulan lambda dizisi icin
optimumlambda<-cv.fitridge$lambda.min #optimum lambda icin bunlar icerisinden min olan secilir. 
lambda_1SE<-cv.fitridge$lambda.1se
ridgemodel1<-glmnet(x1,veritrain1$Total.Turnover,alpha=0,lambda=optimumlambda)
rmse<-function(true, predicted,n) {sqrt(sum((predicted - true)^2)/n)}
ypredictedridge <- predict(ridgemodel, s = optimumlambda, newx = as.matrix(veritest1[,-1]))# kurulan model uzerinden elde edilen tahmin degerleri
rsquare <- function(true, predicted) { 
  sse <- sum((predicted - true)^2) 
  sst <- sum((true - mean(true))^2) 
  rsq <- 1 - sse / sst 
  rsq }
ridgerkare<-rsquare(veritest1$Total.Turnover,ypredictedridge) 
ridgermse<-rmse(veritest1$Total.Turnover,ypredictedridge,length(veritest1$Total.Turnover))
ridgeartik<-veritest1$Total.Turnover-(ypredictedridge)
ridgeaic<-nrow(GoldData)*(log(2*pi)+1+log((sum((ridgeartik)^2)/nrow(GoldData))))+((length(ridgemodel$Total.Turnover)+1)*2) 
ridgebic<-nrow(GoldData)*(log(2*pi)+1+log((sum((ridgeartik)^2)/nrow(GoldData))))+((length(ridgemodel$Total.Turnover)+1)*log(nrow(GoldData)))
cv.fitlasso<-cv.glmnet(x1,veritrain1$Total.Turnover,alpha=1,lambda = lambda) #lambda dizisinde belirledigimiz lamdalar kullanilir.
optimallambda<-cv.fitlasso$lambda.min
lambda_1SE1<-cv.fitlasso$lambda.1se
lassomodel<-glmnet(x1,veritrain1$Total.Turnover,alpha=1,lambda=optimallambda)
ypredictedlasso <- predict(lassomodel, s = optimallambda, newx = as.matrix(veritest1[,-1]))
lassorkare<-rsquare(veritest1$Total.Turnover,ypredictedlasso)
lassormse<-rmse(veritest1$Total.Turnover,ypredictedlasso,length(veritest1$Total.Turnover))
lassoartik<-veritest1$Total.Turnover-(ypredictedlasso)
lassoaic<-nrow(GoldData)*(log(2*pi)+1+log((sum((lassoartik)^2)/nrow(GoldData))))+((length(lassomodel$Total.Turnover)+1)*2)
lassobic<-nrow(GoldData)*(log(2*pi)+1+log((sum((lassoartik)^2)/nrow(GoldData))))+((length(lassomodel$Total.Turnover)+1)*log(nrow(GoldData)))
cv.fitelasticnet<-cv.glmnet(x1,veritrain1$Total.Turnover,alpha=0.5,lambda = lambda)
optlambda<-cv.fitelasticnet$lambda.min
lambda_1SE2<-cv.fitelasticnet$lambda.1se
elasticmodel<-glmnet(x1,veritrain1$Total.Turnover,alpha=0.5,lambda=optlambda)
ypredictedelasticnet <- predict(elasticmodel, s = optlambda, newx = as.matrix(veritest1[,-1]))
elasticrkare<-rsquare(veritest1$Total.Turnover,ypredictedelasticnet)
elasticrmse<-rmse(veritest1$Total.Turnover,ypredictedelasticnet,length(veritest1$Total.Turnover))
elasticartik<-veritest1$Total.Turnover-(ypredictedelasticnet)
elasticaic<-nrow(GoldData)*(log(2*pi)+1+log((sum((elasticartik)^2)/nrow(GoldData))))+((length(elasticmodel$Total.Turnover)+1)*2)
elasticbic<-nrow(GoldData)*(log(2*pi)+1+log((sum((elasticartik)^2)/nrow(GoldData))))+((length(elasticmodel$Total.Turnover)+1)*log(nrow(GoldData)))
tablo<-matrix(c(ridgeaic,ridgebic,ridgermse,ridgerkare,
                lassoaic,lassobic,lassormse,lassorkare,
                elasticaic,elasticbic,elasticrmse,elasticrkare),3,4,byrow = TRUE)
row.names(tablo)<-c("Ridge","Lasso","Elasticnet") 
colnames(tablo)<-c("AIC","BIC","RMSE","Rkare")
lmod2 <- lm(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty,data=veritrain1) 
rmse1<- function(x,y) sqrt(mean((x-y)^2))
library(pls)
pcrmodel <- pcr(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty,data=veritrain1,scale=T) 
pcrmse <- RMSEP(pcrmodel)
pcrmse1<- RMSEP(pcrmodel,newdata=veritest1)
set.seed(2) 
pcrmodel1 <- pcr(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty,data=veritrain1,scale=T,validation="CV") 
pcrCV <- RMSEP(pcrmodel1, estimate="CV")
model1 <- lm(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty,data=golddata) 
fit<- fitted(model1)
resid <-residuals(model1)
library(ggplot2)
library(faraway) 
stud <- rstudent(model1)
library(MASS)
hubermod <- rlm(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty ,data=golddata)
stud1<- rstudent(hubermod) #robust regresyonunun standart artiklari
bisquaremod <- rlm(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty ,data=golddata,psi=psi.bisquare)
biweights <- data.frame(state= golddata$Date, resid = bisquaremod$resid, weight = bisquaremod$w)
biweights2 <- biweights[order(bisquaremod$w), ] 


ui<-dashboardPage(
    dashboardHeader(title ="ILERI REGRESYON"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Veri Tanitilmasi", tabName = "first",icon = icon("th")),
            
            menuItem("Model Kurulumu", tabName = "second", icon = icon(("th"))),
            
            menuItem("Varsayimlarin Sinanmasi", tabName = "third", icon = icon("th"),
                     
                     menuSubItem(
                         "Sabit Varyanslilik", tabName = "homojenvaryanslilik", icon = icon("th")),
                     
                     menuSubItem(
                         "Degisken Varyanslilik", tabName = "degiskenvaryanslilik", icon = icon("th"))),
            
            menuItem("Agirlikli EKK", tabName = "EKK", icon = icon("th")),
            
            menuItem("Collinearity", tabName = "Collinearity", icon = icon("th"),
                     
                     menuSubItem(
                       "collinearity", tabName = "collinearity", icon = icon("th")),
                     menuSubItem(
                         "Ridge Regresyon", tabName = "ridge", icon = icon("th")),
                     menuSubItem(
                         "Lasso Regresyon", tabName = "lasso", icon = icon("th")),
                     menuSubItem(
                         "Elastic Net Regresyon", tabName = "elastic", icon = icon("th")),
                     menuSubItem(
                         "Temel Bilesenler Regresyonu", tabName = "pcr", icon = icon("th"))),
            
            menuItem("Outlier", tabName = "outlier", icon = icon("th"),
                     menuSubItem(
                         "Normallik", tabName = "normallik",icon = icon("th")),
                     
                     menuSubItem(
                         "Robust Regresyon", tabName = "robust", icon = icon("th"))
            ))
        
        
    ), 
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "first",
                    h1("MELIKE FATMA ALKAN 121516061"),
                    h1("1.Veri Tanitilmasi:ARABA FIYAT TAHMINI"),
                    h2("Veri Seti Tanimi"),
                       p("Cinli bir otomobil sirketi Geely Auto, orada uretim birimlerini kurarak ve ABD ve Avrupali meslektaslarina rekabet edebilmek icin yerel olarak otomobil ureterek ABD pazarina girmeyi hedefliyor.
                        Otomobillerin fiyatlandirmasinin hangi faktorlere bagli oldugunu anlamak icin bir otomobil danismanlik sirketi ile anlastilar.Ozellikle , Amerikan pazarinda araba fiyatlandirmasini etkileyen faktorleri anlamak istiyorlar cunku bunlar Cin pazarindan cok farkli olabilir.Sirket bilmek istiyor:
                            Bir arabanin fiyatini tahmin etmede hangi degiskenler onemlidir.Bu degiskenler bir arabanin fiyatini
                        ne kadar iyi tanimlamaktadir.Danismanlik sirketi,cesitli pazar arastirmalarina dayanarak, Amerika pazarinda farkli tipte araclardan olusan buyuk bir veri seti topladi."),
                        p("Veri setimiz 26 degiskenli ve 205 gozleme sahiptir."),
                        h3("Hedefimiz"),
                        p("Mevcut bagimsiz degiskenlere sahip araclarin fiyatini modellememiz gerekmektedir.Yonetim tarafindan fiyatlarin bagimsiz degiskenlerle tam olarak nasil degistigini anlamak icin kullanilacaktir.Bu nedenle,belirli fiyat seviyelerini karsilamak icin arabalarin tasarimini, is stratejisini vb. Manipule edebilirler.Ayrica,model yonetimin yeni bir pazarin fiyatlandirma dinamiklerini anlamasi icin iyi bir yol olacaktir."),
                        
                        h2("Degiskenler:"),
                            
                        p("car_ID:Araba Numarasi"),
                        p("symboling:Simgesel"),
                        p("CarName:Araba modeli"),
                        p("fueltype:Yakit tipi(gaz/dizel)"),
                        p("aspiration:Havalandirma"),
                        p("doornumber:kapi sayisi"),
                        p("carbody:Arac govdesi"),
                        p("drivewheel:Kuvvet ileten ve torku lastiklerden yola cekis kuvvetine donusturerek aracin hareket etmesine neden  olan bir motorlu tasitin tekerlegidir."),
                        p("enginelocation:Motor yeri"),
                        p("wheelbase:Tekerlek acikligi"),
                        p("carlength:Araba boyu"),
                        p("carwidth:Araba genisligi"),
                        p("carheight:Araba yuksekligi"),
                        p("curbweight:Firen agirligi"),
                        p("enginetype:Motor tipi"),
                        p("cylindernumber:Silindir numarasi"),
                        p("enginesize:Motor genisligi"),
                        p("fuelsystem:Yakit sistemi"),
                        p("boreratio:Pistonlu bir piston motorunda,delik/strok orani veya strok/delik orani ile tanimlanan strok orani, silindir capi ve piston strok uzunlugu arasindaki orani aciklayan bir terimdir."),
                        p("stroke:Inme (pistonun her iki yonde silindir boyunca tam hareketini ifade eder)"),
                        p("compressionratio:Sikistirma orani"),
                        p("horsepower:Beygir gucu"),
                        p("peakrpm:En yuksek devir"),
                        p("citympg:Sehir mpg"),
                        p("highwaympg:Kara yolu mpg"),
                        p("price:Fiyat"),
                    
                 h1("2.Veri Tanitilmasi:ALTIN FIYAT VERI SETI"),
                 h2("Veri Seti Tanimi"),
                       p("Hargreaves Lansdown sirketi ile Spread Co Sirketinin altin fiyat veri setidir.Bu veri setinde altin fiyatinin etkileyen degiskenler verilmistir.Verilen degiskenlere bakilarak en yuksek en dusuk gibi bagimsiz degiskenlerin  
                         toplam ciroyu etkileyip etkilemedigini kontrol edilir."),
                       p("Altin Fiyat Veri Setimiz 12 Degiskenli ve 1660 gozlemlidir."),
                       h2("Degiskenler:"),
                       
                       p("Total.Turnover:Altin fiyatinin toplam cirosu"),
                       p("Open:Acilis fiyati"),
                       p("High:Altin fiyatinin en yuksek noktasi"),
                       p("Low:Altin fiyatinin en dusuk noktasi"),
                       p("Close:Kapanis fiyati"),
                       p("WAP:Hacim agirlikli ortalama fiyat"),
                       p("No..of.Shares:Pay sayisi"),
                       p("No..of.Trades:Islem Sayisi"),
                       p("Deliverable.Quantity:Teslim edilebilir miktar"),
                       p("X..Deli..Qty.to.Traded.Qty:Islem miktari"),
                       p("Spread.H.L:Altin fiyatinin yayilimi(H.L Sirketi)"),
                       p("Spread.C.O:Altin fiyatinin yayilimi(C.O Sirketi)")),
                 
            
            tabItem(tabName = "second",
                    h1("Model Kurulumu"),
                    p("1.Verimiz icin (ARABA FIYAT TAHMINI COKLU DOGRUSAL REGRESYON) Regresyon Modelimizi Kuralim"),
                    p("lmod<- lm(price~wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg,data=veritrain)"),
                    box(verbatimTextOutput("summarymodel"), width = 12),
                    p("Kurulan regresyon modelinin anlamliligina baktigimizda p value yaklasik 0'dir.Yani 0.05'ten kucuk oldugundan kurulan model anlamlidir.")),
                    
            
            
            
            tabItem(tabName = "third",
                    h1("Varsayimlarin Sinanmasi"),
                    p("yorumlar buraya yazılacak")),
            
            tabItem(tabName = "homojenvaryanslilik",
                    h2("Sabit Varyanslilik Testleri"),
                    p("Sabit varyansliligin en kullanisli teshis yontemi artiklara (residuals(lmod)) karsilik tahmin (fitted(lmod)) degerlerinin  plotlanmasidir."),
                    box(plotOutput("homojenvaryanslilik1"),width = 12),
                    p("Cizdirgimiz grafikte sifir etrafinda nasil dagildigini gormek icin h=0 ile yataya cizgi ekledik.Grafigimiz bize duzgun bir sekil vermedigi icin sabit varyansli mi diye emin olamiyoruz.Bunun icin degisken varyanslilik testlerine bakmaliyiz.")),
            
            tabItem(tabName ="degiskenvaryanslilik",
                    h2("Degisken Varyans Testleri"),
                    h2("BREUSCH-PAGAN TESTI"),
                    p("H0:Heterosce Dosticity (Degisken Varyanslilik) problemi yok. 
                       H1:Heterosce Dosticity (Degisken Varyanslilik) problemi vardir."),
                    box(verbatimTextOutput("degiskenvaryanslilik"),width = 12),
                    p("Breusch pagan testimizin sonucuna gore p-value degerimiz 3.869e-10 yani yaklasik 0 cikmistir.P-value degerimiz 0.05'ten kucuk  oldugundan H0 hipotezi reddedilir yani heterocedosticity  (degisken varyanslilik) problemi vardir deriz."),
                    h2("WHITE TEST"),
                    p("wmod<-lm(residuals(lmod)^2~fitted(lmod)+fitted(lmod)^2,veritrain)"),
                    box(verbatimTextOutput("degiskenvaryanslilik1"),width = 12),
                    p("White testimizin sonucuna gore p-value degerimiz 1.911e-13 yani yaklasik 0 cikmistir.P-value degerimiz 0.05'ten kucuk oldugundan H0 hipotezi reddedilir yani heterocedosticity  (degisken varyanslilik) problemi vardir deriz.")),
            
            tabItem(tabName = "EKK",
                    h2("Agirlikli En Kucuk Kareler"),
                    p("Varyanslarin homojenligi saglanmamasi durumunda Yanit degiskeni uzerinde donusum yapmak veya Agirlikli En Kucuk Kareler yontemlerine basvururuz."),
                    p("Siradan en kucuk kareler yontemi, hata varyanslarinin sabit oldugunu varsayar(homoscedasticity). Agirlikli en kucuk kareler yontemi bu varsayim saglanmadigi durumlarda kullanilir."),
                    p("Varyansi buyuk olan degiskenin model uzerinde etkisi fazla olur. 
                       EKK nin en iyi calisabilmesi icin hata varyansi  Sigma2i   i=1,2,....n birbirine esit olmasi gerekir. Eger Sigma2i ler esit degil ise agirlikli en kucuk kareler yontemine gecmeliyiz."),
                    p("Her bir  Sigma2i varyansina karsilik wi= 1/Sigma2i agirligini tanimlariz.(Agirliklari 1/Sigma2i almamizin sebebi hepsinin varsayansini 1 e ve birbirlerine esitlemeye calismamizdir. Sigma2i*(1/Sigma2i))"),
                    p("Bu sekilde agirligi buyuk olanin agirligini alip, agirligi kucuk olana agirlik yukluyoruz.
                       Buradaki zorluk Sigma2i parametresinin bilinmemesinden kaynaklanir ve w matrisi kolay belirlenemez."),
                    p("Agirliklari belirlemek icin ilk olarak EKK regresyon modeli kurulur artiklar elde edilir.Agirliklar belirlendikten sonra agirliklandirilmis EKK kullanilir.Regresyon modeli uzerinden artiklar hesaplanip agirlik incelemesi yapilir. Gercekten kullanilan agirlikla varyans homojen hale gelmis mi diye bakilir.Eger varyans homojenligi saglanmamissa tekrar agirliklandirma yapilir buna iteratif agirliklandirma denir."),
                    h2("Bazi olasi varyans ve standart sapma fonksiyonu tahminleri:"),
                    p("1) Aciklayici degiskenlere karsilik artiklarin grafigini cizdirip megafon sekli varmi diye bakilir.Eger megafon sekli var ise aciklayici degiskenler ile mutlak artiklarin arasinda regresyon modeli kurulur.Bu regresyon modeli uzerinden tahmin degeri elde edilir.Bu elde elilen tahmin degerlerini Sigmai yerine kullaniriz.Agirliklar da 1/Sigma2i diye olusturulur."),
                    p("2) Ilk basta kurulan EKK modelindeki tahmin edilen yanitlara (y sapkalara) karsilik ei lerin grafigi megafon seklinde ise artiklarin mutlak degerine karsilik y sapkalarin regresyon modeli kurulur.Bu kurulan modelden elde edilen tahmin degerlerini Sigmai yerine kullaniriz.Agirliklari da wi= 1/Sigma2i seklinde olustururuz."),
                    p("3) Aciklayici degiskene karsi ei kare grafigi artan seklindeyse ei karelere karsilik o aciklayici degiskenin regresyon modeli kurulur.Bu modelin tahminlerini Sigma2i yerine kullaniriz.Agirliklari da  wi= 1/Sigma2i seklinde olustururuz."),
                    p("4) Tahmin edilen yanitlara (y sapka) karsi ei kare grafigi artan seklindeyse ei karelere karsilik tahmin edilen yanitlara regresyon modeli kurulur. Bu modelden elde edilen tahminler Sigma2i tahminleri olarak kullanilir. Agirliklari da  wi= 1/Sigma2i seklinde olustururuz."),
                    p("Bunlardan hangisi daha uygun gorulurse agirlik o sekilde belirlenmelidir."),
                    p("Simdi verimiz uzerinde bu anlatilanlari yapmaya baslayalim"),
                    p("EKK modelimizdeki elde edilen artiklar ve tahmin degerlerini verimize degisken olarak ekledik."),
                    p("Simdi Bazi olasi varyans ve standart sapma fonksiyonu tahminleri nden 1.sini inceleyelim."),
                    box(plotOutput("pairs1"),width = 12),
                    p("Artiklar ile compressionratio sacinim grafigi megafon seklindedir. Bu bagimsiz degisken ile artiklarin mutlak degeri arasinda regresyon modeli kuralim."),
                    p("weightedleastsquaremod1<-lm(price~wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg, data= veritrain, weights = weights1)"),
                    box(verbatimTextOutput("weightedleastsquaremod1"), width = 12),
                    p("Summary kodumuza baktigimizda Residual standard error : 1.484 ve Adjusted R-squared : 0.8459 cikmistir."),
                    p("Simdi Bazi olasi varyans ve standart sapma fonkiyonu tahminleri nden 2.sini inceleyelim;"),
                    box(plotOutput("pairs2"),width = 12),
                    box(verbatimTextOutput("weightedleastsquaremod2"), width = 12),
                    p("Summary kodumuza baktigimizda Residual standard error : 1.245 ve Adjusted R-squared : 0.7825 cikmistir."),
                    p("Simdi Bazi olasi varyans ve standart sapma fonkiyonu tahminleri nden 3.sunu inceleyelim;"),
                    box(plotOutput("pairs3"),width = 12),
                    p("Aciklayici degiskene karsi ei kare grafigi artan seklinde oldugu icin ei karelere karsilik o aciklayici degiskenin regresyon modeli kurulur."),
                    box(verbatimTextOutput("weightedleastsquaremod3"), width = 12),
                    p("Summary kodumuza baktigimizda Residual standard error : 0.0002807 ve Adjusted R-squared : 0.8834 cikmistir."),
                    p("Simdi Bazi olasi varyans ve standart sapma fonkiyonu tahminleri nden 4.sunu inceleyelim;"),
                    box(plotOutput("pairs4"),width = 12),
                    p("Tahmin edilen yanitlara karsilik hatalarin karelerinin grafigi artan seklinde olmadi icin  bu yontemi kullanamayiz."),
                    p("EKK modelimizin Residual standard error : 3206 ve Adjusted R-squared : 0.846 dir."),
                    p("1. yontem ile agirliklandirma yaptigimizda Residual standard error : 1.484 ve Adjusted R-squared : 0.8459 dir."),
                    p("2. yontem ile agirliklandirma yaptigimizda Residual standard error : 1.245 ve Adjusted R-squared : 0.7825 dir."),
                    p("3. yontem ile agirliklandirma yaptigimizda Residual standard error : 0.0002807 ve Adjusted R-squared : 0.8834 dir."),
                    p("Ayrica agirliklandirma yaptigimda kullanilan bagimsiz degiskenlerimizin katsayilarinda degisimler meydana gelmistir."),
                    p("Simdi degisken varyanslilik probleminin giderildigini kontrol edelim."),
                    box(plotOutput("kontrol"), width = 12),
                    p("EKK modeli Basit EKK modeline donusturelim"),
                    box(verbatimTextOutput("out1"), width = 12),
                    p("Birinci sutun donusum ile cikan Beta katsayilarini,Ikinci sutun lm kodu ile elde edilen Beta katsayilarini gosterir.Agirliklandirilmis EKK modelindeki Beta katsayilari ile donusum yapildiginda cikan Beta katsayilari birebir ayni cikti"),
                    box(verbatimTextOutput("out2"), width = 12),
                    p("Birinci sutun donusturulmus artiklari , ikinci sutun Agirliklandirilmis EKK modelinin artiklarini gostermektedir.Modelin artiklarina bakildiginda birbirinden ayri cikmistir."),
                    p("lm modeli ile kurulan Agirliklandirilmis EKK modelinin artiklarini kok icinde w ile carparak donusturulmus modelin artiklari elde edilir."),
                    box(verbatimTextOutput("out3"), width = 12),
                    p(" Birinci sutun donusturulmus artiklari , ikinci sutun Agirliklandirilmis EKK modelinin artiklarini gostermektedir.Modelin artiklarina bakildiginda birbirleriyle ayni cikmistir."),
                    p("Eger agirliklandirilmis ekk uzerinden residuals standart error hesaplarsak;"),
                    box(verbatimTextOutput("out4"), width = 12),
                    p("Agirliklandirilmis ekk modelinin residuals standart erroru ile ayni cikmamistir"),
                    p("Simdi donusturulmus artiklarin standart errorunu hesaplayalim;"),
                    box(verbatimTextOutput("out5"), width = 12),
                    p("Simdi Agirliklandirilmis modelimiz icin BREUSCH-PAGAN TESTI yapalim;"),
                    h2("BREUSCH-PAGAN TESTI"),
                    p("H0:Heterosce Dosticity (Degisken Varyanslilik) problemi yok."),
                    p("H1:Heterosce Dosticity (Degisken Varyanslilik) problemi vardir."),
                    p("bpmod<-lm(donart^2~ wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg,data=veritrain)"),
                    box(verbatimTextOutput("bpt"), width = 12),
                    p("BREUSCH-PAGAN Testimizin sonucuna gore p-value degerimiz 0.1771 cikmistir.P-value degerimiz 0.05'ten buyuk oldugundan H0 hipotezi kabul edilir yani degisken varyanslilik problemi yoktur deriz. Goruldugu uzere agirliklandirma yaparak degisken varyanslilik problemini ortadan kaldirmis olduk.")),
                    
                    
            tabItem(tabName = "Collinearity",
                    h2("Aciklayici Degiskenlerle Ilgili Problemler")),
                    
                  
                    tabItem(tabName = "collinearity",
                    h2("Altin Fiyat Verisi ile calisacagiz"),
                    h2("IC ILISKI (COLLINEARITY)"),
                    p("Iki degisken arasi lineer iliskiyi gosterir.Eger bir aciklayici degisken ve bir diger aciklayici degiskenin veya degiskenlerin lineer bir kombinasyonlari ise bu durumda x transpoz x matrisi (X'X) singuler olur ve tersi alinamaz. Bu durumdan ilgili degiskenlerden biri modelden cikartilarak kurtulunur.
                       Gozlem sayisi arttikca ic iliski durumu azalir."),
                    p("lmod1<- lm(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty,data=GoldData)"),
                    box(verbatimTextOutput("lmod1"), width = 12),
                    p("Kurulan regresyon modelinin anlamliligina baktigimizda p value yaklasik 0 cikmistir.P-value degerimiz 0.05'ten kucuk oldugundan HO red edilir yani kurulan model anlamlidir deriz."),
                    p("Simdi Collinearity teshisi icin korelasyon matrisi, kosul indeksi ve vif e bakacagiz"),
                    h2("KORELASYON MATRISI"),
                    p("Simdi x in korelasyon matrisine bakalim. Bunun icin yanit degiskenini (Total.Turnover) veriden cikartmaliyiz.Geri kalanlarin korelasyon matrisine bakmaliyiz."),
                    box(verbatimTextOutput("cor"), width = 12),
                    p("Korelasyon matrisine baktigimizda ornegin en yuksek Open ile High (aralarindaki korelasyon 0.9986117) bagimsiz degiskenlerinin iliskili oldugu gorulmektedir. Diger bagimsiz degiskenler arasinda da korelasyon oldukca yuksektir."),
                    p("Bu korelasyona simdi korelasyon plotu ile bakalim."),
                    box(plotOutput("cor1"), width = 12),
                    p("Korelasyon plotunda Pozitif korelasyonlar mavi, negatif korelasyonlar kirmizi renkte gosterilir."),
                    p("Korelasyon plotu ile korelasyon matrisimizin sonuclari ayni cikmistir.Bagimsiz degiskenlerin arasinda pozitif yonlu iliskinin yuksek oldugu gorulmektedir.(Mavinin tonuna gore en yuksek iliskiden en dusuk iliskiye gore koyu maviden aciga dogru gidiyor.)"),
                    h2("KOSUL INDEKSI"),
                    p("Kappa degeri > 30 ise orta derece collinearity , Kappa degeri > 100 ise guclu collinearity oldugunu gosterir."),
                    p("Kurulan regresyon modelimizi matrix haline donusturelim"),
                    box(verbatimTextOutput("matrix"), width = 12),
                    p("Verimizden yanit degiskenini cikartip sadece aciklayici degiskenlerden olusan matrix formuna donusturuyoruz."),
                    p("Simdi x transpoz x in eigen valuelerini hesaplayalim"),
                    box(verbatimTextOutput("eigen"), width = 12),
                    p("Kappa degeri : sqrt(en buyuk ozdeger / en kucuk ozdeger)"),
                    box(verbatimTextOutput("kappa"), width = 12),
                    p("Sonucumuzda Kappa = 894070.7 > 30 oldugundan sonucumuza gore collinearity (ic iliski) problemi vardir."),
                    h2("VIF"),
                    p("Xi degiskenlerinin diger bagimsiz degiskenler ile regresyonundan elde edilen R kare degerlerinin yuksekligi Collinearity (ic iliski) nin varligini gosterir. Buna bagli gelistirilmis olcut VIF dir"),
                    p("VIF degerinin 10 dan buyuk olmasi collinearity (ic iliski) probleminin oldugunu soyler"),
                    p("Hazir kod ile vif degerlerine bakmak icin car paketi kullanilir."),
                    box(verbatimTextOutput("vif"), width = 12),
                    p("Bagimsiz degiskenlerimiz icin hesaplatilan vif degerlerimiz 10 dan buyuk oldugundan bagimsiz degiskenler arasinda collinearity(ic iliski) problemi vardir deriz."),
                    p("Bu uc tanimlama yontemi de bize bu veride collinearity problemi oldugunu isaret etmektedir")),
            
            tabItem(tabName = "ridge",
                    h2("Ridge Regresyon"),
                    p("Ridge regresyon EKK optimizasyon yontemine bir kisit getirir. Bu kisit Beta katsayilarinin karelerinin toplaminin uzerinedir."),
                    p("Lambda buyudukce Beta parametreleri 0 a dogru yaklasir.Parametrelerin buyumesi parametre tahminlerinin varyansini dusurur. Negatif etkisi modele yanlilik katmasidir."),
                    p("Var(Betasapka ridge)= Sigma^2 / (x'x + LambdaI) burada Lambda yi buyuk tutarsak varyans kuculur ayni zamanda yanlilik artar. Ikisi arasinda denge kuracak sekilde Lambda belirlenmelidir. Multicollinearity problemini halledebilecek en kucuk Lambda yi belirlemeliyiz."),
                    p("EKK tahmin edicisi yansiz bir tahmin edici iken Ridge Regresyonunun tahmin edicisi yanli bir tahmin ediciye donusur.Ic iliski durumlarinda varyanslar buyukken Ridge Regresyonunda varyanslar daha kucuktur"),
                    p("Ridge Regresyon da aciklayici degiskenlerin tamamini modele dahil eder ve kompleks model olusmasini saglar.
                      (Modele degisken ekledikce hata duser fakat kompleks hale de gelebilir.)"),
                    p("lambda<- 10^seq(-15, 9, length.out = 200)"),
                    p("Ridge'de lambda parametresinin secimi icin en iyi yontem Cross Validationdur.Bu sebeple lambdalar icin oncelikle bir dizi olusturduk"),
                    box(verbatimTextOutput("x1"), width = 12),
                    p("Train verimizi matrix haline donusturuyoruz (yanit degiskenini (Total.Turnover) cikardik)"),
                    p("Lambdanin farkli degerleri icin degiskenlerin aldigi farkli degerlerin grafigini cizdirelim Ridge Regresyonda alpha = 0 degerini alir."),
                    box(plotOutput("ridgemodel"), width = 12),
                    p("Bu grafik lambdanin farkli degerleri icin degiskenlerin aldigi farkli degerlerin grafigidir."),
                    p("Grafikte  cizgilere baktigimizda cok buyuk degisim gosteren stabil olmayan degiskenler vardir.Bu degiskenin degeri digerlerine gore degisim gosteriyor. Bu multicollinearity nin bir etkisidir."),
                    p("Grafigimizin ust bolumunde gozuken 9 degerleri Ridge Regresyonun hicbir bagimsiz degiskeni atmayip tamamini kullanmasindandir"),
                    p("Grafikte gorulen her bir cizgi bir degiskene karsilik gelmektedir"),
                    p("Bu grafik ile model katsayilarinin lambdaya gore nasil degistigini gosterdik. Simdi Cross Validation Yontemi ile optimal lambdayi belirleyelim."),
                    p("Cross Validationda 9 fold ile model kurulur 10 uncu fold da bu modelin performansi incelenir MSE'ye bakilir.Herbir lambda degeri icin Cross Validation yapilir.Tum bulunan MSE ortalamalari alinir ve minimum RMSE degerini veren lambda degeri secilir."),
                    box(plotOutput("ridgemodel1"), width = 12),
                    p("Grafikte gozuken kirmizi noktalar her lambda degeri icin 10 folddan gelen MSE'lerin otalamasidir."),
                    p("Gra???kteki ilk dogru minimum MSE degerini veren lambda degerinin logaritmasini, ikinci dogru ise foldlardan elde edilen MSE degerlerinin standart sapmasinin 1 oldugu lambda degerinin logaritmasini gostermektedir."),
                    p("Grafigimizin ust bolumunde gozuken 9 degerleri Ridge Regresyonunun tum degiskenleri kullanmasidir."),
                    p("Simdi optimal lambda degerini kullanarak Ridge Regresyon modelimizi kuralim ve bu modelin test verisi uzerinde RMSE ve R kare degerlerini hesaplayalim."),
                    p("Performans kiyaslamasi her zaman test veri seti uzerinden yapilir.Cunku multicollinearity nin train e bir etkisi yoktur ama teste etkisi vardir."),
                    box(verbatimTextOutput("optimumlambda"), width = 12),
                    box(verbatimTextOutput("lambda_1SE"), width = 12),
                    p("Grafigimizde cikan cizgilerimizin yerine bakarsak ;"),
                    p("Ilk Dogru: log(Lambdamin)=log(14992.68)= 9.615317"),
                    p("Ikinci Dogru : log(Lambda1SE)=log(1683180)= 14.3362"),
                    p("Ridge Regresyon modeli;"),
                    p("ridgemodel1<-glmnet(x,veritrain$Total.Turnover,alpha=0,lambda=optimumlambda)"),
                    p("RMSE ve R kare hesaplayan fonksiyonlar;"),
                    p("rmse<-function(true, predicted,n) {sqrt(sum((predicted - true)^2)/n)}"),
                    p("ypredictedridge <- predict(ridgemodel, s = optimumlambda, newx = as.matrix(veritest[,-1]))# kurulan model uzerinden elde edilen tahmin degerleri"),
                    p("rsquare <- function(true, predicted) { 
                      sse <- sum((predicted - true)^2) 
                      sst <- sum((true - mean(true))^2) 
                      rsq <- 1 - sse / sst 
                      rsq }),"),
                    p("Test verisi uzerinden Ridge modelinin R karesini hesaplatalim;"),
                    box(verbatimTextOutput("ridgerkare"), width = 12),
                    p("Ridge Regresyon icin test verisi uzerinden R kare  0.8349589 cikmistir."),
                    p("Test verisi uzerinden Ridge modelinin RMSE sini hesaplatalim;"),
                    box(verbatimTextOutput("ridgermse"), width = 12),
                    p("Ridge Regresyon icin test verisi uzerinden RMSE 1269120 cikmistir."),
                    p("AIC ve BIC performans degerlendirme kriterleridir. Modeller arasinda kiyas yaparken kullanilir. Genel olarak AIC ve BIC degerleri daha kucuk olan model diger modellere gore daha iyidir. AIC ve BICde artik degerler kullanilir."),
                    p("Test verisi icin artiklar;"),
                    box(verbatimTextOutput("ridgeartik"), width = 12),
                    h2("AIC"),
                    box(verbatimTextOutput("ridgeaic"), width = 12),
                    p("Ridge Regresyon icin AIC degeri  48222.39 cikmistir."),
                    h2("BIC"),
                    box(verbatimTextOutput("ridgebic"), width = 12),
                    p("Ridge regresyon icin BIC degeri 48227.8 cikmistir.")),
            
            tabItem(tabName = "lasso",
                    h2("Lasso Regresyon"),
                    p("Degisken secimi icin kullanilir. Lassoda katsayilarin bazilari ridgeden farkli olarak sifirlanmaktadir."),
                    p("Lasso multicollinearity problemini cozerken ayni zamanda degisken secimi de yapabilme yetenegine sahiptir.Lassoda da Lambda ceza parametresi vardir. Lambda buyudukce modelden atilan (disarida birakilan) degisken sayimiz artiyor."),
                    p("Bazen Lasso cok fazla degiskeni disarida birakmaktadir. Bu modelin tahmin performansini dusurur. Modelde gormek istedigimiz degiskeni goremeyebiliriz."),
                    p("Ilk olarak Cross Validation ile Optimal Lambda degerini belirleyelim.Lasso Regresyonunda alpha = 1 degerini alir."),
                    box(plotOutput("lassomodel"), width = 12),
                    p("Grafigimizin ust bolumunde gozuken degerler Lasso Regresyonunun bagimsiz degiskenlerinin tamamini kullanmayarak modelden atmasindan dolayi kaynaklanir."),
                    p("Grafikte gozuken kirmizi noktalar her lambda degeri icin 10 folddan gelen MSE'lerin ortalamasidir."),
                    p("Grafikteki ilk dogru minimum MSE degerini veren lambda degerinin logaritmasini, ikinci dogru ise foldlardan elde edilen MSE degerlerinin standart sapmasinin 1 oldugu lambda degerinin logaritmasini gostermektedir."),
                    p("Optimal Lambda degeri;"),
                    box(verbatimTextOutput("optimallambda"), width = 12),
                    box(verbatimTextOutput("lambda_1SE1"), width = 12),
                    p("Grafigimizde cikan cizgilerimizin yerine bakarsak ;"),
                    p("Ilk Dogru: log(Lambdamin)=log(1231.551)= 7.11603"),
                    p("Ikinci Dogru : log(Lambda1SE)=log(240940.4)= 12.3923"),
                    p("Optimal Lambda degerini kullanarak Lasso Regresyon modelimizi kuralim ve bu modelin test verisi uzerinde RMSE ve R kare degerlerini hesaplayalim."),
                    p("Lasso Regresyon modeli;"),
                    box(verbatimTextOutput("lassomodel1"), width = 12),
                    p("9 degiskenli bir modeldi.Lasso Regresyonu WAP degiskenini atarak 8 degisken ile calismistir.Bu degiskene iliskin katsayilar sifirlanmistir."),
                    p("Simdi test verisi uzerinden Lasso Regresyon modelimizin performansina bakalim."),
                    p("Kurulan model uzerinden elde edilen tahmin degerleri;"),
                    box(verbatimTextOutput("ypredictedlasso"), width = 12),
                    p("Test verisi icin Lasso Regresyon modelinin R karesi;"),
                    box(verbatimTextOutput("lassorkare"), width = 12),
                    p("Test verisi icin Lasso Regresyon Modelinin R karesi 0.8356641 cikmistir."),
                    p("Test verisi icin Lasso Regresyon Modelinin RMSE si;"),
                    box(verbatimTextOutput("lassormse"), width = 12),
                    p("Test verisi icin Lasso Regresyon Modelinin RMSE si 1266406 cikmistir."),
                    p("Lasso Regresyon Modeli icin artiklar;"),
                    box(verbatimTextOutput("lassoartik"), width = 12),
                    p("Lassonun artiklari uzerinden AIC;"),
                    box(verbatimTextOutput("lassoaic"), width = 12),
                    p("Lasso Regresyon icin AIC degeri 48215.28 cikmistir."),
                    p("Lassonun artiklari uzerinden BIC;"),
                    box(verbatimTextOutput("lassobic"), width = 12),
                    p("Lasso regresyon icin BIC degeri 48220.69 cikmistir.")),
            
            tabItem(tabName = "elastic",
                    h2("Elastic Net Regresyon"),
                    p("Elatic net Ridge Regresyon Modeli ile Lasso Regresyon Modelinin bir kombinasyonudur.Ridge tarzi cezalandirma ve Lasso tarzi degisken secimi yapar. Lasso ve Ridge'de bulunan  lambda parametresi haricinde ikinci bir parametre olan alpha parametresi de vardir. Ozellikle yuksek korelasyonlu degisken gruplari oldugunda onerilir."),
                    p("Ilk olarak Cross Validation ile Optimal Lamda degerini belirleyelim.Lambda= 0.5 alindiginda Elastic Net Regresyon Modeli elde edilir."),
                    box(plotOutput("elasticmodel"), width = 12),
                    p("Grafigimizin ust bolumunde gozuken degerler Elastic Net Regresyonunun bagimsiz degiskenlerinin tamamini kullanmayarak modelden atmasindan dolayi kaynaklanir."),
                    p("Grafikte gozuken kirmizi noktalar her lambda degeri icin 10 folddan gelen MSE'lerin ortalamasidir."),
                    p("Gra???kteki ilk dogru minimum MSE degerini veren lambda degerinin logaritmasini, ikinci dogru ise foldlardan elde edilen MSE degerlerinin standart sapmasinin 1 oldugu lambda degerinin logaritmasini gostermektedir."),
                    p("Optimal Lambda degeri;"),
                    box(verbatimTextOutput("optlambda"), width = 12),
                    box(verbatimTextOutput("lambda_1SE2"), width = 12),
                    p("Grafigimizde cikan cizgilerimizin yerine bakarsak ;"),
                    p("Ilk Dogru: log(Lambdamin)=log(2833.096)= 7.949125"),
                    p("Ikinci Dogru : log(Lambda1SE)=log(419870.7)= 12.9477"),
                    p("Simdi Optimal Lambda degerini kullanarak Elastic Net Regresyon Modelimizi kuralim ve bu modelin test verisi uzerinde RMSE ve R kare degerlerini hesaplayalim."),
                    box(verbatimTextOutput("elasticmodel1"), width = 12),
                    p("9 degiskenli modelimizde Elastic Net Regresyonu WAP degiskenini atarak 8 degisken ile calismistir.Bu degiskene iliskin katsayilar sifirlanmistir."),
                    p("Simdi test verisi uzerinden Elastic Net Regresyon Modelimizin performansina bakalim."),
                    p("Kurulan model uzerinden elde edilen tahmin degerleri;"),
                    box(verbatimTextOutput("ypredictedelasticnet"), width = 12),
                    p("Test verisi icin Elastic Net Regresyon Modelinin R karesi;"),
                    box(verbatimTextOutput("elasticrkare"), width = 12),
                    p("Test verisi icin Elastic Net Regresyon Modelinin R karesi  0.8357445 cikmistir."),
                    p("Test verisi icin Elastic Net Regresyon Modelinin RMSE si;"),
                    box(verbatimTextOutput("elasticrmse"), width = 12),
                    p("Test verisi icin Elastic Net Regresyon Modelinin RMSE si 1266096 cikmistir."),
                    p("Elastic Net Regresyon Modeli icin artiklar;"),
                    box(verbatimTextOutput("elasticartik"), width = 12),
                    p("Elastic Net in artiklari uzerinden AIC;"),
                    box(verbatimTextOutput("elasticaic"), width = 12),
                    p("Elastic Net Regresyon icin AIC degeri 48214.47 cikmistir."),
                    p("Elastic Net in artiklari uzerinden BIC;"),
                    box(verbatimTextOutput("elasticbic"), width = 12),
                    p("Elastic Net Regresyon icin BIC degeri 48219.88 cikmistir."),
                    p("Simdi kurulan modellerde AIC, BIC, RMSE ve R kare degerlerini karsilastirip model secimi yapalim."),
                    box(verbatimTextOutput("tablo"), width = 12),
                    p("Secim yaparken AIC BIC ve RMSE degerleri kucuk , R kare degeri buyuk olan model secilmelidir."),
                    p("Modellerin R2 degerlerini, RMSE degerlerini, AIC, BIC degerlerini karsilastirdigimizda;"),
                    p("En kucuk rmse degeri Elasticnet Regresyon Modelinde(1266096),"), 
                    p("En kucuk AIC degeri Elasticnet Regresyon Modelinde(48214.47),"),
                    p("En kucuk BIC degeri Elasticnet Regresyon Modelinde(48219.88),"),
                    p("En buyuk R kare degeri Elasticnet Regresyon Modelinde(0.8357445) cikmistir."),
                    p("Bu sebeple calisabilecegimiz en iyi regresyon modeli Elastic Net Regresyon Modelidir. Bu veriyi modellemede Elastic Net Regresyon Modeli daha uygundur."),
                    p("Multicollinearity nin cozumunde Ridge,Lasso,Elastic Net haricinde Temel Bilesenler regresyonu da kullanilir. Simdi multicollinearity probleminden Temel Bilesenler Yolu ile kurtulmayi deneyelim.")),
            
            tabItem(tabName = "pcr",
                    h2("Temel Bilesenler Regresyonu(PCR)"),
                    p("Veri setindeki tum degiskenler vektoru ifade eder. Degiskenler arasinda iliski olmadiginda bu vektorler birbirlerine diktir.Temel bilesenler analizinin amaci, X matrisine bir donusum uygulayarak birbirlerine dik vektorlerden olusan bir sistem elde etmektir. Yuksek boyutlu verilerde dusuk boyutlu dogrusal yapi elde etmek icin kullanilan bir yontemdir. Vektor boyutlari kisa olanlar goz ardi edilir."),
                    p("Oncelikle Train veri seti uzerinde kurdugumuz EKK Regresyon Modelini kullanarak, hem train hem de test veri seti uzerinde RMSE degerlerini hesaplayalim."),
                    p("EKK Regresyon Modelimiz;"),
                    box(verbatimTextOutput("lmod2"), width = 12),
                    p("Kurulan regresyon modelinin anlamliligina baktigimizda p value degerimiz yaklasik=0 cikmistir.P-value degerimiz 0.05'ten kucuk oldugundan H0 red edilir yani kurulan model anlamlidir deriz."),
                    p("Simdi train ve test veri seti uzerinde RMSE degerlerini hesaplayalim"),
                    box(verbatimTextOutput("rmse1"), width = 12),
                    p("Train veri seti uzerinden kurulan regresyon modelinin RMSE degeri 1191567 dir."),
                    box(verbatimTextOutput("rmse2"), width = 12),
                    p("Test veri seti uzerinden kurulan regresyon modelinin RMSE degeri 1268034 dir."),
                    p("Iki rmse degeri arasinda fark olmasinin sebebi multicollinearity nin varliginin gostergesidir."),
                    box(plotOutput("ucluplot"), width = 12),
                    p("Bazi aciklayici degiskenler arasinda sacinim grafigi cizdirdik. Bunlar arasinda lineer iliski goruluyor. Bu da multicollinearity'nin varliginin bir gostergesidir."),
                    p("Simdi Temel Bilesenler Regresyonunu yapalim."),
                    p("Simdi tum componentleri kullanarak bir Temel Bilesenler Regresyon Modeli kuralim."),
                    box(verbatimTextOutput("pcrmodel"), width = 12),
                    p("Ciktinin ilk satiri componentlerin X matrisindeki aciklayicilik oranlarini, ikinci satir ise yanit degiskenindeki aciklayicilik miktarlarini gostermektedir."),
                    p("Verimizde 9 adet vektor vardi.Dokuzuncu component ile yanittaki degiskenligin yaklasik %83.15 i aciklanabilmektedir."),
                    p("Train ne kadar cok componentle calisirsa o kadar iyi sonuc alinir. Cunku multicollinearity nin train uzerinde etkisi yoktur fakat test verisi uzerinde etkisi vardir."),
                    box(plotOutput("validationplot"), width = 12),
                    p("Bu grafik bize her componente karsilik gelen RMSE degerlerini gosterir. Train veri seti uzerinden cizilen grafikte component sayisi arttikca RMSE degerleri dusmektedir."),
                    p("En buyuk degisim intercepten birinci componente geciste yasanmistir.Ucuncu componentten sonra bir degisim olmamistir.Toplamda intercept ile birlikte 9 component var."),
                    p("Bu grafige iliskin RMSE degerleri;"),
                    box(verbatimTextOutput("pcrmse"), width = 12),
                    p("RMSE degeri component sayisi arttikca azalmaktadir. En dusuk RMSE degeri ilk olarak birinci componentte gorulmektedir."),
                    p("Simdi RMSE icin yaptiklarimizi R kare icinde yapalim."),
                    box(plotOutput("validationplot1"), width = 12),
                    p("Bu grafik bize her componente karsilik gelen R kare degerlerini gosterir. Train veri seti uzerinden cizilen grafikte component sayisi arttikca R kare degerleri artmalidir."),
                    p("En buyuk degisim sifirinci intercepten birinci componente geciste yasanmistir.Ucuncu componentten sonra bir degisim olmamistir.Toplamda intercept ile birlikte 9 component vardir."),
                    p("Multicollinearity icin train veri setine bakmak uygun olmadigindan test veri setine bakmak daha uygun olacaktir."),
                    p("Test seti uzerinde component sayilarina gore RMSE degerlerini de RMSEP fonksiyonunu kullanarak bulabiliriz."),
                    box(verbatimTextOutput("pcrmse1"), width = 12),
                    p("Test veri seti uzerinden baktigimizda performans acisindan en kucuk RMSE degeri 1266618 ile besinci componenttedir."),
                    p("Simdi 5 component ile kurulan modelin katsayilarina bakalim."),
                    box(verbatimTextOutput("coef1"), width = 12),
                    p("Bu cikti bize pcr 5 component icin modelin yanit degiskeni tahminlerini verir."),
                    p("Simdi bu modeli kullanarak train ve test veri seti uzerinden RMSE degerlerini hesaplayalim."),
                    box(verbatimTextOutput("rmse3"), width = 12),
                    p("Train veri seti uzerinden RMSE degeri 1195298 dir."),
                    box(verbatimTextOutput("rmse4"), width = 12),
                    p("Test veri seti uzerinden RMSE degeri 1266618 dir"),
                    p("Tahmin performansimiz hala cok iyi degildir ama parametre tahminlerimiz daha stabil cikmistir.Optimal component sayisini belirlerken test verisi uzerindeki performansa baktik. Bu is icin Cross Validation yonteminin kullanilmasi aslinda daha dogru bir yaklasim olacaktir."),
                    h2("CROSS-VALIDATION ILE TEMEL BILESEN ANALIZI"),
                    p("Kac component olmasi gerektigini daha iyi verecek yontemdir Diger yontemlerde her yapista farkliliklar olmaktadir."),
                    p("Train veri seti uzerinden Cross Validation hesaplatalim;"),
                    box(verbatimTextOutput("pcrCV"), width = 12),
                    p("RMSE degeri en dusuk besinci componentte(1204988) cikmistir."),
                    box(plotOutput("plotCV"), width = 12),
                    p("Cross Validation icin componentlere karsilik RMSE degerlerinin grafigi yukarida gosterilmektedir"),
                    p("pcrcv icin en kucuk RMSE degerini alan componentin hangisi olduguna bakmak icin;"),
                    box(verbatimTextOutput("min1"), width = 12),
                    p("Grafikte Cross Validated RMSE degerleri vardir . 10 fold cross validation ile pcrCV degerlerinin altincisi yani besinci componente karsilik gelen RMSE degeri minimum cikti."),
                    box(verbatimTextOutput("coefCV"), width = 12),
                    p("Bu cikti bize pcrCV 5 component icin modelin yanit degiskeni tahminlerini verir.Yani tahmin performansimiz besinci componentte en iyi olmaktadir."),
                    p("Datayi golddata alarak modelimizi kuralim."),
                    p("model1 <- lm(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty,data=golddata)"),
                    box(verbatimTextOutput("model1"), width = 12),
                    p("Regresyon modelimizin ciktisina baktigimizda p value degerimiz yaklasik 0 cikmistir.P-value degerimiz 0.05'ten kucuk oldugu icin kurulan modelimiz anlamlidir."),
                    p("Tahmin degerlerini gosterelim"),
                    box(verbatimTextOutput("fit"), width = 12),
                    p("Artik degerlerini gosterelim"),
                    box(verbatimTextOutput("resid"), width = 12)),
                  
            
            tabItem(tabName = "outlier",
                    h2("Outlier")),
            
            tabItem(tabName= "normallik",
                   h2("HATA ILE ILGILI VARSAYIMLAR"),
                   h2("NORMALLIK VARSAYIMININ KONTROLU TESTI"),
                   p("Tum normallik testlerini (Shapiro-Wilk,Kolmogorov-Smirnov,Cramer-von Mises,Anderson-Darling) bir arada gormek icin asagidaki kodu kullanmaliyiz;"),
                   box(verbatimTextOutput("normalliksinamasi"),width = 12),
                   p("Normallik testlerimizin p-valuelerine baktigimizda 0.05'ten kucuk oldugunu goruyoruz yani artiklarimizin dagilimi normal degildir deriz."),
                   box(plotOutput("PlotNormallik"), width = 12),
                   p("Histogram grafigimize baktigimizda kirmizi cizgi bize normal dagilim egrisini verir.Mavi olan cizgi ise sinif orta noktalarindan gecen diyagramdir.Mavi ve kirmizi cizgilerimiz birbirine benzemedigi icin verinin normal dagilmadigini soyleyebiliriz."),
                   p("Q-Q Plot grafigimize baktigimizda verimiz Q-Q cizgisi uzerinde olmadigi icin normal dagilim varsayiminin saglanmadigi gorulmektedir."),
                   p("Yogunluk Grafigimize baktigimizda sag kuyruk daha uzun oldugu icin grafigimiz sola carpiktir deriz."),
                   h2("SABIT VARYANSLIK"),
                   p("Sabit varyansliligin en kullanisli teshis yontemi artiklara  karsilik tahmin  degerlerinin  plotlanmasidir."),
                   box(plotOutput("ggplot"), width = 12),
                   p("Cizdirdigimiz grafigimiz bize duzgun bir sekil vermedigi icin sabit varyansli olup olmadigina emin olamiyoruz.Bu yuzden degisken varyanslilik testlerine bakmaliyiz."),
                   h2("DEGISKEN VARYANSLILIK TESTLERI"),
                   h2("BREUSCH-PAGAN TESTI"),
                   p("H0:Heterosce Dosticity (Degisken Varyanslilik) problemi yok."),
                   p("H1:Heterosce Dosticity (Degisken Varyanslilik) problemi vardir."),
                   box(verbatimTextOutput("bp1"),width = 12),
                   p("BREUSCH-PAGAN Testimizin sonucuna gore p-value degerimiz yaklasik 0 cikmistir.P-Value degerimiz 0.05'ten kucuk oldugu icin H0 hipotezi reddedilir yani Heteroscedosticity (degisken varyanslilik) problemi vardir deriz."),
                   h2("ILISKILI HATALAR (OTOKORELASYON)"),
                   p("H0: Otokorelasyon yoktur."),
                   p("H1: Otokorelasyon vardir"),
                   p("dwtest(Total.Turnover~Open+High+Low+Close+ WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+ X..Deli..Qty.to.Traded.Qty ,data=golddata))"),
                   box(verbatimTextOutput("dw"),width = 12),
                   p("Sonucumuza gore p-value degerimiz yaklasik 0 cikmistir.P-value degerimiz 0.05'ten kucuk oldugu icin HO reddedilir.Bu nedenle Otokorelasyon vardir deriz."),
                   h2("OLAGAN DISI GOZLEMLER (AYKIRI GOZLEMLERIN BELIRLENMESI)"),
                   h2("OUTLIER;"),
                   p("Model tarafindan iyi tahmin edilemeyen gozlemlere denir.(Hatalara buyuk olan gozlemlere denir.)"),
                   p("Simdi kurulan regresyon modelimizin grafiklerini inceleyelim;"),
                   p("Oncelikle modelimizdeki supheli outlier icin varsayimlarimiza bakalim;"),
                   box(plotOutput("plotm1"),width = 12),
                   p("Cizdirdigimiz grafikte kirmizi olan gozlemler supheli outlier degerlerimizdir.Bu grafik sadece varsayim yapmaktadir.Bu sebeple oncelikle library(faraway) ile plot cizdirerek modelimizdeki outlier suphesi olan gozlemlere bakalim."),
                   box(plotOutput("plotm"),width = 12),
                   p("1.Grafik icin Artiklara karsi cizdirdigimiz grafige baktigimizda varyanslarin homojen olmadigi gorulmektedir.Outlier suphesi olan gozlemlerimiz 1454,1458,1461.gozlem cikmistir."),
                   p("2.Grafik icin Normal Q-Q Plot Grafigimize baktigimizda verimiz Q-Q Plot cizgisi uzerinde olmadigindan normal dagilim varsayiminin saglanmadigi gorulmektedir.Outlier suphesi olan gozlemlerimiz 1454,1458,1461.gozlem cikmistir."),
                   p("3.Grafik icin Standartlastirilmis artiklarin karekokune karsi cizdirdigimiz grafige baktigimizda varyanslarin homojen olmadigi gorulmektedir.Outlier suphesi olan gozlemlerimiz 1454,1458,1461.gozlem cikmistir."),
                   p("4.Grafik icin Cooks Distance'a gore grafikte cikan degerler : 1413,1419,1423.gozlemlere outlier suphesi ile yaklasilir"),
                   p("Bu grafiklere baktigimizda 1413,1419,1423,1454,1458,1461. gozlemler muhtemelen sorunlu olarak tanimlayabiliriz."),
                   p("Verimizde bu gozlemlerin hangi durumlari temsil ettigini gormek icin bu gozlemlere bakmaliyiz;"),
                   box(verbatimTextOutput("gozlem"),width = 12),
                   p("Modelimiz icin standartlastirilmis artiklarimizi bakacak olursak;"),
                   box(verbatimTextOutput("stud"),width = 12),
                   p("Simdi standartlastirilmis artiklarimizin mutlakca en buyugune bakmaliyiz."),
                   box(verbatimTextOutput("stud1"),width = 12),
                   p("En buyuk standartlastirilmis artik degerimiz 1458. gozlem olup degeri mutlakca 11.59116 dir.Fakat aykiri gozlem midir bakalim"),
                   p("Benferonni duzelltmesi yaparsak;Simdi cut point belirlemeliyiz."),
                   box(verbatimTextOutput("qt"),width = 12),
                   p("Burada en buyuk standartlastirilmis mutlak artik degerini 11.59116 olarak bulmustuk.|11.59116| >|-4.184228| oldugundan  1458. gozlem bizim icin outlierdir."),
                   p("Bunu hazir kod ile yaptigimizda;"),
                   box(verbatimTextOutput("outlierTest"),width = 12),
                   p("En buyuk aykiri deger 1458 gozleme ait olmakla beraber diger outlier degerler ise 1461,1454,1464,1400,1419,1659,1444,1428,1425. degerlere aittir. Veri setimizde aykiri gozlemler mevcuttur."),
                   p("Varsayimlar gerceklesmediginde ve aykiri gozlemler oldugunda ROBUST REGRESYON yontemi kullanilabilir.")),
                   
            tabItem(tabName = "robust",
                    h2("Robust Regresyon"),
                    p("Tum regresyon varsayimlari gecerli oldugunda , dogrusal regresyon icin normal EKK tahminleri en uygunudur.Bu varsayimlardan bazilari gecersiz oldugunda , EKK regresyonu kotu performans gosterebilir"),
                    p("Aykiri gozlemler regresyon dogrusunu kendine gore kendine dogru ceker.Bu sekilde parametre tahminlerini olmasi gerektigi yerden cok uzaga tasiyabilir.Model uzerinde etkileri diger gozlemlerden daha fazla olur.Bu durum bizim model tahmin performansimizi dusurur.Bu durumda robust regresyon kullanilir."),
                    p("Bu gozlemlerin model uzerinde etkisini dusunerek agirliklandirma yapiyoruz."),
                    p("Birden cok aykiri gozlem varsa modele bundan etkilenebiliyor(maskeleme-swamping).Bu sekilde kurulan modelde yanlis olmus oluyor ve bu modelin artiklari uzerinden yorum yapmak da cok dogru olmuyor."),
                    p("Aykiri gozlem calismasi yapilacaksa Robust Regresyon Modeli kurup bu regresyon modelinin artiklari uzerinden konusmak daha dogru olacaktir."),
                    p("Robust Regresyon iteratif yeniden agirlikli En Kucuk Kareler (IRLS) ile yapilir.Robust Regresyon calistirma komutu MASS paketinde rlm'dir.IRLS icin kullanilabilecek cesitli agirlik fonksiyonlari vardir."),
                    p("Siradan EKK regresyonu ve robust regresyonun sonuclarini karsilastirirken sonuclar cok farkliysa Robust Regresyondan gelen sonuclar kullanilir.Buyuk farkliliklar, model parametrelerinin aykiri degerlerden buyuk oranda etkilendigini gostermektedir.Farkli agirliklandirmalarin avantajlari ve dezavantajlari vardir.Huber agirliklari siddetli aykiri degerlerde zorluklar yasayabilir ve bisquare agirliklar yakinsamada zorluk yasayabilir veya birden fazla cozum verebilir."),
                    p("Oncelikle Robust Regresyon modelimizi kuralim;"),
                    p("hubermod <- rlm(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty ,data=golddata)"),
                    box(verbatimTextOutput("hubermod"),width = 12),
                    p("Huber agirliklari kullanarak kurdugumuz Robust Regresyon Modelimizin Residual Standard Error u 384400 cikmistir."),
                    p("EKK modelimizde Residual Standard Error'umuz 1204000 cikmisti."),
                    p("Iki regresyon modelinin katsayilarini yan yana gosterelim;"),
                    box(verbatimTextOutput("hubermod1"),width = 12),
                    p("Iki modelin ciktilari arasinda gozle gorulur degisimler vardir."),
                    p("Simdi Robust Regresyon icin standart artiklari inceleyelim;"),
                    box(plotOutput("halfnorm"),width = 12),
                    p("Robust Regresyonumuzun ham artiklari 1454,1458,1461,1464. gozlemler olarak cikmistir."),
                    box(verbatimTextOutput("studhubermod"),width = 12),
                    p("1458.gozlem en buyuk standartlastirilmis artik degeridir. Fakat aykiri gozlem midir bakalim"),
                    p("Bonferonni duzeltmesi yaparsak;"),
                    box(verbatimTextOutput("qt1"),width = 12),
                    p("1458.gozlem ( |11.94331 | > |-4.184228| ) bonferonni duzeltmesine gore outlierdir."),
                    p("Diger outlier degerler;"),
                    box(verbatimTextOutput("huberoutlier"),width = 12),
                    p("En buyuk outlier deger 1458.gozleme ait olmakla birlikte diger outlier degerler ise 1461,1454,1464,1400,1419,1423,1413,1444,1425 degerlere aittir.Swamping Outlier yuzunden aslinda outlier olmayan bir gozlemin outliermis gibi gozukmesidir.1659,1428.gozlemlerim swamping olmustur.Masking Bir outlierin baska bir outlieri maskelemesidir.1413 ve 1423.gozlemlerim masking olmustur."),
                    p("Simdi de bisquare agirliklandirmasini kullanarak regresyon modelimizi kuralim;"),
                    p("bisquaremod <- rlm(Total.Turnover~Open+High+Low+Close+WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+X..Deli..Qty.to.Traded.Qty ,data=golddata,psi=psi.bisquare)"),
                    box(verbatimTextOutput("bisquaremod"),width = 12),
                    p("Bisquare agirliklari kullanarak kurdugumuz Robust Regresyon Modelimizin Residual Standard Error u 181600 cikmistir"),
                    p("Huber agirliklari kullanarak kurdugumuz Robust Regresyon Modelimizin Residual Standard Error u 384400 cikmisti.EKK modelimizde Residual Standard Error'umuz 1204000 cikmisti."),
                    p("Bisquare agirliklara kullanarak kurdugumuz Robust Regresyon Modelimizin Residual Standard Erroru daha iyi cikmistir."),
                    p("Simdi tekrardan agirliklara bakalim;"),
                    box(verbatimTextOutput("biweights2"),width = 12),
                    p("Ilk sutun Robust Regresyon Modelinin artiklarini gosterir."),
                    p("Resid ler  ne kadar buyukse ona karsilik gelen weight degeri de o kadar kucuk olmaktadir."),
                    p("Iki modelin residual standart errorlerine bakildigi zaman Bisquare yontemi daha kucuk residual standart error degerine sahiptir."))
            
            
            
        )))



server<- function(input, output){
  
    
    output$degiskenvaryanslilik<- renderPrint(bptest(lmod,data=veritrain))
    
    output$degiskenvaryanslilik1<- renderPrint(summary(wmod))
    
    output$pairs1<-renderPlot(pairs(~residuals(lmod)+wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg+predict(lmod) ,data=veritrain, main="Temel Dagilim Grafigi Matrisi"))
    
    output$weightedleastsquaremod1<-renderPrint(summary(weightedleastsquaremod1))
    
    output$pairs2<-renderPlot(pairs(residuals(lmod)~predict(lmod),data=veritrain, main="Temel Dagilim Grafigi Matrisi"))
    
    output$weightedleastsquaremod2<-renderPrint(summary(weightedleastsquaremod2))
    
    output$pairs3<-renderPlot(pairs(kareresid~wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg,data=veritrain, main="Temel Dagilim Grafigi Matrisi"))
    
    output$weightedleastsquaremod3<-renderPrint(summary(weightedleastsquaremod3))
    
    output$pairs4<-renderPlot(pairs(kareresid~predict(lmod), main="Temel Dagilim Grafigi Matrisi"))
    
    output$kontrol<-renderPlot({par(mfrow=c(1,2))
                               plot(predict(lmod),residuals(lmod))
                               plot(predict(weightedleastsquaremod2),residuals(weightedleastsquaremod2))})
    
    output$out1<-renderPrint(cbind(Betawls,coef(weightedleastsquaremod2)))
    
    output$out2<-renderPrint(cbind(donart,residuals(weightedleastsquaremod2)))
    
    output$out3<-renderPrint(cbind(sqrt(W)%*%residuals(weightedleastsquaremod2)))
    
    output$out4<-renderPrint(sqrt(sum(residuals(weightedleastsquaremod2)^2)/160))
    
    output$out5<-renderPrint(sqrt(sum(donart^2)/160))
    
    output$bpt<-renderPrint(summary(bpmod))
      
    output$lmod1<-renderPrint(summary(lmod1))
    
    output$cor<-renderPrint(cor(veritrain1[,-c(1)]))
    
    output$cor1<-renderPlot({corrplot(cor(veritrain1[,-c(1)]),method = "pie", order="hclust")})
    
    output$matrix<-renderPrint(head(x,10))
    
    output$eigen<-renderPrint(e)
    
    output$kappa<-renderPrint(k)
    
    output$vif<-renderPrint(vif(lmod1))
    
    output$x1<-renderPrint(head(x1,10))
    
    output$ridgemodel<-renderPlot({plot(ridgemodel,xvar = "lambda")})
    
    output$ridgemodel1<-renderPlot({plot(cv.fitridge)})
    
    output$optimumlambda<-renderPrint(optimumlambda)
    
    output$lambda_1SE<-renderPrint(lambda_1SE)
    
    output$ridgerkare<-renderPrint(ridgerkare)
    
    output$ridgermse<-renderPrint(ridgermse)
    
    output$ridgeartik<-renderPrint(head(ridgeartik,10))
    
    output$ridgeaic<-renderPrint(ridgeaic)
    
    output$ridgebic<-renderPrint(ridgebic)
    
    output$lassomodel<-renderPlot(plot(cv.fitlasso))
    
    output$optimallambda<-renderPrint(optimallambda)
    
    output$lambda_1SE1<-renderPrint(lambda_1SE1)
    
    output$lassomodel1<-renderPrint(coef(lassomodel))
    
    output$ypredictedlasso<-renderPrint(head(ypredictedlasso,10))
    
    output$lassorkare<-renderPrint(lassorkare)
    
    output$lassormse<-renderPrint(lassormse)
    
    output$lassoartik<-renderPrint(head(lassoartik,10))
    
    output$lassoaic<-renderPrint(lassoaic)
    
    output$lassobic<-renderPrint(lassobic)
    
    output$elasticmodel<-renderPlot(plot(cv.fitelasticnet))
    
    output$optlambda<-renderPrint(optlambda)
    
    output$lambda_1SE2<-renderPrint(lambda_1SE2)
    
    output$elasticmodel1<-renderPrint(coef(elasticmodel))
    
    output$ypredictedelasticnet<-renderPrint(head(ypredictedelasticnet,10))
    
    output$elasticrkare<-renderPrint(elasticrkare)
    
    output$elasticrmse<-renderPrint(elasticrmse)
    
    output$elasticartik<-renderPrint(head(elasticartik,10))
    
    output$elasticaic<-renderPrint(elasticaic)
    
    output$elasticbic<-renderPrint(elasticbic)
    
    output$tablo<-renderPrint(tablo)
    
    output$lmod2<-renderPrint(summary(lmod2))
    
    output$rmse1<-renderPrint(rmse1(predict(lmod2), veritrain1$Total.Turnover))
    
    output$rmse2<-renderPrint(rmse1(predict(lmod2, veritest1), veritest1$Total.Turnover))
    
    output$ucluplot<-renderPlot({par(mfrow=c(1,3))
        plot(High~Open,veritrain1)
        plot(Low~High,veritrain1)
        plot(WAP~Close,veritrain1)})
    
    output$pcrmodel<-renderPrint(summary(pcrmodel))
    
    output$validationplot<-renderPlot(validationplot(pcrmodel,val.type = "RMSE",col="green"))
    
    output$pcrmse<-renderPrint(pcrmse)
    
    output$validationplot1<-renderPlot(validationplot(pcrmodel,val.type = "R2",col="orangered3"))
    
    output$pcrmse1<-renderPrint(pcrmse1)
    
    output$coef1<-renderPrint(coef(pcrmodel,ncomp=5))
    
    output$rmse3<-renderPrint(rmse1(predict(pcrmodel, ncomp=5), veritrain1$Total.Turnover))
    
    output$rmse4<-renderPrint(rmse1(predict(pcrmodel, veritest1, ncomp=5), veritest1$Total.Turnover))
    
    output$pcrCV<-renderPrint(pcrCV)
    
    output$plotCV<-renderPlot(plot(pcrCV,main="",col="red"))
    
    output$min1<-renderPrint(which.min(pcrCV$val))
    
    output$coefCV<-renderPrint(coef(pcrmodel1,ncomp=5))
    
    output$model1<-renderPrint(summary(model1))
    
    output$fit<-renderPrint(head(fit,10))
    
    output$resid<-renderPrint(head(resid,10))
    
    output$normalliksinamasi<- renderPrint(ols_test_normality(model1))
    
    output$PlotNormallik<-renderPlot({par(mfrow=c(1,3)) 
                                     x2 <-residuals(model1)
                                     histogram <-hist(x2, breaks=10, density=10,col="darkgrey",xlab="Residuals", main="Histogram")
                                     abline(v=mean(x2), col="darkgreen", lwd=2)
                                     multiplier <- histogram$counts / histogram$density 
                                     mydensity <- density(x2) 
                                     mydensity$y <- mydensity$y * multiplier[1] 
                                     lines(mydensity,col="blue", lwd=2)
                                     xfit <- seq (min(x2), max(x2), length=40) 
                                     yfit <- dnorm(xfit, mean =mean(x2), sd = sd(x2))
                                     yfit <- yfit *diff(histogram$mids[1:2]) *length(x2) 
                                     lines(xfit, yfit, col="red", lwd=2)
                                     qqnorm(residuals(model1),ylab="residuals",main="QQPLOT",col="orange") 
                                     qqline(residuals(model1),col="darkblue")
                                     d <- density(x2) 
                                     plot(d,main = "Yogunluk Grafigi") 
                                     polygon(d, col="violet", border="blue")})
    
    output$ggplot<-renderPlot({ggplot(data=golddata,mapping=aes(x=fit,y=resid))+ 
                                geom_jitter(color="purple")+ 
                                geom_hline(yintercept=0,color="orange")+ggtitle("RESID & FITTED ")+xlab(" FITTED ")+ylab("RESID")})
    
    output$bp1<-renderPrint(bptest(model1,data=golddata))
    
    output$dw<-renderPrint(dwtest(Total.Turnover~Open+High+Low+Close+ WAP+No..of.Shares+No..of.Trades+Deliverable.Quantity+ X..Deli..Qty.to.Traded.Qty ,data=golddata))
    
    output$plotm1<-renderPlot(ols_plot_resid_stud_fit(model1))
    
    output$plotm<-renderPlot({par(mfrow=c(1,4))
                              plot(model1)})
    
    output$gozlem<-renderPrint(golddata[c(1413,1419,1423,1454,1458,1461), ])
    
    output$stud<-renderPrint(head(stud,10))
    
    output$stud1<-renderPrint(stud[which.max(abs(stud))])
    
    output$qt<-renderPrint(qt(0.05/(length(stud)*2) , (length(golddata$Total.Turnover)-10-1)))
    
    output$outlierTest<-renderPrint(outlierTest(model1))
    
    output$hubermod<-renderPrint(summary(hubermod))
    
    output$hubermod1<-renderPrint(cbind(coef(model1),coef(hubermod)))
    
    output$halfnorm<-renderPlot(halfnorm(residuals(hubermod),4,ylab = "hubermod residuals"))
    
    output$studhubermod<-renderPrint(stud1[which.max(abs(stud1))])
      
    output$qt1<-renderPrint(qt(.05/(length(stud)*2),length(golddata$Total.Turnover)-10-1))
    
    output$huberoutlier<-renderPrint(outlierTest(hubermod))
    
    output$bisquaremod<-renderPrint(summary(bisquaremod))
    
    output$biweights2<-renderPrint(biweights2[120:150,])
      
    output$summarymodel<- renderPrint({summary(lmod)})
    
    output$homojenvaryanslilik1<- renderPlot({plot(fitted(lmod),residuals(lmod), xlab = "fitted y" ,ylab = "residuals",col="purple",main="Artiklar-Tahmin Grafigi")
      abline(h=0,col="yellow")}
        
    )}

shinyApp(ui, server)