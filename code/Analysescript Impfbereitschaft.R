#Verwendete R-Codes für die Datenanalyse

#1. R-Pakete laden

library(car)
library(psych)
library(jtools)
library(olsrr)
library(moments)
library(lmtest)
library(lsr)

#2. Soziodemographie

#2.1 Alter
describe(Datensatz_Komplett$SD01_01)

#2.2 Politische Einstellung
describe(Datensatz_Komplett$SD02_01)

#2.3 Impfstatus
table(Datensatz_Komplett$IM01)

#2.4 Politische Einstellung nach Impfstatus
Geimpft <- subset(Datensatz_Komplett, IM01 == 1)
Ungeimpft <- subset(Datensatz_Komplett, IM01 == 2)
describe(Geimpft$SD02_01)
describe(Ungeimpft$SD02_01)


#3. Teildatensätze erstellen

impfeinstellung <- Datensatz_Komplett[,c("IM02_01", "IM02_02", "IM02_03", "IM02_04", "IM02_05", "IM02_06", "IM02_07", "IM02_08", "IM02_09")]
TV <- Datensatz_Komplett[,c("TV01_01", "TV01_02", "TV01_03", "TV01_04", "TV01_05", "TV01_06", "TV01_07", "TV01_08", "TV01_09")]
Demokratie <- Datensatz_Komplett[,c("DE01_01", "DE01_02", "DE01_03", "DE01_04", "DE01_05", "DE01_06", "DE01_07", "DE01_08", "DE01_09")]
Individualismus <- Datensatz_Komplett[,c("IN01_01", "IN01_02", "IN01_03", "IN01_04", "IN01_05", "IN01_06", "IN01_07", "IN01_08", "IN01_09", "IN01_10", "IN01_11", "IN01_12", "IN01_13", "IN01_14", "IN01_15")]


#4. Reliabilitäten

alpha(impfeinstellung)
alpha(TV)
alpha(Demokratie)
alpha(Individualismus)


#5. Itemschwierigkeiten

#5.1 Impfbereitschaft

impfneu <- impfeinstellung-1
round(mean((impfneu$IM02_01), na.rm = TRUE)/4*100, 0)
round(mean((impfneu$IM02_02), na.rm = TRUE)/4*100, 0)
round(mean((impfneu$IM02_03), na.rm = TRUE)/4*100, 0)
round(mean((impfneu$IM02_04), na.rm = TRUE)/4*100, 0)
round(mean((impfneu$IM02_05), na.rm = TRUE)/4*100, 0)
round(mean((impfneu$IM02_06), na.rm = TRUE)/4*100, 0)
round(mean((impfneu$IM02_07), na.rm = TRUE)/4*100, 0)
round(mean((impfneu$IM02_08), na.rm = TRUE)/4*100, 0)
round(mean((impfneu$IM02_09), na.rm = TRUE)/4*100, 0)


#5.2 Einstellung ggü. öffentlich-rechtlichen Medien

tvneu <- TV-1
round(mean((tvneu$TV01_01), na.rm = TRUE)/4*100, 0)
round(mean((tvneu$TV01_02), na.rm = TRUE)/4*100, 0)
round(mean((tvneu$TV01_03), na.rm = TRUE)/4*100, 0)
round(mean((tvneu$TV01_04), na.rm = TRUE)/4*100, 0)
round(mean((tvneu$TV01_05), na.rm = TRUE)/4*100, 0)
round(mean((tvneu$TV01_06), na.rm = TRUE)/4*100, 0)
round(mean((tvneu$TV01_07), na.rm = TRUE)/4*100, 0)
round(mean((tvneu$TV01_08), na.rm = TRUE)/4*100, 0)
round(mean((tvneu$TV01_09), na.rm = TRUE)/4*100, 0)


#5.3 Vertrauen in demokratische Prozesse

demokratieneu <- Demokratie-1
round(mean((demokratieneu$DE01_01), na.rm = TRUE)/4*100, 0)
round(mean((demokratieneu$DE01_02), na.rm = TRUE)/4*100, 0)
round(mean((demokratieneu$DE01_03), na.rm = TRUE)/4*100, 0)
round(mean((demokratieneu$DE01_04), na.rm = TRUE)/4*100, 0)
round(mean((demokratieneu$DE01_05), na.rm = TRUE)/4*100, 0)
round(mean((demokratieneu$DE01_06), na.rm = TRUE)/4*100, 0)
round(mean((demokratieneu$DE01_07), na.rm = TRUE)/4*100, 0)
round(mean((demokratieneu$DE01_08), na.rm = TRUE)/4*100, 0)
round(mean((demokratieneu$DE01_09), na.rm = TRUE)/4*100, 0)


#6. Trennschärfen

#6.1 Impfbereitschaft

round(cor(rowMeans(impfneu[,c(2:9)]), impfneu$IM02_01, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(impfneu[,c(1,3:9)]), impfneu$IM02_02, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(impfneu[,c(1:2,4:9)]), impfneu$IM02_03, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(impfneu[,c(1:3,5:9)]), impfneu$IM02_04, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(impfneu[,c(1:4,6:9)]), impfneu$IM02_05, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(impfneu[,c(1:5,7:9)]), impfneu$IM02_06, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(impfneu[,c(1:6,8:9)]), impfneu$IM02_07, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(impfneu[,c(1:7,9)]), impfneu$IM02_08, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(impfneu[,c(1:8)]), impfneu$IM02_09, use = "pairwise.complete.obs"),2)


#6.2 Einstellung ggü. öffentlich-rechtlichen Medien

round(cor(rowMeans(tvneu[,c(2:9)]), tvneu$TV01_01, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(tvneu [,c(1,3:9)]), tvneu$TV01_02, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(tvneu [,c(1:2,4:9)]), tvneu$TV01_03, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(tvneu [,c(1:3,5:9)]), tvneu$TV01_04, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(tvneu [,c(1:4,6:9)]), tvneu$TV01_05, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(tvneu [,c(1:5,7:9)]), tvneu$TV01_06, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(tvneu [,c(1:6,8:9)]), tvneu$TV01_07, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(tvneu [,c(1:7,9)]), tvneu$TV01_08, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(tvneu [,c(1:8)]), tvneu$TV01_09, use = "pairwise.complete.obs"),2)


#6.3 Vertrauen in demokratische Prozesse

round(cor(rowMeans(demokratieneu[,c(2:9)]), demokratieneu$DE01_01, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(demokratieneu [,c(1,3:9)]), demokratieneu$DE01_02, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(demokratieneu [,c(1:2,4:9)]), demokratieneu$DE01_03, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(demokratieneu [,c(1:3,5:9)]), demokratieneu$DE01_04, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(demokratieneu [,c(1:4,6:9)]), demokratieneu$DE01_05, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(demokratieneu [,c(1:5,7:9)]), demokratieneu$DE01_06, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(demokratieneu [,c(1:6,8:9)]), demokratieneu$DE01_07, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(demokratieneu [,c(1:7,9)]), demokratieneu$DE01_08, use = "pairwise.complete.obs"),2)
round(cor(rowMeans(demokratieneu [,c(1:8)]), demokratieneu$DE01_09, use = "pairwise.complete.obs"),2)


#7. Faktorenstruktur und Scree-Plot

#7.1 Impfbereitschaft 

impfmodell <- fa(impfeinstellung, nfactors = 1, rotate = "promax", fm = "pa")
plot(impfmodell$values, type = "b")

#7.2 Einstellung ggü. öffentlich-rechtlichen Medien
tvmodell <- fa(TV, nfactors = 1, rotate = "promax", fm = "pa")
plot(tvmodell$values, type = "b")

#7.3 Vertrauen in demokratische Prozesse 
demokratiemodell <- fa(Demokratie, nfactors = 1, rotate = "promax", fm = "pa")
plot(demokratiemodell$values, type = "b")


#8. Korrelationstests

#8.1 Korrelation zwischen Politischer Einstellung und Impfbereitschaft

#Impfbereitschaft als Mittelwert einer Person über die Items hinweg
Datensatz_Komplett$Impfen <- rowMeans(subset(Datensatz_Komplett, select = c(IM02_01, IM02_02, IM02_03, IM02_04, IM02_05, IM02_06, IM02_07, IM02_08, IM02_09)), na.rm = TRUE)

#Korrelationstest
cor.test(Datensatz_Komplett$Impfen, Datensatz_Komplett$SD02_01)

#quadratischen Term hinzunehmen
Datensatz_Komplett$politikquadriert <- Datensatz_Komplett$SD02_01^2
einfacheregression <- lm(Datensatz_Komplett$Impfen~Datensatz_Komplett$SD02_01)
quadratischeregression <- lm(Datensatz_Komplett$Impfen~Datensatz_Komplett$SD02_01+Datensatz_Komplett$politikquadriert)
anova(einfacheregression, quadratischeregression)

#Streudiagramm 
plot(Datensatz_Komplett$SD02_01, Datensatz_Komplett$Impfen)


#8.2 Korrelation zwischen Demokratievertrauen und Impfbereitschaft

#Demokratievertrauen als Mittelwert einer Person über die Items hinweg
Datensatz_Komplett$DV <- rowMeans(subset(Datensatz_Komplett, select = c(DE01_01, DE01_02, DE01_03, DE01_04, DE01_05, DE01_06, DE01_07, DE01_08, DE01_09)), na.rm = TRUE)

#Korrelationstest
cor.test(Datensatz_Komplett$Impfen, Datensatz_Komplett$DV)

#Streudiagramm 
plot(Datensatz_Komplett$DV, Datensatz_Komplett$Impfen)


#8.3 Korrelation zwischen Einstellung ggü. ÖR-Medien und Impfbereitschaft

#Einstellung ggü. ÖR-Medien als Mittelwert einer Person über die Items hinweg
Datensatz_Komplett$TVEinstellung <- rowMeans(subset(Datensatz_Komplett, select = c(TV01_01, TV01_02, TV01_03, TV01_04, TV01_05, TV01_06, TV01_07, TV01_08, TV01_09)), na.rm = TRUE)

#Korrelationstest
cor.test(Datensatz_Komplett$Impfen, Datensatz_Komplett$TVEinstellung)

#Streudiagramm 
plot(Datensatz_Komplett$TVEinstellung, Datensatz_Komplett$Impfen)

#8.4 Korrelation zwischen Individualismus und Impfbereitschaft

#Individualismus als Mittelwert einer Person über die Items hinweg
Datensatz_Komplett$Individ <- rowMeans(subset(Datensatz_Komplett, select = c(IN01_01, IN01_02, IN01_03, IN01_04, IN01_05, IN01_06, IN01_07, IN01_08, IN01_09, IN01_10, IN01_11, IN01_12, IN01_13, IN01_14, IN01_15)), na.rm = TRUE)

#Korrelationstest
cor.test(Datensatz_Komplett$Impfen, Datensatz_Komplett$Individ)

#Streudiagramm 
plot(Datensatz_Komplett$Individ, Datensatz_Komplett$Impfen)


#9. Multiple Regression

#9.1 Modellspezifikation
modell <- lm(Impfen ~ SD02_01 + DV + TVEinstellung + Individ, data = Datensatz_Komplett)
konfidenz <- 0.95
nachkomma <- 3

#9.2 Voraussetzungen

#Homoskedastizität
ols_plot_resid_fit(modell)
ols_test_breusch_pagan(modell)

#Normalverteilung der Residuen
ols_plot_resid_hist(modell)
ols_plot_resid_qq(modell)
shapiro.test(modell$residuals)
agostino.test(modell$residuals)
anscombe.test(modell$residuals)

#Linearität
daten.plot <- data.frame(Datensatz_Komplett$Impfen, Datensatz_Komplett$SD02_01, Datensatz_Komplett$DV, Datensatz_Komplett$TVEinstellung, Datensatz_Komplett$Individ)
pairs(daten.plot, pch = 19, lower.panel = NULL)
raintest(modell)

#Abwesenheit von Multikollinearität
ols_vif_tol(modell)


#9.3 Output mit jtools
summ(modell, confint = TRUE, ci.width = konfidenz, digits = nachkomma)
summ(modell, scale = TRUE, transform.response = TRUE, digits = nachkomma)


#10. Weitere Analysen: Unterschied Geimpfte/ Ungeimpfte

#10.1 Alter
describeBy(Datensatz_Komplett$SD01_01, Datensatz_Komplett$IM01)
t.test(Datensatz_Komplett$SD01_01~Datensatz_Komplett$IM01)
cohensD(Datensatz_Komplett$SD01_01~Datensatz_Komplett$IM01)

#10.2 Politische Einstellung
describeBy(Datensatz_Komplett$SD02_01, Datensatz_Komplett$IM01)
t.test(Datensatz_Komplett$SD02_01~Datensatz_Komplett$IM01)
cohensD(Datensatz_Komplett$SD02_01~Datensatz_Komplett$IM01)

#10. 3 Impfbereitschaft
describeBy(Datensatz_Komplett$Impfen, Datensatz_Komplett$IM01)
t.test(Datensatz_Komplett$Impfen~Datensatz_Komplett$IM01)
cohensD(Datensatz_Komplett$Impfen~Datensatz_Komplett$IM01)

#10.4 Einstellung ggü. öffentlich-rechtlichen Medien
describeBy(Datensatz_Komplett$TVEinstellung, Datensatz_Komplett$IM01)
t.test(Datensatz_Komplett$TVEinstellung~Datensatz_Komplett$IM01)
cohensD(Datensatz_Komplett$TVEinstellung~Datensatz_Komplett$IM01)

#10.5 Individualismus
describeBy(Datensatz_Komplett$Individ, Datensatz_Komplett$IM01)
t.test(Datensatz_Komplett$Individ~Datensatz_Komplett$IM01)
cohensD(Datensatz_Komplett$Individ ~Datensatz_Komplett$IM01)

#10.6 Vertrauen in demokratische Prozesse
describeBy(Datensatz_Komplett$DV, Datensatz_Komplett$IM01)
t.test(Datensatz_Komplett$DV~Datensatz_Komplett$IM01)
cohensD(Datensatz_Komplett$DV ~Datensatz_Komplett$IM01)
