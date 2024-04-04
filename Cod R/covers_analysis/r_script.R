rm(list = ls())

PackageNames <- c(
  "tidyverse", "stargazer", "magrittr", "lmtest", "sandwich",
  "olsrr", "moments", "whitestrap", "ggplot2", "DataCombine",
  "car", "tseries", "caret", "mltools", "MLmetrics", "caret",
  "splines", "mgcv", "glmnet", "psych", "readxl"
)
for (i in PackageNames) {
  if (!require(i, character.only = T)) {
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# install.packages("readxl")
library("readxl")
#excel_file_path <- "Dataset only.xlsx"
excel_file_path <- "ComunicariStiintificeAnaliza.xlsx"


# Regresie multipla
popularity_1 <- read_excel(excel_file_path)
summary(popularity_1)
popularity_1 %<>% select(
  Year_yt, Views_yt, Spotify_plays, Likes_yt, Popularity_spotify, Duration,
  Tempo, Debut, Career_cover_years, Band, Alive_then, Live, Longer
)
# cor(popularity_1)
head(popularity_1, 10)

model_1 <- lm(log(Views_yt) ~ log(Tempo) + Career_cover_years + Live, popularity_1) # best
summary(model_1)
aic <- AIC(model_1) # 185
cat(aic)
bic <- BIC(model_1) # 193
cat(bic)

model_2 <- lm(log(Views_yt) ~ log(Tempo) + log(Career_cover_years) + Live, popularity_1)
summary(model_2)
aic <- AIC(model_2) # 187
cat(aic)
bic <- BIC(model_2) # 195
cat(bic)

model_3 <- lm(log(Views_yt) ~ Tempo + log(Career_cover_years) + Live, popularity_1)
summary(model_3)
aic <- AIC(model_3) # 188
cat(aic)
bic <- BIC(model_3) # 196
cat(bic)

model_4 <- lm(log(Views_yt) ~ Tempo + log(Career_cover_years) + Live + Year_yt, popularity_1)
summary(model_4)
aic <- AIC(model_4) # 190
cat(aic)
bic <- BIC(model_4) # 200
cat(bic)

popularity_1 <- transform(popularity_1, v = 2023 - Year_yt)
print(popularity_1)

model_5 <- lm(log(Views_yt) ~ Tempo + log(Career_cover_years) + Live + v, popularity_1)
summary(model_5)
aic <- AIC(model_5) # 190
cat(aic)
bic <- BIC(model_5) # 200
cat(bic)

model_6 <- lm(log(Views_yt) ~ Tempo + log(Career_cover_years) + Band, popularity_1)
summary(model_6)
aic <- AIC(model_6) # 190
cat(aic)
bic <- BIC(model_6) # 198
cat(bic)

model_7 <- lm(log(Views_yt) ~ log(Tempo) + log(Career_cover_years) + Band, popularity_1)
summary(model_7)
aic <- AIC(model_7) # 189
cat(aic)
bic <- BIC(model_7) # 197
cat(bic)

model_8 <- lm(log(Views_yt) ~ log(Tempo) + Career_cover_years + Band, popularity_1)
summary(model_8)
aic <- AIC(model_8) # 187
cat(aic)
bic <- BIC(model_8) # 195
cat(bic)

model_9 <- lm(log(Views_yt) ~ Tempo + log(Career_cover_years) + Longer, popularity_1)
summary(model_9)
aic <- AIC(model_9) # 194
cat(aic)
bic <- BIC(model_9) # 202
cat(bic)

model_10 <- lm(log(Views_yt) ~ log(Tempo) + Career_cover_years + Longer, popularity_1)
summary(model_10)
aic <- AIC(model_10) # 193
cat(aic)
bic <- BIC(model_10) # 201
cat(bic)

model_11 <- lm(log(Views_yt) ~ log(Tempo) + log(Career_cover_years) + Longer, popularity_1)
summary(model_11)
aic <- AIC(model_11) # 193
cat(aic)
bic <- BIC(model_11) # 201
cat(bic)

model_12 <- lm(log(Views_yt) ~ log(Tempo) + Career_cover_years + Duration + Live, popularity_1)
summary(model_12)
aic <- AIC(model_12) # 187
cat(aic)
bic <- BIC(model_12) # 197
cat(bic)

model_13 <- lm(log(Views_yt) ~ log(Tempo) + log(Career_cover_years) + Duration + Live, popularity_1)
summary(model_13)
aic <- AIC(model_13) # 189 :((
cat(aic)
bic <- BIC(model_13) # 199
cat(bic)

model_14 <- lm(log(Views_yt) ~ log(Tempo) + Career_cover_years + log(Duration) + Live, popularity_1)
summary(model_14)
aic <- AIC(model_14) # 187
cat(aic)
bic <- BIC(model_14) # 197
cat(bic)

model_14 <- lm(log(Views_yt) ~ log(Tempo) + Career_cover_years + log(Duration) + Live, popularity_1)
summary(model_14)
aic <- AIC(model_14) # 187
cat(aic)
bic <- BIC(model_14) # 197
cat(bic)

model_15 <- lm(log(Views_yt) ~ log(Tempo) + Career_cover_years + Live, popularity_1)
summary(model_15)
aic <- AIC(model_15) # 186
cat(aic)
bic <- BIC(model_15) # 196

# RAMANEM CU MODELUL 1, a iesit cel mai bine
vif(model_1)

# testam pt Spotify_plays acum:

model_s1 <- lm(log(Spotify_plays) ~ log(Tempo) + Career_cover_years + Live, popularity_1) # NU
summary(model_s1)
aic <- AIC(model_s1) # 194
cat(aic)
bic <- BIC(model_s1) # 203
cat(bic)

model_s2 <- lm(log(Spotify_plays) ~ Tempo + Career_cover_years + Live, popularity_1) # NU
summary(model_s2)
aic <- AIC(model_s2) # 194
cat(aic)
bic <- BIC(model_s2) # 203
cat(bic)

model_s3 <- lm(log(Spotify_plays) ~ Tempo + Career_cover_years + Live, popularity_1) # NU
summary(model_s3)
aic <- AIC(model_s3) # 194
cat(aic)
bic <- BIC(model_s3) # 203
cat(bic)


###################################################
###################################################
# TESTE SI GRAFICE PT model_1 (fara Spotify_plays), cel recomandat de profa
###################################################
###################################################

summary(model_1)
aic <- AIC(model_1) # 185 inainte de corectare ipoteze
cat(aic)
bic <- BIC(model_1) # 193 initial inainte de corectare ipoteze
cat(bic)

popularity_2 <- popularity_1

popularity_2 %<>% mutate(
  popularity_hat = fitted(model_1),
  uhat = residuals(model_1)
)

# Criteriul Akaike pentru modelul simplu
aic <- AIC(model_1)
cat("AIC (Akaike):", aic, "\n")

# Criteriul Schwarz pentru modelul simplu
bic <- BIC(model_1)
cat("BIC (Schwarz):", bic, "\n")

plot(model_1)

pl# Graficul Q-Q Plot
ols_plot_resid_qq(model_1)

# Histograma
# Avem valori extreme deoarece, in histograma, coada dreapta a curbei este mai alungita decat cea din stanga
ols_plot_resid_hist(model_1)

skewness(popularity_1$uhat) # => Histograma este centrata dreapta deoarece coeficientul este > 0

kurtosis(popularity_1$uhat) # => Histograma are o forma ... deoarece coeficientul este < 3

boxplot(model_1$residuals, main = "Box Plot reziduuri")

shapiro.test(popularity_2$uhat)
jarque.bera.test(popularity_2$uhat) # reziduurile NU sunt normal distribuite (p-value > 0.05)
ols_plot_cooksd_bar(model_1) # Distante cook pentru outliers
ols_plot_cooksd_chart(model_1)

popularity_2_cook <- popularity_2[-c(3, 35), ]
model_1_normalizat <- lm(log(Views_yt) ~ log(Tempo) + Career_cover_years + Live, popularity_2_cook)
summary(model_1_normalizat)

ols_test_normality(model_1_normalizat) # acum suntem siguri ca reziduurile sunt normal distribuite
shapiro.test(popularity_2_cook$uhat) # bine, normalizat
jarque.bera.test(popularity_2_cook$uhat) # bine, normalizat

# Testarea ipotezei de homoscedasticitate -----------------
# Graficul reziduurilor fata de variabila independenta Views_yt
ggplot(data = popularity_2_cook, mapping = aes(x = log(Views_yt), y = uhat)) +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(y = "Reziduuri", x = "log(Views_yt), log(thousand views)")

# Graficul reziduurilor fata de valorile estimate de model
ggplot(data = popularity_2_cook, mapping = aes(x = log(Views_yt), y = uhat)) +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(y = "Reziduuri", x = "Valori estimate")

bptest(model_1_normalizat)
white_test(model_1_normalizat) # sunt HOMOschedastice

# Testarea non-autocorelarii folosind teste statistice:
# 1. Durbin-Watson
dwtest(model_1_normalizat) # e bine, reziduuri non-autocorelate
bptest(model_1_normalizat)

summary(model_1_normalizat)
aic <- AIC(model_1_normalizat) # 162 dupa corectare ipoteze
cat(aic)
bic <- BIC(model_1_normalizat) # 170 dupa corectare ipoteze
cat(bic)

#######################################################
#######################################################
# Pentru modelul initial, care contine si Spotify_plays
#######################################################
#######################################################
summary(model_multiplu_popularity)
aic <- AIC(model_multiplu_popularity) # 170
cat(aic)
bic <- BIC(model_multiplu_popularity) # 180
cat(bic)

model_multiplu_popularity <- lm(log(Views_yt) ~ log(Tempo) + Career_cover_years + Spotify_plays + Live, popularity_1)
summary(model_multiplu_popularity)

# Valorile previzionate si reziduuri
popularity_1 %<>% mutate(
  popularity_hat = fitted(model_multiplu_popularity),
  uhat = residuals(model_multiplu_popularity)
)

# Criteriul Akaike pentru modelul simplu
aic <- AIC(model_multiplu_popularity)
cat("AIC (Akaike):", aic, "\n")

# Criteriul Schwarz pentru modelul simplu
bic <- BIC(model_multiplu_popularity)
cat("BIC (Schwarz):", bic, "\n")

# Multicoliniaritate:
# Rulam regresia si calculam VIF. Daca VIF>10 atunci trebuie sa stergem variabila respectiva.
# Deoarece nicio variabila nu are valoarea VIF>10 => modelul de regresie multipla nu prezinta multicoliniaritate
vif(model_multiplu_popularity)

# TESTAREA IPOTEZELOR
# Ipoteza de Normalitate
# Grafic pentru a prezenta Residuals vs FItted
plot(model_multiplu_popularity)

# Graficul Q-Q Plot
ols_plot_resid_qq(model_multiplu_popularity)

# Histograma
# Avem valori extreme deoarece, in histograma, coada dreapta a curbei este mai alungita decat cea din stanga
ols_plot_resid_hist(model_multiplu_popularity)

skewness(popularity_1$uhat) # => Histograma este centrata dreapta deoarece coeficientul este > 0

kurtosis(popularity_1$uhat) # => Histograma are o forma platicurtica deoarece coeficientul este < 3

boxplot(model_multiplu_popularity$residuals, main = "Box Plot reziduuri")

shapiro.test(popularity_1$uhat)
jarque.bera.test(popularity_1$uhat) # reziduurile SUNT normal distribuite (p-value > 0.05)

ols_test_normality(model_multiplu_popularity) # acum suntem siguri ca residurile sunt normal distribuite

# Testarea ipotezei de homoscedasticitate -----------------
# Graficul reziduurilor fata de variabila independenta Views_yt
ggplot(data = popularity_1, mapping = aes(x = log(Views_yt), y = uhat)) +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(y = "Reziduuri", x = "log(Views_yt), log(thousand views)")

# Graficul reziduurilor fata de valorile estimate de model
ggplot(data = popularity_1, mapping = aes(x = log(Views_yt), y = uhat)) +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(y = "Reziduuri", x = "Valori estimate")

bptest(model_multiplu_popularity)
white_test(model_multiplu_popularity) # sunt HOMOschedastice

# Testarea non-autocorelarii folosind teste statistice:
# 1. Durbin-Watson
dwtest(model_multiplu_popularity)
bptest(model_multiplu_popularity)

summary(model_multiplu_popularity)
aic <- AIC(model_multiplu_popularity) # 170
cat(aic)
bic <- BIC(model_multiplu_popularity) # 180
cat(bic)

# TESTAREA TUTUROR IPOTEZELOR
# Ipoteza 1 - Este modelul liniar in parametri?
# Da, deoarece poate fi scris ca o functie liniara

# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model_multiplu_popularity) > (model_multiplu_popularity$rank - 1)

# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru

# Ipoteza 4 - Variabilitatea in x este pozitiva
var(popularity_1$Views_yt)
var(popularity_1$Spotify_plays)
# toate valorile > 0 => ipoteza acceptata

# Ipoteza 5 - Media reziduurilor este 0
mean(model_multiplu_popularity$residuals) # medie aproape de 0 => ipoteza acceptata

# Ipoteza 6 - Testare multicoliniaritate
vif(model_multiplu_popularity) # nu avem valori pt VIF > 10 => ipoteza acceptata

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(popularity_1$Spotify_plays, model_multiplu_popularity$residuals) # p-value > 0.1 => NU sunt corelate, e bine
cor.test(popularity_1$Tempo, model_multiplu_popularity$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(popularity_1$Career_cover_years, model_multiplu_popularity$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(popularity_1$Live, model_multiplu_popularity$residuals) # p-value > 0.1 => nu sunt corelate
# => ipoteza acceptata

# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model_multiplu_popularity) # homoschedastice
white_test(model_multiplu_popularity) # homoschedastice

summary(model_multiplu_popularity)

# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(model_multiplu_popularity$residuals) # nu sunt autocorelate
dwtest(model_multiplu_popularity) # p-value > 0.05 => reziduuri nonautocorelate
bgtest(model_multiplu_popularity) # p-value > 0.1 => reziduuri nonautocorelate

# Ipoteza 10 -  Reziduurile sunt normal distribuite
jarque.bera.test(model_multiplu_popularity$residuals)
shapiro.test(model_multiplu_popularity$residuals)

summary(model_multiplu_popularity)


####################
# ANALIZA DE CLUSTERI
####################

install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Încarcă datele
data <- read_excel(excel_file_path)
# Filter out the row with "Mickey Gilley" in the artist column
data <- data[data$Artist != "Mickey Gilley", ]

# Verifică structura și primele rânduri ale datelor
str(data)
head(data)

# Selectează doar caracteristicile numerice relevante pentru clusterizare

# data_cluster <- data[, c("Views_yt", "Spotify_plays", "Popularity_spotify", "Duration", "Tempo")]
data_cluster <- data[, c("Duration", "Tempo")]


# Normalizarea datelor
data_cluster <- scale(data_cluster)

# Determinarea numărului optim de clustere folosind metoda cotului
wss <- (nrow(data_cluster) - 1) * sum(apply(data_cluster, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(data_cluster, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# Aplicarea algoritmului K-Means cu numărul optim de clustere identificat (de exemplu, 5)
set.seed(123)
kmeans_model <- kmeans(data_cluster, centers = 5)

# Adaugă etichetele clusterele la setul de date original
data$cluster <- as.factor(kmeans_model$cluster)

# Vizualizarea rezultatelor
ggplot(data, aes(x = Views_yt, y = Spotify_plays, color = cluster)) +
  geom_point() +
  labs(title = "Clusterizarea coverurilor muzicale", x = "Vizualizări pe YouTube", y = "Vizualizări pe Spotify") +
  theme_minimal()

#########################################
############ Analiza tendințele temporale

# Calcularea sumarului pe ani
summary_year <- data %>%
  group_by(Year_yt) %>%
  summarize(
    mean_Views_yt = mean(Views_yt),
    mean_Spotify_plays = mean(Spotify_plays),
    mean_Popularity_spotify = mean(Popularity_spotify),
    mean_Likes_yt = mean(Likes_yt)
  )

# Vizualizarea tendințelor temporale folosind grafice de linie
ggplot(summary_year, aes(x = Year_yt)) +
  geom_line(aes(y = mean_Views_yt, color = "Views_yt"), size = 1) +
  geom_line(aes(y = mean_Spotify_plays, color = "Spotify_plays"), size = 1) +
  geom_line(aes(y = mean_Popularity_spotify, color = "Popularity_spotify"), size = 1) +
  geom_line(aes(y = mean_Likes_yt, color = "Likes_yt"), size = 1) +
  labs(
    title = "Evoluția datelor în funcție de ani",
    x = "Anul",
    y = "Valoare medie"
  ) +
  scale_color_manual(
    name = "Variabile",
    values = c(
      "Views_yt" = "blue", "Spotify_plays" = "green",
      "Popularity_spotify" = "red", "Likes_yt" = "orange"
    )
  ) +
  theme_minimal()

# ANALIZA IN COMP PRINCIPALE (ACP)

#################################
######## ANALIZA DE CORESPONDENTA

# Selectarea doar a variabilelor categorice
install.packages("FactoMineR")
library(FactoMineR)
data_categ <- data[, c("Band", "Alive_then", "Live", "Longer")]

# Analiza de corespondență
res_ca <- CA(data_categ)

# Vizualizarea rezultatelor
plot(res_ca)


############### ANOVA #################
# Convertirea variabilelor categorice la factori
data$Band <- as.factor(data$Band)
data$Alive_then <- as.factor(data$Alive_then)
data$Live <- as.factor(data$Live)
data$Longer <- as.factor(data$Longer)

# Analiză ANOVA pentru variabila dependentă Views_yt
anova_results <- aov(Views_yt ~ Band + Alive_then + Live + Longer, data = data)

# Sumarul analizei ANOVA
summary(anova_results)
