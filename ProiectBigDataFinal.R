library(tidyverse)
library(modelr) 
library(scatterplot3d)
library("dplyr") 
library(GGally)

#pentru arbore
library(rpart) # necesar pentru CART Decision Trees
library(rpart.plot) #pentru cart========
library(caret)
library(rsample)
library(readr)

#citire si atribuire de date in variabila
Case <- read_csv("Houses.csv")
View(Case)

#selectare coloane necesare
PreturiCase <- select(Case, city, floor , price, rooms, sq) 
View(PreturiCase)


#redenumire coloane
PreturiCase <- rename(PreturiCase, c("Oras" = "city"))
PreturiCase <- rename(PreturiCase, c("Etaj" = "floor"))
PreturiCase <- rename(PreturiCase, c("Pret" = "price"))
PreturiCase <- rename(PreturiCase, c("NrCamere" = "rooms"))
PreturiCase <- rename(PreturiCase, c("Suprafata" = "sq"))
View(PreturiCase)

#vizualizare tipuri de date ale coloanelor
str(PreturiCase)

#transformarea in factor a coloanei oras
PreturiCase <- PreturiCase %>%
  mutate(
    Oras=factor(Oras)
  )

str(PreturiCase$Oras) #vizualizare tip de date
levels(PreturiCase$Oras)  #vizualizare nivele factor
freq_oras<-table(PreturiCase$Oras)  
freq_oras #tabel de frecventa pt oras


PreturiCase <- PreturiCase %>%
  mutate(
    NrCamere=factor(NrCamere)
  )
str(PreturiCase$NrCamere) #vizualizare tip de date
levels(PreturiCase$NrCamere)  #vizualizare nivele factor
freq_NrCamere<-table(PreturiCase$NrCamere)  
freq_NrCamere #tabel de frecventa pt NrCamere


PreturiCase <- PreturiCase %>%
  mutate(
    Etaj=factor(Etaj)
  )

str(PreturiCase$Etaj) #vizualizare tip de date
levels(PreturiCase$Etaj)  #vizualizare nivele factor
freq_Etaj<-table(PreturiCase$Etaj)  
freq_Etaj #tabel de frecventa pt etaj

str(PreturiCase)

#vizualizari ale datelor
PreturiCase %>% 
  ggplot(aes(Suprafata, Pret)) + geom_point() + geom_smooth()

PreturiCase %>% 
  ggplot(aes(NrCamere, Pret)) + geom_point() + geom_smooth()


PreturiCase %>% 
  ggplot(aes(Etaj, Pret)) + geom_point() + geom_smooth()

PreturiCase %>% 
  ggplot(aes(Oras, Pret)) + geom_point() + geom_smooth()


# arbori de decizie
set.seed(1) # facem seed pentru sa nu difere "random" de la o executie la alta
impartire_case <- initial_split(PreturiCase, prop = 0.7) #
antrenament_case <- training(impartire_case) # set de training
test_case <- testing(impartire_case) # set de testing

m1 <- rpart(
  formula = Pret ~ ., # Pret AGAINST all others
  data = antrenament_case,
  method = "anova" # pentru predictie numerica
) # algoritmul va face cu setarile default (nu i-am dat nici CP nici min/max depth)
m1 # vizualizare arbore text
# 1. conditie dupa care facem impartirea
# 2. Nr. NODURI
# 3. devianta sau SSE
# 4. yval sau valoarea prezisa
rpart.plot(m1) # vizualizare arbore grafic

plotcp(m1) # 0.077-0.048 -> valoare pentru alpha astef incat: (SSE + alpha * T) este minim
# marimea arborelui este 7
# linia reprezinta acea combinatie unde SSE + alpha * T este MINIM
m1$cptable # CP - cost complexity = alpha

pred <- predict(m1, newdata = test_case)
RMSE(pred = pred, obs = test_case$Pret)



# se obtine un arbore cu parametri minsplit si maxdepth specificati
m2 <- rpart(
  formula = Pret ~ . ,
  data = antrenament_case,
  method = "anova",
  control = list(minsplit = 10, maxdepth = 12, xval = 10)
  # va continua numai daca sunt mai mult de 10 instante in nodul respectiv si adancimea nu e mai mare de 12
) # xval = cross-validation 10 -> va face pt fiecare cate 10 arbori si luam media
m2
plotcp(m2)   
rpart.plot(m2)

pred <- predict(m2, newdata = test_case)
RMSE(pred = pred, obs = test_case$Pret)


#cautam cele mai bune valori pentru parametri minsplit si maxdepth
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)
head(hyper_grid)
models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = Pret ~. ,
    data = antrenament_case,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}
get_cp <- function(x) {
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
}
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}


mutated_grid <- hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  )  
mutated_grid %>%      #top 5 arbori cu eroarea cea mai mica
  arrange(error) %>%
  top_n(-5, wt=error)

optimal_tree <- rpart(
  formula = Pret ~ .,
  data = antrenament_case,
  method = "anova",
  control = list(minsplit = 5, maxdepth = 8, cp = 0.01) #am pus parametrii obtinuti la top mai sus
)
plotcp(optimal_tree)           #a, gasit arborele cel mai bun
rpart.plot(optimal_tree)

pred <- predict(optimal_tree, newdata = test_case)
RMSE(pred = pred, obs = test_case$Pret) #eroare
optimal_tree


#regresia liniara pentru pret si suprafata
Pret_Suprafata <- lm (data = PreturiCase, Pret ~ Suprafata)
summary(Pret_Suprafata)

confint(Pret_Suprafata) #ne da intervalele de incredere 

grid_Suprafata <- PreturiCase %>%
  data_grid(Suprafata = seq_range(Suprafata,100)) %>%
  add_predictions(Pret_Suprafata, "Pret")
grid_Suprafata

ggplot(PreturiCase, aes(Suprafata, Pret))+ geom_point() + geom_line(data = grid_Suprafata , color = "red" , size =1) 
confint(Pret_Suprafata)

#regresia liniara pentru pret si camere
Pret_NrCamere <- lm(data = PreturiCase , Pret ~ NrCamere)
summary(Pret_NrCamere)

confint(Pret_NrCamere) #ne da intervalele de incredere

#regresia liniara pentru pret si etaj
Pret_Etaj <- lm(data = PreturiCase , Pret ~ Etaj)
summary(Pret_Etaj)

confint(Pret_Etaj) #ne da intervalele de incredere


#regresia liniara pentru pret si oras

Pret_Oras <- lm(data = PreturiCase , Pret ~ Oras)
summary(Pret_Oras)

confint(Pret_Oras) #ne da intervalele de incredere


Pret_Suprafata_NrCamere <- lm(data = PreturiCase, Pret ~ Suprafata + NrCamere)
summary(Pret_Suprafata_NrCamere)

Suprafata_NrCamere_Etaj <- lm(data = PreturiCase, Suprafata ~ NrCamere + Etaj)
summary(Suprafata_NrCamere_Etaj)

Suprafata_NrCamere_Etaj_Oras <- lm(data = PreturiCase, Suprafata ~ NrCamere + Etaj + Oras  )
summary(Suprafata_NrCamere_Etaj_Oras)

Pret_Suprafata_NrCamere_Etaj_Oras <- lm(data = PreturiCase, Pret ~ Suprafata + Etaj + Oras + NrCamere  )
summary(Pret_Suprafata_NrCamere_Etaj_Oras)




