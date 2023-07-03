library(tidyverse)
library(haven)
library(ggplot2)
library(lme4)
library(lattice)
library(sf)
library(pROC)
library(caret)

census88 <- read_dta("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/census88.dta")
polls <- read_dta("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/polls.dta")
polls.subset <- read.table("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/polls.subset.dat", header = TRUE)
presvote <- read_dta("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/presvote.dta")
election88 <- read_dta("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/election88.dta")
candidate_effects <- read.table("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/candidate_effects.dat", header = TRUE)
election88.data <- read.table("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/election88.data.r", header = TRUE)
st_by_region <- read.csv("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/st_by_region.csv")
polls_subset_tratado <- read.csv("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/polls_subset_tratado.csv")
polls_tratado_state <- read.csv("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/polls_tratado_state.csv")
polls_tratado_region <- read.csv("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/polls_tratado_region.csv")
polls_tratado_black_state <- read.csv("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/polls_tratado_black_state.csv")
sp_state <- read_sf("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/states_polygon/cb_2018_us_state_20m.shp")
sp_region <- read_sf("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/region_polygon/cb_2018_us_region_20m.shp")


#-------------------------------------
# EDA
#-------------------------------------

#-------------------------------------
# Adicionando coluna com sigla dos estados


polls.subset <- polls.subset %>% 
  rename("id_state" = "state")

polls.subset$state <- polls.subset$id_state

cont <- 1
for (st in election88$st){
  polls.subset$state[polls.subset$state == cont] <- st
  cont <- cont + 1
}


#-------------------------------------
# adicionando regiões

polls.subset$region <- polls.subset$state

for (st in st_by_region$State.Code){
  polls.subset$region[polls.subset$state == st] <- st_by_region$Region[st_by_region$State.Code == st]
}

region <- c("Northeast", "South", "Midwest","West")

polls.subset$id_region <- polls.subset$region

cont <- 1
for (reg in region){
  polls.subset$id_region[polls.subset$id_region == reg] <- cont
  cont <- cont + 1
}

polls.subset <- polls.subset[,-10]

polls.subset$id_region <- as.numeric(polls.subset$id_region)


#-------------------------------------
# Removendo NA

polls.subset <- polls.subset[!is.na(polls.subset$bush),]


#-------------------------------------
# Salvando mudanças em csv

write_csv(polls.subset, "C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/polls_subset_tratado.csv")


#-------------------------------------
# agredando dados por estado e região

us_pop <- read.csv("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/us_pop.csv", sep=";", header=T)

us_pop_tratado <- us_pop[-c(2,3,13,14,38,43,49),]

us_pop_tratado <- read.csv("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/us_pop_tratado.csv")

polls_tratado_state <- aggregate(bush~state, data=polls_subset_tratado, FUN= sum)
polls_tratado_region <- aggregate(bush~region, data=polls_subset_tratado, FUN= sum)


black_state <- aggregate(black~state, data=polls_subset_tratado, FUN= sum)
polls_tratado_state$prop_resp <- polls_tratado_state$bush/respostas_state$n
black_state$prop_black <- black_state$black/respostas_state$n

respostas_state <- polls.subset %>% count(state)
respostas_region <- polls.subset %>% count(region)

polls_tratado_state$n_ent <- respostas_state$n
polls_tratado_state$prop_resp <- polls_tratado_state$bush/respostas_state$n

polls_tratado_state$prop_ent <- respostas_state$n/sum(respostas_state$n)

polls_tratado_state$prop_pop <- respostas_state$n/sum(respostas_state$n)

polls_tratado_black_state$prop_black <- polls_tratado_black_state$black/sum(respostas_state$n)

for (st in polls_tratado_state$state){
  polls_tratado_state$prop_pop[polls_tratado_state$state == st] <- us_pop_tratado$"X1990"[us_pop_tratado == st]/us_pop_tratado[50,"X1990"]
}

polls_tratado_region$prop_resp <- polls_tratado_region$bush/respostas_region$n
polls_tratado_region$prop_ent <- respostas_region$n/sum(respostas_region$n)


write_csv(polls_tratado_state, "C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/polls_tratado_state.csv")
write_csv(polls_tratado_region, "C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/polls_tratado_region.csv")
write_csv(polls_tratado_black_state, "C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/data/election88/polls_tratado_black_state.csv")

#-------------------------------------
# proporção de respostas por estado

sp_state_merged <- merge(sp_state, polls_tratado_state, by.x ="STUSPS", by.y="state", duplicateGeoms=T)
black_state_merged <- merge(sp_state, black_state, by.x ="STUSPS", by.y="state", duplicateGeoms=T)

COLOR_CASES_LOW = "#FEEDDE"
COLOR_CASES_HIGH = "#A63603"

ggplot() + 
  geom_sf(data=sp_state_merged, aes(fill=prop_resp))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Proporção de \n respostas \n favoráveis \n ao Bush"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  )-> Map_prop_resp_por_state

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/Map_prop_resp_por_state.png',
       plot = Map_prop_resp_por_state)



ggplot() + 
  geom_sf(data=sp_state_merged, aes(fill=bush))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Respostas \n favoráveis \n ao Bush"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  )-> Map_resp_por_state

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/Map_resp_por_state.png',
       plot = Map_resp_por_state)

ggplot() + 
  geom_sf(data=black_state_merged, aes(fill=black))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Número de \n afro-americanos"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  )-> Map_prop_black_por_state

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/Map_prop_black_por_state.png',
       plot = Map_prop_black_por_state)


#-------------------------------------
# proporção de entrevistados por estado

ggplot() + 
  geom_sf(data=sp_state_merged, aes(fill=prop_ent))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Proporção de \n entrevistados"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) -> Map_prop_ent_por_state

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/Map_prop_ent_por_state.png',
       plot = Map_prop_ent_por_state)

ggplot() + 
  geom_sf(data=sp_state_merged, aes(fill=n_ent))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Número de \n entrevistados \n por estado"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  )-> Map_ent_por_state

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/Map_ent_por_state.png',
       plot = Map_ent_por_state)

#-------------------------------------
# proporção da população por estado

ggplot() + 
  geom_sf(data=sp_state_merged, aes(fill=prop_pop))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Proporção da \n população"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) -> Map_prop_pop_por_state

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/Map_prop_pop_por_state.png',
       plot = Map_prop_pop_por_state)

#-------------------------------------
# proporção de black por estado

sp_black_state_merged <- merge(sp_state, polls_tratado_black_state, by.x ="STUSPS", by.y="state", duplicateGeoms=T)

ggplot() + 
  geom_sf(data=sp_black_state_merged, aes(fill=prop_black))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Proporção de \n negros"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  )

#-------------------------------------
# proporção de respostas por região

sp_region_merged <- merge(sp_region, polls_tratado_region, by.x ="NAME", by.y="region", duplicateGeoms=T)

COLOR_CASES_LOW = "#FEEDDE"
COLOR_CASES_HIGH = "#A63603"

b.box <- st_bbox(sp_region)

ggplot() + 
  geom_sf(data=sp_region_merged, aes(fill=prop_resp))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Proporção de \n respostas \n favoráveis \n ao Bush"
  ) +
  coord_sf(
    xlim = c(b.box[1] + 50, b.box[3] - 240),
    ylim = c(b.box[2] +5, b.box[4] - 15),
    expand = FALSE
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) -> Map_prop_resp_por_region

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/Map_prop_resp_por_region.png',
       plot = Map_prop_resp_por_region)

#-------------------------------------
# proporção de entrevistados por região

COLOR_CASES_LOW = "#FEEDDE"
COLOR_CASES_HIGH = "#A63603"

b.box <- st_bbox(sp_region)

ggplot() + 
  geom_sf(data=sp_region_merged, aes(fill=prop_ent))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Proporção de \n entrevistados"
  ) +
  coord_sf(
    xlim = c(b.box[1] + 50, b.box[3] - 240),
    ylim = c(b.box[2] +5, b.box[4] - 15),
    expand = FALSE
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) -> Map_prop_ent_por_region

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/Map_prop_ent_por_region.png',
       plot = Map_prop_ent_por_region)

#-------------------------------------
# respostas por educação

ggplot(polls_subset_tratado, aes(edu, fill = factor(bush))) + 
  geom_histogram(alpha = 0.5, position = 'identity') +
  guides(fill=guide_legend(title="Bush(1) \n      X \nDukakis(0)")) -> hist_resp_por_edu

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/hist_resp_por_edu.png',
       plot = hist_resp_por_edu)


#-------------------------------------
# respostas por idade

ggplot(polls_subset_tratado, aes(age, fill = factor(bush))) + 
  geom_histogram(alpha = 0.5, position = 'identity') +
  guides(fill=guide_legend(title="Bush(1) \n      X \nDukakis(0)")) -> hist_resp_por_age

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/hist_resp_por_age.png',
       plot = hist_resp_por_age)

#-------------------------------------
# respostas por sexo

table(polls_subset_tratado$bush, polls_subset_tratado$female)

ggplot(polls.subset, aes(female, fill = factor(bush))) + 
  geom_histogram(alpha = 0.5, position = 'identity')


#-------------------------------------
# respostas por etnia

table(polls_subset_tratado$bush, polls_subset_tratado$black)

ggplot(polls.subset, aes(black, fill = factor(bush))) + 
  geom_histogram(alpha = 0.5, position = 'identity')


ggplot(polls.subset, aes(edu, fill = factor(age))) + 
  geom_histogram(alpha = 0.5, position = 'identity')


#-------------------------------------
# Modelos

# dividindo dado em treinamento e teste

sample <- sample(c(TRUE, FALSE), nrow(polls_subset_tratado), replace=TRUE, prob=c(0.8,0.2))
data_train <- polls_subset_tratado
data_test <- polls_subset_tratado

# modelo simples

M1 <- glm(bush ~ edu + age + female + black, data = data_train, family=binomial(link="logit"))

summary(M1)
confint(M1)

prediction_M1 <- predict(M1, data_test,
                      type="response")


pred_M1_state <- data_test
pred_M1_state$pred_M1 <- as.numeric(ifelse(prediction_M1 > 0.5, "1", "0"))
respostas_state <- pred_M1_state %>% count(state)
pred_M1_state <- aggregate(pred_M1~state, data=pred_M1_state, FUN= sum)
pred_M1_state$pred_M1 <- pred_M1_state$pred_M1/respostas_state$n


data_test$pred_M1 <- ifelse(prediction_M1 > 0.5, "1", "0")
data_test$pred_M1 <- as.factor(data_test$pred_M1)

roc_object_M1 <- roc(data_test$bush,  prediction_M1)
plot(roc_object_M1, legacy.axes = TRUE)


# TN FN
# FP TP


confusionMatrix(data= data_test$pred_M1, reference= as.factor(data_test$bush), positive = "1")

recall_M1 <- recall(data= data_test$pred_M1, reference= as.factor(data_test$bush), relevant = "1")
precision_M1 <- precision(data= data_test$pred_M1, reference= as.factor(data_test$bush), relevant = "1")
f1_M1 <- 2 * (precision_M1 * recall_M1) / (precision_M1 + recall_M1)

print(recall_M1)
print(precision_M1)
print(f1_M1)

auc(roc_object_M1)


pred_M1_state_merged <- merge(sp_state, pred_M1_state, by.x ="STUSPS", by.y="state", duplicateGeoms=T)


COLOR_CASES_LOW = "#FEEDDE"
COLOR_CASES_HIGH = "#A63603"


ggplot() + 
  geom_sf(data=pred_M1_state_merged, aes(fill=pred_M1))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Predição"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) -> pred_state_M1

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/pred_state_M1.png',
       plot = pred_state_M1)


# Intercepto variando por estado

M2 <- glmer(bush ~ (1 | id_state) + edu + female + black, data = data_train, family=binomial(link="logit"))

summary(M2)
confint(M2)

dotplot(ranef(M2))

prediction_M2 <- predict(M2, data_test,
                         type="response")



pred_M2_state <- data_test
pred_M2_state$pred_M2 <- as.numeric(ifelse(prediction_M2 > 0.5, "1", "0"))
respostas_state <- pred_M2_state %>% count(state)
pred_M2_state <- aggregate(pred_M2~state, data=pred_M2_state, FUN= sum)
pred_M2_state$pred_M2 <- pred_M2_state$pred_M2/respostas_state$n


data_test$pred_M2 <- ifelse(prediction_M2 > 0.5, "1", "0")
data_test$pred_M2 <- as.factor(data_test$pred_M2)

roc_object_M2 <- roc(data_test$bush, prediction_M2)
plot(roc_object_M2, legacy.axes = TRUE)
auc(roc_object_M2)


confusionMatrix(data= data_test$pred_M2, reference= as.factor(data_test$bush), positive = "1")

recall_M2 <- recall(data= data_test$pred_M2, reference= as.factor(data_test$bush), relevant = "1")
precision_M2 <- precision(data= data_test$pred_M2, reference= as.factor(data_test$bush), relevant = "1")
f1_M2 <- 2 * (precision_M2 * recall_M2) / (precision_M2 + recall_M2)

print(recall_M2)
print(precision_M2)
print(f1_M2)




intercepto_state_M2 <- coef(M2)$id_state
intercepto_state_M2 <- cbind(id_state = rownames(intercepto_state_M2), intercepto_state_M2)
intercepto_state_M2$state <- intercepto_state_M2$id_state


for (id in intercepto_state_M2$id_state){
  intercepto_state_M2$state[intercepto_state_M2$id_state == id] <- unique(polls_subset_tratado$state[polls_subset_tratado$id_state == id])
}

intercepto_state_M2_merged <- merge(sp_state, intercepto_state_M2, by.x ="STUSPS", by.y="state", duplicateGeoms=T)
pred_M2_state_merged <- merge(sp_state, pred_M2_state, by.x ="STUSPS", by.y="state", duplicateGeoms=T)

intercepto_state_M2_merged <- intercepto_state_M2_merged %>% 
  rename("intercepto" = "(Intercept)")

COLOR_CASES_LOW = "#FEEDDE"
COLOR_CASES_HIGH = "#A63603"

ggplot() + 
  geom_sf(data=intercepto_state_M2_merged, aes(fill=intercepto))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Intercepto"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) -> intercepto_state_M2

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/intercepto_state_M2.png',
       plot = intercepto_state_M2)

ggplot() + 
  geom_sf(data=pred_M2_state_merged, aes(fill=pred_M2))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Predição"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) -> pred_state_M2

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/pred_state_M2.png',
       plot = pred_state_M2)



# intercepto variando por estado com interação

M3 <- glmer(bush ~ (1 | id_state) + black + female:black, data = data_train, family=binomial(link="logit"))

summary(M3)
confint(M3)

dotplot(ranef(M3))


prediction_M3 <- predict(M3, data_test,
                         type="response")


pred_M3_state <- data_test
pred_M3_state$pred_M3 <- as.numeric(ifelse(prediction_M3 > 0.5, "1", "0"))
respostas_state <- pred_M3_state %>% count(state)
pred_M3_state <- aggregate(pred_M3~state, data=pred_M3_state, FUN= sum)
pred_M3_state$pred_M3 <- pred_M3_state$pred_M3/respostas_state$n


data_test$pred_M3 <- ifelse(prediction_M3 > 0.5, "1", "0")
data_test$pred_M3 <- as.factor(data_test$pred_M3)

roc_object_M3 <- roc(data_test$bush, prediction_M3)
plot(roc_object_M3, legacy.axes = TRUE)
auc(roc_object_M3)

confusionMatrix(data= data_test$pred_M3, reference= as.factor(data_test$bush), positive = "1")

recall_M3 <- recall(data= data_test$pred_M3, reference= as.factor(data_test$bush), relevant = "1")
precision_M3 <- precision(data= data_test$pred_M3, reference= as.factor(data_test$bush), relevant = "1")
f1_M3 <- 2 * (precision_M3 * recall_M3) / (precision_M3 + recall_M3)

print(recall_M3)
print(precision_M3)
print(f1_M3)


intercepto_state_M3 <- coef(M3)$id_state
intercepto_state_M3 <- cbind(id_state = rownames(intercepto_state_M3), intercepto_state_M3)
intercepto_state_M3$state <- intercepto_state_M3$id_state


for (id in intercepto_state_M3$id_state){
  intercepto_state_M3$state[intercepto_state_M3$id_state == id] <- unique(polls_subset_tratado$state[polls_subset_tratado$id_state == id])
}

intercepto_state_M3_merged <- merge(sp_state, intercepto_state_M3, by.x ="STUSPS", by.y="state", duplicateGeoms=T)
pred_M3_state_merged <- merge(sp_state, pred_M3_state, by.x ="STUSPS", by.y="state", duplicateGeoms=T)

intercepto_state_M3_merged <- intercepto_state_M3_merged %>% 
  rename("intercepto" = "(Intercept)")

COLOR_CASES_LOW = "#FEEDDE"
COLOR_CASES_HIGH = "#A63603"

ggplot() + 
  geom_sf(data=intercepto_state_M3_merged, aes(fill=intercepto))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Intercepto"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) -> intercepto_state_M3

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/intercepto_state_M3.png',
       plot = intercepto_state_M3)


ggplot() + 
  geom_sf(data=pred_M3_state_merged, aes(fill=pred_M3))+
  scale_fill_gradient(
    low = COLOR_CASES_LOW,
    high = COLOR_CASES_HIGH,
    name = "Predição"
  ) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5),
      linetype = "dashed",
      size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) -> pred_state_M3

ggsave('C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/Grad MAp FGV/Disciplinas/5 P/Modelagem Estatística/A2/A2-Modelagem_Estatistica/images/pred_state_M3.png',
       plot = pred_state_M3)
