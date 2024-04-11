library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(MatchIt)
library(cobalt)
library(ggplot2)
baseline = read_excel("./ScreenTime-hw3Q3.xlsx",sheet = 2)
ST = read_excel("./ScreenTime-hw3Q3.xlsx",sheet = 1)
idB = baseline[baseline$Treatment == "B","pseudo_id"]
STB = ST[ST$pseudo_id %in% idB$pseudo_id, ]
ST_id <- split(STB, STB$pseudo_id)
for(id in idB$pseudo_id){
  ST_loop = ST_id[[as.character(id)]]
  ST_loop$Y_lag1 = lag(ST_loop$Pickups, n = 1, default = NA)
  ST_loop$B = ifelse(ST_loop$Phase == "Treatment",1,0)
  ST_loop$X = ifelse(ST_loop$Day == "Sa" | ST_loop$Day == "Su",0,1)
  ST_id[[as.character(id)]] <- ST_loop
}
model2 = glm(Pickups ~ log(Y_lag1) + B + X,
             offset=log(Tot.Scr.Time),
             family=poisson(link="log"),data=ST_id[["2"]][-1,])
model3 = glm(Pickups ~ log(Y_lag1) + B + X,
             offset = log(Tot.Scr.Time),
             family=poisson(link="log"),data=ST_id[["3"]][-1,])
model4 = glm(Pickups ~ log(Y_lag1) + B + X,
             offset = log(Tot.Scr.Time),
             family=poisson(link="log"),data=ST_id[["4"]][-1,])
model5 = glm(Pickups ~ log(Y_lag1) + B + X,
             offset = log(Tot.Scr.Time),
             family=poisson(link="log"),data=ST_id[["5"]][-1,])
model8 = glm(Pickups ~ log(Y_lag1) + B + X,
             offset = log(Tot.Scr.Time),
             family=poisson(link="log"),data=ST_id[["8"]][-1,])
model15 = glm(Pickups ~log(Y_lag1) + B + X,
              offset = log(Tot.Scr.Time),
              family=poisson(link="log"),data=ST_id[["15"]][-1,])
model16= glm(Pickups ~log(Y_lag1) + B + X,
             offset = log(Tot.Scr.Time),
             family=poisson(link="log"),data=ST_id[["16"]][-1,])
model18 = glm(Pickups ~log(Y_lag1) + B + X,
              offset = log(Tot.Scr.Time),
              family=poisson(link="log"),data=ST_id[["18"]][-1,])
models <- list(model2, model3, model4, model5, model8, model15, model16, model18)
estimates0 <- numeric(length(models))
SE0 <- numeric(length(models))
estimatesY <- numeric(length(models))
SEY <- numeric(length(models))
estimatesB <- numeric(length(models))
SEB <- numeric(length(models))
estimatesX <- numeric(length(models))
SEX <- numeric(length(models))
for (i in seq_along(models)) {
  model_summary <- summary(models[[i]])
  estimates0[i] <- coef(models[[i]])["(Intercept)"]
  SE0[i] <- model_summary$coefficients["(Intercept)", "Std. Error"]
  estimatesY[i] <- coef(models[[i]])["log(Y_lag1)"]
  SEY[i] <- model_summary$coefficients["log(Y_lag1)", "Std. Error"]
  estimatesB[i] <- coef(models[[i]])["B"]
  SEB[i] <- model_summary$coefficients["B", "Std. Error"]
  estimatesX[i] <- coef(models[[i]])["X"]
  SEX[i] <- model_summary$coefficients["X", "Std. Error"]
}
meta = function(beta,SE){
  numerator = 0
  demorator = 0
  for(i in seq_along(models)){
    numerator = numerator + beta[i]/(SE[i])^2
    demorator = demorator + 1/(SE[i])^2
  }
  beta = numerator/demorator
  variance = 1 / demorator
  return(list(beta = beta, SE = sqrt(variance)))
}
beta0 = meta(estimates0,SE0)
betaY = meta(estimatesY,SEY)
betaB = meta(estimatesB,SEB)
betaX = meta(estimatesX,SEX)
meta_df <- data.frame(
  beta = c(beta0$beta, betaY$beta,betaB$beta,betaX$beta),
  variance = c(beta0$SE, betaY$SE,betaB$SE,betaX$SE)
)
rownames(meta_df) <- c("Intercept", "Log(LagY)", "B", "X")
meta_df
t_statistics = betaB$beta / betaB$SE
n = nrow(STB)
p_value <- 2 * (1 - pt(abs(t_statistics), n - 4))
cat("pvalue = ",p_value ,"\n")
baseline = read_excel("./ScreenTime-hw3Q3.xlsx",sheet = 2)
ST = read_excel("./ScreenTime-hw3Q3.xlsx",sheet = 1)

idB = baseline[baseline$Treatment == "B","pseudo_id"]
STB = ST[ST$pseudo_id %in% idB$pseudo_id, ]
STB_with_baseline <- left_join(STB, baseline, by = "pseudo_id")

idA = baseline[baseline$Treatment == "A","pseudo_id"]
STA = ST[ST$pseudo_id %in% idA$pseudo_id, ]
STA_with_baseline <- left_join(STA, baseline, by = "pseudo_id")

STB_with_baseline <- STB_with_baseline %>%
  group_by(pseudo_id) %>%
  mutate(Y_lag1 = lag(Pickups, n = 1, default = NA)) %>%
  ungroup()
STB_with_baseline$Trt = ifelse(STB_with_baseline$Phase == "Treatment",
                               1,0)
STB_with_baseline$X = ifelse(STB_with_baseline$Day == "Sa"| 
                               STB_with_baseline$Day =="Su",
                             0,1)

STA_with_baseline <- STA_with_baseline %>%
  group_by(pseudo_id) %>%
  mutate(Y_lag1 = lag(Pickups, n = 1, default = NA)) %>%
  ungroup()
STA_with_baseline$Trt = ifelse(STA_with_baseline$Phase == "Treatment",
                               1,0)
STA_with_baseline$X = ifelse(STA_with_baseline$Day == "Sa"| 
                               STA_with_baseline$Day =="Su",
                             0,1)

modelA = glm(Pickups ~ log(Y_lag1) + Trt + X + sex + 
               age + pets + siblings +offset(log(Tot.Scr.Time)),
             family=poisson(link="log"),data=STA_with_baseline)
modelB = glm(Pickups ~ log(Y_lag1) + Trt + X + sex + 
               age + pets + siblings +offset(log(Tot.Scr.Time)),
             family=poisson(link="log"),data=STB_with_baseline)
estimateA = summary(modelA)$coefficients[,"Estimate"]
estimateB = summary(modelB)$coefficients[,"Estimate"]
stdA = summary(modelA)$coefficients[,"Std. Error"]
stdB = summary(modelB)$coefficients[,"Std. Error"]

estimatesMeta <- numeric(8)
SEMeta <- numeric(8)
for(i in seq_along(estimateA)){
  numerator = estimateA[i]/(stdA[i])^2 + estimateB[i]/(stdB[i])^2
  demorator = 1/(stdA[i])^2 + 1/(stdB[i])^2
  estimatesMeta[i] = numerator / demorator
  SEMeta[i] = 1/sqrt(demorator)
}
meta_df <- data.frame(
  beta = estimatesMeta,
  StdError = SEMeta
)
rownames(meta_df) <- c("Intercept", "Log(LagY)", "Trt", "X",
                       "sex","age","pets","siblings")
meta_df
BetaTrt = meta_df["Trt","beta"]
SETrt = meta_df["Trt","StdError"]
t_statistics = BetaTrt/SETrt
n = nrow(STA_with_baseline) + nrow(STB_with_baseline)
p_value <- 2 * (1 - pt(abs(t_statistics), n - 8))
p_value
cat("pvalue = ",p_value,"\n")
dataAB = rbind(STA_with_baseline, STB_with_baseline)
dataAB = na.omit(dataAB)
dataAB$Rt = ifelse(dataAB$Phase == "Treatment",1,0)
model = glm(Pickups ~ log(Y_lag1) + Rt + X + sex + 
              age + pets + siblings +offset(log(Tot.Scr.Time)),
            family=poisson(link="log"),data=dataAB)
summary(model)
