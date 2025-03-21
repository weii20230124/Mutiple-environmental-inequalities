#N
library(nlme)
library(lme4)
library(piecewiseSEM)
library(QuantPsyc)
library(glmmTMB)

# 读取数据
# data <- read.csv(file.choose(), header = TRUE, row.names = 1)
data <- read.csv("D:\\昆士兰\\SEM-100-total\\SEM-N-v2.csv", header = TRUE, row.names = 1)
data <- scale(data)
data <- as.data.frame(data)

# 合成最终变量 RE
data <- mutate(data, GR = GR1 + GR2 + GR3 )
data <- mutate(data, PM25 = PM2.5_1 + PM2.5_2 + PM2.5_3 + PM2.5_4)
# 创建线性混合效应模型,大数据库
# 第一个类型比如气候条件
model1 <- glmmTMB(GR ~ ALS + P1 + P2 + P3 + T1 + T2 + T3 + (1 | site), data = data)
summary(model1)
model11 <- glmmTMB(PM25 ~ ALS + P1 + P2 + P3 + T1 + T2 + T3 + (1 | site), data = data)
summary(model11)
# 第二个类型，比如污水来源 PW UW YW NW
model2 <- glmmTMB(GR ~ CO2_1 + CO2_2 + CO2_3 + (1 | site), data = data)
summary(model2)
model22 <- glmmTMB(PM25 ~ CO2_1 + CO2_2 + CO2_3 + (1 | site), data = data)
summary(model22)
# 第三个类型，比如用地类型 UA CA AA FA GA
model3 <- glmmTMB(GR ~ pop1 + pop2 + pop3 + (1 | site), data = data)
summary(model3)
model33 <- glmmTMB(PM25 ~ pop1 + pop2 + pop3 + (1 | site), data = data)
summary(model33)
# 第四个类型变量，比如植被 Q1 Q2 Q3 Q4 Q5 Q6
model4 <- glmmTMB(GR ~ GDP1 + GDP2 + GDP3 + (1 | site), data = data)
summary(model4)
model44 <- glmmTMB(PM25 ~ GDP1 + GDP2 + GDP3 + (1 | site), data = data)
summary(model44)
# 第五个类型变量，比如微生物 Y1 Y2
model5 <- glmmTMB(GR ~ area1 + area2 + area3 + (1 | site), data = data)
summary(model5)
model55 <- glmmTMB(PM25 ~ area1 + area2 + area3 + (1 | site), data = data)
summary(model55)

model6 <- glmmTMB(GR ~ light + (1 | site), data = data)
summary(model6)
model66 <- glmmTMB(PM25 ~ light + (1 | site), data = data)
summary(model66)

model7 <- glmmTMB(GR ~ income + (1 | site), data = data)
summary(model7)
model77 <- glmmTMB(PM25 ~ income + (1 | site), data = data)
summary(model77)
# 计算交互项
Rt <- mutate(data, 
             Geo = scale(ALS) * scale(P1) * scale(P2) * scale(P3) * scale(T1) * scale(T2) * scale(T3),
             pop = scale(pop1) * scale(pop2) * scale(pop3),
             GDP = scale(GDP1) * scale(GDP2) * scale(GDP3),
             area = scale(area1) * scale(area2) * scale(area3),
             light = scale(light),
             income = scale(income),
             CO2 = scale(CO2_1) * scale(CO2_2) * scale(CO2_3),
             GR = scale(GR1) * scale(GR2) * scale(GR3),
             PM25 = scale(PM2.5_4) * scale(PM2.5_1) * scale(PM2.5_2) * scale(PM2.5_3)
)

write.csv(Rt, "D:/昆士兰/SEM-100-total/RtN1.csv")
Rt <- read.csv("D:/昆士兰/SEM-100-total/RtN1.csv", header = TRUE, row.names = 1)

microbe.list <- list(
  lme(GR ~ Geo + pop + GDP + area + light + income , random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(PM25 ~ Geo + pop + GDP + area + light + income + CO2 +GR, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(pop ~ Geo, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(GDP ~ pop + Geo, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(area ~ pop + Geo + GDP + income, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(light ~ GDP + pop + area + income, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(income ~ GDP + pop + Geo, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(CO2 ~ GDP + income + pop + area + light +GR, random = ~ 1 | site, na.action = na.omit, data = Rt)
  
)

microbe.psem <- as.psem(microbe.list)
(new.summary <- summary(microbe.psem, .progressBar = FALSE))

plot(microbe.psem)








#S
library(nlme)
library(lme4)
library(piecewiseSEM)
library(QuantPsyc)
library(glmmTMB)

# 读取数据
# data <- read.csv(file.choose(), header = TRUE, row.names = 1)
data <- read.csv("D:\\昆士兰\\SEM-100-total\\SEM-S-v2.csv", header = TRUE, row.names = 1)
data <- scale(data)
data <- as.data.frame(data)

# 合成最终变量 RE
data <- mutate(data, GR = GR1 + GR2 + GR3 )
data <- mutate(data, PM25 = PM2.5_1 + PM2.5_2 + PM2.5_3 + PM2.5_4)
# 创建线性混合效应模型,大数据库
# 第一个类型比如气候条件
model1 <- glmmTMB(GR ~ ALS + P1 + P2 + P3 + T1 + T2 + T3 + (1 | site), data = data)
summary(model1)
model11 <- glmmTMB(PM25 ~ ALS + P1 + P2 + P3 + T1 + T2 + T3 + (1 | site), data = data)
summary(model11)
# 第二个类型，比如污水来源 PW UW YW NW
model2 <- glmmTMB(GR ~ CO2_1 + CO2_2 + CO2_3 + (1 | site), data = data)
summary(model2)
model22 <- glmmTMB(PM25 ~ CO2_1 + CO2_2 + CO2_3 + (1 | site), data = data)
summary(model22)
# 第三个类型，比如用地类型 UA CA AA FA GA
model3 <- glmmTMB(GR ~ pop1 + pop2 + pop3 + (1 | site), data = data)
summary(model3)
model33 <- glmmTMB(PM25 ~ pop1 + pop2 + pop3 + (1 | site), data = data)
summary(model33)
# 第四个类型变量，比如植被 Q1 Q2 Q3 Q4 Q5 Q6
model4 <- glmmTMB(GR ~ GDP1 + GDP2 + GDP3 + (1 | site), data = data)
summary(model4)
model44 <- glmmTMB(PM25 ~ GDP1 + GDP2 + GDP3 + (1 | site), data = data)
summary(model44)
# 第五个类型变量，比如微生物 Y1 Y2
model5 <- glmmTMB(GR ~ area1 + area2 + area3 + (1 | site), data = data)
summary(model5)
model55 <- glmmTMB(PM25 ~ area1 + area2 + area3 + (1 | site), data = data)
summary(model55)

model6 <- glmmTMB(GR ~ light + (1 | site), data = data)
summary(model6)
model66 <- glmmTMB(PM25 ~ light + (1 | site), data = data)
summary(model66)

model7 <- glmmTMB(GR ~ income + (1 | site), data = data)
summary(model7)
model77 <- glmmTMB(PM25 ~ income + (1 | site), data = data)
summary(model77)
# 计算交互项
Rt <- mutate(data, 
             Geo = scale(ALS) * scale(P1) * scale(P2) * scale(P3) * scale(T1) * scale(T2) * scale(T3),
             pop = scale(pop1) * scale(pop2) * scale(pop3),
             GDP = scale(GDP1) * scale(GDP2) * scale(GDP3),
             area = scale(area1) * scale(area2) * scale(area3),
             light = scale(light),
             income = scale(income),
             CO2 = scale(CO2_1) * scale(CO2_2) * scale(CO2_3),
             GR = scale(GR1) * scale(GR2) * scale(GR3),
             PM25 = scale(PM2.5_4) * scale(PM2.5_1) * scale(PM2.5_2) * scale(PM2.5_3)
)

write.csv(Rt, "D:/昆士兰/SEM-100-total/RtS1.csv")
Rt <- read.csv("D:/昆士兰/SEM-100-total/RtS1.csv", header = TRUE, row.names = 1)

microbe.list <- list(
  lme(GR ~ Geo + pop + GDP + area + light + income , random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(PM25 ~  GDP + area + income + CO2 +GR, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(pop ~ Geo, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(GDP ~ pop, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(area ~ pop + Geo + GDP + income, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(light ~ GDP + Geo + pop + area + income, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(income ~ GDP + pop + Geo, random = ~ 1 | site, na.action = na.omit, data = Rt),
  lme(CO2 ~ GDP + pop + income + area + light + Geo +GR, random = ~ 1 | site, na.action = na.omit, data = Rt)
)

microbe.psem <- as.psem(microbe.list)
(new.summary <- summary(microbe.psem, .progressBar = FALSE))

plot(microbe.psem)
