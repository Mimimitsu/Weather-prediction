library(pROC)
#��ȡ���ݣ�ע���ȡ·��
unclean1 = read.csv("D:/Data/Mylecture/DATA7001/Group Project/Step2/weatherAUS.csv")
clean1 = unclean1

#��������
clean1[which(clean1$RainToday == 'Yes'), "RainToday"] = 1
clean1[which(clean1$RainToday == 'No'), "RainToday"] = 0
clean1$RainToday = as.numeric(clean1$RainToday)

clean1[which(clean1$RainTomorrow == 'Yes'), "RainTomorrow"] = 1
clean1[which(clean1$RainTomorrow == 'No'), "RainTomorrow"] = 0
clean1$RainTomorrow = as.numeric(clean1$RainTomorrow)

clean1<-subset(clean1,clean1$RainToday != "NA")
clean1<-subset(clean1,clean1$RainTomorrow != "NA")
clean1<-subset(clean1,clean1$MinTemp != "NA")
clean1<-subset(clean1,clean1$WindGustDir != "NA")
clean1<-subset(clean1,clean1$WindGustSpeed != "NA")
clean1<-subset(clean1,clean1$WindDir9am != "NA")
clean1<-subset(clean1,clean1$Humidity3pm != "NA")
clean1<-subset(clean1,clean1$Pressure9am != "NA")
clean1<-subset(clean1,clean1$Pressure3pm != "NA")
clean1<-subset(clean1,clean1$Temp9am != "NA")
clean1<-subset(clean1,clean1$Temp3pm != "NA")

#ȥ����numeric�͵�����
clean1_num <- subset(clean1, select = -c(WindGustDir, WindDir9am, WindDir3pm))

##########################################�в�#####################################

#�����Ӽ�,����AliceSpings��Uluru
mid <- clean1_num[clean1_num$Location == 'AliceSprings'|clean1_num$Location == 'Uluru', ]
mid$RainTomorrow = as.numeric(mid$RainTomorrow)

#����ѵ�����Ͳ��Լ�,����Ϊ7��3
set.seed(33)
sampleidx <- sample(x=1:nrow(mid), size=nrow(mid)*0.7)
mid_train <- mid[sampleidx, ]
mid_test <- mid[-sampleidx, ]

#Uluru <- clean1_num[clean1_num$Location == 'Uluru', ]
#AliceSpings$RainTomorrow = as.numeric(AliceSpings$RainTomorrow)
#Uluru$RainTomorrow = as.numeric(Uluru$RainTomorrow)

#��Ԫ�߼��ع飬�����������Ե�����ԣ�ȥ��pֵ����0.05������
rain_logistic <- glm(RainTomorrow ~ MinTemp + MaxTemp + Evaporation + Humidity9am + Humidity3pm + Pressure9am + Pressure3pm + WindGustSpeed + WindSpeed9am + WindSpeed3pm,
                     data = mid_train, family = binomial)

summary(rain_logistic)

#�ڶ��ζ�Ԫ�߼��ع飬�ó����ģ��
rain_logistic_alt <- glm(RainTomorrow ~ Humidity3pm + Pressure9am + Pressure3pm + WindGustSpeed + WindSpeed3pm,
                     data = mid_train, family = binomial)

summary(rain_logistic_alt)

#ʹ�ò��Լ�mid_test���в��ԣ�����ROC���ж�������
pre_logistic <- as.numeric(predict(rain_logistic_alt, newdata = mid_test, type = "response")>0.5)
obs_p_logistic = data.frame(porb = pre_logistic, obs = mid_test$RainTomorrow)
table(mid_test$RainTomorrow, pre_logistic, dnn = c("Actual Value", "Predict Value"))
library(pROC)
logistic_roc <- roc(mid_test$RainTomorrow, pre_logistic, levels = c(1, 0), direction='>')
plot(logistic_roc, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), grid.col = c('green','red'), max.auc.polygon=TRUE, auc.polygon.col='skyblue', print.thres=TRUE, main='ROC of Logistic Regression')

###############################################����####################################
#�����Ӽ�,����Perth��PerthAirport
west <- clean1_num[clean1_num$Location == 'Perth'|clean1_num$Location == 'PerthAirport', ]
west$RainTomorrow = as.numeric(west$RainTomorrow)

#����ѵ�����Ͳ��Լ�,����Ϊ7��3
set.seed(33)
sampleidx <- sample(x=1:nrow(west), size=nrow(west)*0.7)
west_train <- west[sampleidx, ]
west_test <- west[-sampleidx, ]

#��Ԫ�߼��ع飬�����������Ե�����ԣ�ȥ��pֵ����0.05������
rain_logistic <- glm(RainTomorrow ~ MinTemp + MaxTemp + Evaporation + Humidity9am + Humidity3pm + Pressure9am + Pressure3pm + WindGustSpeed + WindSpeed9am + WindSpeed3pm,
                     data = west_train, family = binomial)

summary(rain_logistic)

#�ڶ��ζ�Ԫ�߼��ع飬�ó����ģ��
rain_logistic_alt <- glm(RainTomorrow ~ MaxTemp + Evaporation + Humidity3pm + Pressure9am + Pressure3pm + WindGustSpeed + WindSpeed3pm,
                         data = west_train, family = binomial)

summary(rain_logistic_alt)

#ʹ�ò��Լ�mid_test���в��ԣ�����ROC���ж�������
pre_logistic <- as.numeric(predict(rain_logistic_alt, newdata = west_test, type = "response")>0.5)
obs_p_logistic = data.frame(porb = pre_logistic, obs = west_test$RainTomorrow)
table(west_test$RainTomorrow, pre_logistic, dnn = c("Actual Value", "Predict Value"))
logistic_roc <- roc(west_test$RainTomorrow, pre_logistic, levels = c(1, 0), direction='>')
plot(logistic_roc, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), grid.col = c('green','red'), max.auc.polygon=TRUE, auc.polygon.col='skyblue', print.thres=TRUE, main='ROC of Logistic Regression')

####################################����###############################################
#�����Ӽ�,����Sydney, SydeneyAirport��WaggaWagga
east <- clean1_num[clean1_num$Location == 'Sydney'|clean1_num$Location == 'SydneyAirport'|clean1_num$Location == 'WaggaWagga', ]
east$RainTomorrow = as.numeric(east$RainTomorrow)

#����ѵ�����Ͳ��Լ�,����Ϊ7��3
set.seed(33)
sampleidx <- sample(x=1:nrow(east), size=nrow(east)*0.7)
east_train <- east[sampleidx, ]
east_test <- west[-sampleidx, ]

#��Ԫ�߼��ع飬�����������Ե�����ԣ�ȥ��pֵ����0.05������
rain_logistic <- glm(RainTomorrow ~ MinTemp + MaxTemp + Evaporation + Humidity9am + Humidity3pm + Pressure9am + Pressure3pm + WindGustSpeed + WindSpeed9am + WindSpeed3pm,
                     data = east_train, family = binomial)

summary(rain_logistic)

#�ڶ��ζ�Ԫ�߼��ع飬�ó����ģ��
rain_logistic_alt <- glm(RainTomorrow ~ Humidity9am + Humidity3pm + Pressure9am + Pressure3pm + WindGustSpeed + WindSpeed3pm,
                         data = east_train, family = binomial)

summary(rain_logistic_alt)

#ʹ�ò��Լ�mid_test���в��ԣ�����ROC���ж�������
pre_logistic <- as.numeric(predict(rain_logistic_alt, newdata = east_test, type = "response")>0.5)
obs_p_logistic = data.frame(porb = pre_logistic, obs = east_test$RainTomorrow)
table(east_test$RainTomorrow, pre_logistic, dnn = c("Actual Value", "Predict Value"))
logistic_roc <- roc(east_test$RainTomorrow, pre_logistic, levels = c(1, 0), direction='>')
plot(logistic_roc, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), grid.col = c('green','red'), max.auc.polygon=TRUE, auc.polygon.col='skyblue', print.thres=TRUE, main='ROC of Logistic Regression')
