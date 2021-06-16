# --------------------------------------------------------------------------- #
# 시각화 png 저장 함수

# 1920, 1017

saveggplot <- function(plot, fileName, width, height)
{
  png(
    filename=paste0(fileName, ".png"),
    width=width,
    height=height,
    unit="px" )
  
  print( plot )
  
  dev.off()
}

# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# 산술 통계 분석 함수

# 산술 통계 전처리 후
# hist 그래프로 이미지를 저장합니다.

ArithmeticStatFuc <- function( inputData = AllData,
                               AppliedFunction, StatisticalName )
{
  # 데이터 전처리
  tmpDataFrame <- data.frame(
    tmpData = apply(inputData[,1:15000], 1, AppliedFunction),
    event = as.factor(inputData$event)
  )
  
  # 데이터 시각화
  tmpDataPlot <- tmpDataFrame %>% 
    ggplot( aes(x=tmpData, fill=event)) +
    geom_histogram(alpha=0.6, position = 'identity') + 
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_ipsum() +
    labs(fill="") + 
    ggtitle(paste0(StatisticalName, "DataFrame"))
  
  # 시각화 저장
  
  
  saveggplot(tmpDataPlot, StatisticalName, 1920, 1017)
}

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 데이터의 총 합 시각화 비교

ArithmeticStatFuc( inputData = TrainUPData,
                   sum,
                   "sum" )

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 데이터의 총 평균 시각화 비교

ArithmeticStatFuc( inputData = TrainUPData,
                   mean,
                   "mean" )

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 데이터의 총 최소값 시각화 비교

ArithmeticStatFuc( inputData = TrainUPData,
                   min,
                   "min" )

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 데이터의 총 최대값 시각화 비교

ArithmeticStatFuc( inputData = TrainUPData,
                   max,
                   "max" )

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 데이터의 총 기하 평균 시각화 비교

ArithmeticStatFuc( inputData = TrainUPData,
                   psych::geometric.mean,
                   "geometricMean" )

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 데이터의 총 중위값 시각화 비교

ArithmeticStatFuc( inputData = TrainUPData,
                   median,
                   "median" )

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 데이터의 총 표준편차 시각화 비교

ArithmeticStatFuc( inputData = TrainUPData,
                   stats::sd,
                   "Standard Deviation" )

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 데이터의 총 왜도 시각화 비교

ArithmeticStatFuc( inputData = TrainUPData,
                   e1071::skewness,
                   "skewnesss" )

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 모델 데이터 

# train
statisticsPreProcessTrainData <- data.frame(
  sum = apply(TrainUPData[,1:15000], 1, sum),
  mean = apply(TrainUPData[,1:15000], 1, mean),
  min = apply(TrainUPData[,1:15000], 1, min),
  max = apply(TrainUPData[,1:15000], 1, max),
  geometricMean = apply(TrainUPData[,1:15000], 1, psych::geometric.mean),
  median = apply(TrainUPData[,1:15000], 1, median),
  sd = apply(TrainUPData[,1:15000], 1, stats::sd,),
  skewness = apply(TrainUPData[,1:15000], 1,  e1071::skewness),
  event = as.factor(TrainUPData$event)
)

# Test
statisticsPreProcessTestData <- data.frame(
  sum = apply(TestData[,1:15000], 1, sum),
  mean = apply(TestData[,1:15000], 1, mean),
  min = apply(TestData[,1:15000], 1, min),
  max = apply(TestData[,1:15000], 1, max),
  geometricMean = apply(TestData[,1:15000], 1, psych::geometric.mean),
  median = apply(TestData[,1:15000], 1, median),
  sd = apply(TestData[,1:15000], 1, stats::sd,),
  skewness = apply(TestData[,1:15000], 1,  e1071::skewness),
  event = as.factor(TestData$event)
)

# save(statisticsPreProcessTrainData, file="statisticsPreProcessTrainData.RData")
# save(statisticsPreProcessTestData, file="statisticsPreProcessTestData.RData")

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #

# 모델 호출
RF <- RWeka::make_Weka_classifier("weka/classifiers/trees/RandomForest")

RFModelstatistic <- RF(as.factor(event)~., data=statisticsPreProcessTrainData)

summary(RFModelstatistic)

Folds10 <- evaluate_Weka_classifier(RFModelstatistic, 
                                    numFolds = 10, complexity = TRUE, class = TRUE)

# 모델 저장
# setwd(MODEL_PATH)
# .jcache(RFModelstatistic$classifier)
# save(RFModelstatistic, file="RFModelstatistic.rda")

# 예측
predStatistic <- predict(RFModelstatistic, newdata = statisticsPreProcessTestData[1:8])

# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #

predShowConfusionMatrix <- function(TargetData, predData)
{
  # ------------------------------------------------ # 
  # confusionMatrix 시각화를 만드는 함수 입니다.
  # ------------------------------------------------ # 
  
  confusionMatrixData <- tibble("target" = TargetData,
                                "prediction" = predData)
  
  basic_table  <- table(confusionMatrixData)
  
  cfm <- as_tibble(basic_table)
  
  cvms::plot_confusion_matrix(cfm, 
                              target_col = "target", 
                              prediction_col = "prediction",
                              counts_col = "n",
                              palette = "Greens" )
}

AllDataCM <- predShowConfusionMatrix(statisticsPreProcessTestData$event, predStatistic)

# 이미지 저장
saveggplot( plot = AllDataCM, fileName = "AllDataCM", width = 600, height = 500)

# --------------------------------------------------------------------------- #
