
# --------------------------------------------------------------------------- #
# 사용함수
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

# --------------------------------------------------------------------------- #
# 교수님이 주신 전처리 Data_set 헝태로 함수 인자로 넣어주세요
# --------------------------------------------------------------------------- #

# Data_set


makeStatisticsTestData <- function( inputData=Data_set )
{
  # ---------------------------------------------------- # 
  # 산술통계 TEST 데이터를 return 합니다.
  # ---------------------------------------------------- #
  AllData <- data.frame()
  
  print("Data_set 데이터를 하나로 합치고 있습니다.")
  for (num in 1:length(Data_set)){
    
    AllData <- rbind(AllData, inputData[[num]])  
    print(paste0(num, " > 완료"))
    
  }
  
  print("statisticsTestData 전처리를 시작합니다.")
  statisticsTestData <- data.frame(
    sum = apply(AllData[,1:15000], 1, sum),
    mean = apply(AllData[,1:15000], 1, mean),
    min = apply(AllData[,1:15000], 1, min),
    max = apply(AllData[,1:15000], 1, max),
    geometricMean = apply(AllData[,1:15000], 1, psych::geometric.mean),
    median = apply(AllData[,1:15000], 1, median),
    sd = apply(AllData[,1:15000], 1, stats::sd,),
    skewness = apply(AllData[,1:15000], 1,  e1071::skewness),
    event = as.factor(AllData$event)
  )
  
  print(" statisticsTestData 전처리 완료! ")
  return(statisticsTestData)
}


# TEST 데이터
s_TestData <- makeStatisticsTestData(Data_set)

# setwd("현재 파일 경로")
# 모델
RFModelstatistic <- get(load("./model/RFModelstatistic.rda"))

# 예측
predStatistic <- predict(RFModelstatistic, newdata = s_TestData[1:8])

StatisticCM <- predShowConfusionMatrix(s_TestData$event, predStatistic)




