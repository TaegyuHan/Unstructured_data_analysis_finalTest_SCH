
# --------------------------------------------------------------------------- #
# 산술통계와, Peak에서만든 TEST 데이터를 사용합니다.
# --------------------------------------------------------------------------- #

makePKSTTestData <- function(P_TestData, S_TestData)
{
  # ---------------------------------------------------- # 
  # 산술통계 TEST 데이터와 Peak TEST 데이터를 합쳐줍니다.
  # ---------------------------------------------------- #
  
  nameslist <- c()
  
  for (i in 85:75){
    nameslist <- c(nameslist, paste0("nrow",i))
  }
  
  PeakstatisticTestData <- cbind(s_TestData[c("mean", "min")],
                                 P_TestData[nameslist],
                                 event = P_TestData$event)
  
  return(PeakstatisticTestData)
}

# TEST 데이터
PS_TestData <- makeStatisticsTestData(P_TestData, S_TestData)

# setwd("현재 파일 경로")
# 모델
RFModelPKST <- get(load("./model/RFModelPKST.rda"))

# 예측
predPKST <- predict(RFModelPKST, newdata = PS_TestData[1:13])

AllDataPKST <- predShowConfusionMatrix(PeakstatisticTestData$event, predPKST)