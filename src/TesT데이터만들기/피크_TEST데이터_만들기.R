
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

makePeakTestData <- function( inputData=Data_set )
{
  # ---------------------------------------------------- # 
  # Peak TEST 데이터를 return 합니다.
  # ---------------------------------------------------- #
  
  AllData <- data.frame()
  
  print("Data_set 데이터를 하나로 합치고 있습니다.")
  for (num in 1:length(inputData)){
    
    AllData <- rbind(AllData, inputData[[num]])  
    print(paste0(num, " > 완료"))
    
  }
  
  # 병렬처리 
  
  print("병렬처리로 Peak 데이터를 추출합니다. 시간이 오래 걸립니다.")
  
  datalistTest <- list()
  
  # 사용할 코어 갯수와 클러스터 선정
  cores <- parallel::detectCores() - 1
  cluster <- parallel::makeCluster(spec = cores)
  
  # 클러스터를 등록
  doParallel::registerDoParallel(cl = cluster)
  
  for ( num in 100:50 ){
    
    print(num)
    
    datalistTest[[paste0('v',num)]] <- foreach(i = 1:nrow(AllData),
                                               .packages=c('dplyr','pracma'),
                                               .combine=rbind ) %dopar% {
                                                 
       tmp <- pracma::findpeaks(-as.numeric(AllData[i,1:15000]),
                                zero = "-", # 80이하인 피크만 추출
                                minpeakheight = -num ) %>% nrow
       
       if(is.null(tmp)) {tmp <- -1}
       
       c( i, tmp )                        
    }
  }
  
  # 클러스터 해제
  parallel::stopCluster(cluster)
  
  # 데이터 합치기
  # col 이름 수정
  print("col 이름 수정")
  for (i in 100:50){
    colnames(datalistTest[[paste0('v',i)]]) <- c("rowNumber", paste0('nrow',i))
  }

  # 데이터 합치기
  print("데이터 합치기")
  PeakTestdataframe <- data.frame(
    rowNumber = seq(nrow(datalistTest[["v100"]]))
  )
  for (i in 100:50){
    PeakTestdataframe <- inner_join(PeakTestdataframe, 
                                    as.data.frame(datalistTest[[paste0('v',i)]]),
                                    by='rowNumber')
  }
  
  PeakTestdataframe[["event"]] <- AllData$event
  
  
  
  # 데이터 -1 > 0으로 변경
  print("데이터 -1 > 0으로 변경")
  for (i in 100:50){
    PeakTestdataframe[[paste0('nrow',i)]][PeakTestdataframe[[paste0('nrow',i)]]==-1] <- 0
  }
  
  return(PeakTestdataframe)
}

P_TestData <- makePeakTestData(Data_set)

# setwd("현재 파일 경로")
# 모델
RFModelPeak <- get(load("./model/RFModelPeak.rda"))

# 예측
predPeak <- predict(RFModelPeak, newdata = s_TestData[1:8])

PeakCM <- predShowConfusionMatrix(P_TestData$event, predPeak)



