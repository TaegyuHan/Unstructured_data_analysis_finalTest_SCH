
# --------------------------------------------------------------------------- #
# 저혈압 데이터 추출
LowBPData <- TrainData %>% filter(
  event == 1
)

# 저혈압 데이터 추출
NomalBPData <- TrainData %>% filter(
  event == 0
)
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 첫번째 행 데이터

# 저혈압
LowBPDataRowOne <- data.frame(
  xValue = seq(15000),
  BP = as.numeric(LowBPData[1,1:15000])
)

# 정상혈압
NomalBPDataRowOne <- data.frame(
  xValue = seq(15000),
  BP = as.numeric(NomalBPData[1,1:15000])
)

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 저혈압 데이터
LowBPPlot <- LowBPDataRowOne %>%  
  ggplot( aes(x=xValue ,y=BP)) +
  geom_line() + 
  ggtitle("LowBPPlot")

# 이미지 저장
saveggplot( plot = LowBPPlot, fileName = "LowBPPlot", width = 600, height = 500)
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# 정상 데이터 데이터
NomalBPPlot <-  NomalBPDataRowOne %>% 
  ggplot( aes(x=xValue ,y=BP)) +
  geom_line() + 
  ggtitle("NomalBPPlot")

# 이미지 저장
saveggplot( plot = NomalBPPlot, fileName = "NomalBPPlot", width = 600, height = 500)
# --------------------------------------------------------------------------- #




# --------------------------------------------------------------------------- #
# 피크 시각화 함수

MakePeakPlot <- function(LinePlotData, PointPlotData, filename)
{
  # ------------------------------------------------ # 
  # confusionMatrix 시각화를 만드는 함수 입니다.
  # ------------------------------------------------ # 
  
  tmpPeakPlot <- LinePlotData %>% 
    ggplot() +
    geom_line(aes(x=xValue ,y=BP)) +
    geom_point(data = PointPlotData,
               aes(x=PointPlotData[,2], y=PointPlotData[,1]),
               color="red") + 
    ggtitle(paste0(filename))
  
  saveggplot( plot = tmpPeakPlot,
              fileName = filename, width = 600, height = 500)
}

# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# 피크 추출

# 저혈압 
LowPeak <- as.data.frame(pracma::findpeaks(LowBPDataRowOne$BP))

MakePeakPlot(
  LinePlotData = LowBPDataRowOne,
  PointPlotData = LowPeak,
  filename = "LowBPDataRowOne"
)


# 정상 혈압
NomalPeak <- as.data.frame(pracma::findpeaks(NomalBPRowOne$BP))

MakePeakPlot(
  LinePlotData = NomalBPDataRowOne,
  PointPlotData = NomalPeak,
  filename = "NomalBPDataRowOne"
)

# 피크의 수
LowPeak %>% nrow
# [1] 1392

NomalPeak %>% nrow
# [1] 1493
# --------------------------------------------------------------------------- #



# --------------------------------------------------------------------------- #
# 피크 추출

# 시계열 데이터를 뒤집고
# Peak 추출한 시각화

# 저혈압 
LowCHPeak <- as.data.frame(pracma::findpeaks(-LowBPDataRowOne$BP,
                                             zero = "-", # 80이하인 피크만 추출
                                             minpeakheight = -80))
LowBPCHDataRowOne <- data.frame(
  xValue = LowBPDataRowOne$xValue,
  BP = -LowBPDataRowOne$BP
)

MakePeakPlot(
  LinePlotData = LowBPCHDataRowOne,
  PointPlotData = LowCHPeak,
  filename = "LowBPCHDataRowOne"
)


# 정상 혈압
NomalCHPeak <- as.data.frame(pracma::findpeaks(-NomalBPRowOne$BP,
                                               zero = "-", # 80이하인 피크만 추출
                                               minpeakheight = -80))
NomalBPCHDataRowOne <- data.frame(
  xValue = NomalBPRowOne$xValue,
  BP = -NomalBPRowOne$BP
)

MakePeakPlot(
  LinePlotData = NomalBPCHDataRowOne,
  PointPlotData = NomalCHPeak,
  filename = "NomalBPCHDataRowOne"
)


# 피크의 수
NomalCHPeak %>% nrow
# [1] 2664
LowCHPeak %>% nrow
# [1] 2392
# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# Train 피크 전부 추출

# 병렬처리 

# datalist <- list()

# 사용할 코어 갯수와 클러스터 선정
cores <- parallel::detectCores() - 1
cluster <- parallel::makeCluster(spec = cores)

# 클러스터를 등록
doParallel::registerDoParallel(cl = cluster)

for ( num in 100:50 ){
  
    print(num)
  
    datalist[[paste0('v',num)]] <- foreach(i = 1:nrow(TrainUPData),
                                        .packages=c('dplyr','pracma'),
                                        .combine=rbind ) %dopar% {
                                          
      tmp <- pracma::findpeaks(-as.numeric(TrainUPData[i,1:15000]),
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
for (i in 100:50){
  colnames(Peakdatalist[[paste0('v',i)]]) <- c("rowNumber", paste0('nrow',i))
}

# 데이터 합치기
Peakdataframe <- data.frame(
  rowNumber = seq(5264)
)

for (i in 100:50){
  Peakdataframe <- inner_join(Peakdataframe, 
                              as.data.frame(Peakdatalist[[paste0('v',i)]]),
                              by='rowNumber')
}

Peakdataframe[["event"]] <- TrainUPData$event

# 데이터 -1 > 0으로 변경
for (i in 100:50){
  Peakdataframe[[paste0('nrow',i)]][Peakdataframe[[paste0('nrow',i)]]==-1] <- 0
}

# 데이터 저장
# save(Peakdataframe, file = "Peakdatalist.RData")

# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# Peak 예시 시각화

# 데이터 만들기
LowPeakPlotData <- Peakdataframe %>% 
                      filter(evnet ==1) %>%
                      select(2:52) %>% 
                      apply(2,base::mean) %>% t

NomalPeakPlotData <- Peakdataframe %>% 
                       filter(evnet ==0) %>%
                       select(2:52) %>% 
                       apply(2,base::mean) %>% t

PeakLineData <- rbind(LowPeakPlotData, NomalPeakPlotData) %>%
  t %>%
  as.data.frame

colnames(PeakLineData) <- c('LowPeakPlotData', 'NomalPeakPlotData')
PeakLineData["xlabel"] <- 100:50

# 전체
PeakLinePlot <- PeakLineData %>% 
  ggplot(aes(x=xlabel)) +
  geom_line(aes(y=LowPeakPlotData), color="red") +
  geom_point(aes(y=LowPeakPlotData), color="red") +
  geom_line(aes(y=NomalPeakPlotData), color="blue") +
  geom_point(aes(y=NomalPeakPlotData), color="blue") +
  xlim(100,50) +
  ggtitle("minpeakheight plot")

# 이미지 저장
saveggplot( plot = PeakLinePlot, fileName = "PeakLinePlot", width = 600, height = 500)

# 중간 선 그래프 
PeakLinePlotMid <- PeakLineData %>% 
  ggplot(aes(x=xlabel)) +
  geom_line(aes(y=LowPeakPlotData), color="red") +
  geom_point(aes(y=LowPeakPlotData), color="red") +
  geom_line(aes(y=NomalPeakPlotData), color="blue") +
  geom_point(aes(y=NomalPeakPlotData), color="blue") +
  geom_vline(xintercept = c(85, 75)) +
  xlim(100,50) +
  ggtitle("minpeakheight plot")

# 이미지 저장
saveggplot( plot = PeakLinePlotMid, fileName = "PeakLinePlotMid", width = 600, height = 500)


# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# Test 피크 전부 추출
TestData
# 병렬처리 

datalistTest <- list()

# 사용할 코어 갯수와 클러스터 선정
cores <- parallel::detectCores() - 1
cluster <- parallel::makeCluster(spec = cores)

# 클러스터를 등록
doParallel::registerDoParallel(cl = cluster)

for ( num in 100:50 ){
  
  print(num)
  
  datalistTest[[paste0('v',num)]] <- foreach(i = 1:nrow(TestData),
                                     .packages=c('dplyr','pracma'),
                                     .combine=rbind ) %dopar% {
                                       
       tmp <- pracma::findpeaks(-as.numeric(TestData[i,1:15000]),
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
for (i in 100:50){
  colnames(datalistTest[[paste0('v',i)]]) <- c("rowNumber", paste0('nrow',i))
}


# 데이터 합치기
PeakTestdataframe <- data.frame(
  rowNumber = seq(nrow(datalistTest[["v100"]]))
)

for (i in 100:50){
  PeakTestdataframe <- inner_join(PeakTestdataframe, 
                              as.data.frame(datalistTest[[paste0('v',i)]]),
                              by='rowNumber')
}

PeakTestdataframe[["event"]] <- TestData$event

# 데이터 -1 > 0으로 변경
for (i in 100:50){
  PeakTestdataframe[[paste0('nrow',i)]][PeakTestdataframe[[paste0('nrow',i)]]==-1] <- 0
}

# 데이터 저장
# save(PeakTestdataframe, file = "PeakTestdataframe.RData")

# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# RF

# 모델 호출
RF <- RWeka::make_Weka_classifier("weka/classifiers/trees/RandomForest")

RFModelPeak <- RF(as.factor(event)~., data=Peakdataframe[,2:53])

summary(RFModelPeak)

Folds10 <- evaluate_Weka_classifier(RFModelPeak, numFolds = 10, complexity = TRUE, class = TRUE)


# 모델 저장
# setwd(MODEL_PATH)
# .jcache(RFModelPeak$classifier)
# save(RFModelPeak, file="RFModelPeak.rda")


# 예측
Peakpred <- predict(RFModelPeak, newdata = PeakTestdataframe[2:52])


PeackaCM <- predShowConfusionMatrix(PeakTestdataframe$event, Peakpred)

# 이미지 저장
saveggplot( plot = PeackaCM, fileName = "PeackaCM", width = 600, height = 500)

# --------------------------------------------------------------------------- #



# --------------------------------------------------------------------------- #
# NB 모델
# 모델 호출

NBModelPK <- naiveBayes(as.factor(event)~., data=Peakdataframe[,2:53])

summary(NBModelPK)

# 모델 저장
# setwd(MODEL_PATH)
# save(NBModelPK, file="NBModelPK.rda")

# 예측
predNBPK <- predict(NBModelPK, newdata = PeakTestdataframe[2:52])

showNBPK <- predShowConfusionMatrix(PeakTestdataframe$event, predNBPK)

# 이미지 저장
saveggplot( plot = showNBPK, fileName = "showNBPK", width = 600, height = 500)

# --------------------------------------------------------------------------- #


