# --------------------------------------------------------------------------- #
# 데이터 합치기
# --------------------------------------------------------------------------- #


RDATA_PATH <- "C:/Users/gksxo/Desktop/순천향대학교/2학년2학기(3학년_4학년수업)/비정형데이터 분석/github/Unstructured_data_analysis_SCH/기말고사_소스/DATA_Rfile"
DATA_PATH <- "C:/Users/gksxo/Desktop/순천향대학교/2학년2학기(3학년_4학년수업)/비정형데이터 분석/github/Unstructured_data_analysis_SCH/기말고사_소스/DATA"


# 변수 선언
for (fn in list.files()){
  
  # file name
  fn
  
  # values name
  val <- strsplit(strsplit(fn, split= "_")[[1]][2], split=".csv")[[1]]
  
  # 변수 선언
  assign(val, read_csv(fn))
  
}

# 변수 이름 반환 함수
makeDataList <- function()
{
  setwd(DATA_PATH)
  
  dataNameList <- c()
  
  # 변수 리스트
  for (fn in list.files()){
    
    # values name
    val <- strsplit(strsplit(fn, split= "_")[[1]][2], split=".csv")[[1]]
    
    
    dataNameList <- c(dataNameList, val)  
    
  }
  
  return(dataNameList)
}

dataNameList <- makeDataList()


# ---------------------------------------------------------------------------- #
# BP DATA 추출
# ---------------------------------------------------------------------------- #

# 혈압 추출 함수
findBloodPressure <- function(row) 
{
  strsplit(row, split=",")[[1]][2]  
}


# signal 데이터 벡터형으로 추출
for (i in 2:length(dataNameList)) {
  
  print(dataNameList[i])
  
  # 혈압 데이터 추출
  tmp <- unlist(lapply(get(dataNameList[i])$signal[3:length(get(dataNameList[i])$signal)], findBloodPressure))
  
  rm(list = dataNameList[i])
  
  assign(paste0(dataNameList[i], "_SignalData"), tmp)
  
}


tmp <- unlist(lapply(get(val)$signal[3:length(get(dataNameList[1])$signal)], findBloodPressure))

rm(list="tmp")


for ( val in dataNameList ){
  
  print((val))
  
}

# ---------------------------------------------------------------------------- #
# 저장

setwd(RDATA_PATH)

# save(slp01a_SignalData, file = "slp01a_SignalData.RData")
# save(slp01b_SignalData, file = "slp01b_SignalData.RData")
# save(slp02a_SignalData, file = "slp02a_SignalData.RData")
# save(slp02b_SignalData, file = "slp02b_SignalData.RData")
# save(slp03_SignalData, file = "slp03_SignalData.RData")
# save(slp04_SignalData, file = "slp04_SignalData.RData")
# save(slp14_SignalData, file = "slp14_SignalData.RData")
# save(slp16_SignalData, file = "slp16_SignalData.RData")
# save(slp32_SignalData, file = "slp32_SignalData.RData")
# save(slp37_SignalData, file = "slp37_SignalData.RData")
# save(slp41_SignalData, file = "slp41_SignalData.RData")
# save(slp45_SignalData, file = "slp45_SignalData.RData")
# save(slp48_SignalData, file = "slp48_SignalData.RData")
# save(slp59_SignalData, file = "slp59_SignalData.RData")
# save(slp60_SignalData, file = "slp60_SignalData.RData")
# save(slp61_SignalData, file = "slp61_SignalData.RData")
# save(slp66_SignalData, file = "slp66_SignalData.RData")
# save(slp67x_SignalData, file = "slp67x_SignalData.RData")

# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# 데이터 load

# load("slp01a_SignalData.RData")
# load("slp01b_SignalData.RData")
# load("slp02a_SignalData.RData")
# load("slp02b_SignalData.RData")
# load("slp03_SignalData.RData")
# load("slp04_SignalData.RData")
# load("slp14_SignalData.RData")
# load("slp16_SignalData.RData")
# load("slp32_SignalData.RData")
# load("slp37_SignalData.RData")
# load("slp41_SignalData.RData")
# load("slp45_SignalData.RData")
# load("slp48_SignalData.RData")
# load("slp59_SignalData.RData")
# load("slp60_SignalData.RData")
# load("slp61_SignalData.RData")
# load("slp66_SignalData.RData")
# load("slp67x_SignalData.RData")

# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# 교수님 코드


ma <- function(x, n = 5)
{
  stats::filter(x, rep(1 / n, n), sides = 2)
}


SRATE <- 250
MINUTES_AHEAD <- 1
Data_set <- list() # 샘플 생성후 저장할 공간


for (file in dataNameList){
  
  print(paste0(file, "_SignalData"))
  
  IBP <- as.numeric(get(paste0(file, "_SignalData")))
  # IBP <- as.numeric(slp01a_SignalData)
  
  i <- 1
  
  IBP_data<-data.frame()
  
  while (i < length(IBP) - SRATE*(1+1+MINUTES_AHEAD)*60){
    
    segx <- IBP[i:(i+SRATE*1*60-1)]
    segy <- IBP[(i+SRATE*(1+MINUTES_AHEAD)*60):(i+SRATE*(1+1+MINUTES_AHEAD)*60-1)]
    segxd <- IBP[i:(i+SRATE*(1+MINUTES_AHEAD)*60-1)]
    
    if(is.na(mean(segx)) |
       is.na(mean(segy)) |
       max(segx)>200 | min(segx)<20 |
       max(segy)>200 | max(segy)<20 |
       max(segx) - min(segx) < 30 |
       max(segy) - min(segy) < 30|(min(segxd,na.rm=T) <= 50)){
    }
    else{ #나머지의 경우
      # segy <- ma(segy, 2*SRATE)
      event <- ifelse(min(segy,na.rm=T) <= 50, 1, 0)
      # print(event)
      IBP_data<- rbind(IBP_data, cbind(t(segx), event))
    }
    
    i <- i+1*60*SRATE
  }
  
  Data_set[[file]] <- IBP_data
}

# setwd(RDATA_PATH)
# save(Data_set, file ="Data_set.RData")

# ---------------------------------------------------------------------------- #



# --------------------------------------------------------------------------- #
# Data 하나로 합치기

#Data_set

for (fn in dataNameList){
  print(paste0("Data_set$", fn))
}

AllData <- rbind( Data_set$slp01a,
                  Data_set$slp01b,
                  Data_set$slp02a,
                  Data_set$slp02b,
                  Data_set$slp03,
                  Data_set$slp04,
                  Data_set$slp14,
                  Data_set$slp16,
                  Data_set$slp32,
                  Data_set$slp37,
                  Data_set$slp41,
                  Data_set$slp45,
                  Data_set$slp48,
                  Data_set$slp59,
                  Data_set$slp60,
                  Data_set$slp61,
                  Data_set$slp66,
                  Data_set$slp67x )

setwd(RDATA_PATH)

# save(AllData, file = "AllData.RData")

# AllData

# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# 정상과 저혈압 데이터를 나눔

NormalData <- AllData %>% 
  filter(event == 0) %>% nrow
# 2067 rows


lowData <- AllData %>% 
  filter(event == 1) %>% nrow
# 22 rows

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Train 데이터 Test데이터 나누기

trainIndex <- as.numeric(createDataPartition(AllData$event,p=2/3, list = F))

TrainData <- AllData[trainIndex, ]
TestData <- AllData[-trainIndex, ]

TrainData %>% nrow()
TrainData %>% ncol()
# > TrainData %>% nrow()
# [1] 2662
# > TrainData %>% ncol()
# [1] 15001

TestData %>% nrow()
TestData %>% ncol()
# > TestData %>% nrow()
# [1] 1331
# > TestData %>% ncol()
# [1] 15001

# setwd(RDATA_PATH)

# save(TrainData, file = "TrainData.RData")
# save(TestData, file = "TestData.RData")

# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Up sampleling
TrainUPData <- groupdata2::upsample(
  TrainData, # data
  cat_col = "event"
)

# save(TrainUPData, file = "TrainUPData.RData")
# --------------------------------------------------------------------------- #





