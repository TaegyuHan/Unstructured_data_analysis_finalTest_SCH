# --------------------------------------------------------------------------- #
# Folder Path

RDATA_PATH <- "C:/Users/gksxo/Desktop/순천향대학교/2학년2학기(3학년_4학년수업)/비정형데이터 분석/github/Unstructured_data_analysis_finalTest_SCH/RData"
DATA_PATH <- "C:/Users/gksxo/Desktop/순천향대학교/2학년2학기(3학년_4학년수업)/비정형데이터 분석/github/Unstructured_data_analysis_SCH/기말고사_소스/DATA"
IMG_PATH <-"C:/Users/gksxo/Desktop/순천향대학교/2학년2학기(3학년_4학년수업)/비정형데이터 분석/github/Unstructured_data_analysis_SCH/기말고사_소스/img"
MODEL_PATH <-"C:/Users/gksxo/Desktop/순천향대학교/2학년2학기(3학년_4학년수업)/비정형데이터 분석/github/Unstructured_data_analysis_SCH/기말고사_소스/model"

# --------------------------------------------------------------------------- #

setwd(RDATA_PATH)

# csv의 혈압 데이터 추출한 데이터
load("slp01a_SignalData.RData")
load("slp01b_SignalData.RData")
load("slp02a_SignalData.RData")
load("slp02b_SignalData.RData")
load("slp03_SignalData.RData")
load("slp04_SignalData.RData")
load("slp14_SignalData.RData")
load("slp16_SignalData.RData")
load("slp32_SignalData.RData")
load("slp37_SignalData.RData")
load("slp41_SignalData.RData")
load("slp45_SignalData.RData")
load("slp48_SignalData.RData")
load("slp59_SignalData.RData")
load("slp60_SignalData.RData")
load("slp61_SignalData.RData")
load("slp66_SignalData.RData")
load("slp67x_SignalData.RData")

# 교수님 word Data
load("Data_set.RData")

# list 전부 합친 데이터
load("AllData.RData")

# AllData을 Train과 Test 로 나눈 데이터
load("TestData.RData")
load("TrainData.RData")

# 업 샘플링한 데이터
load("UPAllData.RData")
load("TrainUPData.RData")

# statistics 데이터
load("statisticsPreProcessTrainData.RData")
load("statisticsPreProcessTestData.RData")

# Peak 데이터
load("Peakdatalist.RData") # Train 데이터
load("PeakTestdataframe.RData") # Test 데이터


# statistics & Peak 데이터
load("PeakstatisticTestData.RData") # Train 데이터
load("PeakstatisticTrainData.RData") # Test 데이터

