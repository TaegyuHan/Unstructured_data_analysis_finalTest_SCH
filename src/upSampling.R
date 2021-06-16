# --------------------------------------------------------------------------- #
# 데이터 up 샘플링 작업
# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# 시각화 png 저장 함수

# 화면 크기 : 1920, 1017 
# 

saveggplot <- function(plot, fileName, width, height)
{
  png(filename=fileName,
      width=width,
      height=height,
      unit="px")
  
  print( plot )
  
  dev.off()
}

# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# Up sampleling
AllData2 <- groupdata2::upsample(
  AllData, # data
  cat_col = "event"
)
# --------------------------------------------------------------------------- #


# --------------------------------------------------------------------------- #
# 시각화

# up샘플링 전
upsampleBarPlotB <- data.frame( event = as.factor(AllData$event)) %>% 
                           ggplot() + 
                           geom_bar(aes(event, fill=event)) +
                           ggtitle("Up sample before")

saveggplot( upsampleBarPlotB, "upsampleBarPlotB.png" ,width = 600, height = 500)


# up샘플링 후
upsampleBarPlotA <- data.frame( event = as.factor(AllData$event)) %>% 
                          ggplot() + 
                          geom_bar(aes(event, fill=event)) +
                          ggtitle("Up sample after")

saveggplot( upsampleBarPlotA, "upsampleBarPlotA.png", width = 600, height = 500)


# --------------------------------------------------------------------------- #
