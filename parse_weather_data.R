#' @author skydome20

# 來源網站： http://e-service.cwb.gov.tw/HistoryDataQuery/index.jsp #
# 區間：2015/01/01 ~ 2016/05/31: 2hrs(run time) #
# 用來parse 台灣氣象局資料的程式碼
my_Parse_Weather <- function(url){
  
  # parse html file by a given url
  parsedHtml <- htmlTreeParse(url, useInternalNodes=T, encoding="UTF-8")
  # 只取時間、氣壓、氣溫、濕度、風速、風向、降水量的欄位
  colName <- xpathSApply(parsedHtml, "//div//table//tbody//tr//th",xmlValue)[c(10,11,13,15,16,17,20)]
  # 找出以上欄位，在html中對應的標籤
  weatherInfo <- xpathSApply(parsedHtml, "//div//table//tbody//tr//td",xmlValue)
  table <- data.frame(
    # 時間
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 1]),
    
    # 氣壓
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 2]),
    
    # 氣溫
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 4]),
    
    # 濕度
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 6]),
    
    # 風速
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 7]),
    
    # 風向
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 8]),
    
    # 降水量
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 11])
  )
  
  colnames(table) <- colName
  table
}


#### 1. 直接下載歷史天氣資料 ####
# 2015/01/01 ~ 2016/05/31: 1.933hrs #
dir.create("D:/ETC_Data", showWarnings = FALSE)
dir.create("D:/ETC_Data/Weather", showWarnings = FALSE)

t <- Sys.time() # 紀錄執行時間

url <- "http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=467080&datepicker=2015-03-23"

require(XML)

# 氣象資料的時間區間
start.time <- "2015/1/1"
end.time <- "2016/5/31"
time.series <- as.character(seq(from=as.Date(start.time), to=as.Date(end.time), "day"))

# 各城市監測站的編號，用於下面網址，擷取該城市歷史天氣資料
monitor.id <- c("466920", "466880","467050","467490","467410","467440","466940","C0D660",
                "467571","C0E420","C0G640","C0K240","467480","C0R170","467080","467650")

# 建立以城市為單位的資料夾，儲存該城市的歷史天氣資料 (create city directory which stores weather data)
city.name <- c("台北市","新北市","桃園市","台中市","台南市","高雄市","基隆市","新竹市",
               "新竹縣","苗栗縣","彰化縣","雲林縣","嘉義市","屏東縣","宜蘭縣","南投縣")
for(city in city.name){
  dir.create(paste("D:/ETC_Data/Weather/", city, sep=""))
}


# 擷取氣象局的歷史資料(get history weather data)

for(i in 1:length(monitor.id)){ # 地區
  error.day <- vector() # 準備紀錄出現錯誤的日期
  
  path1 <- paste("D:/ETC_Data/Weather", city.name[i], sep="/") # 地區的資料夾
  
  # 該城市的天氣資料存取
  for(date in time.series){ # 日期
    path2 <- paste(path1, paste(date,".csv", sep=""), sep="/") # 日期的檔案
    
    # 要存取的網址(監測站編號+日期)
    url <- paste("http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=",
                 monitor.id[i],     # monitor.id
                 "&datepicker=",
                 date,              # date
                 sep="")
    
    
    # 存取網站資料，會因為網路/流量等原因，而出現存取錯誤
    # 當發生這樣的情況時，為避免R當掉而不繼續執行，故用tryCatch進行error handle
    #---------------------------------try catch-----------------------------------------------------#
    possible.error <- tryCatch( ## error handle ##
      # thing
      {
        table <- my_Parse_Weather(url)          # 將parse html的結果，存成一個table
        write.csv(table, path2, row.names = F)  # table寫出來成csv
        
        #Sys.sleep(0.01) # 避免存取網站速度過快，導致網站流量太大，回傳error
      },
      
      # error
      error=function(e){
        e
      }
    )
    #-----------------------------------------------------------------------------------------------#
    
    # 當error發生時，紀錄日期
    if(inherits(possible.error, "error")){
      error.day <- append(error.day, date)
    }
    
  } #時間for-loop end
  
  # 紀錄遺漏的日期
  cur.time <- format(Sys.time(), "%Y-%m-%d_%H%M")
  path3 <- paste(path1, paste(cur.time,"_errorDay.txt", sep=""), sep="/")
  write(error.day, path3)
  
} # 地區for-loop end

download.weather.time <- Sys.time()-t # 紀錄執行時間


#### 2. 檢查下載好的歷史資料中，是否有遺漏的日期，重新補上資料 ####
# 2015/01/01 ~ 2016/05/31: 1.89hrs #
t <- Sys.time() # 紀錄執行時間

# 有時候存取網站，會因為不知名的原因當掉，因此加上while迴圈，檢查遺漏(error)的日期，並重新補上資料

require(XML)
# 各城市監測站的編號，用於下面網址，擷取該城市歷史天氣資料
monitor.id <- c("466920", "466880","467050","467490","467410","467440","466940","C0D660",
                "467571","C0E420","C0G640","C0K240","467480","C0R170","467080","467650")
# 上面監測站編號所對應的城市
city.name <- c("台北市","新北市","桃園市","台中市","台南市","高雄市","基隆市","新竹市",
               "新竹縣","苗栗縣","彰化縣","雲林縣","嘉義市","屏東縣","宜蘭縣","南投縣")


# 想要 check 的時間區間
start.time <- "2015/1/1"
end.time <- "2016/5/31"
time.series <- as.character(seq(from=as.Date(start.time), to=as.Date(end.time), "day"))

# 開始依照地區/日期存取資料
for(i in 1:length(monitor.id)){
  count <- 0 # 用來離開while迴圈(避免有一個日期真的完全沒資料，陷入無窮迴圈之中)
  
  path1 <- paste("D:/ETC_Data/Weather", city.name[i], sep="/") # 地區的資料夾
  
  # 檢查遺漏的日期
  in.directory.file <- list.files(path1)          # 目前在資料夾裡的日期天氣資料
  need.file <- paste(time.series, ".csv", sep="") # 必須要有的日期天氣資料
  error.file <- need.file[(need.file %in% in.directory.file) == F] # 以上兩者比對，得出應該補上的日期資料
  error.date <- gsub(".csv", "", error.file)      # 遺漏的日期，應該在下面補上
  
  # 加上while迴圈，重新補上遺漏(error)的日期資料
  while(identical(error.date, character(0)) == F){
    error.day <- vector()# 紀錄再次出現遺漏的日期
    
    # 遺漏日期的資料存取
    for(err.d in error.date){ # 遺漏的日期
      path2 <- paste(path1, paste(err.d,".csv", sep=""), sep="/") # 遺漏日期的檔案
      
      # 要存取的網址(變因：監測站編號+日期)
      url <- paste("http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=",
                   monitor.id[i],     # monitor.id
                   "&datepicker=",
                   err.d,             # error date
                   sep="")
      
      
      # 存取網站資料，會因為網路/流量等原因，而出現存取錯誤
      # 當發生這樣的情況時，為避免R當掉而不繼續執行，故用tryCatch進行error handle
      #------------------------------try catch--------------------------------------------------------#
      possible.error <- tryCatch( ## error handle ##
        # thing
        {
          table <- my_Parse_Weather(url)           # 將parse html的結果，存成一個table
          write.csv(table, path2, row.names = F)   # 把table寫出來成csv
          
          #Sys.sleep(0.01) # 避免存取網站速度過快，導致網站流量太大，回傳error
        },
        
        # error
        error=function(e){
          e
        }
      )
      #-----------------------------------------------------------------------------------------------#
      
      # 當error發生時，紀錄日期
      if(inherits(possible.error, "error")){
        error.day <- append(error.day, err.d)
      }
      
    } # 日期的for-loop end
    
    
    # 紀錄遺漏的日期
    cur.time <- format(Sys.time(), "%Y-%m-%d_%H%M")
    path3 <- paste(path1, paste(cur.time,"_errorDay.txt", sep=""), sep="/")
    write(error.day, path3)
    
    # 最後，檢查是否還有遺漏的日期
    in.directory.file <- list.files(path1)
    error.file <- need.file[(need.file %in% in.directory.file) == F]
    
    old.error.date <- error.date                 # 上一次的erorr date
    error.date <- gsub(".csv", "", error.file)   # 這一次的error date
    
    # 本次遺漏日期和上次一樣的話，紀錄一次count
    if(identical(old.error.date, error.date)==T) count <- count + 1 
    # 當同個日期一直取不到資料(150次)，那就離開無窮迴圈
    if(count > 150) break  
    
  } # while-loop end
  
}# 地區的for-loop end


check.time <- Sys.time() - t    # 紀錄執行時間



