#' @author skydome20

# �ӷ������G http://e-service.cwb.gov.tw/HistoryDataQuery/index.jsp #
# �϶��G2015/01/01 ~ 2016/05/31: 2hrs(run time) #
# �Ψ�parse �x�W��H����ƪ��{���X
my_Parse_Weather <- function(url){
  
  # parse html file by a given url
  parsedHtml <- htmlTreeParse(url, useInternalNodes=T, encoding="UTF-8")
  # �u���ɶ��B�����B��šB��סB���t�B���V�B�����q�����
  colName <- xpathSApply(parsedHtml, "//div//table//tbody//tr//th",xmlValue)[c(10,11,13,15,16,17,20)]
  # ��X�H�W���A�bhtml������������
  weatherInfo <- xpathSApply(parsedHtml, "//div//table//tbody//tr//td",xmlValue)
  table <- data.frame(
    # �ɶ�
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 1]),
    
    # ����
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 2]),
    
    # ���
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 4]),
    
    # ���
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 6]),
    
    # ���t
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 7]),
    
    # ���V
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 8]),
    
    # �����q
    gsub("\U00A0", "", weatherInfo[(1:length(weatherInfo)) %% 15 == 11])
  )
  
  colnames(table) <- colName
  table
}


#### 1. �����U�����v�Ѯ��� ####
# 2015/01/01 ~ 2016/05/31: 1.933hrs #
dir.create("D:/ETC_Data", showWarnings = FALSE)
dir.create("D:/ETC_Data/Weather", showWarnings = FALSE)

t <- Sys.time() # ��������ɶ�

url <- "http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=467080&datepicker=2015-03-23"

require(XML)

# ��H��ƪ��ɶ��϶�
start.time <- "2015/1/1"
end.time <- "2016/5/31"
time.series <- as.character(seq(from=as.Date(start.time), to=as.Date(end.time), "day"))

# �U�����ʴ������s���A�Ω�U�����}�A�^���ӫ������v�Ѯ���
monitor.id <- c("466920", "466880","467050","467490","467410","467440","466940","C0D660",
                "467571","C0E420","C0G640","C0K240","467480","C0R170","467080","467650")

# �إߥH��������쪺��Ƨ��A�x�s�ӫ��������v�Ѯ��� (create city directory which stores weather data)
city.name <- c("�x�_��","�s�_��","��饫","�x����","�x�n��","������","�򶩥�","�s�˥�",
               "�s�˿�","�]�߿�","���ƿ�","���L��","�Ÿq��","�̪F��","�y����","�n�뿤")
for(city in city.name){
  dir.create(paste("D:/ETC_Data/Weather/", city, sep=""))
}


# �^����H�������v���(get history weather data)

for(i in 1:length(monitor.id)){ # �a��
  error.day <- vector() # �ǳƬ����X�{���~�����
  
  path1 <- paste("D:/ETC_Data/Weather", city.name[i], sep="/") # �a�Ϫ���Ƨ�
  
  # �ӫ������Ѯ��Ʀs��
  for(date in time.series){ # ���
    path2 <- paste(path1, paste(date,".csv", sep=""), sep="/") # ������ɮ�
    
    # �n�s�������}(�ʴ����s��+���)
    url <- paste("http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=",
                 monitor.id[i],     # monitor.id
                 "&datepicker=",
                 date,              # date
                 sep="")
    
    
    # �s��������ơA�|�]������/�y�q����]�A�ӥX�{�s�����~
    # ���o�ͳo�˪����p�ɡA���קKR�����Ӥ��~�����A�G��tryCatch�i��error handle
    #---------------------------------try catch-----------------------------------------------------#
    possible.error <- tryCatch( ## error handle ##
      # thing
      {
        table <- my_Parse_Weather(url)          # �Nparse html�����G�A�s���@��table
        write.csv(table, path2, row.names = F)  # table�g�X�Ӧ�csv
        
        #Sys.sleep(0.01) # �קK�s�������t�׹L�֡A�ɭP�����y�q�Ӥj�A�^��error
      },
      
      # error
      error=function(e){
        e
      }
    )
    #-----------------------------------------------------------------------------------------------#
    
    # ��error�o�ͮɡA�������
    if(inherits(possible.error, "error")){
      error.day <- append(error.day, date)
    }
    
  } #�ɶ�for-loop end
  
  # ������|�����
  cur.time <- format(Sys.time(), "%Y-%m-%d_%H%M")
  path3 <- paste(path1, paste(cur.time,"_errorDay.txt", sep=""), sep="/")
  write(error.day, path3)
  
} # �a��for-loop end

download.weather.time <- Sys.time()-t # ��������ɶ�


#### 2. �ˬd�U���n�����v��Ƥ��A�O�_����|������A���s�ɤW��� ####
# 2015/01/01 ~ 2016/05/31: 1.89hrs #
t <- Sys.time() # ��������ɶ�

# ���ɭԦs�������A�|�]�������W����]�����A�]���[�Wwhile�j��A�ˬd��|(error)������A�í��s�ɤW���

require(XML)
# �U�����ʴ������s���A�Ω�U�����}�A�^���ӫ������v�Ѯ���
monitor.id <- c("466920", "466880","467050","467490","467410","467440","466940","C0D660",
                "467571","C0E420","C0G640","C0K240","467480","C0R170","467080","467650")
# �W���ʴ����s���ҹ���������
city.name <- c("�x�_��","�s�_��","��饫","�x����","�x�n��","������","�򶩥�","�s�˥�",
               "�s�˿�","�]�߿�","���ƿ�","���L��","�Ÿq��","�̪F��","�y����","�n�뿤")


# �Q�n check ���ɶ��϶�
start.time <- "2015/1/1"
end.time <- "2016/5/31"
time.series <- as.character(seq(from=as.Date(start.time), to=as.Date(end.time), "day"))

# �}�l�̷Ӧa��/����s�����
for(i in 1:length(monitor.id)){
  count <- 0 # �Ψ����}while�j��(�קK���@�Ӥ���u�������S��ơA���J�L�a�j�餧��)
  
  path1 <- paste("D:/ETC_Data/Weather", city.name[i], sep="/") # �a�Ϫ���Ƨ�
  
  # �ˬd��|�����
  in.directory.file <- list.files(path1)          # �ثe�b��Ƨ��̪�����Ѯ���
  need.file <- paste(time.series, ".csv", sep="") # �����n��������Ѯ���
  error.file <- need.file[(need.file %in% in.directory.file) == F] # �H�W��̤��A�o�X���ӸɤW��������
  error.date <- gsub(".csv", "", error.file)      # ��|������A���Ӧb�U���ɤW
  
  # �[�Wwhile�j��A���s�ɤW��|(error)��������
  while(identical(error.date, character(0)) == F){
    error.day <- vector()# �����A���X�{��|�����
    
    # ��|�������Ʀs��
    for(err.d in error.date){ # ��|�����
      path2 <- paste(path1, paste(err.d,".csv", sep=""), sep="/") # ��|������ɮ�
      
      # �n�s�������}(�ܦ]�G�ʴ����s��+���)
      url <- paste("http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=",
                   monitor.id[i],     # monitor.id
                   "&datepicker=",
                   err.d,             # error date
                   sep="")
      
      
      # �s��������ơA�|�]������/�y�q����]�A�ӥX�{�s�����~
      # ���o�ͳo�˪����p�ɡA���קKR�����Ӥ��~�����A�G��tryCatch�i��error handle
      #------------------------------try catch--------------------------------------------------------#
      possible.error <- tryCatch( ## error handle ##
        # thing
        {
          table <- my_Parse_Weather(url)           # �Nparse html�����G�A�s���@��table
          write.csv(table, path2, row.names = F)   # ��table�g�X�Ӧ�csv
          
          #Sys.sleep(0.01) # �קK�s�������t�׹L�֡A�ɭP�����y�q�Ӥj�A�^��error
        },
        
        # error
        error=function(e){
          e
        }
      )
      #-----------------------------------------------------------------------------------------------#
      
      # ��error�o�ͮɡA�������
      if(inherits(possible.error, "error")){
        error.day <- append(error.day, err.d)
      }
      
    } # �����for-loop end
    
    
    # ������|�����
    cur.time <- format(Sys.time(), "%Y-%m-%d_%H%M")
    path3 <- paste(path1, paste(cur.time,"_errorDay.txt", sep=""), sep="/")
    write(error.day, path3)
    
    # �̫�A�ˬd�O�_�٦���|�����
    in.directory.file <- list.files(path1)
    error.file <- need.file[(need.file %in% in.directory.file) == F]
    
    old.error.date <- error.date                 # �W�@����erorr date
    error.date <- gsub(".csv", "", error.file)   # �o�@����error date
    
    # ������|����M�W���@�˪��ܡA�����@��count
    if(identical(old.error.date, error.date)==T) count <- count + 1 
    # ���P�Ӥ���@����������(150��)�A���N���}�L�a�j��
    if(count > 150) break  
    
  } # while-loop end
  
}# �a�Ϫ�for-loop end


check.time <- Sys.time() - t    # ��������ɶ�


