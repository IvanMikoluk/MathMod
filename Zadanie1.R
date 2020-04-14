#Миколюк Иван, ПАЭ-124
#Вариант 6
#27 регион - Хабароский край, столица - Хабаровск
#Нужно расчитать рассчитайте урожайность пшеницы в 2012 году, взяв для рассчета средние суммы активных температур за предыдущие 9 лет, с 24 ближайших метеостанций
##Для начала проведен общие манипуляции
#Зададим и проверим деррикторию
setwd("C:/Group_124/Mikoluk/MikolukIvan")
getwd()
#Закачаем и подключим нужные пакеты
#install.packages("tidyverse")
#install.packages("rnoaa")
#install.packages("lubridate")
library(tidyverse)
library(rnoaa)
library(lubridate)
###Создадим векторы с данными для расчета: Р°:
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00) #константа
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00) #константа
dfi = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00) #отношение числа дней i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце
Kf = 300 # Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 # сумма частей основной и побочной продукции
Ej = 25 # стандартная влажность культуры 
#скачиваем станции 
#station_data = ghcnd_stations()
station_data = read.csv("station_data.csv")
#после получения всписка всех станций, получите список станций ближайших к точке в регионе(я брал близкой к столице),создав таблицу с именем региона и координатами
Khabarovsk = data.frame(id = "KHABAROVSK", latitude = 48.28355, longitude = 135.34639)
Khabarovsk
#найдем станции, соответствующие критериям
Khabarovsk_around = meteo_nearby_stations(lat_lon_df = Khabarovsk, station_data = station_data,
                                          limit = 24, var = c("PRCP", "TAVG"),
                                          year_min = 2003, year_max = 2012)
#В итоге получаем список(таблицу) с идентификаторами метиостанций по удаленности их от Хабаровска
Khabarovsk_around
# объединим данные, создав цикл для всех метеостанций, выбрав нужное время
all_Khabarovsk_data = tibble() 
for (v in 1:24) 
{ 
  Khabarovsk_id = Khabarovsk_around[["KHABAROVSK"]][["id"]][v] 
  # 
  data = meteo_tidy_ghcnd(stationid = Khabarovsk_id, 
                          var="TAVG", 
                          date_min="2003-01-01", 
                          date_max="2012-12-31") 
  all_Khabarovsk_data = bind_rows(all_Khabarovsk_data, data) 
} 
### произведем обработку полученных данных 
#Добавим для наших данных колонки year, month для группировки
clean_data = all_Khabarovsk_data %>% 
  mutate(year = year(date), month = month(date)) %>% 
  #И сгрупируем с учетом id наших метеостанций
  group_by(year, month, id) %>% 
  # суммирования с учетом id метеостанций
  summarize(tavg = sum(tavg[tavg>5], na.rm=TRUE)/10) %>% 
  # нахождения средних месячных активных температур, сгрупировав данные: 
  group_by(month) %>% 
  summarize(s = mean(tavg, na.rm = TRUE)) %>% 
  # создадим колонки для расчета: 
  mutate (a = afi, b = bfi, d = dfi) %>% 
  # и рассчитаем урожайность для каждого месяца: 
  mutate (fert = ((a + b * s) * d * Kf) / (Qj * Lj * (100-Ej)) ) 
# сумма урожайностей равна: 
Yield = sum(clean_data$fert); Yield
#Для моего 27 региона (Хабаровского края) урожайность пшеницы в 2012 году составила - 16,3152ц/га

