#Задание 2
#Выполнил Миколюк И.В. ПАЭ 124
#Требуется создать модель множественной линейной регрессии ночных потоков углекислого газа за осенний период 2013 года по данным измерений методом турбулентной пульсации
#Зададим и проверим деррикторию
#setwd("C:/Group_124/Mikoluk/MikolukIvan/ZAD2L")
#getwd()
#Закачаем и подключим нужные пакеты
#install.packages("tidyverse")
#install.packages("string")
#install.packages("dplyr")
#подключаем библиотеки
library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")    
#читаем данные из файла, пропускаем первую строку, заменяем текстовые 'NA', пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 
eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
#убираем ненужную строку и пустой столбец
eddypro = eddypro[-1, ];eddypro = select(eddypro, -(roll))
#преобразуем строковые значения в факторные
eddypro = eddypro %>% mutate_if(is.character, factor)
#Производим замену конфликтующих знаков колонок
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")
#Проверим столбцы в виде векторов
glimpse(eddypro)
# уберем na, так как данные значения будут только мешать работе
eddypro = drop_na(eddypro)
#Осенний период(1 сентября - 30 ноября)ночное время
eddypro = filter(eddypro, DOY >= 243 & DOY < 334, daytime==FALSE)
#указываем, что у нас ночное время 
#переменные типов numeric(только с числами) и  non numeric(с остальными колонками) отдельно
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]
#создадим непересекающиеся выборки
row_numbers = 1:length(eddypro_numeric$co2_flux)
#Обучающая
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
#Тестирующая
test = row_numbers[-teach]
#Обучающая 
teaching_tbl = eddypro_numeric[teach,]
#Тестирующая
testing_tbl = eddypro_numeric[test,]

# МОДЕЛЬ 1 по обучающей выборке
mod1 = lm(co2_flux~ (.) , data = teaching_tbl)
#коэффициенты
coef(mod1)
#остатки
resid(mod1)
#доверительный интервал
confint(mod1)
#P-значения по модели
summary(mod1)
#дисперсионный анализ
anova(mod1)
#графическое представление модели:
plot(mod1)
#необходимо подобрать модель, отсеев наименее значимые переменные, получив оптимальную модель, отражающую зависимость искомой переменный от нескольких наиболее значимых показателей
# МОДЕЛЬ 2 
mod2 = lm ( co2_flux~ DOY  + Tau + qc_Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux + rand_err_co2_flux
            + h2o_flux + rand_err_h2o_flux + H_strg + co2_v_minus_adv  + h2o_v_minus_adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density
            + h2o_mole_fraction + h2o_mixing_ratio + air_temperature + air_pressure + air_density + air_heat_capacity + e 
            + specific_humidity  + VPD + Tdew + u_unrot + w_unrot + u_rot + v_rot  + w_rot + max_speed + yaw + pitch + u_star_ + TKE + `_z_minus_d__div_L` 
            + T_star_ + x_peak + x_offset +  x_50_perc_ + un_Tau  + Tau_scf + un_H + H_scf + LE_scf + un_co2_flux  + h2o_spikes + co2_1  + co2_signal_strength_7200, data = teaching_tbl)
#коэффициенты
coef(mod2)
#остатки
resid(mod2)
#доверительный интервал
confint(mod2)
#P-значения по модели
summary(mod2)
#дисперсионный анализ
anova(mod2)
# Сравниваем 1 и 2 модель
anova(mod2, mod1)
#графическое представление модели:
plot(mod2) 

# МОДЕЛЬ 3 
mod3 = lm ( co2_flux~ DOY + qc_Tau + rand_err_Tau + H + rand_err_H + qc_LE + rand_err_LE + qc_co2_flux + rand_err_co2_flux
            + h2o_flux + rand_err_h2o_flux + co2_mole_fraction  + h2o_mole_fraction + air_heat_capacity + e
            + u_unrot  + u_rot  + pitch  + `_z_minus_d__div_L` + T_star_ + un_H + H_scf  + un_co2_flux, data = teaching_tbl)

#коэффициенты
coef(mod3)
#остатки
resid(mod3)
#доверительный интервал
confint(mod3)
#P-значения по модели
summary(mod3)
#дисперсионный анализ
anova(mod3)
anova(mod3, mod2)
#графическое представление модели:
plot(mod3)
#Проведем корреляционный анализ переменных
# Из таблицы возьмем только учавствующие переменные линейной модели
cor_teaching_tbl = select(teaching_tbl ,co2_flux , DOY , qc_Tau , rand_err_Tau , H , rand_err_H , qc_LE , rand_err_LE , qc_co2_flux , rand_err_co2_flux, h2o_flux , rand_err_h2o_flux
                          , co2_mole_fraction  , h2o_mole_fraction , air_heat_capacity , e, u_unrot  , u_rot  , pitch  , `_z_minus_d__div_L` , T_star_ , un_H , H_scf  , un_co2_flux)
#Таблица коэф. корреляции 
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

##########Проверка моделей
#Изпользуя модель 3, наложим её предсказанные значения 
#Первый график
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
#Второй график
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
#Модель зависит от множества переменных, мы можем вывести много графиков зависимостей co2_flux от учитываемых в моделе параметров
#В идеале предсказанная линия должна пройти через все точки, или как можно ближе к ним на ТЕСТИРУЮЩЕЙ выборке
#Примеры
qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

