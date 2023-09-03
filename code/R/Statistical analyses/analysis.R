setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("SIIE23Scripts/SIIE2023.R")
library(tidyr)
library(xtable)
library(lubridate)

SIIE23doInitDatasets()
SIIE23doLoadSessions()
head(SIIE23RAW)

# ÁNALISIS 1: Fail Ratio

agg_data <- SIIE23RAW %>% group_by(Group, Problem, OutCome) %>% count()
head(agg_data)

agg_data <- pivot_wider(agg_data, names_from = OutCome, values_from = n)
agg_data <- agg_data %>% mutate(all = fail + solved)
agg_data <- agg_data %>% mutate(fr = fail/all)

data <- data.frame(agg_data$Group, agg_data$Problem, agg_data$fr)
data <- data %>% drop_na()
colnames(data) <- c("Group","Problem","fr")
head(data)

LCV_Lime_Theme()
LCV_boxplot(data, "Problem", "fr")

Variable <- "Problem"
Value <- "fr"
data.lm <- lm(data[[Value]]~data[[Variable]], data)
data.aov <- aov(data.lm)
summary <- summary(data.aov)
summary
print(xtable(as.matrix(summary)), include.rownames = TRUE)

LCV_ANOVA(data,"Problem","fr")

data.tukey<-TukeyHSD(data.aov)
data.tukey
print(xtable(as.matrix(data.tukey$`data[[Variable]]`)), include.rownames = TRUE)

LCV_Tomato_Theme()
LCV_confidence_intervals(data, Variable, Value)

kruskal.test(fr ~ Problem, data = data)

# ANÁLISIS 2: Duración

agg_data <- SIIE23RAW
agg_data$SStart <- dmy_hms(agg_data$SStart)
agg_data$SEnd <- dmy_hms(agg_data$SEnd)
head(agg_data)

# Ordenar el dataframe por 'Group' y 'SEnd'
begin_time <- agg_data %>%
  group_by(Group, Problem) %>%
  mutate(start = min(SStart))
begin_time <- unique(begin_time[,c("Group","Problem","start")])
head(begin_time)

end_time <- agg_data %>%  arrange(Group, SEnd)

end_time <- end_time[end_time$OutCome=="solved",] %>% group_by(Group, Problem) %>% slice(1)
head(end_time)
end_time <- end_time[,c("Group","Problem","SEnd")]
nrow(end_time)

# Combinar los datasets por las columnas 'Group' y 'Problem'
dataset <- merge(begin_time, end_time, by = c("Group", "Problem"))
dataset <- dataset %>% mutate(duration = as.numeric(difftime(SEnd, start), units = "hours"))
dataset <- dataset[dataset$duration < 2500,]
head(dataset)

LCV_Lime_Theme()
LCV_boxplot(dataset, "Problem", "duration")

Variable <- "Problem"
Value <- "duration"
data.lm <- lm(dataset[[Value]]~dataset[[Variable]], dataset)
data.aov <- aov(data.lm)
summary <- summary(data.aov)
summary
print(xtable(as.matrix(summary)), include.rownames = TRUE)

LCV_ANOVA(dataset,"Problem","duration")

data.tukey<-TukeyHSD(data.aov)
data.tukey
print(xtable(as.matrix(data.tukey$`dataset[[Variable]]`)), include.rownames = TRUE)

LCV_Tomato_Theme()
LCV_confidence_intervals(dataset, Variable, Value)

kruskal.test(duration ~ Problem, data = dataset)
# Kruskal-Wallis rank sum test

# data:  duration by Problem
# Kruskal-Wallis chi-squared = 29.341, df = 8, p-value = 0.0002763

# HISTOGRAMAS DÍA DE LA SEMANA Y HORA DEL DÍA

# Convertir el campo DateTime al formato de fecha y hora
SIIE23RAW$Time <- dmy_hms(SIIE23RAW$SStart)

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# Obtener el día de la semana y la hora
SIIE23RAW$Weekday <- wday(SIIE23RAW$Time)
SIIE23RAW$Hour <- hour(SIIE23RAW$Time)

LCV_Heaven_Theme()
LCV_histogram(SIIE23RAW,"Weekday")
LCV_histogram(SIIE23RAW,"Hour",flip=TRUE)

# SESIONES POR PROBLEMA

agg_data <- SIIE23RAW %>% group_by(Group, Problem) %>% count()

data <- data.frame(agg_data$Group, agg_data$Problem, agg_data$n)
colnames(data) <- c("Group", "Problem", "sessions")
head(data)

LCV_Lime_Theme()
LCV_boxplot(data, "Problem", "sessions")

# NÚMERO DE DÍAS QUE DURA LA PRÁCTICA CADA AÑO

# Calcular la duración máxima en días por año
result <- SIIE23RAW
# Convertir las columnas SStart y SEnd a objetos POSIXct
result$SStart <- as.POSIXct(result$SStart, format = "%d/%m/%Y %H:%M:%S")
result$SEnd <- as.POSIXct(result$SEnd, format = "%d/%m/%Y %H:%M:%S")

# Calcular la diferencia entre la fecha máxima y la fecha mínima en días por año
result <- result %>%
  group_by(Year) %>%
  summarise(Duration = as.numeric(difftime(max(SEnd), min(SStart), units = "days")))

# Mostrar el resultado
print(result)
summary(result)

data <- c(32, 23, 29, 17, 27, 16, 38)

# Calcular la media
media <- mean(data)
cat("Media:", media, "\n")

# Calcular la desviación estándar
desviacion_estandar <- sd(data)
cat("Desviación estándar:", desviacion_estandar, "\n")

# NÚMERO DE REGISTROS Y NÚMERO DE SESIONES

data <- c(4489, 4538, 3661, 2811, 5156, 3904, 6113)

# Calcular la media
media <- mean(data)
cat("Media:", media, "\n")

# Calcular la desviación estándar
desviacion_estandar <- sd(data)
cat("Desviación estándar:", desviacion_estandar, "\n")

data <- c(12088, 12525, 9088, 5705, 14475, 21188, 11961)

# Calcular la media
media <- mean(data)
cat("Media:", media, "\n")

# Calcular la desviación estándar
desviacion_estandar <- sd(data)
cat("Desviación estándar:", desviacion_estandar, "\n")
