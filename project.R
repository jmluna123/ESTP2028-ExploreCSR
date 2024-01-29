
library(reshape2) #melt
library(dplyr) #select
library(ggplot2) #ggplot
library(readxl) #read_excel


data <- read.csv("datos/filtered_data.csv", col.names = c("matricula","variable","modo","periodo","valor"))

ggplot(data, aes(x = periodo, y = valor, fill = modo)) +
  geom_boxplot() +
  facet_grid(variable ~ ., scales = "free_y")


ggplot(data[!(data$variable == "PBC"),], aes(x = valor, fill = modo)) +
  geom_histogram() +
  facet_grid(variable ~ periodo, scales = "free")

ggplot(data[(data$variable == "PBC"),], aes(x = valor, fill = modo)) +
  geom_histogram() +
  facet_grid(variable ~ periodo, scales = "free")



#Wilcoxon paired test ----------------------------------------------------------

variable <- "PBC" # "PBC" "SN" "BB" "INT"
#general
data_general = data[(data$variable == variable & data$modo == "general"),]

group_by(data_general, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )

res <- wilcox.test(valor ~ periodo, data = data_general, paired = TRUE)
res

#inv
data_inv = data[(data$variable == variable & data$modo == "inv"),]

group_by(data_inv, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )

res <- wilcox.test(valor ~ periodo, data = data_inv, paired = TRUE)
res

#ia
data_ia = data[(data$variable == variable & data$modo == "ia"),]

group_by(data_ia, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )

res <- wilcox.test(valor ~ periodo, data = data_ia, paired = TRUE)
res


prueba_pbc <- data[(data$variable == 'PBC'),]
prueba_int <- data[(data$variable == 'INT'),]





data_pbc_s = data[(data$variable == "PBC" & data$modo == "ia" & data$periodo == "start"),]
data_pbc_e = data[(data$variable == "PBC" & data$modo == "ia" & data$periodo == "end"),]

rx <- function(n) rnorm(n, mean = mean(data_pbc_s$valor), sd = sd(data_pbc_s$valor)) 
ry <- function(n) rnorm(n, mean = mean(data_pbc_e$valor), sd = sd(data_pbc_e$valor))  


sim.ssize.wilcox.test(data_pbc_s$valor, step.size = 2, iter = 10000, type="paired")


summary(data[(data$variable == "SN" & data$modo == "ia" & data$periodo == "start"),])
summary(data[(data$variable == "SN" & data$modo == "ia" & data$periodo == "end"),])


