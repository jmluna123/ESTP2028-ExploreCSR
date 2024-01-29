## Introducción

Este documento es una extensión de la investidación *Promoting
Engagement in Computing Research for Non-CS Majors* sobre el evento
ExploreCSR primera edición 2023 elaborado en la universidad ESPOL.

## Experimento

Según la Teoría del Comportamiento Planificado (TPB), la intención de
una persona en realizar una acción o un comportamiento se encuentra
relacionado a diferentes variables. En este proyecto se busca medir la
intención de los participantes en participar en una investigación
(**INT**) y los factores que influyen en ella. La variable INT se
refiere directamente a la intención del participante de realizar una
determinada conducta. En este caso, los comportamientos están
relacionados con habilidades de ML e investigación. Además, TPB
identifica tres antecedentes en INT que pueden influir en
comportamientos particulares:

-   **Control conductual percibido (PBC):** a menudo denominado
    autoeficacia, se refiere a las percepciones de los individuos sobre
    la facilidad o dificultad asociada con un comportamiento específico.
    Según Bandura (2006), la autoeficacia es la creencia de un individuo
    en sus habilidades.
-   **Creencias conductuales (BB):** se refiere a las creencias u
    opiniones de un individuo con respecto a un comportamiento
    específico asociado con un resultado particular.
-   **Norma Subjetiva (SN):** Involucra las expectativas de otros dentro
    de un contexto o comportamiento.

## Análisis Exploratorio de datos

el CSV *filtered_data.csv* presenta un resumen de los resultados
obtenidos en ambas encuestas realizadas. Este documento presenta las
siguientes columnas:

-   **matricula**: identificador único del estudiante
-   **variable**: Tipo de variable: INT, SN, BB, PBC.
-   **modo**: modo de agrupación: inv (investigación), ia (inteligencia
    artificial) y general (inv & ia)
-   **periodo**: tipo de encuesta realizada: start (encuesta inicio
    evento) y end (encuesta final evento)
-   **valor**: moda obtenida de la agrupación (depende del modo y la
    variable)

``` r
data <- read.csv("datos/filtered_data.csv", col.names = c("matricula","variable","modo","periodo","valor"))

# Muestra el resumen de un data frame
summary(data)
```

    ##    matricula       variable             modo             periodo         
    ##  Min.   : 1.00   Length:576         Length:576         Length:576        
    ##  1st Qu.: 6.75   Class :character   Class :character   Class :character  
    ##  Median :12.50   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :12.50                                                           
    ##  3rd Qu.:18.25                                                           
    ##  Max.   :24.00                                                           
    ##      valor       
    ##  Min.   :  1.00  
    ##  1st Qu.:  5.00  
    ##  Median :  5.00  
    ##  Mean   : 24.89  
    ##  3rd Qu.: 13.75  
    ##  Max.   :100.00

Se agruparon los resultados de las variables INT, BB, PCB y SN. La
columna periodo contiene dos etiquetas, donde *start* se refiere a la
encuesta realizada antes del evento y *end* a la encuesta realizada
después.

A primera vista se observa como los promedios del diagrama de cajas se
elevan en ciertas variables en la encuesta realizada luego del evento.
No obstante, se ven diferencias al momento de comparar los datos
agrupados (general) y desglozados en las variables investigación (inv) y
preguntas relacionadas a inteligencia artificial (ia).

``` r
ggplot(data, aes(x = periodo, y = valor, fill = modo)) +
  geom_boxplot() +
  facet_grid(variable ~ ., scales = "free_y")
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

Las variables INT, SN Y BB tratan de valores en la escala 5 de Linket,
por lo que un histograma de cada una permite obsevar mejor la tendencia
de esta.

``` r
ggplot(data[(data$variable == "INT"),], aes(x = valor)) +
  geom_histogram() +
  facet_grid(modo ~ periodo, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
ggplot(data[(data$variable == "SN"),], aes(x = valor)) +
  geom_histogram() +
  facet_grid(modo ~ periodo, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(data[(data$variable == "BB"),], aes(x = valor)) +
  geom_histogram() +
  facet_grid(modo ~ periodo, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

A diferencia de las demás variables, PBC se trata de una escala de 0 a
100, donde el participante puede colocar una medición subjetivo de su
autoeficacia en relación a habilidades de investigación e IA. Para poder
ser comparada con las demás variables no paramétricas, los valores de
PBC se agruparon de 10 en 10, por lo que tenemos la cantidad de los
valores según esos rangos.

``` r
ggplot(data[(data$variable == "PBC"),], aes(x = valor)) +
  geom_histogram() +
  facet_grid(modo ~ periodo, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

## Análisis de Independencia

La Teoría del Comportamiento Planificado establece que las variables SN,
BB y PBC son dependientes de INT. En este caso, queremos comprobar si,
con la cantidad de participantes encuestados, se muestra dicha
dependencia. Por eso se propone utilizar la prueba Chi cuadrado para
comprobar la independencia de estas variables.

###Independencia (INT & SN)

Se pueden agrupar la cantidad de respuestas según cada valor de escala
de Linket.

``` r
data_int_sn = data[(data$variable == "INT" | data$variable == "SN"),]
chi_table = table(data_int_sn$variable,data_int_sn$valor) 
                 
print(chi_table)
```

    ##      
    ##         1   3   4   5
    ##   INT   1   3  34 106
    ##   SN    1  11  52  80

``` r
print(chisq.test(chi_table))
```

    ## Warning in chisq.test(chi_table): Chi-squared approximation may be incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  chi_table
    ## X-squared = 11.973, df = 3, p-value = 0.007475

Se rechaza H0, sí hay dependencia.

###Independencia (INT & BB)

``` r
data_int_sn = data[(data$variable == "INT" | data$variable == "BB"),]
chi_table = table(data_int_sn$variable,data_int_sn$valor) 
                 
print(chi_table)
```

    ##      
    ##         1   3   4   5
    ##   BB    0   5  32 107
    ##   INT   1   3  34 106

``` r
print(chisq.test(chi_table))
```

    ## Warning in chisq.test(chi_table): Chi-squared approximation may be incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  chi_table
    ## X-squared = 1.5653, df = 3, p-value = 0.6673

No se rechaza H0, no hay dependencia.

###Independencia (INT & PBC)

En este caso, PBC tiene otra escala. Por ello para evaluar la
dependencia se tomó otro enfoque. Se propone usar la correlación de
Spearman para evaluar la asociación monotónica y no paramétrica entre
las dos variables ordinales. A diferencia de la correlación de Pearson,
esta se centra en la relación de rango entre las observaciones y no a
relaciones lineales.

En este caso, el p-value no es exacto al usar la prueba de Spearman, ya
que el software haya encontrado empates (ties) en los datos y no pueda
calcular un p-value exacto debido a esta situación. La prueba de
Spearman exacta implica realizar cálculos exhaustivos, y esto puede ser
computacionalmente costoso o incluso impracticable cuando hay empates.

``` r
datos1 <- data[(data$variable == 'SN'),]
datos2 <- data[(data$variable == 'INT'),]

corr_PBC <- cor.test(datos1$valor, datos2$valor , method = "spearman")
```

    ## Warning in cor.test.default(datos1$valor, datos2$valor, method = "spearman"):
    ## Cannot compute exact p-value with ties

``` r
print(corr_PBC)
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  datos1$valor and datos2$valor
    ## S = 297855, p-value = 6.132e-07
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.4014647

``` r
datos1 <- data[(data$variable == 'BB'),]
datos2 <- data[(data$variable == 'INT'),]

corr_PBC <- cor.test(datos1$valor, datos2$valor , method = "spearman")
```

    ## Warning in cor.test.default(datos1$valor, datos2$valor, method = "spearman"):
    ## Cannot compute exact p-value with ties

``` r
print(corr_PBC)
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  datos1$valor and datos2$valor
    ## S = 215340, p-value = 1.234e-13
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.5672776

``` r
datos1 <- data[(data$variable == 'PBC'),]
datos2 <- data[(data$variable == 'INT'),]

corr_PBC <- cor.test(datos1$valor, datos2$valor , method = "spearman")
```

    ## Warning in cor.test.default(datos1$valor, datos2$valor, method = "spearman"):
    ## Cannot compute exact p-value with ties

``` r
print(corr_PBC)
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  datos1$valor and datos2$valor
    ## S = 415379, p-value = 0.0477
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.1653028

## Wilcoxon paired test

La prueba de Wilcoxon es una prueba no paramétrica que se utiliza para
comparar las medianas de dos muestras relacionadas.

En este caso, la prueba puede tener dificultades para calcular el valor
p exacto Si hay empates (dos o más observaciones con el mismo valor) en
tus datos, el cual es el caso actual. Es por ello que proporciona un
valor-p aproximado.

### Variable Intention (INT)

``` r
data_general = data[(data$variable == "INT" & data$modo == "general"),]

group_by(data_general, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24      5  1   
    ## 2 start      24      5  0.25

``` r
res <- wilcox.test(valor ~ periodo, data = data_general, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): requested
    ## conf.level not achievable

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 18, p-value = 1
    ## alternative hypothesis: true location shift is not equal to 0
    ## 90 percent confidence interval:
    ##  -1  1
    ## sample estimates:
    ## (pseudo)median 
    ##              0

``` r
data_inv = data[(data$variable == "INT" & data$modo == "inv"),]

group_by(data_inv, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24      5  0.25
    ## 2 start      24      5  0.25

``` r
res <- wilcox.test(valor ~ periodo, data = data_inv, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): requested
    ## conf.level not achievable

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 25, p-value = 0.7897
    ## alternative hypothesis: true location shift is not equal to 0
    ## 90 percent confidence interval:
    ##  -0.9999216  1.0000000
    ## sample estimates:
    ## (pseudo)median 
    ##   9.402393e-05

``` r
data_ia = data[(data$variable == "INT" & data$modo == "ia"),]

group_by(data_ia, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24      5     1
    ## 2 start      24      5     0

``` r
res <- wilcox.test(valor ~ periodo, data = data_ia, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 3.5, p-value = 0.04033
    ## alternative hypothesis: true location shift is not equal to 0
    ## 95 percent confidence interval:
    ##  -2.0000178674 -0.0000462652
    ## sample estimates:
    ## (pseudo)median 
    ##             -1

### Perceived Behavioral Control (PBC)

``` r
data_general = data[(data$variable == "PBC" & data$modo == "general"),]

group_by(data_general, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24     90    20
    ## 2 start      24     80    20

``` r
res <- wilcox.test(valor ~ periodo, data = data_general, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 101, p-value = 0.02001
    ## alternative hypothesis: true location shift is not equal to 0
    ## 95 percent confidence interval:
    ##   4.999932 25.000042
    ## sample estimates:
    ## (pseudo)median 
    ##       14.99999

``` r
data_inv = data[(data$variable == "PBC" & data$modo == "inv"),]

group_by(data_inv, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24     90  20  
    ## 2 start      24     90  12.5

``` r
res <- wilcox.test(valor ~ periodo, data = data_inv, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 43, p-value = 0.1188
    ## alternative hypothesis: true location shift is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.999972 20.000006
    ## sample estimates:
    ## (pseudo)median 
    ##       10.00007

``` r
data_ia = data[(data$variable == "PBC" & data$modo == "ia"),]

group_by(data_ia, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24     90    20
    ## 2 start      24     80    20

``` r
res <- wilcox.test(valor ~ periodo, data = data_ia, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 146, p-value = 0.0009628
    ## alternative hypothesis: true location shift is not equal to 0
    ## 95 percent confidence interval:
    ##  10.00002 25.00004
    ## sample estimates:
    ## (pseudo)median 
    ##       19.99994

### Behavioral Beliefs (BB)

``` r
data_general = data[(data$variable == "BB" & data$modo == "general"),]

group_by(data_general, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24      5     1
    ## 2 start      24      5     0

``` r
res <- wilcox.test(valor ~ periodo, data = data_general, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 7, p-value = 0.2402
    ## alternative hypothesis: true location shift is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.500059  1.000000
    ## sample estimates:
    ## (pseudo)median 
    ##     -0.9999859

``` r
data_inv = data[(data$variable == "BB" & data$modo == "inv"),]

group_by(data_inv, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24      5     1
    ## 2 start      24      5     0

``` r
res <- wilcox.test(valor ~ periodo, data = data_inv, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 20, p-value = 0.4374
    ## alternative hypothesis: true location shift is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.0000520  0.9999095
    ## sample estimates:
    ## (pseudo)median 
    ##  -4.831232e-05

``` r
data_ia = data[(data$variable == "BB" & data$modo == "ia"),]

group_by(data_ia, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24      5     1
    ## 2 start      24      5     0

``` r
res <- wilcox.test(valor ~ periodo, data = data_ia, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): requested
    ## conf.level not achievable

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 15, p-value = 0.3506
    ## alternative hypothesis: true location shift is not equal to 0
    ## 80 percent confidence interval:
    ##  -1.000000e+00  3.228296e-05
    ## sample estimates:
    ## (pseudo)median 
    ##  -4.620062e-05

### Subjective Norm (SN)

``` r
data_general = data[(data$variable == "SN" & data$modo == "general"),]

group_by(data_general, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24      5     1
    ## 2 start      24      4     1

``` r
res <- wilcox.test(valor ~ periodo, data = data_general, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): requested
    ## conf.level not achievable

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 82.5, p-value = 0.03551
    ## alternative hypothesis: true location shift is not equal to 0
    ## 80 percent confidence interval:
    ##  4.748758e-05 1.000000e+00
    ## sample estimates:
    ## (pseudo)median 
    ##      0.9999449

``` r
data_inv = data[(data$variable == "SN" & data$modo == "inv"),]

group_by(data_inv, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24      5     1
    ## 2 start      24      5     1

``` r
res <- wilcox.test(valor ~ periodo, data = data_inv, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): requested
    ## conf.level not achievable

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 52, p-value = 0.2669
    ## alternative hypothesis: true location shift is not equal to 0
    ## 90 percent confidence interval:
    ##  -5.801142e-05  1.000000e+00
    ## sample estimates:
    ## (pseudo)median 
    ##   2.117924e-05

``` r
data_ia = data[(data$variable == "SN" & data$modo == "ia"),]

group_by(data_ia, periodo) %>%
  summarise(
    count = n(),
    median = median(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   periodo count median   IQR
    ##   <chr>   <int>  <dbl> <dbl>
    ## 1 end        24      5     1
    ## 2 start      24      4     1

``` r
res <- wilcox.test(valor ~ periodo, data = data_ia, paired = TRUE, conf.int = TRUE)
```

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with ties

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = DATA[[1L]], y = DATA[[2L]], ...): cannot
    ## compute exact confidence interval with zeroes

``` r
print(res)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  valor by periodo
    ## V = 91, p-value = 0.2064
    ## alternative hypothesis: true location shift is not equal to 0
    ## 95 percent confidence interval:
    ##  -2.736276e-05  1.000051e+00
    ## sample estimates:
    ## (pseudo)median 
    ##      0.9999309

## Poder Estadístico

``` r
data_pbc_s = data[(data$variable == "PBC" & data$modo == "ia" & data$periodo == "start"),]
data_pbc_e = data[(data$variable == "PBC" & data$modo == "ia" & data$periodo == "end"),]

rx <- function(n) rnorm(n, mean = mean(data_pbc_s$valor), sd = sd(data_pbc_s$valor)) 
ry <- function(n) rnorm(n, mean = mean(data_pbc_e$valor), sd = sd(data_pbc_e$valor))  

sim.ssize.wilcox.test(rx = rx, n.min=1, n.max = 30, step.size = 2, iter = 10000, type="paired")
```

    ## 
    ##      Wilcoxon signed rank test 
    ## 
    ##               n = 7
    ##              rx = rnorm(n, mean = mean(data_pbc_s$valor), sd = sd(data_pbc_s$valor))
    ##       sig.level = 0.05
    ##       emp.power = 1
    ##     alternative = two.sided
    ## 
    ## NOTE: n is number in *each* group
