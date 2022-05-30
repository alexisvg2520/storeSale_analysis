################################################ 
# Análisis Multivariado y Modelos Estocásticos
# Grupo 1
################################################
#         PROYECTO
################################################
#Paquetes necesarios para la ejecución
install.packages("readr") #Lectura csv
install.packages("ggplot2") #Gráficos
install.packages("corrplot") #Gráficos Correlación
install.packages("ggfortify") #Gráficos Regresión
install.packages("GGally") #ggpairs Correlación
install.packages("caTools") # Training Set & Test Set
install.packages("car") #Homocedasticidad: Test de Breusch Pagan
install.packages("DescTools") #Multicolinealidad
install.packages("psych") #correlación
install.packages("MASS") # box-cox
install.packages("lmtest")#durbin watson test
install.packages("caret") #predicciones

#Carga de paquetes
library(corrplot)
library(ggplot2)
library(ggfortify)
library(readr)
library(modeest)
library(GGally)
library(caTools)
library(car)
library(DescTools)
library(psych)
library(MASS)
library(lmtest)
library(caret)
#--------------Carga del Dataset---------------

#Copia de la ruta de la consola y se guarda en una variable
#Establecer path de ruta de excel
ruta_csv <- "D:\\Documentos\\UPS\\6to_nivel\\Analisis_Estocas\\Grupo\\Proyecto1\\stores.csv"

#Carga del dataframe
store_data <- read.csv(ruta_csv)

#-----------------Data Curation y Pre Procesamiento de Datos---------------

#Verificación de datos completos

#1.Eliminación de columna Store ID
store_data <- store_data[, -c(1)]

#2. Verificación del tipo de dato entero
str(store_data)

#3. Columna Store_Area: conversión de yardas al cuadrado a metros al cuadrado.
store_data$Store_AreaM2 <- store_data$Store_Area / 1.1960
store_data <- store_data[, -c(1)]

#4. Reemplazo de datos faltantes por la media 
#   numéricos -- media mean
    
    med_item= round(mean(store_data$Items_Available))
    med_cust= round(mean(store_data$Daily_Customer_Count))
    med_sale= round(mean(store_data$Store_Sales))
    med_area= round(mean(store_data$Store_AreaM2))
    
    store_data$Items_Available[is.na(store_data$Items_Available)]=med_item
    store_data$Daily_Customer_Count[is.na(store_data$Daily_Customer_Count)]=med_cust
    store_data$Store_Sales[is.na(store_data$Store_Sales)]=med_sale
    store_data$Store_AreaM2[is.na(store_data$Store_AreaM2)]=med_area
    
#5. Comparamos la cantidad de datos de cada columna
for (i in 1:length(store_data)){
  if (dim(store_data[1]) == dim(store_data[i])){
    print(paste("Datos de la columna",names(store_data[i]),"están --> | Completos |"))
  }
  
  else {
    print(paste("Datos de la columna",names(store_data[i]),"están --> | Incompletos |"))
  }
}

#6. Verificación del tipo de numérico
  str(store_data)

#-------------------------MODELO 1---------------------------------------------
#Fórmula para la regresión lineal múltiple <- Variable a Predecir Store_Sales
 
modelo <- lm(formula=Store_Sales ~ ., data = store_data)
ggpairs(store_data, lower = list(continuous = "smooth"), diag = list(continuous = "bar"), axisLabels = "none")

#Comprobación de Suposiciones alpha = 0.01 (nivel de significancia)
# 1. Linealidad
  autoplot(modelo,1)
  mean(modelo$residuals)
#   1.1 Correlación de Variable dependiente con variables dependientes
    #Matriz de Correlación
    data.cor <- cor(store_data, method = "pearson")
    #Gráfico de la Matriz de Correlación
    col <- colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
    corrplot(data.cor, method = "shade", shade.col = NA, tl.col = "black",
             tl.srt = 45, col = col(200),
             addCoef.col = "black", addcolorlabel = "no",
             order = "AOE")
# 2. Normalidad
  autoplot(modelo,2)
#   2.2 Test Estadístico de Normalidad (p-value < alpha -> no es distribución normal)
    shapiro.test(modelo$residuals)
    
    #Gráfico
    ggplot(data = store_data, aes(x=modelo$residuals))+
      geom_histogram(bins = 20 , fill = ' steelblue ', color = ' black ')+
      labs (title = ' Histograma de Residuos Modelo 1', x = ' Residuales ', y = ' Frecuencia ')
# 3. Homocedasticidad 
  autoplot(modelo,3)
#   3.3 Test de Breusch Pagan - Ha: Existe homocedasticidad
    # (p < alpha -> no homocedástico)
    ncvTest(modelo)
# 4. Multicolinealidad - VIF (factor de inflación de varianza)
#                        Cuando es superior a 5, hay multicolinealidad
    VIF(modelo)
# 5.Independencia
  dwtest(modelo)
  #Gráfica
  acf(modelo$residuals)

#Bondad del ajuste
  r2_1=summary(modelo)$adj.r.squared
  autoplot(modelo)

#------------------MODELO 2---------------------------
#Construcción de un modelo óptimo
# 1. Eliminación de una variable independiente (Multicolinealidad - VIF)
#    Eliminación de Store_AreaM2 <- menor correlación con variable dependiente
    store_data2 <- store_data[, -c(4)]

#   Fórmula para la regresión lineal múltiple
  
    modelo2 <- lm(formula <- Store_Sales ~ ., data = store_data2)
    r2_2=summary(modelo2)$adj.r.squared
    summary(modelo2)
    
    autoplot(modelo2)
    
#------------------MODELO 3--------------------------
# 2. Eliminación hacia atrás <- nivel de significacia: 0.01 <- Eliminamos Daily Customer

    modelo3 <- lm(formula <- Store_Sales ~ Items_Available, data = store_data2)
    r2_3=summary(modelo3)$r.squared
    
    autoplot(modelo3)
    
#------------------MODELO 3 (SUPOSICIONES)---------------------------    
#Comprobación de Suposiciones alpha = 0.01 (nivel de significancia)
    # 1. Linealidad
    autoplot(modelo3,1)
    mean(modelo3$residuals)
    # 2. Normalidad
    autoplot(modelo3,2)
    #   2.2 Test Estadístico de Normalidad (p-value < alpha -> no es distribución normal)
    shapiro.test(modelo3$residuals)
    #Gráfico
    ggplot(data = store_data2, aes(x=modelo3$residuals))+
      geom_histogram(bins = 20 , fill = ' steelblue ', color = ' black ')+
      labs (title = ' Histograma de Residuos Modelo 3', x = ' Residuales ', y = ' Frecuencia ')
    # 3. Homocedasticidad 
    autoplot(modelo3,3)
    #   3.3 Test de Breusch Pagan - Ha: Existe homocedasticidad
    # (p < alpha -> no homocedástico)
    ncvTest(modelo3)
    
    # 4.Independencia
    dwtest(modelo3)
    #Gráfica
    acf(modelo3$residuals)
    
    
    #5. Outliers - Atípicos
    x <- store_data$Items_Available
    y <- store_data$Store_Sales
    
    
    modelo4 <- lm(y~x) # Modelo 4
    r2_4=summary(modelo4)$r.squared
    #Método de Residuales Estandarizados
    while(TRUE){
      err = rstandard(modelo4)
      
      min = -2
      maxi = 2
      
      # find outlier
      p <- err[which(err < min | err > maxi)]
      
      if(length(p)==0){
        break;
      }
      
      # remove outlier
      x= x[which(err > min & err < maxi)]
      y= y[which(err > min & err < maxi)]
      modelo4=lm(y~x)
      r2_4=summary(modelo4)$r.squared
    }
    
    #Método de Residuales Estudentizados
    
    while(TRUE){
      t_t= qt(0.01/2,summary(modelo4)$df[2]-1,lower.tail = FALSE)
      err = rstudent(modelo4)
      
      min = -t_t
      maxi = t_t
      
      # find outlier
      p <- err[which(err < min | err > maxi)]
      
      if(length(p)==0){
        break;
      }
      
      # remove outlier
      x= x[which(err > min & err < maxi)]
      y= y[which(err > min & err < maxi)]
      modelo4=lm(y~x)
      r2_4=summary(modelo6)$r.squared
    }
    
    #6. Influyentes
    
    hi_4 <- influence(modelo4)$hat
    thi <- 6 / length(x)
    
    v_hi <- which(hi_4>thi)
    v_hi
    length(v_hi)
    cook <- cooks.distance(modelo4)
    v_cook <- which(cook>1)
    v_cook
    
    #7. Gráfico de Dispersión del Modelo de Regresión
    dg <- ggplot(cbind.data.frame(x,y),aes(x,y)) + geom_point() + 
      labs(title = "Diagrama de dispersión")+ylab("Store Sales") + 
      xlab("Items Available")+  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5))
    dg + geom_smooth(method = "lm", se = FALSE)
#-------------MODELO 4---------------------
    #Comprobación de Suposiciones alpha = 0.01 (nivel de significancia)
    # 1. Linealidad
    autoplot(modelo4,1)
    mean(modelo4$residuals)
    # 2. Normalidad
    autoplot(modelo4,2)
    #   2.2 Test Estadístico de Normalidad (p-value < alpha -> no es distribución normal)
    shapiro.test(modelo4$residuals)
    #Gráfico
    m4 <- cbind.data.frame(x,y) #Data ser para el Modelo 4
    ggplot(data = m4, aes(x=modelo4$residuals))+
      geom_histogram(bins = 20 , fill = ' steelblue ', color = ' black ')+
      labs (title = ' Histograma de Residuos - Modelo 4', x = ' Residuales ', y = ' Frecuencia ')
    
    # 3. Homocedasticidad 
    autoplot(modelo4,3)
    #   3.3 Test de Breusch Pagan - Ha: Existe homocedasticidad
    # (p < alpha -> no homocedástico)
    ncvTest(modelo4)
    
    # 4.Independencia
    dwtest(modelo4)
    #Gráfica
    acf(modelo4$residuals)
    autoplot(modelo4,5)
    autoplot(modelo4)
    #7. División de los datos en conjunto de entrenamiento y conjunto de test
    set.seed(123)
    #Definición de un conjunto de entrenamiento al 70%
    split = sample.split(m4$y, SplitRatio = 0.7)
    training_set = subset(m4,split == TRUE)
    testing_set = subset(m4,split == FALSE)
    
    #Regresión con el conjunto de Entrenamiento para el modelo de regresión 6
    
    reg4 <- lm(formula = y~., data = training_set)
    y_pred4 <- predict(reg4, newdata = testing_set)
    
    #Comprobación gráfica de la efectividad del Modelo de Regresión
    
    #Visualización Conjunto de Entrenamiento
    ggplot()+
      geom_point(aes(x = training_set$x, y = training_set$y),colour = "red")+
      geom_line(aes(x = training_set$x,
                    y = predict(reg4, newdata = training_set)),colour="blue")+
      ggtitle("Store Sales vs Items Available (Conjunto de Entrenamiento)")+
      xlab("Items Available")+
      ylab("Store Sales ($)")
    
    #Visualización de resultados en el Conjunto de Testing
    ggplot()+
      geom_point(aes(x = testing_set$x, y = testing_set$y),colour = "red")+
      geom_line(aes(x = training_set$x,
                    y = predict(reg4, newdata = training_set)),colour="blue")+
      ggtitle("Items Available vs Store Area (Conjunto de Testing)")+
      xlab("Items Available")+
      ylab("Store Sales ($)")
    
    
    
# Cambio de variable dependiente <- Items_Available
    #------------------MODELO 5---------------------------
    m5.vacio <- lm(formula=Items_Available~1,data=store_data)
    summary(m5.vacio)
    
    m5.completo <- lm(formula=Items_Available~.,data=store_data)
    summary(m5.completo) 
    
    #Comprobación de Suposiciones alpha = 0.01 (nivel de significancia)
    # 1. Linealidad
    autoplot(m5.completo,1)
    mean(m5.completo$residuals)
    # 2. Normalidad
    autoplot(m5.completo,2)
    #   2.2 Test Estadístico de Normalidad (p-value < alpha -> no es distribución normal)
    shapiro.test(m5.completo$residuals)
    #Gráfico
    ggplot(data = store_data, aes(x=m5.completo$residuals))+
      geom_histogram(bins = 20 , fill = ' steelblue ', color = ' black ')+
      labs (title = ' Histograma de Residuos - Modelo 5', x = ' Residuales ', y = ' Frecuencia ')
    
    # 3. Homocedasticidad 
    autoplot(m5.completo,3)
    #   3.3 Test de Breusch Pagan - Ha: Existe homocedasticidad
    # (p < alpha -> no homocedástico)
    ncvTest(m5.completo)
    # 4. Multicolinealidad - VIF (factor de inflación de varianza)
    #                        Cuando es superior a 5, hay multicolinealidad
    VIF(m5.completo)
    # 5.Independencia
    dwtest(m5.completo)
    #Gráfica
    acf(m5.completo$residuals)
    
    #Bondad del ajuste
    r2_5=summary(m5.completo)$adj.r.squared
    autoplot(m5.completo)
    
#----------------MODELO 6------------------------
    #Regresión Forward
    m6.backward <- step(m5.completo, scope = list(lower=m5.vacio,upper=m5.completo),
                        direction = "backward")
    r2_6=summary( m6.backward)$r.squared
    summary(m6.backward)
    autoplot(M6.backward)

#----------------MODELO 7--------------------------
    
    #Comprobación de Suposiciones alpha = 0.01 (nivel de significancia)
    # 1. Linealidad
    autoplot(m6.backward,1)
    mean(m6.backwardd$residuals)
    # 2. Normalidad
    autoplot(m6.backward,2)
    #   2.2 Test Estadístico de Normalidad (p-value < alpha -> no es distribución normal)
    shapiro.test(m6.backward$residuals)
    
    #Gráfico
    m6 <- cbind.data.frame(store_data$Store_AreaM2,store_data$Items_Available) #Data ser para el Modelo 4
    ggplot(data = m5, aes(x=m6.backward$residuals))+
      geom_histogram(bins = 20 , fill = ' steelblue ', color = ' black ')+
      labs (title = ' Histograma de Residuos - Modelo 6', x = ' Residuales ', y = ' Frecuencia ')
    # 3. Homocedasticidad 
    autoplot(m6.backward,3)
    #   3.3 Test de Breusch Pagan - Ha: Existe homocedasticidad
    # (p < alpha -> no homocedástico)
    ncvTest(m6.backward)
    
    # 4.Independencia
    dwtest(m6.backward)
    #Gráfica
    acf(m6.backward$residuals)
    
    #5. Outliers - Atípicos
    x2 <- store_data$Store_AreaM2
    y2 <- store_data$Items_Available
    modelo7=lm(y2~x2) #Modelo 7
    
    #Método de Residuales Estandarizados
    while(TRUE){
      err = rstandard(modelo7)
      
      min = -2
      maxi = 2
      
      # find outlier
      p <- err[which(err < min | err > maxi)]
      
      if(length(p)==0){
        break;
      }
      
      # remove outlier
      x2= x2[which(err > min & err < maxi)]
      y2= y2[which(err > min & err < maxi)]
      modelo7=lm(y2~x2)
      r2_7=summary(modelo7)$r.squared
    }
    
    #Método de Residuales Estudentizados
    
    while(TRUE){
      df = summary(modelo7)$df[2]- 1
      t_t= qt(0.01/2,df,lower.tail = FALSE)
      err = rstudent(modelo7)
      
      min = -t_t
      maxi = t_t
      
      # find outlier
      p <- err[which(err < min | err > maxi)]
      
      if(length(p)==0){
        break;
      }
      
      # remove outlier
      x2= x2[which(err > min & err < maxi)]
      y2= y2[which(err > min & err < maxi)]
      modelo7=lm(y~x)
      r2_7=summary(modelo7)$r.squared
    }
    
    #6. Influyentes
    
    hi_9 <- influence(modelo7)$hat
    thi9 <- 6 / length(x2)
    
    v_hi9 <- which(hi_9>thi9)
    length(v_hi9)
    cook9 <- cooks.distance(modelo7)
    v_cook <- which(cook9>1)
    
    #7. Gráfico de Dispersión del Modelo de Regresión
    dg2 <- ggplot(cbind.data.frame(x2,y2),aes(x2,y2)) + geom_point() + 
      labs(title = "Diagrama de dispersión")+ylab("Items Available") + 
      xlab("Store Area")+  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5))
    dg2 + geom_smooth(method = "lm", se = FALSE)
    
#----------------MODELO 9-----------------------------------
    autoplot(modelo7)

    #Comprobación de Suposiciones alpha = 0.01 (nivel de significancia)
    # 1. Linealidad
    autoplot(modelo7,1)
    mean(modelo7$residuals)
    # 2. Normalidad
    autoplot(modelo7,2)
    #   2.2 Test Estadístico de Normalidad (p-value < alpha -> no es distribución normal)
    shapiro.test(modelo7$residuals)
    #Gráfico
    m7 <- cbind.data.frame(x2,y2) #Data ser para el Modelo 4
    ggplot(data = m5, aes(x=m6.backward$residuals))+
      geom_histogram(bins = 20 , fill = ' steelblue ', color = ' black ')+
      labs (title = ' Histograma de Residuos - Modelo 7', x = ' Residuales ', y = ' Frecuencia ')
    # 3. Homocedasticidad 
    autoplot(modelo7,3)
    #   3.3 Test de Breusch Pagan - Ha: Existe homocedasticidad
    # (p < alpha -> no homocedástico)
    ncvTest(modelo7)
    
    # 4.Independencia
    dwtest(modelo7)
    #Gráfica
    acf(modelo7$residuals)
    autoplot(modelo7,5)
    
    autoplot(modelo7)
    
    #7. División de los datos en conjunto de entrenamiento y conjunto de test
    set.seed(123)
    #Definición de un conjunto de entrenamiento al 70%
    split = sample.split(cbind.data.frame(x2,y2)$y, SplitRatio = 0.7)
    trset9 = subset(cbind.data.frame(x2,y2),split == TRUE)
    testset9 = subset(cbind.data.frame(x2,y2),split == FALSE)
    
    #Regresión con el conjunto de Entrenamiento para el modelo de regresión 6
    reg7 <- lm(formula = y2~., data = trset9)
    y_pred7 <- predict(reg7, newdata = testset9)
    
    #Comprobación gráfica de la efectividad del Modelo de Regresión
    
    #Visualización Conjunto de Entrenamiento
    ggplot()+
      geom_point(aes(x = trset9$x2, y = trset9$y2),colour = "red")+
      geom_line(aes(x = trset9$x2,
                    y = predict(reg7, newdata = trset9)),colour="blue")+
      ggtitle("Items Available vs Store Area (Conjunto de Entrenamiento)")+
      xlab("Store Area (m2)")+
      ylab("Items Available")
    
    #Visualización de resultados en el Conjunto de Testing
    ggplot()+
      geom_point(aes(x = testset9$x2, y = testset9$y2),colour = "red")+
      geom_line(aes(x = trset9$x2,
                    y = predict(reg7, newdata = trset9)),colour="blue")+
      ggtitle("Items Available vs Store Area (Conjunto de Testing)")+
      xlab("Store Area (m2)")+
      ylab("Items Available")
    
    
    