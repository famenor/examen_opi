r_10 <- read.csv("/Users/famenor/Desktop/Examen/Problema 2/2016-10.csv", header=TRUE)
r_11 <- read.csv("/Users/famenor/Desktop/Examen/Problema 2/2016-11.csv", header=TRUE)
r_12 <- read.csv("/Users/famenor/Desktop/Examen/Problema 2/2016-12.csv", header=TRUE)

r_10$Hora_01 <- as.integer(format(as.POSIXct(r_10$Hora_Retiro, format="%I:%M:%S %p"), "%H"))
r_10$Hora_02 <- as.integer(format(as.POSIXct(r_10$Hora_Arribo, format="%I:%M:%S %p"), "%H"))

r_11$Hora_01 <- as.integer(format(as.POSIXct(r_11$Hora_Retiro, format="%H:%M:%S"), "%H"))
r_11$Hora_02 <- as.integer(format(as.POSIXct(r_11$Hora_Arribo, format="%H:%M:%S"), "%H"))

r_12$Hora_01 <- as.integer(format(as.POSIXct(r_12$Hora_Retiro, format="%I:%M:%S %p"), "%H"))
r_12$Hora_02 <- as.integer(format(as.POSIXct(r_12$Hora_Arribo, format="%I:%M:%S %p"), "%H"))

ggplot(r_10, aes(x=Hora_01)) + geom_bar(fill="gray") + ggtitle("Hora retiro, Octubre")
ggplot(r_11, aes(x=Hora_01)) + geom_bar(fill="gray") + ggtitle("Hora retiro, Noviembre")

ggplot(rbind(r_11, r_12), aes(x=Hora_01)) + geom_bar(fill="gray") + ggtitle("Hora retiro")
ggplot(rbind(r_10, r_11, r_12), aes(x=Hora_02)) + geom_bar(fill="gray") + ggtitle("Hora arribo")

r_ecobici <- rbind(r_10, r_11, r_12)

afluencia_retiro <- as.data.frame(table(r_ecobici$Ciclo_Estacion_Retiro))
afluencia_arribo <- as.data.frame(table(r_ecobici$Ciclo_Estacion_Arribo))

afluencia_retiro_mayor <- afluencia_retiro[order(-afluencia_retiro$Freq),][1:30,]
afluencia_arribo_mayor <- afluencia_arribo[order(-afluencia_arribo$Freq),][1:30,]

library(rjson)
json_file <- "https://pubsbapi.smartbike.com/oauth/v2/token?client_id=859_5qb43c6635s04ook0csocw4sgsggggk0ss84wwgog0g8c8oww4&client_secret=50crfz9upq80g4k8s0kcso8cscs44gkokokcg4w8osw4sg88og&grant_type=client_credentials"
json_data <- fromJSON(file=json_file)
json_file <- paste("https://pubsbapi.smartbike.com/api/v1/stations.json?access_token=", json_data$access_token, sep="")
json_direcciones <- fromJSON(file=json_file)

identificadores <- c()
nombres <- c()
for (i in 1:452) 
{
    identificadores <- c(identificadores, json_direcciones$stations[[i]]$id)
     nombres <- c(nombres, json_direcciones$stations[[i]]$name)
}

estaciones <- data.frame(id=identificadores, nombre=nombres)
estaciones$nombre <- gsub("<U+00C9", "", estaciones$nombre)
estaciones$nombre <- gsub("<U+00D3", "", estaciones$nombre)
estaciones$nombre <- gsub("<U+00C1", "", estaciones$nombre)


final_retiro <- subset(merge(afluencia_retiro_mayor, estaciones, by=0), select=c('nombre', 'Freq'))
final_arribo <- subset(merge(afluencia_arribo_mayor, estaciones, by=0), select=c('nombre', 'Freq'))

ggplot(data=final_retiro, aes(x=nombre, y=Freq)) + geom_bar(stat="identity", fill="gray") + coord_flip() + ggtitle("Estaciones con mayor retiro") + theme(axis.text.y=element_text(size=rel(0.8)))
ggplot(data=final_arribo, aes(x=nombre, y=Freq)) + geom_bar(stat="identity", fill="gray") + coord_flip() + ggtitle("Estaciones con mayor arribo") + theme(axis.text.y=element_text(size=rel(0.8)))

###

octubre <- as.data.frame(table(r_10$Ciclo_Estacion_Retiro))
noviembre <- as.data.frame(table(r_11$Ciclo_Estacion_Retiro))
diciembre <- as.data.frame(table(r_12$Ciclo_Estacion_Retiro))

subconjunto <- subset(merge(octubre, noviembre, by=0), select=c("Var.1", "Freq.x", "Freq.y"))
colnames(subconjunto) <- c("id", "octubre", "noviembre")
tendencia <- subset(merge(subconjunto, diciembre, by=0), select=c("id", "octubre", "noviembre", "Freq"))
colnames(tendencia)[4] <- "diciembre"

tendencia$on <- tendencia$noviembre - tendencia$octubre
tendencia$nd <- tendencia$diciembre - tendencia$noviembre
tendencia$od <- tendencia$nd + tendencia$on

tendencia$signo <- ifelse(tendencia$od > 100, 1, ifelse(tendencia$od < 100, -1, 0))
tendencia$signo <- ifelse(tendencia$od > 100, 1, ifelse(tendencia$od < -100, -1, 0))
estaciones_aumento <- subset(tendencia[tendencia$signo == 1,], select=c("id", "od"))

estaciones_aumento <- subset(merge(estaciones_aumento, estaciones, by="id"), select=c("id", "nombre", "od"))
colnames(estaciones_aumento) <- c("id", "nombre", "ganancia") 

final_aumento <- estaciones_aumento[order(-estaciones_aumento$ganancia),][1:30,]

###

ocurrencias <- subset(r_ecobici, select=c("Ciclo_Estacion_Retiro", "Ciclo_Estacion_Arribo"))
library(plyr)
conteos <- count(ocurrencias, c('Ciclo_Estacion_Retiro','Ciclo_Estacion_Arribo'))

ggplot(data = conteos, aes(x = Ciclo_Estacion_Retiro, y = Ciclo_Estacion_Arribo)) + geom_tile(aes(fill = log(freq))) + xlim(c(0,100)) + ylim(c(0,100))

correlaciones <- conteos[order(-conteos$freq),][1:20,]

correlaciones$nombre_1 <- estaciones[correlaciones$Ciclo_Estacion_Retiro,][,2]
correlaciones$nombre_2 <- estaciones[correlaciones$Ciclo_Estacion_Arribo,][,2]

####

r_ecobici_red <- subset(r_ecobici, select=c("Ciclo_Estacion_Retiro", "Ciclo_Estacion_Arribo", "Hora_01", "Hora_02"))

sub_1 <- r_ecobici_red[r_ecobici_red$Hora_01 < 6,]
sub_2 <- r_ecobici_red[r_ecobici_red$Hora_01 >= 6 & r_ecobici_red$Hora_01 < 12,]
sub_3 <- r_ecobici_red[r_ecobici_red$Hora_01 >= 12 & r_ecobici_red$Hora_01 < 18,]
sub_4 <- r_ecobici_red[r_ecobici_red$Hora_01 >= 18 & r_ecobici_red$Hora_01 < 24,]
sub_5 <- r_ecobici_red[r_ecobici_red$Hora_02 >= 0 & r_ecobici_red$Hora_02 < 6,]
sub_6 <- r_ecobici_red[r_ecobici_red$Hora_02 >= 6 & r_ecobici_red$Hora_02 < 12,]
sub_7 <- r_ecobici_red[r_ecobici_red$Hora_02 >= 12 & r_ecobici_red$Hora_02 < 18,]
sub_8 <- r_ecobici_red[r_ecobici_red$Hora_02 >= 18 & r_ecobici_red$Hora_02 < 24,]

df_1 <- as.data.frame(table(sub_1$Ciclo_Estacion_Retiro))
df_2 <- as.data.frame(table(sub_2$Ciclo_Estacion_Retiro))
df_3 <- as.data.frame(table(sub_3$Ciclo_Estacion_Retiro))
df_4 <- as.data.frame(table(sub_4$Ciclo_Estacion_Retiro))
df_5 <- as.data.frame(table(sub_5$Ciclo_Estacion_Arribo))
df_6 <- as.data.frame(table(sub_6$Ciclo_Estacion_Arribo))
df_7 <- as.data.frame(table(sub_7$Ciclo_Estacion_Arribo))
df_8 <- as.data.frame(table(sub_8$Ciclo_Estacion_Arribo))

colnames(df_1)[2] <- "Frec1"
colnames(df_2)[2] <- "Frec2"
colnames(df_3)[2] <- "Frec3"
colnames(df_4)[2] <- "Frec4"
colnames(df_5)[2] <- "Frec5"
colnames(df_6)[2] <- "Frec6"
colnames(df_7)[2] <- "Frec7"
colnames(df_8)[2] <- "Frec8"

vars.to.use <- colnames(super)[-1]
pmatrix <- super[,vars.to.use]
distancias <- dist(pmatrix, method="euclidean")
pfit <- hclust(distancias, method="ward")

plot(pfit, labels=super$Var1)
rect.hclust(pfit, k=3)
grupos <- cutree(pfit, k=3)

super$nombre <- estaciones[super$Var1,][,2]

print_clusters <- function(labels, k) {
    for(i in 1:k){
        print(paste("cluster", i))
        print(super[labels==i, c("nombre", "Frec1", "Frec2", "Frec3", "Frec4", "Frec5", "Frec6", "Frec7", "Frec8")])
    }
}
print_clusters(grupos, k=3)

colMeans(super[,c(8,9,7,6,5,4,3,2)][grupos==1,])
colMeans(super[,c(8,9,7,6,5,4,3,2)][grupos==2,])
colMeans(super[,c(8,9,7,6,5,4,3,2)][grupos==3,])











