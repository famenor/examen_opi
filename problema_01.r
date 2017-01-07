nacimiento <- read.table("/Users/famenor/Desktop/Examen/iter2010_ageb.txt", sep=',', header=TRUE)
registros <- nacimiento[grep("lvaro", nacimiento$NOM_LOC),]
registros_red <- subset(registros, select=c("AGEB", "P_0A2"))
registros_red$bebes <- as.numeric(as.character(registros_red$P_0A2))

media <- mean(registros_red$bebes, na.rm = TRUE)
media_entero <- as.integer(media)
registros_red$bebes[is.na(registros_red$bebes)] <- media_entero
resumen <- aggregate(registros_red$bebes, by=list(Category=registros_red$AGEB), FUN=sum)

natalidad_bruta <- data.frame(periodo=seq(from=2006, to=2015, by=1), tasa=c(16.3, 16.1, 15.8, 15.6, 15.4, 15.2, 15.0, 14.8, 14.6, 14.4))
modelo <- lm(tasa~periodo, data=natalidad_bruta)

natalidad_bruta$prediccion <- predict(modelo, newdata=natalidad_bruta)
siguente = data.frame(periodo=c(2016, 2017))
siguiente$prediccion = predict(modelo, newdata=siguiente)

merge = data.frame(periodo=c(natalidad_bruta$periodo, siguiente$periodo), tasa=c(natalidad_bruta$prediccion, siguiente$prediccion))
ggplot(data=merge, aes(x=periodo, y=tasa)) + geom_point(color='blue') + geom_line(data=natalidad_bruta, aes(x=periodo, y=tasa, color="Valor real")) + xlim(2005, 2018) + ggtitle("Tasa de natalidad estimada") + scale_fill_manual(name="") + theme(legend.title=element_blank())

n_10 <- natalidad_bruta$tasa[5]
n_16 <- siguiente$prediccion[1]
n_17 <- siguiente$prediccion[2]

proporcion_1 <- n_17 / n_10
colnames(resumen) <- c('AGEB', 'menores_2_anios')
resumen$menores_2_anios_2017 <- resumen$menores_2_anios * proporcion_1

proporcion_2 <- n_17 / (n_17 + n_16)
proporcion_2 <- proporcion_2 / 2

resumen$bebes_6_meses <- as.integer(resumen$menores_2_anios_2017 * proporcion_2)

ggplot(resumen[0:40,], aes(x=AGEB, y=bebes_6_meses)) + geom_bar(stat="identity", fill="gray") + coord_flip() + theme(axis.text.y=element_text(size=rel(0.8)))



resultado <- subset(resumen, select=c('AGEB', 'bebes_6_meses'))