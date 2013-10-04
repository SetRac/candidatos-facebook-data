#####
#	Cargamos la libreria rjson
library(rjson)

#####
#	Generamos un objeto tipo lista para guardar la data del archivos tipo json en el
json_data <- list()

#####
#	Tomamos el listado de archivos descargados con el API de Facebook y python.
filelist <- dir(getwd(), pattern = "json", recursive = TRUE, all.files = TRUE, full.names = TRUE)

#####
#	Poblamos la lista con la información de los archivos json
for(i in 1:length(filelist)){
	json_data[i] <- fromJSON(paste(readLines(filelist[i]), collapse=""))
}

#####
#	Cargamos la librería plyr
library(plyr)

#####
#	Despues de analizar las variables en los archivos json
#	Extraemos las variables que se consideran significativas para esta primera etapa de análisis
for(i in 1:length(filelist)){
	for(k in 1:length(json_data[[i]]$data)){
		print(paste(i,"con",k))
		if(typeof(json_data[[i]]$data[[k]]$id)=="character"){
			p1 <- json_data[[i]]$data[[k]]$id} else {
			p1 <- NA}
		if(typeof(json_data[[i]]$data[[k]]$from$id)=="character"){
			p2 <- json_data[[i]]$data[[k]]$from$id} else {
			p2 <- NA}
		if(typeof(json_data[[i]]$data[[k]]$from$name)=="character"){
			p3 <- json_data[[i]]$data[[k]]$from$name} else {
			p3 <- NA}
		if(typeof(json_data[[i]]$data[[k]]$created_time)=="character"){
			p4 <- json_data[[i]]$data[[k]]$created_time} else {
			p4 <- NA}
		if(typeof(json_data[[i]]$data[[k]]$message)=="character"){
			p5 <- json_data[[i]]$data[[k]]$message} else {
			p5 <- NA}
		if(typeof(json_data[[i]]$data[[k]]$type)=="character"){
			p6 <- json_data[[i]]$data[[k]]$type} else {
			p6 <- NA}
		if(typeof(json_data[[i]]$data[[k]]$likes$count)=="double"){
			p7 <- json_data[[i]]$data[[k]]$likes$count} else {
			p7 <- NA}
		if(typeof(json_data[[i]]$data[[k]]$shares$count)=="double"){
			p8 <- json_data[[i]]$data[[k]]$shares$count} else {
			p8 <- 0}
		if(typeof(json_data[[i]]$data[[k]]$link)=="character"){
			p9 <- json_data[[i]]$data[[k]]$link} else {
			p9 <- 0}
		test <- list(p1,p2,p3,p4,p5,p6,p7,p8,p9)
		test <- data.frame(test,stringsAsFactors=FALSE)
		#print(test)
		names(test) <- c("id","from_id","from_name","created_time","message","type","likes_count","shares_count","link")
		if(i == 1 & k == 1){
			data <- data.frame(test,stringsAsFactors=FALSE)
		} else {
			data <- rbind.fill(data,test)
		}
	}
}

#####
#	Por la forma acumulativa en que se acumula la data tenemos que eliminar las duplicidades de la base de datos
data <- unique(data)

#####
#	Exportamos la data para trabajo en excel
write.csv(data,"data.csv")

#####
#	Minería de textos

#####
#	Cargamos la librería tm
library(tm)

#####
#	Generamos un objeto con solo los casos que poseen mensaje de texto
forCats <- data[!is.na(data$message),]

#####
#	Hacemos un objeto para cada candidato
forRH <- forCats[forCats$from_name == "Dr. Rodolfo Hernández",]
forJA <- forCats[forCats$from_name == "Johnny Araya",]
forJV <- forCats[forCats$from_name == "José María Villalta Florez-Estrada",]
forLS <- forCats[forCats$from_name == "Luis Guillermo Solís Rivera",]
forOG <- forCats[forCats$from_name == "Otto Guevara Guth",]

#####
#	Convertimos la variable de mensajes en un vector para cada candidato
vecRH <- VectorSource(forRH$message)
vecJA <- VectorSource(forJA$message)
vecJV <- VectorSource(forJV$message)
vecLS <- VectorSource(forLS$message)
vecOG <- VectorSource(forOG$message)

#####
#	Generamos un objeto tipo corpus para los mensajes de cada candidato
corpusRH <- Corpus(vecRH)
corpusJA <- Corpus(vecJA)
corpusJV <- Corpus(vecJV)
corpusLS <- Corpus(vecLS)
corpusOG <- Corpus(vecOG)

#####
#	Realizamos un primer proceso de estandarizacion de la data
corpusRH <- docter(corpusRH)
corpusJA <- docter(corpusJA)
corpusJV <- docter(corpusJV)
corpusLS <- docter(corpusLS)
corpusOG <- docter(corpusOG)

#####
#	Realizamos un segundo proceso de estandarizacion de la data
corpusRH <- docter2(corpusRH)
corpusJA <- docter2(corpusJA)
corpusJV <- docter2(corpusJV)
corpusLS <- docter2(corpusLS)
corpusOG <- docter2(corpusOG)

#####
#	Extraemos la matriz de terminos de los documentos
TDMRH <- TermDocumentMatrix(corpusRH)
TDMJA <- TermDocumentMatrix(corpusJA)
TDMJV <- TermDocumentMatrix(corpusJV)
TDMLS <- TermDocumentMatrix(corpusLS)
TDMOG <- TermDocumentMatrix(corpusOG)

#####
#	Revisamos la aparición de palabras con una frecuencia superior a 20
findFreqTerms(TDMRH, 20)
findFreqTerms(TDMJA, 20)
findFreqTerms(TDMJV, 20)
findFreqTerms(TDMLS, 20)
findFreqTerms(TDMOG, 20)

#####
#	Removemos los terminos que tengan un sparce menor a 0.95
TDMRH.common = removeSparseTerms(TDMRH, 0.95)
TDMJA.common = removeSparseTerms(TDMJA, 0.95)
TDMJV.common = removeSparseTerms(TDMJV, 0.95)
TDMLS.common = removeSparseTerms(TDMLS, 0.95)
TDMOG.common = removeSparseTerms(TDMOG, 0.95)

#####
#	Cargamos la librería slam
library(slam)

#####
#	Extraemos la matris de cada objeto .common
TDMRH.dense <- as.matrix(TDMRH.common)
TDMJA.dense <- as.matrix(TDMJA.common)
TDMJV.dense <- as.matrix(TDMJV.common)
TDMLS.dense <- as.matrix(TDMLS.common)
TDMOG.dense <- as.matrix(TDMOG.common)

#####
#	Cargamos las librerías wordcloud y RColorBrewer
library(wordcloud)
library(RColorBrewer)

#####
#	Definimos los colores a utilizar
palette <- brewer.pal(9,"BuGn")[-(1:4)]

#####
#	Definimos la exportación de los graficos a un archivo pdf
pdf(file=" cloudPoliticos%03d.pdf")

#####
#	Generamos un wordcloud para las palabras comunes de cada candidato
wordcloud(rownames(TDMRH.dense), rowSums(TDMRH.dense), min.freq = 1, color = palette)
wordcloud(rownames(TDMJA.dense), rowSums(TDMJA.dense), min.freq = 1, color = palette)
wordcloud(rownames(TDMJV.dense), rowSums(TDMJV.dense), min.freq = 1, color = palette)
wordcloud(rownames(TDMLS.dense), rowSums(TDMLS.dense), min.freq = 1, color = palette)
wordcloud(rownames(TDMOG.dense), rowSums(TDMOG.dense), min.freq = 1, color = palette)

#####
#	Detenemos la exportación al pdf
dev.off()

#####
#	Funciones generadas especificamente para este procesamiento
docter <- function(corp){
	library(tm)
	#	pasamos a minusculas todas las letras
	corp2 <- tm_map(corp, tolower)
	#	eliminamos toda puntuacion
	corp2 <- tm_map(corp2, removePunctuation)
	#	eliminamos los numeros
	corp2 <- tm_map(corp2, removeNumbers)
	#	eliminamos las palabras comunes del idioma español
	corp2 <- tm_map(corp2, removeWords, stopwords("spanish"))
	#	retorna como resultado un corpus con los cambios anteriores
	result <- corp2
}

docter2 <- function(corp){
	library(SnowballC)
	#	eliminamos los prefijos y sufijos de todas las palabras
	corp2 <- tm_map(corp, stemDocument)
	#	eliminamos espacios en blanco innecesarios generados por procesos anteriores
	corp2 <- tm_map(corp2, stripWhitespace)
	#	retorna como resultado un corpus con los cambios anteriores
	result <- corp2
}

