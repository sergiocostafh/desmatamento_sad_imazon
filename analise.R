library(foreign)
#lista os dbf na pasta
files <- list.files(pattern='.dbf')
#cria lista vazia
dados <- list()
#lê todos os dbf e salva tabela em lista
for(i in 1:length(files)){
  dados[[i]] <- read.dbf(files[i])
}
#verifica todos os nomes de colunas possiveis e determina as tres de interesse
name_vector <- vector()
for(i in 1:length(files)){
  name_vector <- append(name_vector,names(dados[[i]]))
}
ano <- unique(name_vector)[c(3,8,26)]
mes <- unique(name_vector)[c(4,9,25)]
area <- unique(name_vector)[c(5,10,11,16,18,22)]
#confere se todas as tabelas possuem as tres colunas
for (i in 1:length(files)) {
print(paste0(i,'-',
  sum(names(dados[[i]])%in%ano)+
  sum(names(dados[[i]])%in%mes)+
  sum(names(dados[[i]])%in%area)
))
}

#retira colunas duplicadas
dados[[29]] <- dados[[29]][-8]
dados[[30]] <- dados[[30]][-7]
#repete conferencia
for (i in 1:length(files)) {
  print(paste0(i,'-',
               sum(names(dados[[i]])%in%ano)+
                 sum(names(dados[[i]])%in%mes)+
                 sum(names(dados[[i]])%in%area)
  ))
}
#juntar tabela em uma so
ANO <- c()
MES <- c()
AREA <- c()
for (i in 1:length(files)) {
ANO <- append(ANO,as.character(dados[[i]][,which(colnames(dados[[i]])%in%ano)])[1])
MES <- append(MES,as.character(dados[[i]][,which(colnames(dados[[i]])%in%mes)])[1])
AREA <- append(AREA,sum(as.numeric(as.character(dados[[i]][,which(colnames(dados[[i]])%in%area)]))))
}
MES[which(MES%in%seq(1,9,1))] <- paste0(0,MES[which(MES%in%seq(1,9,1))])

dataset <- data.frame(ANO=ANO,MES=MES,AREA=AREA,MES_ANO=paste(ANO,'-',MES,sep=""))

library(zoo)
dataset$MES_ANO <- as.Date(as.yearmon(as.character(dataset$MES_ANO)))
library(lubridate)
dataset$SEMESTRE <- semester(dataset$MES_ANO)
dataset$SEMESTRE_ANO <- semester(dataset$MES_ANO, with_year = TRUE)

dataset$AREA_ACUM_SEM <- dataset$AREA
for(i in 2:nrow(dataset)){
  if(dataset$SEMESTRE_ANO[i]==dataset$SEMESTRE_ANO[i-1]){
  dataset$AREA_ACUM_SEM[i] <- dataset$AREA[i]+dataset$AREA_ACUM_SEM[i-1]
  }
  else{dataset$AREA_ACUM_SEM[i] <- dataset$AREA[i]}
}



library(ggplot2)

ggplot(dataset)+
  geom_line(aes(x=MES_ANO,y=AREA))+
  scale_x_date(breaks = '6 months')+
  theme_light()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

ggplot(subset(dataset, dataset$ANO%in%c(2015,2016,2017,2018,2019)))+
  geom_line(aes(x=as.numeric(MES),y=AREA_ACUM_SEM))+
  facet_grid(ANO~SEMESTRE, scales = 'free_x')
#  scale_x_discrete(labels = c())
  theme_light()+
  facet_grid(SEMESTRE~ANO)



library(xlsx)
write.xlsx(dataset,'dataset.xlsx')
