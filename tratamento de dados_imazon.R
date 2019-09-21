#1. ACESSAR A SESSÃO DE DOWNLOADS DO SITE https://imazongeo.org.br/ E BAIXAR OS DADOS MENSAIS DE DESMATAMENTO ENTRE 08/2009 E 07 DE 2019
#2. EXTRAIR OS ARQUIVOS DE TODOS OS ZIPFILES (PODE SER REALIZADO UTILIZANDO FUNÇÃO `unzip`
#3. COLOCAR OS ARQUIVOS .DBF DE TODOS OS MESES NA PASTA DE TRABALHO
#4. TODAR O SEGUINTE SCRIPT.
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
dados[[28]] <- dados[[28]][-8]
dados[[29]] <- dados[[29]][-7]
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

dataset <- data.frame(ANO=ANO,MES=MES,AREA=AREA)
write.table(dataset, 'dados_sad.txt')