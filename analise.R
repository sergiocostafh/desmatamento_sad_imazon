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

dataset <- data.frame(ANO=ANO,MES=MES,AREA=AREA,
                      `MES.ANO`=as.Date(paste(ANO,'-',MES,'-01', sep='')))

library(ggplot2)
library(plotly)

ggplotly(ggplot(dataset,aes(x=`MES.ANO`,y=AREA))+
  geom_line()+
    scale_x_date(expand = c(0.05,0.05), breaks = '6 months',
                 date_labels = '%b %Y', minor_breaks = NULL)+  scale_y_continuous(expand = c(0,0), limits = c(0,1500),
                     breaks = seq(0,1500,100))+
  xlab(NULL)+
  ylab('Área desmatada (km²)')+
  ggtitle('Área desmatada na Amazônia Legal - Acumulado mensal')+
  theme_light()+
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        plot.title = element_text(hjust=0.5)))

dataset$`AREA ACUMULADA EM 1 ANO` <- NA
for(i in 12:nrow(dataset)){
    dataset$`AREA ACUMULADA EM 1 ANO`[i] <- sum(dataset$AREA[i-c(0:11)])
    }
dataset$`AREA ACUMULADA EM 6 MESES` <- NA
for(i in 6:nrow(dataset)){
  dataset$`AREA ACUMULADA EM 6 MESES`[i] <- sum(dataset$AREA[i-c(0:5)])
  }

ggplotly(ggplot(dataset)+
  geom_line(aes(x=MES.ANO,y=`AREA ACUMULADA EM 1 ANO`, linetype = 'Acumulado em 1 ano'))+
  geom_line(aes(x=MES.ANO,y=`AREA ACUMULADA EM 6 MESES`, linetype = 'Acumulado em 6 meses'))+
  scale_x_date(expand = c(0.05,0.05), breaks = '6 months',
               date_labels = '%b %Y', minor_breaks = NULL)+
  scale_linetype_manual(name = NULL ,values = c('solid','dashed'))+
  xlab(NULL)+
  ylab('Área desmatada (km²)')+
  ggtitle('Área desmatada na Amazônia Legal - Acumulado anual e semestral')+
  theme_light()+
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        plot.title = element_text(hjust=0.5))) %>%
  layout(legend = list(orientation = 'h', bgcolor ='#FFFFFF00'))

dataset$`DIFERENCA - 1 ANO` <- NA
for(i in 24:nrow(dataset)){
  dataset$`DIFERENCA - 1 ANO`[i] <- ((dataset$`AREA ACUMULADA EM 1 ANO`[i]/dataset$`AREA ACUMULADA EM 1 ANO`[i-12])-1)
}

dataset$`DIFERENCA - 6 MESES` <- NA
for(i in 24:nrow(dataset)){
  dataset$`DIFERENCA - 6 MESES`[i] <- ((dataset$`AREA ACUMULADA EM 6 MESES`[i]/dataset$`AREA ACUMULADA EM 6 MESES`[i-12])-1)
}

ggplotly(ggplot(dataset)+
  geom_line(aes(x=MES.ANO,y=`DIFERENCA - 1 ANO`, linetype = 'Acumulado em 1 ano'))+
  geom_line(aes(x=MES.ANO,y=`DIFERENCA - 6 MESES`, linetype = 'Acumulado em 6 meses'))+
  geom_hline(yintercept = 0, color = 'red')+
  scale_x_date(expand = c(0.05,0.05), breaks = '6 months',
               date_labels = '%b %Y', minor_breaks = NULL)+
  scale_y_continuous(expand = c(0,0.4), labels = scales::percent)+
  scale_linetype_manual(name = NULL ,values = c('solid','dashed'))+
  xlab(NULL)+
  ggtitle(label = 'Desmatamento na Amazonônia Legal')+
  ylab('Diferença entre um período e o mesmo período do ano anterior')+
  theme_light()+
  theme(axis.text.x = element_text(angle=45, hjust = 1)))%>%
  layout(legend = list(orientation = 'h', bgcolor ='#FFFFFF00'))

library(xlsx)
write.xlsx(dataset,'dataset.xlsx')

