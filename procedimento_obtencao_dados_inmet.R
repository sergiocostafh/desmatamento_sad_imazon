
#library(devtools)
#install_github('lhmet/inmetr')
library(inmetr)
stations <- as.list(subset(bdmep_meta,bdmep_meta$uf%in%c('SC','PR','SP','MS','GO'))$id)
met_data <- list()
for(i in stations){
  tryCatch({
    met_data[[i]] <- bdmep_import(id = i,
                                  sdate = '01/08/2009',
                                  edate = '31/07/2019',
                                  email = 'seu@email.com',
                                  passwd = 'suasenha',
                                  verbose = TRUE)
  }, error = function(e){})
}

dados_compilados <- as.data.frame(met_data[[ids[1]]])
for(i in 2:length(ids)){
  dados_compilados <- rbind(dados_compilados, data.frame(as.data.frame(met_data[[ids[i]]])))
}
dados_compilados$nome <- bdmep_meta$name[match(dados_compilados$id, bdmep_meta$id)]
dados_compilados$uf <- bdmep_meta$uf[match(dados_compilados$id, bdmep_meta$id)]
dados_compilados$offset_utc <- bdmep_meta$offset_utc[match(dados_compilados$id, bdmep_meta$id)]
dados_compilados$date <- format(as.Date(dados_compilados$date), '%Y-%m')

# Obter precipitação mensal para cada estação
serie_precipitacao <- aggregate(prec ~ date+id, data = dados_compilados, sum)
# Obter média da precipitação mensal considerando todas as estações
serie_precipitacao <- aggregate(prec ~ date, data = serie_precipitacao, mean)
serie_precipitacao$date <- as.Date(paste0(serie_precipitacao$date,'-01'))
plot(prec~date,serie_precipitacao, type = 'l',
     ylim=c(0,400))

write.table(serie_precipitacao,'serie_precipitacao_amazonia_inmet.txt')

