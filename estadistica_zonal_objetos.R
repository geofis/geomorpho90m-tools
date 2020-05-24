ezonalobj <- function(objraster = NULL, nombre = '', objgeometrias = NULL, export = T, nombreexport = '', cuali = F){
  #Ejemplo cuantitativo: ezonal(objraster = 'vrm_90M_n00w090/vrm_mosaico.tif', nombre = 'vrm', objgeometrias = 'divisionRD.gpkg', capa = 'MUNCenso2010', export = T, nombreexport = 'divisionRD_vrm', cuali = F)
  #Ejemplo cualitativo: ezonal(objraster = 'geom_90M_n00w090/geom_mosaico.tif', nombre = 'geomorfonos', objgeometrias = 'divisionRD.gpkg', capa = 'MUNCenso2010', export = T, nombreexport = 'divisionRD_geom', cuali = T)
  require(raster)
  require(sf)
  require(dplyr)
  require(tidyr)
  r <- objraster
  names(r) <- nombre
  geoms <- objgeometrias
  df <- raster::extract(x = r, y = as(geoms, 'Spatial'), df = T)
  if(cuali){
    dfresumen <- df %>%
      count(.[[1]], .[[2]]) %>%
      group_by(.[[1]]) %>%
      mutate(pct=n/sum(n)*100) %>%
      select(-n) %>%
      spread(`.[[2]]`, pct) %>%
      ungroup() %>%
      select(-`.[[1]]`) %>%
      rename_all(function(x) paste(nombre, x, sep = '_'))
  } else {
    dfresumen <- t(sapply(unique(df[,'ID']), function(x) c(
      n = length(na.omit(df[df[,'ID']==x,nombre])),
      min = min(df[df[,'ID']==x,nombre], na.rm = T),
      cuartil_ = quantile(df[df[,'ID']==x,nombre], 1/4, na.rm = T),
      media = mean(df[df[,'ID']==x,nombre], na.rm = T),
      mediana = median(df[df[,'ID']==x,nombre], na.rm = T),
      cuartil_ = quantile(df[df[,'ID']==x,nombre], 3/4, na.rm = T),
      max = max(df[df[,'ID']==x,nombre], na.rm = T),
      desv = sd(df[df[,'ID']==x,nombre], na.rm = T))))
    colnames(dfresumen) <- paste0(nombre, '_', colnames(dfresumen))
    colnames(dfresumen) <- gsub('\\.', '', colnames(dfresumen))
  }
  # return(dfresumen)
  geomsout <- dplyr::bind_cols(geoms, as.data.frame(dfresumen))
  if (export) {
    st_write(geomsout, dsn = paste0('salidas_ezonal/', nombreexport, '.gpkg'), driver = 'GPKG')
    saveRDS(geomsout, file = paste0('salidas_ezonal/', nombreexport,'.RDS'))
    }
  return(geomsout)
}
