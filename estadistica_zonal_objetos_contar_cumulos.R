ezonalclumps <- function(objraster = NULL, nombre = '', objgeometrias = NULL, export = F, nombreexport = ''){
  require(raster)
  require(sf)
  require(dplyr)
  require(tidyr)
  r <- objraster
  names(r) <- nombre
  geoms <- objgeometrias
  df <- raster::extract(x = r, y = as(geoms, 'Spatial'), df = T)
  dfresumen <- t(sapply(unique(df[,'ID']), function(x) {
    d <- data.frame(
      ID = x,
      n = length(na.omit(df[df[,'ID']==x, nombre], na.rm = T)))
    }))
  dfresumen <- dfresumen[,'n', drop = F]
  colnames(dfresumen) <- nombre
  geomsout <- dplyr::bind_cols(geoms, as.data.frame(dfresumen))
  if (export) {
    st_write(geomsout, dsn = paste0('salidas_ezonal/', nombreexport, '.gpkg'), driver = 'GPKG')
    saveRDS(geomsout, file = paste0('salidas_ezonal/', nombreexport,'.RDS'))
    }
  return(geomsout)
}
