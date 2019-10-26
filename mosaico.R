mosaico <- function(carpeta = '', nombre = ''){
  #mosaico(carpeta = 'geom_90M_n00w090', nombre = 'geom')
  #mosaico(carpeta = 'vrm_90M_n00w090', nombre = 'vrm')
  require(gdalUtils)
  archivos <- list.files(path = carpeta, pattern = '*.tif', full.names = T)
  mosaiconombre <- paste0(carpeta, '/', nombre, '_mosaico.tif')
  mosaico <- mosaic_rasters(
    gdalfile = archivos,
    dst_dataset = mosaiconombre,
    co = 'COMPRESS=LZW',
    output_Raster = T)
  return(mosaico)
}
