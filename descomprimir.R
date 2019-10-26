descomp <- function(comprimido = ''){
  #Ejemplo: descomp(comprimido = 'geom_90M_n00w090.tar.gz')
  carpeta <- gsub('.tar.gz', '', comprimido)
  patrones <- c('*90M_n15w075.tif', '*90M_n15w070.tif')
  creardir <- paste('mkdir', carpeta)
  system(creardir)
  extraer <- paste0("tar -xvf ", comprimido, " -C ", carpeta, " --wildcards ", "'", patrones[1], "' '", patrones[2], "'")
  system(extraer)
}
