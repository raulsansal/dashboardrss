# utils.R

#' Función para encontrar el directorio de datos
#' 
#' Busca el directorio de datos en múltiples ubicaciones posibles
#' y devuelve la ruta correcta.
#' 
#' @return Character con la ruta al directorio de datos
find_data_dir <- function() {
  # Lista de posibles ubicaciones (en orden de preferencia)
  posibles_rutas <- c(
    here::here("data"),                    # Con here() (desarrollo local)
    file.path(getwd(), "data"),            # Relativo al directorio de trabajo
    "data",                                # Directorio simple
    file.path("www", "data"),              # En www/data (común en despliegues)
    file.path(dirname(getwd()), "data")    # Directorio padre
  )
  
  # Verificar cada ubicación
  for (ruta in posibles_rutas) {
    if (dir.exists(ruta)) {
      message("✅ Directorio de datos encontrado en: ", ruta)
      return(ruta)
    }
  }
  
  # Si no encontramos el directorio, advertir y devolver una ruta predeterminada
  warning("⚠️ No se encontró el directorio de datos. Usando 'data' por defecto.")
  return("data")
}

#' Función para obtener la ruta completa a un archivo de datos
#' 
#' @param archivo Nombre del archivo
#' @return Ruta completa al archivo
get_data_file_path <- function(archivo) {
  file.path(find_data_dir(), archivo)
}