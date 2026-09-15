library(stringr)
library(dplyr)
library(readr)

#### OPCIÓN 1 ####

# Leer y separar el texto extraído de las imágenes
peru1 <- read_file("C:/Users/ADMIN/Desktop/Peru/data.txt")

imagenes <- str_extract_all(peru1, "IMG_[0-9]+\\.JPEG")[[1]]


bloq <- str_split(peru1, "\\{'image':", simplify = FALSE) [[1]]
bloq <- bloq[-1]

text <- str_replace(bloq, "^\\s*'IMG_[0-9]+\\.JPEG',\\s*'full_text':\\s*", "") 

peru2 <- data.frame( image = imagenes, text = text, stringsAsFactors = FALSE)

peru2$text <- str_replace_all(peru2$text, "\\\\n", "\n") 
peru2$text_bloq <- str_split(peru2$text, "\\n\\n")

# Limpiar los bloques de texto
peru3 <- peru2 %>% select(image, text_bloq) %>% tidyr::unnest(text_bloq)
peru3 <- peru3 %>% filter (!is.na(text_bloq), !str_detect(text_bloq, "^\\s*$"))
peru3 <- peru3 %>% mutate( text_bloq = str_replace_all(text_bloq, "\u00A0", " "), text_bloq = str_trim(text_bloq))

nombre <- '^"?[A-ZÁÉÍÓÚÜÑ]+(?:[ \n]+[A-ZÁÉÍÓÚÜÑ.]+)+$'

# Identificar nombres y agrupar la información asociada
peru3 <- peru3 %>% mutate(tipo = ifelse( str_detect(text_bloq, nombre), "nombre", "otro"))
peru3 %>% filter(tipo == "nombre") %>% select(image, text_bloq)
peru3 <- peru3 %>% mutate(inicio = ifelse(tipo == "nombre", 1, 0), numero = cumsum(inicio))
peru4 <- peru3 %>% filter(numero > 0) %>% group_by(numero) %>% summarise(
  image = first(image),
  nombre = first(text_bloq[tipo == "nombre"]),
  informacion = paste(str_squish(text_bloq[tipo == "otro"]), collapse = " "),
  .groups = "drop"
)

peru4 <- peru4 %>% mutate( nombre = str_squish(nombre))
peru4 <- peru4 %>% select(nombre, informacion)

# Exportar los resultados de la opción 1
write_excel_csv(peru4,"C:/Users/ADMIN/Desktop/Peru/PERU.csv" )
write_xlsx(peru4, "C:/Users/ADMIN/Desktop/Peru/PERU.xlsx")


#### OPCION 2 ####

# Leer y limpiar el archivo CSV
peru <- read_csv("C:/Users/ADMIN/Desktop/Peru/texto1.csv")

# Los nombres se encuentran identificados, pero esto sirve para agrupar los bloques de texto asociados

peru <- peru %>% filter(!is.na(text) & !str_detect(text, "^\\s*$"))
peru <- peru %>% mutate( inicio = ifelse(tipo == "nombre", 1, 0),
                         numero = cumsum(inicio)
                         )

peru <- peru %>% group_by(numero) %>% 
  summarise(
    nombre = first(text[tipo == "nombre"]),
    información = paste(text[tipo == "otro"], collapse = " "),
    .groups = "drop"
  )

# Seleccionar y exportar los resultados de la opción 2
peru <- peru %>% select(nombre, información)
write_xlsx(peru, "C:/Users/ADMIN/Desktop/Peru/peru_1.xlsx")


#Queda pendiente la minería de las columnas de nacimiento, educación, etc. Se encuentra en trabajo, se compartirá como update



