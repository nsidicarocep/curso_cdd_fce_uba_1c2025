
library(tidyverse)
library(rvest)

pagina <- read_html('https://vinotecaligier.com/vino')

cuadros <- pagina %>% 
  html_elements(xpath = '//li[starts-with(@class, "item product product")]')

# Funciones 
reemplazar_vacio <- function(x){
  if(is_empty(x) == TRUE) {
    x <- ''
  } else {
    return(x)
  }
}

# Armar estructura 
datos_finales <- tibble(nombre = character(),
                        precio_viejo = character(),
                        precio_nuevo = character(),
                        todo = character(),
                        url = character()
                        )

# Iteracion
i <- 1
for(i in 1:length(cuadros)){
  tryCatch({
  nombre <- cuadros[[i]] %>% 
      html_elements(css='[class="product-item-link"]') %>% 
      html_text2()
  url <- cuadros[[i]] %>% 
    html_elements(css='[class="product-item-link"]') %>% 
    html_attr('href')
  precio_viejo <- cuadros[[i]] %>% 
    html_elements(css='[class="old-price"]') %>% 
    html_text2()
  precio_viejo <- reemplazar_vacio(precio_viejo)
  precio_nuevo <- cuadros[[i]] %>% 
    html_elements(css='[class="special-price"]') %>% 
    html_text2()
  precio_nuevo <- reemplazar_vacio(precio_nuevo)
  todo <- cuadros[[i]] %>% 
    html_text2()
  # Guardar temporal
  tmp <- tibble(nombre = nombre,
                precio_viejo = precio_viejo,
                precio_nuevo = precio_nuevo,
                todo = todo,
                url = url
  )
  },error=function(e){})
  datos_finales <- bind_rows(datos_finales,tmp)
  print(i)
}
