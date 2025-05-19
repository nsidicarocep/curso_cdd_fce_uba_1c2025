# Analizar precios de productos 

# Librerias

library(tidyverse)
library(rvest)
library(RSelenium)

# Abrir Selenium 
driver <- rsDriver(browser = c("firefox"),chromever = NULL,port=4323L)
remote_driver <- driver[["client"]] 

# Funciones creadas 
reemplaza_vacio <- function(x){
  if(is_empty(x)){
    x <- ''
  } else {x}
}


# Cargar URL 
pais_elegido <- 'Argentina'
urls <- 'https://www.fravega.com/'

# Productos a buscar 
productos <- tibble(categoria = character(),
                    busqueda_producto = character(),
                    busqueda_argentina = character()
)
productos <- productos %>% 
  bind_rows(tibble(categoria = c('Smart TV','Smartphone','Smartphone'),
                   busqueda_producto = c('Smart TV','iPhone','Samsung Galaxy'),
                   busqueda_argentina = c('Smart TV','iPhone','Samsung Galaxy')))
# Armar scraper 
data_final <- tibble()
i<- 1
for(i in 1:length(productos$categoria)){
  
  # Ir a la web 
  remote_driver$navigate(urls)
  
  # Ver si aparece popup y cerrarlo
  Sys.sleep(2)
  boton_pop <- remote_driver$findElements(using='css',value='[class="sc-fKWMtX kNlTZg"]')
  if(length(boton_pop)>0){
    boton_pop[[1]]$clickElement()
  }
  
  # Buscar producto 
  buscador <- remote_driver$findElement(using = 'css', value = '[class="sc-sLsrZ ewPfqC"]')
  #Enviar la informacion necesaria para iniciar sesión
  buscador$sendKeysToElement(list(productos$busqueda_producto[i]))
  #Clickear en buscar 
  button_element <- remote_driver$findElement(using = 'css', value = "[class='sc-fifgRP fjAeyP']")
  button_element$clickElement()
  
  # Buscar cantidad de elementos 
  Sys.sleep(2)
  total_productos <- remote_driver$findElement(using='css',value='[class="sc-7ca66079-9 iqDxcn"]')
  total_productos <- total_productos$getElementText()[[1]]
  total_productos <- str_remove(total_productos,' resultados$')
  total_productos <- str_remove_all(total_productos,'\\.')
  total_productos <- as.double(total_productos)
  
  productos_tmp <- ceiling(total_productos / 15) # Son 15 productos por pagina 
  
  # Guardar URL
  url_buscada <- unlist(remote_driver$getCurrentUrl())
  
  # En total son 33 pagians 
  j <- 1
  for(j in 1:(productos_tmp)){
    
    # Ir a la pagina buscada 
    remote_driver$navigate(paste0(url_buscada,'&page=',j))
    
    # Ver si aparece popup y cerrarlo
    Sys.sleep(2)
    boton_pop <- remote_driver$findElements(using='css',value='[class="sc-fKWMtX kNlTZg"]')
    if(length(boton_pop)>0){
      boton_pop[[1]]$clickElement()
    }
    
    
    #Mover las flechas para simular navegacion
    bajadas <- round(runif(1,1,16))
    webElem <- remote_driver$findElement("css", "body")
    if(as.integer(bajadas/2)==(bajadas/2)){
      webElem$sendKeysToElement(list(key = "down_arrow"))
    }
    for(o in 1:bajadas){
      if(o %in% c(4,5,8,11,12,15,16)){
        webElem$sendKeysToElement(list(key = "up_arrow"))
      }
      t2 <- runif(1,0,0.4)
      Sys.sleep(t2)
    }
    
    # Obtengo los elementos que tienen productos 
    box_data <- remote_driver$findElements(using='css',value='[class="sc-4007e61d-2 kBkTgN"]')
    # Chequeo que box_data tenga 15 elementos para ver si se encontro el elemento
    if(length(box_data) == 15) {
      cat('\nTODO OK: Se encontraron los 15 elementos')
    } else if(length(box_data) > 15) {
      cat('\nADVERTENCIA: Se encontraron elementos demas. Revisar que sucede')
    } else if(length(box_data) < 15 & length(box_data) > 0 & j < productos_tmp){
      cat('\nADVERTENCIA: Se encontraron menos de 15 elementos y no es la ultima pagina buscada. Revisar')
    } else if(length(box_data) == 0){
      cat('\nADVERTENCIA GRAVE: No se encontraron elementos. REVISAR URGENTE')
    }
    
    # Loop para levantar informacion de los 15 elementos
    data_tmp <- tibble()
    k <- 1
    for(k in 1:length(box_data)){
      # Nombre
      # Uso findChildElement porque esta dentro de otro elemento grande (se puede evitar, pero es mas ordenado)
      nombre <- box_data[[k]]$findChildElement(using='css',value='[class="sc-ca346929-0 czeMAx"]')
      # Me quedo con el texto con la funcion getElementText() y lo paso a vector con la funcion unlist
      nombre <- unlist(nombre$getElementText())
      # Utilizo la funcion creada al inicio para reemplazar los vacios si es que llega a haber
      nombre <- reemplaza_vacio(nombre)
      
      # O TAMBIEN PUEDO HACER UNA FUNCION PARA QUE NO SEA TAN REPETITIVO 
      generar_vector_dato <- function(nombre_variable,tipo_elemento,nombre_elemento){
        # Uso elements porque me permite manejar los casos que son vacios 
        nombre_variable <- box_data[[k]]$findChildElements(using=tipo_elemento,value=paste0('[class=','"',nombre_elemento,'"]'))
        if(length(nombre_variable)>0){
          nombre_variable <- unlist(nombre_variable[[1]]$getElementText())
        } else {
          nombre_variable <- reemplaza_vacio(nombre_variable)
        }
        return(nombre_variable)
      }
      
      precio_venta <- generar_vector_dato('precio_venta','css','sc-1d9b1d9e-0 OZgQ')
      precio_lista <- generar_vector_dato('precio_lista','css','sc-66d25270-0 eiLwiO')
      if(precio_lista == '' & precio_venta != ''){
        precio_lista <- precio_venta
      }
      descuento <- generar_vector_dato('descuento','css','sc-e2aca368-0 juwGno')
      precio_sin_impuestos <- generar_vector_dato('precio_sin_descuento','css','sc-9c27ad62-2 eKOGAP')
      precio_venta <- generar_vector_dato('precio_venta','css','sc-1d9b1d9e-0 OZgQ')
      tipo_envio <- generar_vector_dato('tipo_envio','css','sc-47d33c80-0 bYBa-Dq sc-47d33c80-2 deyCFb')
    
      # URL del producto
      url_producto <- box_data[[k]]$findChildElement(using='css',value='[class="sc-4007e61d-0 dcODtv"]')
      url_producto <- unlist(url_producto$getElementAttribute('href'))
      
      # Juntar todo
      tmp_final <- data.frame(nombre,precio_venta,precio_lista,descuento,precio_sin_impuestos,tipo_envio,url_producto)
      tmp_final <- as_tibble(tmp_final)
      
      tmp_final <- tmp_final %>% 
        mutate(producto_buscado = productos$busqueda_producto[i],
               producto_buscado_arg = productos$busqueda_argentina[i],
               categoria = productos$categoria[i])
      data_tmp <- bind_rows(data_tmp,tmp_final)
    }
    # Guardar datos de la pagina
    data_final <- bind_rows(data_final,data_tmp)
    
    Sys.sleep(runif(1,2,3))
    
  }
  
  print(i)
}

data_final <- data_final %>% 
  distinct()
