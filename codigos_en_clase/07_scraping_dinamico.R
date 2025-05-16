# Analizar precios de productos 

# Librerias

library(tidyverse)
library(rvest)
library(RSelenium)

# Abrir Selenium 
driver <- rsDriver(browser = c("firefox"),chromever = NULL,port=4321L)
remote_driver <- driver[["client"]] 

# Funciones creadas 
reemplaza_vacio <- function(x){
  if(is_empty(x)){
    x <- ''
  } else {x}
}


# Cargar URL 
pais_elegido <- 'Argentina'
urls <- 'https://www.mercadolibre.com.ar/'

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
  remote_driver$navigate('https://www.mercadolibre.com.ar/')
  
  # Buscar producto 
  buscador <- remote_driver$findElement(using = 'css', value = '[class="nav-search-input"]')
  #Enviar la informacion necesaria para iniciar sesión
  buscador$sendKeysToElement(list(productos$busqueda_producto[i]))
  #Clickear en buscar 
  button_element <- remote_driver$findElement(using = 'css', value = "[class='nav-icon-search']")
  button_element$clickElement()
  
  # Buscar elemento de filtrado de tiendas oficiales 
  tienda_oficial <- remote_driver$findElements(using='css',value='[aria-label="Solo tiendas oficiales"]')
  remote_driver$executeScript("arguments[0].scrollIntoView(true);", list(tienda_oficial[[1]]))
  # tienda_oficial[[1]]$clickElement()
  
  # Buscar cantidad de elementos 
  total_productos <- remote_driver$findElement(using='css',value='[class="ui-search-search-result__quantity-results"]')
  total_productos <- total_productos$getElementText()[[1]]
  total_productos <- str_remove(total_productos,' resultados$')
  total_productos <- str_remove_all(total_productos,'\\.')
  total_productos <- as.double(total_productos)
  
  if(total_productos > 2000) {
    total_productos <- 2268
  }
  productos_tmp <- ceiling(total_productos / 54)
  productos_tmp <- if(productos_tmp > 42){
    productos_tmp <- 42
  } else {
    productos_tmp <- productos_tmp
  }
  
  for(j in 1:(productos_tmp)){
    
    url_scrape <- remote_driver$getCurrentUrl()
    
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
    
    tmp <- read_html(url_scrape[[1]])
    box_data <- tmp %>% 
      html_elements(css='[class="ui-search-layout__item"]')
    if(length(box_data) == 0){
      box_data <- tmp %>% 
        html_elements(css='[class="andes-card ui-search-result ui-search-result--core andes-card--flat andes-card--padding-16"]')
    }
    data_tmp <- tibble()
    for(k in 1:length(box_data)){
      nombre <- box_data[[k]] %>%
        html_elements(css='[class="ui-search-item__group__element ui-search-link__title-card ui-search-link"]') %>%
        html_text()
      nombre <- reemplaza_vacio(nombre)
      
      # Precio de venta 
      precio_venta <- box_data[[k]] %>%
        html_elements(css='[class="ui-search-item__group__element ui-search-price__part-without-link"]') %>%
        html_elements(css='[class="andes-money-amount__fraction"]') %>%
        html_text()
      if(length(precio_venta)==2){
        precio_venta <- box_data[[k]] %>%
          html_elements(css='[class="ui-search-item__group__element ui-search-price__part-without-link"]') %>%
          html_elements(css='[class="ui-search-price__second-line"]') %>% 
          html_elements(css='[class="andes-money-amount__fraction"]') %>%
          html_text()
      }
      precio_venta <- reemplaza_vacio(precio_venta)
      
      # Moneda de venta 
      moneda_venta <- box_data[[k]] %>%
        html_elements(css='[class="ui-search-item__group__element ui-search-price__part-without-link"]') %>%
        html_elements(css='[class="andes-money-amount__currency-symbol"]') %>%
        html_text()
      if(length(moneda_venta)==2){
        moneda_venta <- box_data[[k]] %>%
          html_elements(css='[class="ui-search-item__group__element ui-search-price__part-without-link"]') %>%
          html_elements(css='[class="ui-search-price__second-line"]') %>% 
          html_elements(css='[class="andes-money-amount__currency-symbol"]') %>%
          html_text()
      }
      moneda_venta <- reemplaza_vacio(moneda_venta)
      
      # Precio de lista 
      precio_lista <- box_data[[k]] %>%
        html_elements(css='[class="andes-money-amount ui-search-price__part ui-search-price__part--small ui-search-price__original-value andes-money-amount--previous andes-money-amount--cents-superscript andes-money-amount--compact"]') %>%
        html_elements(css='[class="andes-money-amount__fraction"]') %>%
        html_text()
      precio_lista <- reemplaza_vacio(precio_lista)
      
      # Descuento
      descuento <- box_data[[k]] %>%
        html_elements(css='[class="ui-search-price__second-line__label"]') %>%
        html_text() %>%
        reemplaza_vacio()
      descuento <- reemplaza_vacio(descuento)
      
      # Tipo de envio
      envio <- box_data[[k]] %>%
        html_elements(css='[class="ui-search-item__group__element ui-search-item__group__element--shipping"]') %>%
        html_text()
      envio <- reemplaza_vacio(envio)
      
      # Destacados
      highlight <- box_data[[k]] %>%
        html_elements(css='[class="ui-search-item__brand-discoverability ui-search-item__group__element"]') %>%
        html_text() %>%
        reemplaza_vacio()
      
      # Cuotas
      cuotas_normales <- box_data[[k]] %>%
        html_elements(css='[class="ui-search-item__group__element ui-search-installments ui-search-color--BLACK"]') %>%
        html_text() %>%
        reemplaza_vacio()
      
      cuotas <- box_data[[k]] %>%
        html_elements(css='[class="ui-search-item__group__element ui-search-installments ui-search-color--LIGHT_GREEN"]') %>%
        html_text() %>%
        reemplaza_vacio()
      
      # Tienda 
      tienda_of <- box_data[[k]] %>% 
        html_elements(css='[class="ui-search-official-store-label ui-search-item__group__element ui-search-color--GRAY"]') %>% 
        html_text() %>% 
        reemplaza_vacio()
      
      # Todo el texto
      todo <- box_data[[k]] %>%
        html_elements(css='[class="ui-search-result__content"]') %>%
        html_text2() %>%
        reemplaza_vacio()
      
      # URL del producto
      url_producto <- box_data[[k]] %>%
        html_elements(css='[class="ui-search-item__group__element ui-search-link__title-card ui-search-link"]') %>%
        html_attr('href')
      
      # Juntar todo
      tmp_final <- data.frame(nombre,precio_venta,moneda_venta,precio_lista,cuotas,cuotas_normales,highlight,envio,descuento,tienda_of,url_producto,todo)
      tmp_final <- as_tibble(tmp_final)
      
      tmp_final <- tmp_final %>% 
        mutate(url_origen = url_scrape[[1]],
               producto_buscado = productos$busqueda_producto[i],
               producto_buscado_arg = productos$busqueda_argentina[i],
               categoria = productos$categoria[i])
      data_tmp <- rbind(data_tmp,tmp_final)
    }
    # Guardar datos de la pagina
    data_final <- rbind(data_final,data_tmp)
    # Esperar una cantidad de segundos random y cambiar de pagina
    boton_siguiente <- remote_driver$findElements(using = 'css',value='[class="andes-pagination__button andes-pagination__button--next"]')
    if(length(boton_siguiente)>0){
      remote_driver$executeScript("arguments[0].scrollIntoView(true);", list(boton_siguiente[[1]]))
      boton_siguiente[[1]]$clickElement() 
      paste(print('Click en página siguiente'),j)
    }
    
    Sys.sleep(runif(1,2,3))
    
  }
  
  print(i)
}

data_final <- data_final %>% 
  distinct()
