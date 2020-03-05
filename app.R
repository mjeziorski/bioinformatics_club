library(shiny)
library(tidyverse)
library(janitor)
library(mxmaps)

# Enlace a los datos cortesia de Ana Betty Villaseñor
# https://datos.gob.mx/busca/dataset/estadisticas-de-mujeres-indicadores-de-
# inclusion-social-igualdad-y-empoderamiento/resource/
# a9b67eea-ef3a-4800-a02d-4f2ffa3b002a

# Paso 1: importar datos, usando locale "latin1" para conservar acentos
# Paso 2: limpiar nombres de variables
# Paso 3: unir con los datos de mxmaps para incorporar nombres compactos de los estados
# Paso 4: quitar valores para nacional
# Paso 5: corregir datos para Ciudad de México
# Paso 6: convertir valores de región a números

homicidios <- readr::read_csv("http://datos.inmujeres.gob.mx/archivo.php?f=f426d&t=csv",
                              locale = locale(encoding = "latin1")) %>%
    dplyr::rename(value = `Tasa por cada cien mil mujeres`) %>%
    janitor::clean_names() %>%
    left_join(select(df_mxstate, state_name_official, state_name, region),
              by = c("entidad" = "state_name_official")) %>%
    mutate(entidad = coalesce(state_name, entidad)) %>%
    filter(entidad != "Nacional") %>%
    replace_na(list(state_name = "Ciudad de México", region = "09")) %>%
    mutate(region = as.integer(region))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Homicidios de mujeres en México, 1990-2018"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "fecha",
                           label = "Escoger el año:",
                           choices = unique(homicidios$fecha),
                           selected = 2018)
        ),

        # Show a map of Mexico
        mainPanel(
           plotOutput("mexmap")
        )
    )
)

# Define server logic required to draw a map
server <- function(input, output) {

    output$mexmap <- renderPlot({
        # choose year based on input$fecha from ui.R
        x <- filter(homicidios, fecha == input$fecha) %>%
            select(region, value)

        # draw the map with the specified year
        mxstate_choropleth(x, title = "Homicidios por estado", num_colors = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
