# --- Pakete laden -----------------------------------------------------------------
library(shiny)
library(leaflet)
library(leaflet.extras)
library(readxl)
library(writexl)
library(DT)
library(dplyr)
library(sf)
library(jsonlite)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(RColorBrewer)
library(here)

# --- Daten & Einstellungen --------------------------------------------------------
Bezirk <- sf::st_read(here::here("Daten", "1 Grundlagen - Regierungsbezirke", "admin1_f.shp"),
                      quiet = TRUE) %>%
  filter(ADM != 2002) %>%
  st_transform(4326)

app_header <- readLines(here::here("Daten_App_Name.txt"))[1]
popup_spalte <- sub('.*:\\s*\\"([^\\"]+)\\".*', '\\1',
                    readLines(here::here("Daten_App_Name.txt"))[2])

read_data <- function(path){
  if(!file.exists(path)){
    return(tibble::tibble(Spalte_A=character(), Spalte_B=character(),
                          Spalte_C=character(), ID=character(),
                          created_at=character(), lon=numeric(), lat=numeric()))
  }
  readxl::read_excel(path) |> as.data.frame()
}
write_data <- function(df, path){ writexl::write_xlsx(df, path) }

# --- UI ---------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(app_header),
  
  # 🔹 Buttons + Dropdown
  fluidRow(
    column(
      3,
      align="center",
      actionButton("new_point", "➕ Neuen Eintrag setzen", class="btn-primary")
    ),
    column(
      3,
      align="center",
      selectInput("color_by", "Färbung nach Spalte:",
                  choices=c("Einfarbig"=""), selected="")
    ),
    column(
      3,
      align="center",
      actionButton("toggle_basemap", "🗺️ Kartenansicht wechseln", class="btn-secondary")
    ),
    column(
      3,
      align="center",
      textInput("search_place", NULL, placeholder="Ort suchen..."),
      actionButton("search_button", "🔍")
    )
  ),
  
  fluidRow(
    column(12, align="center",
           helpText("ℹ️ Um Titel oder Popup-Spalte zu ändern, bitte die .txt Datei im Verzeichnis bearbeiten."))
  ),
  
  leafletOutput("map", height=750),
  br(),
  DTOutput("selected_row_table"),
  actionButton("save_selected", "💾 Änderungen speichern"),
  br(), br(),
  h4("Alle Einträge"),
  DTOutput("all_table"),
  verbatimTextOutput("status")
)

# --- SERVER -----------------------------------------------------------------------
server <- function(input, output, session){
  
  data_path <- "Daten.xlsx"
  df <- reactiveVal(read_data(data_path))
  basemap_state <- reactiveVal("osm")
  
  observeEvent(input$toggle_basemap,{
    if(basemap_state()=="osm"){
      basemap_state("sat")
      leafletProxy("map") |> clearTiles() |> addProviderTiles(providers$Esri.WorldImagery)
    } else {
      basemap_state("osm")
      leafletProxy("map") |> clearTiles() |> addProviderTiles(providers$OpenStreetMap)
    }
  })
  
  observe({
    meta_cols <- c("ID","created_at","lon","lat")
    choices <- setdiff(names(df()), meta_cols)
    updateSelectInput(session,"color_by",
                      choices=c("Einfarbig"="", choices),
                      selected=isolate(input$color_by))
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      setView(11.5,48.9,7) |>
      addPolygons(data=Bezirk, fillColor="transparent", color="black", weight=2)
  })
  
  observe({
    cur <- df()
    if(nrow(cur)==0) return()
    color_col <- input$color_by
    if(is.null(color_col) || color_col==""){
      colors <- rep("blue", nrow(cur))
    } else {
      vals <- unique(cur[[color_col]])
      pal <- colorFactor(RColorBrewer::brewer.pal(min(8,length(vals)),"Set2"), domain=vals)
      colors <- pal(cur[[color_col]])
    }
    leafletProxy("map") |>
      clearGroup("points") |>
      addCircleMarkers(data=cur, lng=~lon, lat=~lat, layerId=~ID,
                       radius=6, color=colors, fillOpacity=0.8,
                       label=~cur[[popup_spalte]], group="points")
  })
  
  # --- 🔍 Echtort-Suche über OSM Nominatim ---------------------------------------
  observeEvent(input$search_button,{
    query <- input$search_place
    if(is.null(query) || query=="") return()
    url <- paste0("https://nominatim.openstreetmap.org/search?format=json&q=",
                  URLencode(query, reserved=TRUE))
    res <- tryCatch(jsonlite::fromJSON(url), error=function(e) data.frame())
    if(nrow(res)>0){
      lon <- as.numeric(res$lon[1])
      lat <- as.numeric(res$lat[1])
      leafletProxy("map") |> setView(lng=lon, lat=lat, zoom=12)
    } else {
      showNotification("Ort nicht gefunden.", type="error")
    }
  })
  
  # --- Punkt hinzufügen ----------------------------------------------------------
  last_click <- reactiveVal(NULL)
  observeEvent(input$map_click,{
    last_click(list(lon=input$map_click$lng, lat=input$map_click$lat))
    leafletProxy("map") |> clearGroup("newpoint") |>
      addMarkers(lng=input$map_click$lng, lat=input$map_click$lat,
                 group="newpoint", popup="Neuer Punkt (nicht gespeichert)")
  })
  
  observeEvent(input$new_point,{
    coords <- last_click()
    if(is.null(coords)){
      showModal(modalDialog(title="Fehler", "Bitte zuerst einen Punkt auf der Karte anklicken.",
                            easyClose=TRUE)); return()
    }
    meta_cols <- c("ID","created_at","lon","lat")
    user_cols <- setdiff(names(df()), meta_cols)
    ui_inputs <- lapply(user_cols, function(col) textInput(paste0("newcol_",col), col, ""))
    showModal(modalDialog(title="Neuen Eintrag hinzufügen",
                          tags$p(sprintf("Koordinaten: %.5f, %.5f", coords$lat, coords$lon)),
                          ui_inputs,
                          footer=tagList(modalButton("Abbrechen"),
                                         actionButton("save_new","Speichern")),
                          easyClose=TRUE))
  })
  
  observeEvent(input$save_new,{
    removeModal()
    coords <- last_click(); if(is.null(coords)) return()
    cur <- df(); meta_cols <- c("ID","created_at","lon","lat")
    user_cols <- setdiff(names(cur), meta_cols)
    new_id <- ifelse(nrow(cur)==0,"1",as.character(max(as.numeric(cur$ID),na.rm=TRUE)+1))
    new_row <- as.list(rep(NA,length(names(cur)))); names(new_row)<-names(cur)
    new_row$ID<-new_id; new_row$created_at<-as.character(Sys.Date())
    new_row$lon<-coords$lon; new_row$lat<-coords$lat
    for(col in user_cols){
      val <- input[[paste0("newcol_",col)]]
      new_row[[col]] <- ifelse(is.null(val), NA, val)
    }
    cur <- bind_rows(cur, as.data.frame(new_row, stringsAsFactors=FALSE))
    df(cur); write_data(cur, data_path)
    output$status <- renderText(paste0("Neuer Eintrag gespeichert (ID=",new_id,")"))
  })
  
  # Tabellen
  output$all_table <- renderDT({
    datatable(df(), selection="single", rownames=FALSE,
              options=list(scrollX=TRUE, paging=FALSE))
  })
  
  last_selected_id <- reactiveVal(NULL)
  selected_row_df <- reactiveVal(NULL)
  observeEvent(input$map_marker_click,{
    sel_id <- input$map_marker_click$id
    last_selected_id(sel_id)
    if(!is.null(sel_id)){
      row <- df() |> filter(ID==sel_id)
      selected_row_df(row)
      output$selected_row_table <- renderDT({
        datatable(row, editable=TRUE, rownames=FALSE, options=list(dom='t'))
      })
    }
  })
  observeEvent(input$all_table_rows_selected,{
    sel <- input$all_table_rows_selected; if(length(sel)==0) return()
    sel_id <- df()$ID[sel[1]]
    last_selected_id(sel_id)
    row <- df() |> filter(ID==sel_id)
    selected_row_df(row)
    output$selected_row_table <- renderDT({
      datatable(row, editable=TRUE, rownames=FALSE, options=list(dom='t'))
    })
  })
  
  observe({
    sel_id <- last_selected_id(); cur <- df()
    leafletProxy("map") |> clearGroup("highlight")
    if(!is.null(sel_id) && sel_id %in% cur$ID){
      sel_row <- cur |> filter(ID==sel_id)
      leafletProxy("map") |>
        addCircleMarkers(data=sel_row, lng=~lon, lat=~lat,
                         radius=10, color="red", weight=3, fill=FALSE,
                         opacity=1, group="highlight")
    }
  })
  
  observeEvent(input$save_selected,{
    edited <- selected_row_df(); if(is.null(edited)) return()
    cur <- df(); idx <- which(cur$ID==edited$ID)
    if(length(idx)==1){
      cur[idx,] <- edited; df(cur); write_data(cur, data_path)
      output$status <- renderText(paste0("Eintrag ID=",edited$ID," gespeichert."))
    }
  })
}

shinyApp(ui, server)
