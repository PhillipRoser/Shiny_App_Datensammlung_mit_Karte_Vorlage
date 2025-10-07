library(shiny)
library(leaflet)
library(readxl)
library(writexl)
library(DT)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(RColorBrewer)


Bezirk    <- sf::st_read(here::here("Daten", "1 Grundlagen - Regierungsbezirke", "admin1_f.shp")) %>% filter(ADM != 2002) %>% sf::st_transform(4326)
app_header <- readLines(here::here("Daten_App_Name.txt"))[1]
popup_spalte <- sub('.*:\\s*\\"([^\\"]+)\\".*', '\\1',
                    readLines(here::here("Daten_App_Name.txt"))[2])

# --- Hilfsfunktionen ------------------------------------------------------------
read_data <- function(path){
  if(!file.exists(path)){
    message("Datei nicht gefunden, erstelle leere Tabelle mit Beispielspalten.")
    return(tibble::tibble(Spalte_A = logical(), Spalte_B = logical(), Spalte_C = logical()))
  }
  df <- readxl::read_excel(path)
  return(as.data.frame(df))
}

write_data <- function(df, path){
  writexl::write_xlsx(df, path)
}

# --- UI -------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Bayern-Karte: Punkte erfassen & bearbeiten"),
  
  # 🔹 Erste Zeile: Button + Dropdown nebeneinander
  fluidRow(
    column(
      width = 6,
      align = "center",
      actionButton("new_point", "➕ Neuen Eintrag setzen", class = "btn-primary")
    ),
    column(
      width = 6,
      align = "center",
      selectInput("color_by", "Färbung nach Spalte:", choices = NULL, selected = NULL, width = "80%")
    )
  ),
  
  # 🔹 Hinweistext unterhalb
  fluidRow(
    column(
      12,
      align = "center",
      helpText("ℹ️ Um Titel oder Popup-Spalte zu ändern, bitte die .txt Datei im Verzeichnis bearbeiten.")
    )
  ),
  
  # 🔹 Karte
  leafletOutput("map", height = 750),
  
  br(),
  DTOutput("selected_row_table"),
  actionButton("save_selected", "💾 Änderungen speichern"),
  br(), br(),
  h4("Alle Einträge"),
  DTOutput("all_table"),
  verbatimTextOutput("status")
)



# --- SERVER ---------------------------------------------------------------------
server <- function(input, output, session){
  
  data_path <- "Daten.xlsx"
  
  # Excel einlesen
  initial_df <- read_data(data_path)
  
  # Spalten-Setup
  ensure_meta <- function(df){
    if(!"ID" %in% colnames(df)) df$ID <- character(nrow(df))
    df$ID <- as.character(df$ID)
    
    if(!"created_at" %in% colnames(df)) df$created_at <- as.character(rep(NA, nrow(df)))
    if(!"lon" %in% colnames(df)) df$lon <- numeric(nrow(df))
    if(!"lat" %in% colnames(df)) df$lat <- numeric(nrow(df))
    df$created_at <- as.character(df$created_at)
    
    meta_cols <- c("ID","created_at","lon","lat")
    user_cols <- setdiff(colnames(df), meta_cols)
    for(col in user_cols){
      df[[col]] <- as.character(df[[col]])
    }
    
    df <- df %>% select(ID, all_of(user_cols), created_at, lon, lat)
    return(df)
  }
  
  initial_df <- ensure_meta(initial_df)
  df <- reactiveVal(initial_df)
  
  # Regierungsbezirke laden
  bezirke_sf <- reactive({
    sf::st_read(here::here("Daten", "1 Grundlagen - Regierungsbezirke", "admin1_f.shp"), quiet = TRUE) %>%
      filter(ADM != 2002) %>%
      st_transform(4326)
  })
  
  
  # Dropdown füllen, wenn df geladen
  observe({
    meta_cols <- c("ID","created_at","lon","lat")
    choices <- setdiff(colnames(df()), meta_cols)
    updateSelectInput(session, "color_by", choices = c("Keine" = "", choices))
  })
  
  # Karte rendern
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 11.5, lat = 48.9, zoom = 7) %>%
      addPolygons(
        data = bezirke_sf(),
        fillColor = "transparent",   # keine Füllung
        color = "black",             # Umrissfarbe
        weight = 2,                  # Liniendicke
        opacity = 1,
        labelOptions = labelOptions(
          direction = "auto",
          style = list("font-size" = "12px")
        ))
    
  })
  
  # Punkte aktualisieren, abhängig von Farbauswahl
  observe({
    cur <- df()
    if(nrow(cur)==0) return()
    color_col <- input$color_by
    
    if(is.null(color_col) || color_col == ""){
      pal <- colorFactor("blue", domain = NULL)
      colors <- rep("blue", nrow(cur))
    } else {
      vals <- unique(cur[[color_col]])
      pal <- colorFactor(brewer.pal(min(8, length(vals)), "Set2"), domain = vals)
      colors <- pal(cur[[color_col]])
    }
    
    leafletProxy("map") %>%
      clearGroup("points") %>%
      addCircleMarkers(data = cur,
                       lng = ~lon, lat = ~lat,
                       layerId = ~ID,
                       radius = 6,
                       color = colors,
                       fillOpacity = 0.8,
                       label = ~cur[[popup_spalte]],
                       group = "points")
  })
  
  # Klick auf Karte
  last_click <- reactiveVal(NULL)
  observeEvent(input$map_click, {
    last_click(list(lon=input$map_click$lng, lat=input$map_click$lat))
    leafletProxy("map") %>%
      clearGroup("newpoint") %>%
      addMarkers(lng=input$map_click$lng, lat=input$map_click$lat,
                 group="newpoint", popup="Neuer Punkt (nicht gespeichert)")
  })
  
  # Neuer Eintrag Modal
  observeEvent(input$new_point, {
    coords <- last_click()
    if(is.null(coords)){
      showModal(modalDialog(title="Fehler", "Bitte zuerst einen Punkt auf der Karte anklicken.", easyClose=TRUE))
      return()
    }
    meta_cols <- c("ID","created_at","lon","lat")
    user_cols <- setdiff(colnames(df()), meta_cols)
    ui_inputs <- lapply(user_cols, function(col){ textInput(paste0("newcol_", col), col, "") })
    showModal(modalDialog(
      title = "Neuen Eintrag hinzufügen",
      tags$p(sprintf("Koordinaten: %.5f, %.5f", coords$lat, coords$lon)),
      ui_inputs,
      footer = tagList(modalButton("Abbrechen"), actionButton("save_new", "Speichern")),
      size = "m",
      easyClose = TRUE
    ))
  })
  
  # Speichern neuer Eintrag
  observeEvent(input$save_new, {
    removeModal()
    coords <- last_click()
    if(is.null(coords)) return()
    cur <- df()
    meta_cols <- c("ID","created_at","lon","lat")
    user_cols <- setdiff(colnames(cur), meta_cols)
    
    new_id <- ifelse(nrow(cur)==0, "1", as.character(max(as.numeric(cur$ID), na.rm=TRUE) + 1))
    new_row <- as.list(rep(NA, ncol(cur)))
    names(new_row) <- colnames(cur)
    new_row$ID <- new_id
    new_row$created_at <- as.character(Sys.Date())
    new_row$lon <- coords$lon
    new_row$lat <- coords$lat
    for(col in user_cols){
      val <- input[[paste0("newcol_", col)]]
      new_row[[col]] <- ifelse(is.null(val), NA, val)
    }
    
    df(ensure_meta(bind_rows(cur, as.data.frame(new_row, stringsAsFactors=FALSE))))
    try({ write_data(df(), data_path) }, silent=TRUE)
    output$status <- renderText(paste0("Neuer Eintrag gespeichert (ID=", new_id, ")"))
  })
  
  # Tabellenanzeige
  output$all_table <- renderDT({
    datatable(df(), selection='single', rownames=FALSE, options=list(scrollX=TRUE, paging=FALSE))
  })
  
  last_selected_id <- reactiveVal(NULL)
  selected_row_df <- reactiveVal(NULL)
  
  observeEvent(input$map_marker_click, {
    sel_id <- input$map_marker_click$id
    last_selected_id(sel_id)
    if(!is.null(sel_id)){
      row <- df() %>% filter(ID==sel_id)
      selected_row_df(row)
      output$selected_row_table <- renderDT({ datatable(row, editable=TRUE, rownames=FALSE, options=list(dom='t')) })
    }
  })
  
  observeEvent(input$all_table_rows_selected, {
    sel <- input$all_table_rows_selected
    if(length(sel)==0) return()
    sel_id <- df()$ID[sel[1]]
    last_selected_id(sel_id)
    row <- df() %>% filter(ID==sel_id)
    selected_row_df(row)
    output$selected_row_table <- renderDT({ datatable(row, editable=TRUE, rownames=FALSE, options=list(dom='t')) })
  })
  
  # 🔴 Punkt auf der Karte hervorheben, wenn in Tabelle ausgewählt
  observe({
    sel_id <- last_selected_id()
    cur <- df()
    
    leafletProxy("map") %>%
      clearGroup("highlight")  # alte Hervorhebung löschen
    
    if(!is.null(sel_id) && sel_id %in% cur$ID) {
      sel_row <- cur %>% filter(ID == sel_id)
      leafletProxy("map") %>%
        addCircleMarkers(
          data = sel_row,
          lng = ~lon,
          lat = ~lat,
          radius = 10,                # etwas größerer Kreis
          color = "red",              # roter Rand
          weight = 3,                 # dicke Linie
          fill = FALSE,               # nur Kreisumrandung
          opacity = 1,
          group = "highlight"
        )
    }
  })
  
  
  observeEvent(input$selected_row_table_cell_edit, {
    info <- input$selected_row_table_cell_edit
    row <- selected_row_df()
    if(is.null(row)) return()
    colname <- colnames(row)[info$col + 1]
    row[1, colname] <- info$value
    selected_row_df(row)
  })
  
  observeEvent(input$save_selected, {
    edited <- selected_row_df()
    if(is.null(edited)) return()
    cur <- df()
    idx <- which(cur$ID==edited$ID)
    if(length(idx)==1){
      cur[idx,] <- edited
      df(cur)
      try({ write_data(df(), data_path) }, silent=TRUE)
      output$status <- renderText(paste0("Eintrag ID=", edited$ID, " gespeichert."))
    }
  })
  
}

shinyApp(ui, server)
