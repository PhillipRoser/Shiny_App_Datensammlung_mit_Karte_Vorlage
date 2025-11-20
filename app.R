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
library(lubridate)

# --- Daten & Einstellungen --------------------------------------------------------
Bezirk <- sf::st_read(here::here("Daten", "1 Grundlagen - Regierungsbezirke", "admin1_f.shp"),
                      quiet = TRUE) %>%
  filter(ADM != 2002) %>%
  st_transform(4326)

app_header <- readLines(here::here("Daten_App_Name.txt"))[1]
popup_spalte <- sub('.*:\\s*\\"([^\\"]+)\\".*', '\\1',
                    readLines(here::here("Daten_App_Name.txt"))[2])

data_path <- here::here("Daten.xlsx")
archive_path <- here::here("Daten", "Archiv")
dir.create(archive_path, showWarnings = FALSE)

read_data <- function(path){
  if(!file.exists(path)){
    stop("Datei existiert nicht: ", path)
  }
  
  dat <- readxl::read_excel(path)
  dat <- as.data.frame(dat)
  
  # Falls leer, Spalten-Typen erzwingen
  for(col in names(dat)){
    if(col %in% c("ID","created_at")) dat[[col]] <- as.character(dat[[col]])
    else if(col %in% c("lon","lat")) dat[[col]] <- as.numeric(dat[[col]])
    else dat[[col]] <- as.character(dat[[col]])
  }
  
  return(dat)
}

write_data <- function(df, path){ writexl::write_xlsx(df, path) }

# Archiv speichern
save_current_data <- function(cur){
  timestamp <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  arch_path <- file.path(archive_path, paste0(timestamp, " Daten.xlsx"))
  write_data(cur, arch_path)
}

# --- UI ---------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(app_header),
  
  fluidRow(
    column(3,
           div(style="display:flex; align-items:center;",
               textInput("search_place", NULL, placeholder="Ort suchen...", width="100%"),
               actionButton("search_button", "🔍", class="btn-secondary", style="margin-left:5px; height:45px;")
           )
    ),
    column(3, uiOutput("archive_ui")),
    column(3, align="center", actionButton("new_point", "➕ Neuen Eintrag setzen", class="btn-primary")),
    column(3, align="center", actionButton("toggle_basemap", "🗺️ Kartenansicht wechseln", class="btn-secondary")),
    column(3, align="center", selectInput("color_by", "Färbung nach Spalte:", choices=c("Einfarbig"=""), selected=""))
  )
  ,
  
  fluidRow(
    column(12, align="center",
           helpText("ℹ️ Um Titel oder Popup-Spalte zu ändern, bitte die .txt Datei im Verzeichnis bearbeiten."))
  ),
  
  leafletOutput("map", height=750),
  br(),
  DTOutput("selected_row_table"),
  actionButton("save_selected", "💾 Änderungen speichern"),
  actionButton("add_info", "ℹ️ Informationen hinzufügen"),
  br(), br(),
  h4("Alle Einträge"),
  DTOutput("all_table"),
  verbatimTextOutput("status")
)

# --- SERVER -----------------------------------------------------------------------
server <- function(input, output, session){
  
  df <- reactiveVal(read_data(data_path))

  basemap_state <- reactiveVal("osm")
  
  reactive_data <- reactive({
    sel <- input$archive_select
    if(is.null(sel) || sel=="Aktueller Datensatz"){
      df()
    } else {
      read_data(file.path(archive_path, sel))
    }
  })
  
  observe({
    # Archiv Dropdown
    files <- list.files(archive_path, pattern="\\.xlsx$", full.names=FALSE)
    choices <- c("Aktueller Datensatz", sort(files, decreasing=TRUE))
    output$archive_ui <- renderUI({
      selectInput("archive_select", "Datensatz wählen:", choices=choices, selected="Aktueller Datensatz")
    })
  })
  
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
    updateSelectInput(session,"color_by", choices=c("Einfarbig"="", choices), selected=isolate(input$color_by))
  })
  
  output$map <- renderLeaflet({
    leaflet() |> addProviderTiles(providers$OpenStreetMap) |>
      setView(11.5,48.9,7) |>
      addPolygons(data=Bezirk, fillColor="transparent", color="black", weight=2)
  })
  
  observe({
    cur <- reactive_data()
    if(nrow(cur)==0) return()
    color_col <- input$color_by
    colors <- if(is.null(color_col) || color_col=="") rep("blue", nrow(cur)) else {
      vals <- unique(cur[[color_col]])
      pal <- colorFactor(RColorBrewer::brewer.pal(min(8,length(vals)),"Set2"), domain=vals)
      pal(cur[[color_col]])
    }
    leafletProxy("map") |>
      clearGroup("points") |>
      addCircleMarkers(data=cur, lng=~lon, lat=~lat, layerId=~ID,
                       radius=6, color=colors, fillOpacity=0.8,
                       label=~cur[[popup_spalte]], group="points")
  })
  
  last_click <- reactiveVal(NULL)
  observeEvent(input$map_click,{
    last_click(list(lon=input$map_click$lng, lat=input$map_click$lat))
    leafletProxy("map") |> clearGroup("newpoint") |>
      addMarkers(lng=input$map_click$lng, lat=input$map_click$lat,
                 group="newpoint", popup="Neuer Punkt (nicht gespeichert)")
  })
  
  last_selected_id <- reactiveVal(NULL)
  selected_row_df <- reactiveVal(NULL)
  
  observeEvent(input$archive_select,{
    last_selected_id(NULL)
    selected_row_df(NULL)
    output$selected_row_table <- renderDT(NULL)
    output$all_table <- renderDT({
      datatable(reactive_data(), selection="single", rownames=FALSE, options=list(scrollX=TRUE, paging=FALSE))
    })
  })
  
  observeEvent(input$all_table_rows_selected,{
    if(input$archive_select != "Aktueller Datensatz") return()
    sel <- input$all_table_rows_selected; if(length(sel)==0) return()
    cur <- df()
    sel_id <- cur$ID[sel[1]]
    last_selected_id(sel_id)
    row <- cur |> filter(ID==sel_id)
    selected_row_df(row)
    output$selected_row_table <- renderDT({
      datatable(row, editable=TRUE, rownames=FALSE, options=list(dom='t'))
    })
  })
  
  observeEvent(input$map_marker_click,{
    if(input$archive_select != "Aktueller Datensatz") return()
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
  
  observe({
    sel_id <- last_selected_id(); cur <- reactive_data()
    leafletProxy("map") |> clearGroup("highlight")
    if(!is.null(sel_id) && sel_id %in% cur$ID){
      sel_row <- cur |> filter(ID==sel_id)
      leafletProxy("map") |>
        addCircleMarkers(data=sel_row, lng=~lon, lat=~lat,
                         radius=10, color="red", weight=3, fill=FALSE,
                         opacity=1, group="highlight")
    }
  })
  
  # --- Aktionen nur für aktuellen Datensatz ---
  observe({
    sel <- input$archive_select
    if(is.null(sel)) return()  # falls noch nichts ausgewählt
    if(sel != "Aktueller Datensatz"){
      shinyjs::disable("new_point")
      shinyjs::disable("save_selected")
      shinyjs::disable("add_info")
    } else {
      shinyjs::enable("new_point")
      shinyjs::enable("save_selected")
      shinyjs::enable("add_info")
    }
  })
  
  observeEvent(input$search_button, {
    req(input$search_place)
    place <- URLencode(input$search_place)
    url <- paste0("https://nominatim.openstreetmap.org/search?q=", place, "&format=json&limit=1")
    res <- jsonlite::fromJSON(url)
    
    if(length(res$lat) > 0){
      lat <- as.numeric(res$lat[1])
      lon <- as.numeric(res$lon[1])
      leafletProxy("map") |> setView(lng=lon, lat=lat, zoom=14)
    } else {
      showNotification("Ort nicht gefunden.", type="warning")
    }
  })
  
  
  # --- Neuer Eintrag ---
  observeEvent(input$new_point,{
    if(input$archive_select != "Aktueller Datensatz") return()
    coords <- last_click()
    if(is.null(coords)){
      showModal(modalDialog(title="Fehler", "Bitte zuerst einen Punkt auf der Karte anklicken.", easyClose=TRUE))
      return()
    }
    cur <- df(); meta_cols <- c("ID","created_at","lon","lat")
    user_cols <- setdiff(names(cur), meta_cols)
    ui_inputs <- lapply(user_cols, function(col) textInput(paste0("newcol_",col), col, ""))
    showModal(modalDialog(title="Neuen Eintrag hinzufügen",
                          tags$p(sprintf("Koordinaten: %.5f, %.5f", coords$lat, coords$lon)),
                          ui_inputs,
                          footer=tagList(modalButton("Abbrechen"), actionButton("save_new","Speichern")),
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
    df(cur)
    save_current_data(cur)
    write_data(cur, data_path)
    output$status <- renderText(paste0("Neuer Eintrag gespeichert (ID=",new_id,")"))
  })
  
  # --- Änderungen speichern ---
  observeEvent(input$save_selected, {
    cat("\n--- save_selected gestartet ---\n")
    
    # Änderungen nur für den aktuellen Datensatz zulassen
    if(input$archive_select != "Aktueller Datensatz") {
      showNotification("Archivierte Daten können nicht bearbeitet werden.", type="warning")
      return()
    }
    
    
    req(input$selected_row_table_cell_edit)
    
    edit <- input$selected_row_table_cell_edit
    i <- edit$row           
    colname <- names(df())[edit$col + 1]  
    value <- edit$value
    
    # Meta-Spalten dürfen nicht bearbeitet werden
    meta_cols <- c("ID", "created_at", "lon", "lat")
    if (colname %in% meta_cols) {
      showModal(modalDialog(
        title = "Bearbeitung nicht erlaubt",
        paste0("Die Spalte '", colname, "' kann nicht bearbeitet werden."),
        easyClose = TRUE
      ))
      return()
    }
    
    cur <- df()
    sel_row <- selected_row_df()
    if (is.null(sel_row)) return()
    
    id <- sel_row$ID[i]         
    idx <- which(cur$ID == id)  
    
    if(length(idx) != 1){
      msg <- paste0("Fehler: Zeile mit ID=", id, " nicht eindeutig gefunden")
      cat(msg, "\n")
      showNotification(msg, type = "error", duration = 5)
      return()
    }
    
    # --- Archiv sichern vor Änderungen ---
    arch_path <- here::here("Daten", "Archiv")
    if(!dir.exists(arch_path)) dir.create(arch_path)
    ts <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
    write_data(cur, file.path(arch_path, paste0(ts, " Daten.xlsx")))
    cat("Archiv erstellt: ", file.path(arch_path, paste0(ts, " Daten.xlsx")), "\n")
    
    # Änderungen anwenden
    cat("Update: ID=", id, ", Spalte=", colname, ", neuer Wert=", value, "\n")
    cur[idx, colname] <- value   
    df(cur)                       
    write_data(cur, data_path)    
    cat("Daten geschrieben für ID=", id, "\n")
    
    output$status <- renderText(paste0("Eintrag ID=", id, " gespeichert."))
    cat("--- save_selected beendet ---\n")
  })
  
  
  # --- Informationen anhängen ---
  observeEvent(input$add_info, {
    if(input$archive_select != "Aktueller Datensatz") return()
    sel_row <- selected_row_df(); if(is.null(sel_row) || nrow(sel_row)==0){
      showNotification("Keine Zeile ausgewählt.", type="warning"); return()
    }
    meta_cols <- c("ID","created_at","lon","lat")
    choices <- setdiff(names(df()), meta_cols)
    showModal(modalDialog(
      title="Information hinzufügen",
      textAreaInput("info_text","Text eingeben:","",width="100%",height="100px"),
      selectInput("info_column","Spalte auswählen:",choices=choices),
      footer=tagList(modalButton("Abbrechen"), actionButton("append_info","Zum Zellinhalt hinzufügen")),
      easyClose=TRUE
    ))
  })
  
  observeEvent(input$append_info,{
    removeModal()
    sel_row <- selected_row_df(); if(is.null(sel_row) || nrow(sel_row)==0) return()
    col_to_update <- input$info_column; text_to_add <- input$info_text
    cur <- df(); id <- sel_row$ID[1]; idx <- which(cur$ID==id)
    if(length(idx)!=1){
      showNotification(paste0("Fehler: Zeile mit ID=", id, " nicht gefunden."), type="error")
      return()
    }
    old_value <- as.character(cur[idx,col_to_update])
    if(is.na(old_value)) old_value <- ""
    cur[idx,col_to_update] <- paste0(old_value,"\n",Sys.Date(),":\n",text_to_add)
    df(cur)
    save_current_data(cur)
    write_data(cur, data_path)
    selected_row_df(cur[idx,,drop=FALSE])
    output$selected_row_table <- renderDT({
      datatable(cur[idx,,drop=FALSE], editable=TRUE, rownames=FALSE, options=list(dom='t'))
    })
    output$status <- renderText(paste0("Text zu Spalte '",col_to_update,"' hinzugefügt (ID=",id,")."))
  })
  
  # --- Alle Einträge Tabelle ---
  output$all_table <- renderDT({
    datatable(reactive_data(), selection="single", rownames=FALSE, options=list(scrollX=TRUE, paging=FALSE))
  })
  
}

shinyApp(ui, server)
