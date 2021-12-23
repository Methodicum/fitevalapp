#### Packages ####
# Pakete installieren noch hinzufügen!
library(shiny)
library(DT)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(see)
library(shinyWidgets)
library(shinythemes)


#### SERVER ####
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  lexica <- read.table("lexica.txt", header = T, sep = "\t", encoding = "UTF-8")
  lexica_long <- tidyr::pivot_longer(lexica, cols = everything())
  lexica_long <- lexica_long[nchar(lexica_long$value) > 0,]
  
  lexica_long$value <- tolower(lexica_long$value)
  
  lexica_long$value <- gsub("Ã¼", "ue", lexica_long$value) # kleines ü (ue)
  lexica_long$value <- gsub("Ãœ", "Ue", lexica_long$value) # großes Ü (Ue)
  lexica_long$value <- gsub("Ã¤", "ae", lexica_long$value) # kleines ä (ae)
  lexica_long$value <- gsub("Ã¶", "oe", lexica_long$value) # kleines ö (oe)
  lexica_long$value <- gsub("ÃŸ", "ss", lexica_long$value) # kleines sz/ß
  
  #### Tab: Wörterbuch ####
  # Zuerst das Lexikon als veränderbare Datentabelle ausgeben
  output$lexi <- renderDT(lexica_long, server = T,
                          editable = "cell",
                          rownames = F,
                          colnames = c("Sportkategorie", "Begriff"),
                          filter = "top",
                          options = list(
                            pageLength = 10
                          ))
  
  # Event: sobald ein Eintrag in der Tabelle geändert wird:
  observeEvent(input$lexi_cell_edit, {
    lexica_long <<- editData(data = lexica_long, 
                             info = input$lexi_cell_edit,
                             proxy = "lexi",
                             rownames = F
    )
  })
  
  
  #### Tab: Externe Daten einlesen ####
  # Reaktives Element: einlesen eines Datensatzes.
  # Reaktivität ermöglicht, dass später Spaltenauswahl aktualisier wird
  df <- reactive({
    # Formatierungen
    data_dec <- input$Data_Dec
    data_sep <- input$Data_Sep
    data_head <- input$Data_Head
    data_encode <- input$Data_Encode
    inFile <- input$Import
    DataFile <- inFile$datapath
    if(is.null(inFile)) return(NULL)
    read.table(DataFile,
               header = data_head,
               sep = data_sep,
               dec = data_dec)
  })
  
  
  # Observer definieren: hier wird geschaut, ob ein reaktives Element
  # genutzt wurde. Sobald dies passiert, werden die Auswahloptionen
  # angepasst
  observeEvent(input$Import, {
    # Aktualisierung der Auswahloption (single choice) für die Personen-ID
    # Variable
    updateSelectInput(session, inputId = "ID_Var",
                      choices = names(df()),
                      selected = "")
    # Aktualisierung der Auswahloption für die Variablen, in denen
    # die Text-Einträge stehen.
    updateSelectizeInput(session, inputId = "Variablen",
                         choices = names(df()))
    updateSelectizeInput(session, inputId = "Variablen2",
                         choices = names(df()))
    updateSelectizeInput(session, inputId = "Variablen3",
                         choices = names(df()))
    
  })
  
  # Output für die Tabellenvorschau: sollte keine Auswahl getroffen werden, so
  # erscheint nur ein Hinweistext, der Auffordert eine Auswahl zu treffen.    
  # Sollten bei der ID/Text-Spalten eine Auswahl getroffen worden
  # sein, so werden die ausgewählten Spalten gezeigt,
  output$daten <- renderDT({
    if(is.null(input$Variablen) & is.null(input$ID_Var))
      NULL
    else
      head(df()[, names(df()) %in% c(input$ID_Var, input$Variablen,
                                     input$Variablen2,
                                     input$Variablen3)], n = 5) %>%
      as.data.frame() %>% 
      datatable(., rownames = F,
                options = list(scrollX = T,
                               pagelength = 5,
                               dom = "tip"))
  })
  
  
  # Platzhalter für veränderbare Daten (wird für observeEvent benötigt)
  DFs <- reactiveValues(df2 = NA)
  
  ## EVENT: Drücken des Knopfes Aufbereiten.
  # Dies führt zu einer Datenaufbereitung der zuvor ausgewählten Variablen
  # Die Aufbereitung wird in einem eigenen Objekt "x" abgespeichert, das
  # wiederum an das reactiveValue-Objekt "Dfs" übergeben wird.
  observeEvent(input$Aufbereiten, {
    
    idvar <- input$ID_Var
    text_entry <- df()[, names(df()) %in% c(input$ID_Var, 
                                            input$Variablen)]
    # Falls Werte mit "-99" kodiert sind, werden diese in NA umgewandelt
    text_entry <- as.data.frame(sapply(text_entry, 
                                       function(x) {
                                         ifelse(x == "-99", NA, x)
                                       }))
    
    ## Texteinträge long-format
    text_entry <- tidyr::pivot_longer(text_entry, 
                                      cols = -idvar,
                                      names_to = "Variablenname",
                                      values_to = "Eintrag")
    
    # ASCII-Kodierungen manuell ersetzen, sollte das Encoding nicht klappen...
    text_entry$Eintrag <- gsub("Ã¼|ü", "ue", text_entry$Eintrag) # kleines ü (ue)
    text_entry$Eintrag <- gsub("Ãœ|Ü", "Ue", text_entry$Eintrag) # großes Ü (Ue)
    text_entry$Eintrag <- gsub("Ã¤|ä", "ae", text_entry$Eintrag) # kleines ä (ae)
    text_entry$Eintrag <- gsub("Ã¶|ö", "oe", text_entry$Eintrag) # kleines ö (oe)
    text_entry$Eintrag <- gsub("ÃŸ|ß", "ss", text_entry$Eintrag) # kleines sz/ß
    
    # UTF-8 Format 
    text_entry$Eintrag <- enc2utf8(as.character(text_entry$Eintrag))
    
    
    ## wie viele Tage pro Woche? long-format
    sport_tim <- df()[, names(df()) %in% c(input$ID_Var,
                                           input$Variablen2)]
    sport_tim <- as.data.frame(sapply(sport_tim, function(x) {
      ifelse(x == "-99", NA, x)
    }))
    sport_tim <- tidyr::pivot_longer(sport_tim,
                                     cols = -idvar,
                                     names_to = "Variablenname2",
                                     values_to = "times_of_sport")
    
    # Wie lange pro Einheit? long-format
    sport_dur <- df()[, names(df()) %in% c(input$ID_Var,
                                           input$Variablen3)]
    sport_dur <- as.data.frame(sapply(sport_dur, function(x) {
      ifelse(x == "-99", NA, x)
    }))
    sport_dur <- tidyr::pivot_longer(sport_dur,
                                     cols = -idvar,
                                     names_to = "Variablenname3",
                                     values_to = "duration_per_unit")
    
    # gesamtdatensatz im long-format erstellen
    # die id-variablen werden nur aus einem Datensatz benötigt
    x <- as.data.frame(cbind(text_entry, 
                             sport_tim[,3], 
                             sport_dur[,3]))
    
    x$times_of_sport <- as.numeric(x$times_of_sport)
    x$duration_per_unit <- as.numeric(x$duration_per_unit)
    
    
    # NA-Einträge leeren: sobald kein text-eintrag vorliegt -> raus
    x <- filter(x, !is.na(text_entry))
    # Variablennamen ergänzen, damit später klar ist, das dies die bereits
    # fertig aufbereiteten und kategorisierten variablen sind
    x$Variablenname <- paste(x$Variablenname, "neu", sep = "_")
    
    # # ASCII-Kodierungen manuell ersetzen, sollte das Encoding nicht klappen...
    # x$Eintrag <- gsub("Ã¼", "ü", x$Eintrag) # kleines ü (ue)
    # x$Eintrag <- gsub("Ãœ", "Ü", x$Eintrag) # großes Ü (Ue)
    # x$Eintrag <- gsub("Ã¤", "ä", x$Eintrag) # kleines ä (ae)
    # x$Eintrag <- gsub("Ã¶", "ö", x$Eintrag) # kleines ö (oe)
    # x$Eintrag <- gsub("ÃŸ", "ß", x$Eintrag) # kleines sz/ß
    
    # Einträge aus dem Lexikon werden gematcht mit den "wahren" Einträgen
    # aus dem Datensatz. Die Einträge werden dafür in lower-case formatiert
    # WICHTIG: im Lexikon dürfen keine regex (| als "oder") genutzt werden,
    # da sich dies mit dem Argument collapse aus der nachfolgenden Funktion
    # beißt
    x$value <- stringr::str_match(tolower(x$Eintrag), 
                                  paste(lexica_long$value, collapse="|"))
    
    
    # Kategorisierungen anfügen. Alle nicht kategorisierbaren Einträge
    # erhalten ein NA-Eintrag
    x <- merge(x, lexica_long, by = "value", all.x = TRUE)
    # value-dummy-variable entfernen
    x <- dplyr::select(x, -value)
    # NA in den Kategorien werden inhaltlich kodiert, damit sie
    # aktiv gesucht werden können
    # x$name <- ifelse(is.na(x$name), "NICHT KATEGORISIERT!", x$name)
    x$name <- case_when(is.na(x$Eintrag) ~ "fehlend",
                        is.na(x$name) ~ "NICHT KATEGORISIERT",
                        TRUE ~ x$name)
    
    if(input$adjustDuration == "ja" & input$adjustFrequency == "ja") {
      x$SA <- case_when(x$name == "kein" ~ 0,
                        x$duration_per_unit > input$cutDuration & x$times_of_sport > input$cutFrequency ~ input$cutDuration*input$cutFrequency/4,
                        x$duration_per_unit > input$cutDuration & x$times_of_sport < input$cutFrequency ~ input$cutDuration*x$times_of_sport/4,
                        x$duration_per_unit < input$cutDuration & x$times_of_sport > input$cutFrequency ~ x$duration_per_unit*input$cutFrequency/4,
                        TRUE ~ x$duration_per_unit*x$times_of_sport/4)
      
    } else if(input$adjustDuration == "ja" & input$adjustFrequency == "nein") {
      x$SA <- case_when(x$name == "kein" ~ 0,
                        x$duration_per_unit > input$cutDuration ~ input$cutDuration*x$times_of_sport/4,
                        TRUE ~ x$duration_per_unit*x$times_of_sport/4)
      
    } else if(input$adjustDuration == "nein" & input$adjustFrequency == "ja") {
      x$SA <- case_when(x$name == "kein" ~ 0,
                        x$times_of_sport > input$cutFrequency ~ x$duration_per_unit*input$cutFrequency/4,
                        TRUE ~ x$duration_per_unit*x$times_of_sport/4)
      
    } else {
      x$SA <- case_when(x$name == "kein" ~ 0,
                        TRUE ~ x$duration_per_unit*x$times_of_sport/4)
    }
    
    
    
    
    # Messzeitpunkt bei Bedarf extra kodieren!
    if(input$MZPID != "keine Messwiederholung") {
      x$MZP <- stringr::str_extract(x$Variablenname, input$MZPID)
    } else {x$MZP <- "keine Messwiederholung"}
    
    
    DFs$df2 <- x # übergibt "x" an reactiveValue-Objekt oben!!
  })
  
  observeEvent(input$Aufbereiten, {
    output$aufbereitet_info <- renderText({
      paste("<font color=\"#FF0000\">Die Aufbereitung ist abgeschlossen. Für eine weitere Bearbeitung sowie Export der Daten bitte oben auf den Reiter \"Daten aufbereiten\" klicken.</font>")
    })
  })
  
  #### Tab: Daten aufbereiten ####
  ## OBSERVER für das reactiveValue-Objekt
  # Notiz: läuft scheinbar auch ohne aber in Beispielen war das immer drin
  # also hier auch noch drin
  observe({
    DFs$df2
  })
  
  # Tabellenoutput
  observeEvent(input$getData, {
    # isolate nutzen, damit das data frame nicht bei einem Eintrag
    # immer wieder resettet, sondern auf der Seite bleibt,
    # auf der die Änderung passiert.
    output$editDF <- renderDT(isolate(DFs$df2),
                              server = T,
                              editable = "cell",
                              rownames = F,
                              colnames = c("Case-Id",
                                           "Item",
                                           "Raw text entry",
                                           "Frequency (per weeK)",
                                           "Duration",
                                           "Category",
                                           "Scoring",
                                           "Measurement time point"),
                              filter = "top",
                              options = list(
                                # dom = 'Bfrtip',
                                pageLength = 5
                              )
    )
  })
  
  
  
  ## EVENT: sobald ein Eintrag in der Tabelle geändert wird:
  observeEvent(input$editDF_cell_edit, {
    DFs$df2 <<- editData(data = DFs$df2,
                         info = input$editDF_cell_edit,
                         proxy = "editDF", 
                         resetPaging = FALSE,
                         rownames = FALSE
    )
  })
  
  ## EVENT: sobald der Knopf "Export" gedrückt wird.
  # Der bearbeitete Datensatz wird dann in wide-Foramt gepackt
  # und als csv (;-getrennt) exportiert.
  # Außerdem wird ein Datensatz exportiert in dem nur die Personenscores
  # pro Messzeitpunkt enthalten sind. Auch dieser ist im wide-format exportiert.
  # observeEvent(input$Export, {
  #   
  #   x <- DFs$df2
  #   x <- dplyr::select(x, -Eintrag)
  #   x <- tidyr::pivot_wider(x,
  #                           names_from = Variablenname,
  #                           values_from = name)
  #   
  #   y <- DFs$df2
  #   y <- dplyr::select(y, idvar = input$ID_Var, MZP, SA)
  #   y <- y %>% group_by(idvar, MZP) %>% 
  #     summarise(SA = sum(SA, na.rm = T), .groups = "drop")
  #   y <- tidyr::pivot_wider(y,
  #                           names_from = MZP,
  #                           values_from = SA)
  #   
  #   # where <- choose.dir()
  #   # setwd(where)
  #   # write.csv2(x, "sa_output_test.csv", row.names = F)
  #   # write.csv2(y, "sa_Personscore.csv", row.names = F)
  #   
  # })
  
  output$Export1 <- downloadHandler(
    filename = function() {
      paste("FFEA_fulldata-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(DFs$df2 %>% 
                   dplyr::select(-Eintrag) %>%
                   pivot_wider(names_from = "Variablenname",
                               values_from = "name"), 
                 row.names = F,
                 file)
    }
  )
  
  output$Export2 <- downloadHandler(
    filename = function() {
      paste("FFEA_personscore-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(DFs$df2 %>% 
                   dplyr::select(idvar = input$ID_Var, MZP, SA) %>% 
                   group_by(idvar, MZP) %>% 
                   summarise(SA = sum(SA, na.rm = T), .groups = "drop") %>% 
                   pivot_wider(names_from = "MZP",
                               values_from = "SA"), 
                 row.names = F,
                 file)
    }
  )
  
  #### Tab: Deskriptive Statistik ####
  
  observeEvent(input$getData, {
    
    output$Grafik_SA <- renderPlot(
      ggplot(DFs$df2,
             aes(x = 1, y = SA)) +
        facet_wrap(~ MZP) +
        geom_violinhalf(position = position_nudge(x = 0.15)) +
        geom_point(position = position_jitter(width = .12), 
                   alpha = .7, color = "lightblue") +
        geom_boxplot(width = .15, alpha = .5, 
                     fill = "lightblue",
                     outlier.color = "red") +
        labs(x = "",
             y = "Fitness-Score") +
        theme_bw() +
        theme(axis.text = element_text(size = 12, color = "black"),
              axis.title = element_text(size = 13, color = "black"),
              strip.text = element_text(size = 14, color = "black"),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank())
    )
  })
  
  #### Tabs: Ende #### 
})

# Run the application 
# shinyApp(ui = ui, server = server)
