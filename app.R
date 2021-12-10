#### Packages ####
# Paketinstalliren noch hinzufügen!
library(shiny)
library(DT)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(see)
library(shinyWidgets)
library(shinythemes)

#### Lexikon ####
lexica <- read.table("bsa_lexica.txt", header = T, sep = "\t")
lexica_long <- tidyr::pivot_longer(lexica, cols = everything())
lexica_long <- lexica_long[nchar(lexica_long$value) > 0,]

# Maximale Dateigröße die Shiny hochlädt erhöht von 5MB(standard) zu 30MB
options(shiny.maxRequestSize=30*1024^2)
#### APP ####
#### UI ####
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("sandstone"),
    navbarPage(
        title = "BSA-Fragebogen",
        #### Tab: Wörterbuch ####
        tabPanel(
            "Wörterbuch",
            fluidRow(
                column(4,
                       selectInput(inputId = "Lex_Cat",
                                   label = "Welche Kategorie wollen Sie wählen?",
                                   choices = c("Alle", unique(lexica_long$name)),
                                   selected = "Alle")
                       ),
                column(6,
                       HTML("Die Suchbegriffe müssen nicht zwingend komplett sein. Es können auch nur Wortkomponenten gesucht werden.",
                            '<br>',
                            "Die Einträge in der Spalte \"Begriff\" sind sog. regular Expressions. Das Wort wird in der abgebildeten Form gesucht.",
                            "Die Sonderzeichen haben folgende Bedeutung:",
                            '<br>',
                            "^ = Suche von Beginn an, $ = Suche vom Ende her, * = Platzhalter für variable Einträge.",
                            '<br>',
                            "Beispiel: Der Suchbegriff \"fit\" wird alle Wörter identifizieren, in dem der Begriff existiert.",
                            "Hingegen würde der Suchbegriff \"^fit\", nur solche Einträge identifizieren, die mit dem Wort \"fit\" beginnen.",
                            '<br><br>'),
                       DT::dataTableOutput("lexi")
                       )
            )
        ),
        #### Tab: Externe Daten einlesen ####
        tabPanel(
            "Daten einlesen",
            fluidRow(
                column(6,
                       HTML("<b>Anleitung:</b><br>"),
                       HTML("1. Schritt: Einlesen",
                            '<br>',
                            "Zuerst bitte den gewünschten Datensatz auswählen, der eingelesen werden soll.",
                            "Bitte nur Text-Formate (.txt, .csv) einlesen.",
                            "Die Grundeinstellungen für das Format sind in den Optionen bereits ausgewählt. Manuelle Anpassungen für Spaltentrenner, Dezimalzeichen usw. bitte ändern.",
                            "Wichtig: Das Einleseformat muss passend sein, sonst ist eine weitere Aufbereitung nicht möglich.",
                            '<br><br>'),
                       HTML("2. Schritt: Variablenauswahl",
                            '<br>',
                            "Mittels Dropdown-Menü zuerst die Id-Variable, welche Personen kennzeichnet, auswählen. Diese Variable wird sowohl für die Aufbereitung, den Export als auch das spätere, in externer Software durchzuführnde Matchen mit den Originaldaten benötigt.",
                            "Anschließend können im Dropdown-Menü die im eingelesenen Datensatz vorhandenen Variablen ausgewählt werden, welche die Texteinträge für den BSA beinhalten.",
                            "Die gewählten Variablen werden unten in einer Vorschau angezeigt.",
                            '<br><br>'),
                       HTML("3. Schritt: Messwiederholung kodieren",
                            '<br>',
                            "Liegt eine Messwiederholung vor, so geben Sie hier bitte die Zeichenkombination ein, nach der diese Messwiederholung anhand des Spaltennamens erkannt werden kann.",
                            "Beispiel: Im Spaltennamen werden Messzeitpunkte mit der Zeichenfolge \"T0\" oder \"T1\" usw. kodiert. Geben Sie diese Reihenfolge in Form von sog. regular Expression an.",
                            "Eingabe wäre dann T[:digit:]. Auf die Art werden alle Kombinationen von T gefolgt von einer Zahl ausgewählt.",
                            '<br><br>'),
                       HTML("4. Schritt: Adjustierungen",
                            '<br>',
                            "Sollen für die Berechnung bestimmte Werte geglättet werden, so kann dies hier eingestellt werden.",
                            "Wird die Option ausgewählt, wird die eingetragene Zahl als Cutoff genutzt und alle Werte oberhalb dieses Cutoffs werden auf den Cutoff reduziert. Bsp. Zeiteintrag von 200 Minuten wird bei Cutoff von 120 auf 120 Minuten reduziert.",
                            '<br><br>'),
                       HTML("5. Schritt: Aufbereitung",
                            '<br>',
                            "Durch betätigen der Schaltfläche werden alle Daten aufbereitet.",
                            "Dabei werden den Original-Einträgen im Datensatz Einträge aus dem Wörterbuch zugeordnet, die auf die Originaleinträge passen.",
                            "Wenn im Wörterbuch kein Eintrag gefunden wird, wird ein \"NICHT KATEGORISIERT\" eingetragen.",
                            "Das Ergebnis der Aufbereitung kann unter dem Reiter \"Daten aufbereiten\" eingesehen werden. Dort sind auch manuelle Korrekturen möglich.",
                            '<br><br>'),
                       HTML("Hinweis:",
                            '<br>',
                            "Schritte 2 bis 5 sind unter dem Reiter \"BSA-Variablen auswählen\" zu finden.")
                       ),
                column(4,
                       HTML("<b>Schritt 1: Daten einlesen</b><br><br>"),
                       radioButtons(inputId = "Data_Encode",
                                    label = "Textformatierung wählen",
                                    choices = c("UFT-8" = "UTF-8",
                                                "Latin" = "latin1"),
                                    selected = "UTF-8"),
                       checkboxInput(inputId = "Data_Head",
                                     label = "Gibt es Spaltenüberschriften?",
                                     value = TRUE),
                       radioButtons(inputId = "Data_Dec",
                                    label = "Was ist das Dezimaltrennzeichen?",
                                    choices = c("Punkt" = ".",
                                                "Kommata" = ","),
                                    selected = ","),
                       radioButtons(inputId = "Data_Sep",
                                    label = "Wie werden die Spalten getrennt?",
                                    choices = c("Semikolon" = ";",
                                                "Kommata" = ",",
                                                "Tabulator" = "\t"),
                                    selected = ";"),
                       fileInput("Import",
                                 label = "Einzulesende Daten")
                )
            )
        ),
        #### Tab: Daten auswählen ####
        tabPanel(
          "BSA-Variablen auswählen",
          fluidRow(
            column(12,
                   #### Variablenauswahl-Bereich ####
                   fluidRow(
                     column(9,
                            HTML("<b>2. Schritt: Variablen auswählen</b><br><br>"),
                            fluidRow(
                              column(4,
                                     multiInput(inputId = "Variablen",
                                                label = "Spalten: Texteinträge",
                                                choices = NA,
                                                selected = "")
                                     ),
                              column(4,
                                     multiInput(inputId = "Variablen2",
                                                label = "Spalten: Anzahl-Tage",
                                                choices = NA,
                                                selected = "")
                                     ),
                              column(4,
                                     multiInput(inputId = "Variablen3",
                                                label = "Spalten: Anzahl Minuten",
                                                choices = NA,
                                                selected = "")
                                     )
                              ),
                            fluidRow(
                              column(12,
                                     HTML("<b>Vorschau der Auswahl</b><br>"),
                                     DT::dataTableOutput("daten"),
                                     tableOutput("prep")
                                     )
                              )
                            ),
                     #### Einstellungs-Bereich ####
                     
                     #switchInput color while on
                     tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-danger,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-danger {
                                        background: green;
                                        color: white;
                                        }'))),
                     
                     #switchInput color while off
                     tags$head(tags$style(HTML('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-info,
                                       .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-info {
                                        background: darkgrey;
                                        color: black;
                                        }'))),
                     
                     column(3,
                            HTML("<br> Einstellungen"),
                            fluidRow(
                              column(12,
                                     selectInput(
                                       inputId = "ID_Var",
                                       label = "Spalte mit ID-Variable",
                                       choices = "",
                                       multiple = F,
                                       selectize = F
                                       ),
                                     HTML("<br><br>"),
                                     HTML("<b>3. Schritt: Kennung für Messwiederholung</b>"),
                                     textInput(
                                       inputId = "MZPID",
                                       label = "",
                                       value = "keine Messwiederholung"
                                       ),
                                     fluidRow(
                                       column(12,
                                              HTML("<br><b>4. Schritt: Adjustierungen</b><br><br>"),
                                              fluidRow(
                                         column(8,
                                                switchInput(
                                                  inputId = "adjustDuration",
                                                  label = "Adjustuierung Dauer (Minuten pro Aktivität)", 
                                                  value = FALSE, 
                                                  onLabel = "+", 
                                                  offLabel = "-", 
                                                  onStatus = "danger",
                                                  offStatus = "info", 
                                                  labelWidth = "500px"
                                                  )
                                                ),
                                        column(4,
                                               numericInput(
                                                 inputId = "cutDuration",
                                                 label = "Minuten",
                                                 value = 120,
                                                 min = 1, 
                                                 width = "80px",
                                                 )
                                               )
                                        ),
                                        fluidRow(
                                          column(8,
                                                 switchInput(
                                                   inputId = "adjustFrequency",
                                                   label = "Adjustuierung Häufigkeit (Tage mit Übungen)", 
                                                   value = FALSE, 
                                                   onLabel = "+", 
                                                   offLabel = "-",
                                                   onStatus = "danger",
                                                   offStatus = "info",
                                                   labelWidth = "500px"
                                                 )
                                          ),
                                          column(4,
                                                 numericInput(
                                                   inputId = "cutFrequency",
                                                   label = "Tage",
                                                   # label = " ",
                                                   value = 31,
                                                   min = 1,
                                                   max = 31, 
                                                   width = "80px"
                                                   )
                                                 )
                                          ))
                                       
                                       # column(9,
                                       #        # radioGroupButtons(
                                       #        #   inputId = "adjustDuration",
                                       #        #   label = "Adjustuierung der Sportdauer",
                                       #        #   choices = c("ja", "nein"),
                                       #        #   selected = "nein"
                                       #        #   ),
                                       #        switchInput(
                                       #          inputId = "adjustDuration",
                                       #          label = "Adjustuierung der Sportdauer", 
                                       #          value = FALSE, 
                                       #          onLabel = "ja", offLabel = "nein"
                                       #        ),
                                       #        switchInput(
                                       #          inputId = "adjustFrequency",
                                       #          label = "Adjustuierung der Häufigkeit", 
                                       #          value = FALSE, 
                                       #          onLabel = "ja", offLabel = "nein", 
                                       #          width = "250px"
                                       #          )
                                       #        ),
                                       # column(3,
                                       #        # radioGroupButtons(
                                       #        #   inputId = "adjustFrequency",
                                       #        #   label = "Adjustuierung der Häufigkeit",
                                       #        #   choices = c("ja", "nein"),
                                       #        #   selected = "nein"
                                       #        #   ),
                                       #        numericInput(
                                       #          inputId = "cutDuration",
                                       #          # label = "Cutoff\n(in Minuten)",
                                       #          label = " ",
                                       #          value = 120,
                                       #          min = 1, 
                                       #          width = "80px"
                                       #        ),
                                       #        numericInput(
                                       #          inputId = "cutFrequency",
                                       #          # label = "Cutoff\n(Tage im Monat)",
                                       #          label = " ",
                                       #          value = 31,
                                       #          min = 1,
                                       #          max = 31, 
                                       #          width = "80px"
                                       #          )
                                       #        )
                                       ),
                                     HTML("<b>5. Schritt: Aufbereitung</b><br>"),
                                     actionButton(
                                       inputId = "Aufbereiten",
                                       label = "Aufbereiten",
                                       width = "300px"
                                       ),
                                     htmlOutput("aufbereitet_info")
                                     ),
                              )
                            ),
                     )
                   )
            )
          ),
          # fluidRow(
          #   column(3,
          #          multiInput(inputId = "Variablen",
          #                     label = "Spalten: Texteinträge",
          #                     choices = NA,
          #                     selected = "")
          #   ),
          #   column(3,
          #          multiInput(inputId = "Variablen2",
          #                     label = "Spalten: Anzahl-Tage",
          #                     choices = NA,
          #                     selected = "")
          #   ),
          #   column(3,
          #          multiInput(inputId = "Variablen3",
          #                     label = "Spalten: Anzahl Minuten",
          #                     choices = NA,
          #                     selected = "")
        #     ),
        #     column(3,
        #            selectInput(inputId = "ID_Var",
        #                        label = "Spalte mit ID-Variable",
        #                        choices = "", 
        #                        multiple = F, 
        #                        selectize = F),
        #            HTML("<br><br>"),
        #            textInput(inputId = "MZPID", 
        #                      label = "3. Schritt: Kennung für Messwiederholung",
        #                      value = "keine Messwiederholung"),
        #            HTML("<b>4. Schritt: Aufbereitung</b><br><br>"),
        #            actionButton(inputId = "Aufbereiten",
        #                         label = "Aufbereiten"),
        #            htmlOutput("aufbereitet_info"),
        #            
        #            fluidRow(
        #              column(6,
        #                     radioGroupButtons(inputId = "adjustDuration",
        #                                       label = "Adjustuierung der Sportdauer",
        #                                       choices = c("ja", "nein"), 
        #                                       selected = "nein"),
        #                     numericInput(inputId = "cutDuration",
        #                                  label = "Cutoff\n(in Minuten)", 
        #                                  value = 120,
        #                                  min = 1, width = "100px")
        #              ),
        #              column(6,
        #                     radioGroupButtons(inputId = "adjustFrequency",
        #                                       label = "Adjustuierung der Häufigkeit",
        #                                       choices = c("ja", "nein")),
        #                     numericInput(inputId = "cutFrequency",
        #                                  label = "Cutoff\n(Tage im Monat)", 
        #                                  value = 31,
        #                                  min = 1, max = 31, width = "100px")
        #                     )
        #            )
        #            ),
        #     
        #   ),
        #   fluidRow(
        #     column(12,
        #            HTML("<b>Vorschau der Auswahl</b><br>"),
        #            tableOutput("daten"),
        #            tableOutput("prep")
        #     )
        #   )
        # ),
        #### Tab: Daten aufbereiten ####
        tabPanel(
          "Daten aufbereiten",
          fluidRow(
            column(12,
                   HTML(
                     "In diesem Abschnitt werden die aufbereiteten Daten gezeigt. In der Tabelle können die Einträge sowie Kategorien manuell Bearbeitet werden.",
                     "<br>",
                     "Alle nicht kategorisierbaren Einträge können im Suchfeld der Kategorien unter \"NICHT KATEGORISIERT\" gefiltert werden."
                     ),
                   fluidRow(
                     column(9,
                            fluidRow(
                              column(12,
                                     DT::dataTableOutput("editDF"),
                                     htmlOutput("keinedaten"))
                            )
                            ),
                     column(3,
                            fluidRow(
                              column(12,
                                     actionButton(inputId = "getData",
                                                  label = "aufbereitete Daten anzeigen"),
                                     HTML(
                                       "<br><br><br>",
                                       "Durch ein Klicken auf \"Export\" werden die aufbereiteten Daten als *.csv-Daten exportiert.",
                                       "<br><br><br>"
                                       ),
                                     actionButton(inputId = "Export",
                                                  label = "Daten exportieren")
                                     ))
                              )
                            )
                   )
            #     column(4,
            #            HTML(
            #                "In diesem Abschnitt werden die aufbereiteten Daten gezeigt. In der Tabelle können die Einträge sowie Kategorien manuell Bearbeitet werden.",
            #                "<br><br>",
            #                "Alle nicht kategorisierbaren Einträge können im Suchfeld der Kategorien unter \"NICHT KATEGORISIERT\" gefiltert werden."
            #                )
            #            ),
            #     column(4,
            #            actionButton(inputId = "getData",
            #                         label = "aufbereitete Daten anzeigen")
            #            ),
            #     column(4,
            #            HTML(
            #                "Durch ein Klicken auf \"Export\" werden die aufbereiteten Daten als *.csv-Daten exportiert.",
            #                "<br><br><br>"
            #            ),
            #            actionButton(inputId = "Export",
            #                         label = "Daten exportieren")
            #            )
            # ),
            # fluidRow(
            #     column(10,
            #            DT::dataTableOutput("editDF"),
            #            htmlOutput("keinedaten")
            #            )
            )
        ),
        #### Tab: Deskriptive Statistiken ####
        tabPanel(
          "Deskriptive Statistiken",
          column(5,
                 plotOutput("Grafik_BSA")
          )
        )
        
        #### Tabs: Ende ####            
    )
)

    
    
    

#### SERVER ####
# Define server logic required to draw a histogram
server <- function(input, output, session) {
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
    
    # output$lexi <- DT::renderDataTable({
    #     if(input$Lex_Cat == "Alle")
    #         DT::datatable(lexica_long, 
    #                       rownames = F,
    #                       colnames = c("Sportkategorie", "Eintrag"),
    #                       filter = "top",
    #                       options = list(
    #                           pageLength = 10
    #                       ))
    #     else
    #         DT::datatable(lexica_long[lexica_long$name == input$Lex_Cat,],
    #                       rownames = F,
    #                       colnames = c("Sportkategorie", "Eintrag"),
    #                       filter = "top",
    #                       options = list(
    #                           pageLength = 10
    #                       ))
    # })
    
    
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
        # die Text-BSA-Einträge stehen.
        updateSelectizeInput(session, inputId = "Variablen",
                             choices = names(df()))
        updateSelectizeInput(session, inputId = "Variablen2",
                             choices = names(df()))
        updateSelectizeInput(session, inputId = "Variablen3",
                             choices = names(df()))
        
    })
    
    # Output für die Tabellenvorschau: sollte keine Auswahl getroffen werden, so
    # erscheint nur ein Hinweistext, der Auffordert eine Auswahl zu treffen.    
    # Sollten bei der ID/BSA-Spalten eine Auswahl getroffen worden
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
        
        # ASCII-Kodierungen manuell ersetzen, sollte das Encoding nicht klappen...
        x$Eintrag <- gsub("Ã¼", "ü", x$Eintrag) # kleines ü (ue)
        x$Eintrag <- gsub("Ãœ", "Ü", x$Eintrag) # großes Ü (Ue)
        x$Eintrag <- gsub("Ã¤", "ä", x$Eintrag) # kleines ä (ae)
        x$Eintrag <- gsub("Ã¶", "ö", x$Eintrag) # kleines ö (oe)
        x$Eintrag <- gsub("ÃŸ", "ß", x$Eintrag) # kleines sz/ß
        
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

        # BSA Berechnung
        # Wenn die Kategorie "kein Sport ist" -> 0
        # Der Zeitantrag kann manuell angepasst werden. 
        # Wenn der Zeiteintrag unplausibel ist -> reduzieren auf 120 Minuten
        # Da die Eingaben über 4 Wochen sein sollen, wird durch 4 geteilt. Das
        # Scoring gibt dann den Wert pro Eintrag im Monat an.
        # 
        # x$BSA <- case_when(x$name == "kein" ~ 0,
        #                    x$duration_per_unit > 120 ~ 120*x$times_of_sport/4,
        #                    TRUE ~ x$duration_per_unit*x$times_of_sport/4)
        
        if(input$adjustDuration == "ja" & input$adjustFrequency == "ja") {
          x$BSA <- case_when(x$name == "kein" ~ 0,
                             x$duration_per_unit > input$cutDuration & x$times_of_sport > input$cutFrequency ~ input$cutDuration*input$cutFrequency/4,
                             x$duration_per_unit > input$cutDuration & x$times_of_sport < input$cutFrequency ~ input$cutDuration*x$times_of_sport/4,
                             x$duration_per_unit < input$cutDuration & x$times_of_sport > input$cutFrequency ~ x$duration_per_unit*input$cutFrequency/4,
                             TRUE ~ x$duration_per_unit*x$times_of_sport/4)
          
        } else if(input$adjustDuration == "ja" & input$adjustFrequency == "nein") {
          x$BSA <- case_when(x$name == "kein" ~ 0,
                             x$duration_per_unit > input$cutDuration ~ input$cutDuration*x$times_of_sport/4,
                             TRUE ~ x$duration_per_unit*x$times_of_sport/4)

        } else if(input$adjustDuration == "nein" & input$adjustFrequency == "ja") {
          x$BSA <- case_when(x$name == "kein" ~ 0,
                             x$times_of_sport > input$cutFrequency ~ x$duration_per_unit*input$cutFrequency/4,
                             TRUE ~ x$duration_per_unit*x$times_of_sport/4)

        } else {
          x$BSA <- case_when(x$name == "kein" ~ 0,
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
                                  colnames = c("Id",
                                               "Item",
                                               "Eintrag",
                                               "Anzahl in 4 Wochen",
                                               "Dauer pro Einheit",
                                               "Kategorie",
                                               "BSA-Scoring pro Eintrag",
                                               "Messzeitpunkt"),
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
    observeEvent(input$Export, {

        x <- DFs$df2
        x <- dplyr::select(x, -Eintrag)
        x <- tidyr::pivot_wider(x,
                                names_from = Variablenname,
                                values_from = name)
        
        y <- DFs$df2
        y <- dplyr::select(y, idvar = input$ID_Var, MZP, BSA)
        y <- y %>% group_by(idvar, MZP) %>% 
          summarise(BSA = sum(BSA, na.rm = T), .groups = "drop")
        y <- tidyr::pivot_wider(y,
                                names_from = MZP,
                                values_from = BSA)

        write.csv2(x, "BSA_output_test.csv", row.names = F)
        write.csv2(y, "BSA_Personscore.csv", row.names = F)
    })
            
    #### Tab: Deskriptive Statistik ####
    
    observeEvent(input$getData, {
      
      output$Grafik_BSA <- renderPlot(
        ggplot(DFs$df2,
               aes(x = 1, y = BSA)) +
          facet_wrap(~ MZP) +
          geom_violinhalf(position = position_nudge(x = 0.15)) +
          geom_point(position = position_jitter(width = .12), 
                     alpha = .7, color = "lightblue") +
          geom_boxplot(width = .15, alpha = .5, 
                       fill = "lightblue",
                       outlier.color = "red") +
          labs(x = "",
               y = "BSA-Score") +
          theme_bw() +
          theme(axis.text = element_text(size = 12, color = "black"),
                axis.title = element_text(size = 13, color = "black"),
                strip.text = element_text(size = 14, color = "black"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank())
      )
    })
    
    #### Tabs: Ende #### 
}

# Run the application 
shinyApp(ui = ui, server = server)
