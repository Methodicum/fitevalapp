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
lexica <- read.table("lexica.txt", header = T, sep = "\t")
lexica_long <- tidyr::pivot_longer(lexica, cols = everything())
lexica_long <- lexica_long[nchar(lexica_long$value) > 0,]

# Maximale Dateigröße die Shiny hochlädt erhöht von 5MB(standard) zu 30MB
options(shiny.maxRequestSize=30*1024^2)
#### APP ####
#### UI ####
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("sandstone"),
    navbarPage(
        title = "Fitness-Fragebogen-Evaluation-App (FFEA)",
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
                            "Anschließend können im Dropdown-Menü die im eingelesenen Datensatz vorhandenen Variablen ausgewählt werden, welche die Texteinträge beinhalten.",
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
                            "Schritte 2 bis 5 sind unter dem Reiter \"Variablen auswählen\" zu finden.")
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
          "Variablen auswählen",
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
                 plotOutput("Grafik_SA")
          )
        )
        
        #### Tabs: Ende ####            
    )
  )
)
    
    
    

