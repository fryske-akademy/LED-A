library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjqui)
library(shinysky)
library(readr)
library(openxlsx)
library(stringr)
library(fs)
library(plyr)
library(ipa)
library(tuneR)
library(proxy)
library(dtw)
library(udpipe)
library(ggplot2)
library(ggh4x)
library(ggrepel)
library(deldir)
library(ggdendro)
library(dynamicTreeCut)
library(apcluster)
library(dbscan)
library(fpc)
library(MASS)
library(pcaPP)
library(geodist)
library(colouR)
library(leaflet)
library(leaflet.extras)
library(sf)
library(Rtsne)
library(grid)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(svglite)
library(Cairo)
library(tikzDevice)
library(htmlwidgets)
library(webshot2)
library(callr)

# sudo apt install libudunits2-dev libgdal-dev libfontconfig1-dev libcairo2-dev libxt-dev libharfbuzz-dev libfribidi-dev chromium-browser texlive-full

# setwd("~/Data/FA/LED-A/pkgs/")
# install.packages("RJSONIO")
# install.packages("shinysky_0.1.3.tar.gz", repos = NULL, type = "source")

# install.packages("remotes")
# remotes::install_github("ropensci/rnaturalearthhires")

################################################################################

users = reactiveValues(count = 0)

################################################################################

addResourcePath("docs"      , "docs"      )
addResourcePath("datasets1" , "datasets1" )
addResourcePath("datasets2" , "datasets2" )
addResourcePath("datasets3" , "datasets3" )
addResourcePath("standalone", "standalone")

################################################################################

# fc-list
Fonts <- c("DejaVu Sans", "DejaVu Serif", "FreeSans", "FreeSerif", "Latin Modern Sans", "Latin Modern Roman", "Liberation Sans", "Liberation Serif", "TeX Gyre Adventor", "TeX Gyre Bonum", "TeX Gyre Chorus", "TeX Gyre Heros", "TeX Gyre Schola", "TeX Gyre Pagella", "TeX Gyre Termes")

################################################################################

# https://leaflet-extras.github.io/leaflet-providers/preview/
Providers <- c("CartoDB Positron", "CartoDB Positron No Labels", "CartoDB Voyager", "CartoDB Voyager No Labels", "Esri World Terrain", "Esri World Gray Canvas")

################################################################################

ui <- tagList(
  includeCSS("www/styles.css"), tags$head(includeHTML("GA.html")),

  titlePanel(title = HTML("<img style='height: 70px; margin-top: -10px; display: block; margin-left: auto; margin-right: auto;' src='led2.png'>"), windowTitle = "LED-A"),

  tags$head(
    tags$link(rel="icon", href="led2.png"),

    tags$meta(charset="UTF-8"),
    tags$meta(name   ="description", content="LED-A is a web app for calculating linguistic distances with Levenshtein distance using phonetic IPA transcriptions."),
    
    tags$script('$(document).on("shiny:connected", function(e) 
                 {
                   Shiny.onInputChange("winWidth" , window.innerWidth);
                   Shiny.onInputChange("winHeight", window.innerHeight);
                 });
                 
                 $(window).resize(function(e) 
                 {
                   Shiny.onInputChange("winWidth" , window.innerWidth);
                   Shiny.onInputChange("winHeight", window.innerHeight);
                 });
               ')    
  ),

  navbarPage
  (
    title = NULL, id = "navBar", collapsible = FALSE,

    navbarMenu("Run",
    
    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-pencil' style='font-size: 100%'></span>&nbsp;Transcriptions"),
      value = "transcriptions",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
 
        br(),
 
        fluidPage(
          style = "border: 1px solid silver; min-height: 601px; max-width: 680px; background-color: #ffffff",
          
          br(), br(),

          ### Upload table with transcriptions

          fluidPage(
            style = "margin-left: 120px",

            bsButton("butDataTab", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modDataTab", "Upload table with transcriptions", "butDataTab", size = "large",
                     p("Transcriptions should be in IPA/Unicode, see the IPA keyboard ",
                     a("here", href = "https://westonruter.github.io/ipa-chart/keyboard/", target = "_blank"), ".")),

            HTML("<span style='font-weight: bold;'>&nbsp;Upload table with transcriptions:</span>"),
            p(style='height: 8px')
          ),

          fluidPage(
            align = "center",
            fileInput(
              inputId    = "dataTab",
              label      = NULL,
              multiple   = FALSE,
              accept     = ".xlsx",
              width      = "250px")
          ),

          ### Format

          div(style='height: 12px'),

          fluidPage(
            style = "margin-left: 120px",

            bsButton("butDataForm", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modDataForm", "Format", "butDataForm", size = "large", "Dit is de helptekst"),

            HTML("<span style='font-weight: bold;'>&nbsp;Format:</span>"),
            p(style='height: 8px')
          ),

          fluidPage(
            style = "margin-left: 184px",

            radioButtons(
              inputId    = 'dataForm',
              label      = NULL,
              choices    = c("rows are varieties, columns are items",
                             "rows are items, columns are varieties"),
              selected   =   "",
              inline     = FALSE
            )
          ),

          ### Upload table with coordinates

          br(),

          fluidPage(
            style = "margin-left: 120px",

            bsButton("butGeoTab", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modGeoTab", "Upload table with coordinates", "butGeoTab", size = "large", "Dit is de helptekst"),

            HTML("<span style='font-weight: bold;'>&nbsp;Upload table with coordinates (optional):</span>"),
            p(style='height: 8px')
          ),

          uiOutput('geoTab'),

          ### Segment distances

          div(style='height: 12px'),

          fluidPage(
            style = "margin-left: 120px",

            bsButton("butSegmDist", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modSegmDist", "Method", "butSegmDist", size = "large", 
                     span("For info about the implementation of the methods that are used here see "), 
                     a("A&B", href = "docs/A&B.pdf", target = "_blank"),
                     span(" and "),
                     a("PMI", href = "docs/PMI.pdf", target = "_blank"),
                     span(".")),

            HTML("<span style='font-weight: bold;'>&nbsp;Method:</span>"),
            p(style='height: 8px')
          ),

          fluidPage(
            style = "margin-left: 184px",

            radioButtons(
              inputId    = 'segmDist',
              label      = NULL,
              choices    = c("binary item comparison",
                             "plain  sub. = 1 indel = 0.5",
                             "plain  cost = 1",
                             "A&B  sub. ≤ 1 indel ≤ 0.5",
                             "A&B  cost ≤ 1",
                             "PMI   cost ≤ 1",
                             "upload my own distances"),
              selected   =   "plain  sub. = 1 indel = 0.5",
              inline     = FALSE
            )
          ),

          br(),

          ### Upload segment distances

          uiOutput('segmTab'),

          ### Extra allowed segment alignments

          uiOutput('extraAli'),

          ### Process

          uiOutput('selProc'),

          ### Normalization of word pair distances

          uiOutput('selNorm'),

          ### Measurements

          uiOutput('selMeasurements'),

          ### Output

          fluidPage(
            style = "margin-left: 120px",

            bsButton("butSelOutput", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modSelOutput", "Output", "butSelOutput", size = "large", "Dit is de helptekst"),

            HTML("<span style='font-weight: bold;'>&nbsp;Output:</span>"),
            p(style='height: 8px')
          ),

          uiOutput("selOutput"),

          ### Go!
          
          br(), br(),

          fluidPage(
            align = "center",

            shiny::actionButton("goButton", "Go!"),
            busyIndicator(text = NULL, wait = 1000),
            uiOutput("showResults")
          ),
        
          br()
        ),
        
        br()
      )
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-music' style='font-size: 100%'></span>&nbsp;Acoustic data"),
      value = "acousticdata",
      
      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        
        br(),
        
        fluidPage(
          style = "border: 1px solid silver; min-height: 601px; max-width: 680px; background-color: #ffffff",
          
          br(),
          
          ### Upload sound files
          
          br(),
          
          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butDataTab1", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modDataTab1", "Upload sound files", "butDataTab1", size = "large", "Dit is de helptekst"),
            
            HTML("<span style='font-weight: bold;'>&nbsp;Upload sound files:</span>"),
            p(style='height: 8px')
          ),
          
          fluidPage(
            align = "center",
            fileInput(
              inputId    = "dataTab1",
              label      = NULL,
              multiple   = TRUE,
              accept     = "*",
              width      = "250px")
          ),

          ### Format
          
          div(style='height: 12px'),
          
          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butDataForm1", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modDataForm1", "Format of file names", "butDataForm1", size = "large", "Dit is de helptekst"),
            
            HTML("<span style='font-weight: bold;'>&nbsp;Format of file names:</span>"),
            p(style='height: 8px')
          ),
          
          fluidPage(
            style = "margin-left: 184px",
            
            radioButtons(
              inputId      = 'dataForm1',
              label        = NULL,
              choiceNames  = list(HTML("<span style='font-family:monospace'>variety_item</span>.wav"),
                                  HTML("<span style='font-family:monospace'>variety.item</span>")),
              choiceValues = c("1", "2"),
              selected     =   "",
              inline       = FALSE
            )
          ),

          ### Preprocess samples
          
          br(),
          
          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butTrim", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modTrim", "Preprocess samples", "butTrim", size = "large", "Dit is de helptekst"),
            
            HTML("<span style='font-weight: bold;'>&nbsp;Preprocess samples:</span>"),
            p(style='height: 8px')
          ),
          
          fluidPage(
            style = "margin-left: 184px",
            
            checkboxInput(
              inputId    = 'trim',
              label      = "Trim leading and trailing silence",
              value      = FALSE
            )
          ),

          ### Upload table with genders
          
          div(style='height: 26px'),

          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butGebTab1", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modGenTab1", "Upload table with coordinates", "butGenTab1", size = "large", "Dit is de helptekst"),
            
            HTML("<span style='font-weight: bold;'>&nbsp;Upload table with genders (recommended):</span>"),
            p(style='height: 8px')
          ),
          
          uiOutput('genTab1'),

          ### Upload table with coordinates
          
          br(),
          
          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butGeoTab1", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modGeoTab1", "Upload table with coordinates", "butGeoTab1", size = "large", "Dit is de helptekst"),
            
            HTML("<span style='font-weight: bold;'>&nbsp;Upload table with coordinates (optional):</span>"),
            p(style='height: 8px')
          ),
          
          uiOutput('geoTab1'),

          # Comparison method

          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butSelDTW", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modSelDTW", "Method", "butSelDTW", size = "large",
                     span("For a description of how acoustic distances are measured see"), 
                     a("DTW", href = "docs/DTW.pdf", target = "_blank"),
                     span(".")),
            
            HTML("<span style='font-weight: bold;'>&nbsp;Method:</span>"),
            p(style='height: 8px')
          ),
          
          fluidPage(
            style = "margin-left: 184px",
            
            radioButtons(
              inputId    = 'selDTW',
              label      = NULL,
              choices    = c("DTW using Cosine θ", "DTW using Pearson's r"),
              selected   = "DTW using Cosine θ",
              inline     = FALSE
            )
          ),
          
          br(),

          ### Output
          
          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butSelOutput1", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modSelOutput1", "Output", "butSelOutput1", size = "large", "Dit is de helptekst"),
                     
            HTML("<span style='font-weight: bold;'>&nbsp;Output:</span>"),
            p(style='height: 8px')
          ),
          
          uiOutput("selOutput1"),
          
          ### Go!
          
          br(), br(),
          
          fluidPage(
            align = "center",
            
            shiny::actionButton("goButton1", "Go!"),
            busyIndicator(text = NULL, wait = 1000),
            uiOutput("showResults1")
          ),
          
          br()
        ),
        
        br()
      )
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-tag' style='font-size: 100%'></span>&nbsp;POS-tag data"),
      value = "postaggeddata",
      
      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        
        br(),
        
        fluidPage(
          style = "border: 1px solid silver; min-height: 601px; max-width: 680px; background-color: #ffffff",
          
          br(),
          
          ### Upload texts with POS tags
          
          br(),
          
          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butDataTab2", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modDataTab2", "Upload texts with POS tags", "butDataTab2", size = "large", "Dit is de helptekst"),
            
            HTML("<span style='font-weight: bold;'>&nbsp;Upload texts with POS tags:</span>"),
            p(style='height: 8px')
          ),
          
          fluidPage(
            align = "center",
            fileInput(
              inputId    = "dataTab2",
              label      = NULL,
              multiple   = TRUE,
              accept     = c(".xlsx", ".conllu"),
              width      = "250px")
          ),
          
          ### Format
          
          div(style='height: 12px'),
          
          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butDataForm2", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modDataForm2", "Format of file names", "butDataForm2", size = "large", "Dit is de helptekst"),
            
            HTML("<span style='font-weight: bold;'>&nbsp;Format of file names:</span>"),
            p(style='height: 8px')
          ),
          
          fluidPage(
            style = "margin-left: 184px",
            
            radioButtons(
              inputId      = 'dataForm2',
              label        = NULL,
              choiceNames  = list(HTML("<span style='font-family:monospace'>variety</span>.xlsx"),
                                  HTML("<span style='font-family:monospace'>variety</span>.conllu")),
              choiceValues = c("1", "2"),
              selected     =   "",
              inline       = FALSE
            )
          ),
            
          ### Upload table with coordinates
            
          br(),
            
          fluidPage(
            style = "margin-left: 120px",
              
            bsButton("butGeoTab2", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modGeoTab2", "Upload table with coordinates", "butGeoTab2", size = "large", "Dit is de helptekst"),
              
            HTML("<span style='font-weight: bold;'>&nbsp;Upload table with coordinates (optional):</span>"),
            p(style='height: 8px')
          ),
            
          uiOutput('geoTab2'),

          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butSelGrams", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modSelGrams", "Method", "butSelGrams", size = "large",
                     span("For a description of how POS-tag n-gram distances are measured see"), 
                     a("POS-tag n-gram distances", href = "docs/Ngram.pdf", target = "_blank"),
                     span(".")),
          
            HTML("<span style='font-weight: bold;'>&nbsp;Method:</span>"),
            p(style='height: 8px')
          ),
          
          fluidPage(
            style = "margin-left: 184px",
            
            radioButtons(
              inputId    = 'selGrams',
              label      = NULL,
              choices    = c("2-gram", "3-gram", "4-gram", "5-gram"),
              selected   = "3-gram",
              inline     = FALSE
            )
          ),
          
          br(),
          
          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butSelScale", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modSelScale", "Scaling of POS-tag frequencies", "butSelScale", size = "large", "Dit is de helptekst"),
            
            HTML("<span style='font-weight: bold;'>&nbsp;Scaling of POS-tag frequencies:</span>"),
            p(style='height: 8px')
          ),
          
          fluidPage(
            style = "margin-left: 184px",
            
            radioButtons(
              inputId    = 'selScale',
              label      = NULL,
              choices    = c("linear", "logarithmic"),
              selected   = "logarithmic",
              inline     = FALSE
            )
          ),
          
          br(),
          
          fluidPage(
            style = "margin-left: 120px",
            
            bsButton("butSelComp", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modSelComp", "Comparison of frequency vectors", "butSelComp", size = "large", "Dit is de helptekst"),
            
            HTML("<span style='font-weight: bold;'>&nbsp;Comparison of frequency vectors:</span>"),
            p(style='height: 8px')
          ),
          
          fluidPage(
            style = "margin-left: 184px",
            
            radioButtons(
              inputId    = 'selComp',
              label      = NULL,
              choices    = c("Cosine θ", "Pearson's r", "Spearman's ρ", "Kendall's τ"),
              selected   = "Cosine θ",
              inline     = FALSE
            )
          ),
          
          br(),
          
          ### Output
            
          fluidPage(
            style = "margin-left: 120px",
              
            bsButton("butSelOutput2", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modSelOutput2", "Output", "butSelOutput2", size = "large", "Dit is de helptekst"),
              
            HTML("<span style='font-weight: bold;'>&nbsp;Output:</span>"),
            p(style='height: 8px')
          ),
            
          uiOutput("selOutput2"),
            
          ### Go!
            
          br(), br(),
            
          fluidPage(
            align = "center",
              
            shiny::actionButton("goButton2", "Go!"),
            busyIndicator(text = NULL, wait = 1000),
            uiOutput("showResults2")
          ),
            
          br()
        ),
          
        br()
      )
    ),
      
    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-unchecked' style='font-size: 100%'></span>&nbsp;Distance table"),
      value = "distancetable",
      
      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        
        br(),
        
        fluidPage(
          style = "border: 1px solid silver; min-height: 385px; max-width: 680px; background-color: #ffffff",
          
          br(), br(),
        
          ### Upload table with distances
          
          fluidPage(
            style = "margin-left: 120px",
          
            bsButton("butDataTab8", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modDataTab8", "Upload table with distances", "butDataTab8", size = "large", "Dit is de helptekst"),
          
            HTML("<span style='font-weight: bold;'>&nbsp;Upload table with distances:</span>"),
            p(style='height: 8px')
          ),
        
          fluidPage(
            align = "center",
            fileInput(
              inputId    = "dataTab8",
              label      = NULL,
              multiple   = FALSE,
              accept     = ".xlsx",
              width      = "250px")
          ),
        
          ### Upload table with coordinates
        
          br(),
        
          fluidPage(
            style = "margin-left: 120px",
          
            bsButton("butGeoTab8", label = NULL, icon = icon("info"), size = "extra-small"),
            bsModal ("modGeoTab8", "Upload table with coordinates", "butGeoTab8", size = "large", "Dit is de helptekst"),
          
            HTML("<span style='font-weight: bold;'>&nbsp;Upload table with coordinates (optional):</span>"),
            p(style='height: 8px')
          ),
        
          uiOutput('geoTab8'),
        
          ### Go!
        
          br(), br(),
        
          fluidPage(
            align = "center",
          
            shiny::actionButton("goButton8", "Go!"),
            busyIndicator(text = NULL, wait = 1000)
          ),
        
          br()
        ),
        
        fluidPage(
          style = "min-height: 216px;"
        ),
        
        br()
      )
    )),
    
    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-eye-open' style='font-size: 100%'></span>&nbsp;Display"),
      value = "display",
      
      splitLayout
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        cellWidths = c("32%", "68%"),
        cellArgs = list(style = "padding: 6px"),
        
        column
        (
          width=12,
          
          br(),

          uiOutput("selItem7"),

          bsButton("butMarginX7", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modMarginX7", "Width of margin", "butMarginX7", size = "large", "Dit is de helptekst"),
          
          HTML("<span style='font-weight: bold;'>&nbsp;Width of margin ⇄:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', numericInput(
            inputId = "marginX7",
            label   = NULL,
            value   = 0,
            min     = 0,
            max     = 10,
            step    = 0.1,
            width   = "100px")),
          
          br(),
          
          bsButton("butMarginY7", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modMarginY7", "Width of margin", "butMarginY7", size = "large", "Dit is de helptekst"),
          
          HTML("<span style='font-weight: bold;'>&nbsp;Width of margin ⇅:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', numericInput(
            inputId = "marginY7",
            label   = NULL,
            value   = 0,
            min     = 0,
            max     = 10,
            step    = 0.1,
            width   = "100px")),
          
          br(),
          
          bsButton("butDotRadius7", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modDotRadius7", "Radius of dot", "butDotRadius7", size = "large", "Dit is de helptekst"),
          
          HTML("<span style='font-weight: bold;'>&nbsp;Radius of dot:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', numericInput(
            inputId = "dotRadius7",
            label   = NULL,
            value   = 2,
            min     = 1,
            max     = 20,
            step    = 1,
            width   = "100px")),
        ),
        
        column
        (
          width=12,
          
          br(),
          
          fluidPage(
            align="center",
            style="min-height: 550px; padding: 0; margin: 0;",
            uiOutput("Graph7"),
            busyIndicator(text = NULL, wait = 1000),
          ),
          
          div(style="height: 10px"),
          
          splitLayout
          (
            align="center",
            cellWidths = c("180px", "100px", "100px", "100px", "auto"),
            
            uiOutput('selFont7'),
            uiOutput('selPoint7'),
            uiOutput('selFormat7'),
            uiOutput('selDPI7'),
            downloadButton('downLoad7', 'Graph')
          )
        )
      )
    ),
    
    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-signal' style='font-size: 100%'></span>&nbsp;Graphs"),
      value = "graphs",

      splitLayout
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        cellWidths = c("32%", "68%"),
        cellArgs = list(style = "padding: 6px"),

        column
        (
          width=12,

          br(),

          bsButton("butSelClass3", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modSelClass3", "Explorative method", "butSelClass3", size = "large", "Dit is de helptekst"),

          HTML("<span style='font-weight: bold;'>&nbsp;Explorative method:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', radioButtons('selClass3', NULL, c("Cluster analysis", "Multidimensional scaling"), selected = "Cluster analysis", inline = FALSE)),

          uiOutput('selMethod3'),
          uiOutput("selClus3"),
          uiOutput("selMult3"),
          uiOutput("maxOverlaps"),
          uiOutput("selColors3"),
          uiOutput("showLegend"),
          uiOutput("setLabels"),
        ),

        column
        (
          width=12,

          br(),

          fluidPage(
            align="center",
            style="min-height: 550px; padding: 0; margin: 0;",
            uiOutput("Graph3"),
            busyIndicator(text = NULL, wait = 1000),
          ),

          div(style="height: 10px"),

          splitLayout
          (
            align="center",
            cellWidths = c("180px", "100px", "100px", "100px", "auto"),

            uiOutput('selFont3'),
            uiOutput('selPoint3'),
            uiOutput('selFormat3'),
            uiOutput('selDPI3'),
            downloadButton('downLoad3', 'Graph')
          )
        )
      )
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-map-marker' style='font-size: 100%'></span>&nbsp;Maps"),
      value = "maps",

      splitLayout
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        cellWidths = c("32%", "68%"),
        cellArgs = list(style = "padding: 6px"),

        column
        (
          width=12,

          br(),

          bsButton("butSelClass5", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modSelClass5", "Map visualization", "butSelClass5", size = "large", "Dit is de helptekst"),

          HTML("<span style='font-weight: bold;'>&nbsp;Map visualization:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', radioButtons('selClass5',
                                                      NULL,
                                                      c("Beam map", "Network map", "Area map", "RGB map", "Reference point map"),
                                                      selected = "Area map",
                                                      inline = FALSE)),

          br(),

          uiOutput("beamWeight"   ),
          uiOutput("networkWeight"),

          uiOutput("selRGB") ,
          uiOutput("selRef") ,
          
          uiOutput("dotRadiusBeam"   ),
          uiOutput("dotRadiusNetwork"),
          uiOutput("dotRadiusArea5"  ),
          uiOutput("dotRadiusRGB"    ),
          uiOutput("dotRadiusRef"    ),

          uiOutput("selBorder5"),
          
          uiOutput("selCophenetic"),
          uiOutput("posLegend")
        ),

        column
        (
          width=12,

          br(),

          fluidPage(
            align="center",
            style="min-height: 550px; padding: 0; margin: 0;",
            uiOutput("Graph5"),
            busyIndicator(text = NULL, wait = 1000)
          ),

          div(style="height: 10px"),

          splitLayout
          (
            align="center",
            cellWidths = c("180px", "100px", "100px", "100px", "auto"),

            selectInput ('selMap5' , NULL, Providers, selected = "CartoDB Positron", selectize=FALSE, multiple = FALSE),
            numericInput('selSize5', NULL, value = 550, min = 550, step = 50),
            uiOutput('selFormat5'),
            uiOutput('selDPI5'),
            downloadButton('downLoad5', 'Graph')
          )
        )
      )
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-random' style='font-size: 100%'></span>&nbsp;Partitions"),
      value = "partitions",

      splitLayout
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        cellWidths = c("32%", "68%"),
        cellArgs = list(style = "padding: 6px"),

        column
        (
          width=12,

          br(),

          bsButton("butReplyMethod6", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modReplyMethod6", "Cluster method", "butReplyMethod6", size = "large", "Dit is de helptekst"),

          HTML("<span style='font-weight: bold;'>&nbsp;Cluster method:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', radioButtons('replyMethod6',
                                                      NULL,
                                                      c("Dynamic tree cut", "Bootstrap", "Noise", "Affinity propagation", "HDBSCAN"),
                                                      selected = "Dynamic tree cut",
                                                      inline = FALSE)),

          br(),

          uiOutput("degStab6"      ),
          uiOutput("minPts6"       ),
          uiOutput("dotRadiusArea6"),
          uiOutput("selBorder6"    ),
          
          br(),

          div(style='margin-left: 85px', shiny::actionButton("goButton6", "Go!")),
          busyIndicator(text = NULL, wait = 1000)
        ),

        column
        (
          width=12,

          br(),

          fluidPage(
            align="center",
            style="min-height: 550px; padding: 0; margin: 0;",
            uiOutput("Graph6"),
            busyIndicator(text = NULL, wait = 1000)
          ),

          div(style="height: 10px"),

          splitLayout
          (
            align="center",
            cellWidths = c("180px", "100px", "100px", "100px", "auto"),

            selectInput ('selMap6' , NULL, Providers, selected = "CartoDB Positron", selectize=FALSE, multiple = FALSE),
            numericInput('selSize6', NULL, value = 550, min = 550, step = 50),
            uiOutput('selFormat6'),
            uiOutput('selDPI6'),
            downloadButton('downLoad6', 'Graph')
          )
        )
      )
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-th' style='font-size: 100%'></span>&nbsp;Segments"),
      value = "segments",

      splitLayout
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        cellWidths = c("32%", "68%"),
        cellArgs = list(style = "padding: 6px"),

        column
        (
          width=12,

          br(),

          bsButton("butSelSegment4", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modSelSegment4", "Segment class", "butSelSegment4", size = "large", "Dit is de helptekst"),

          HTML("<span style='font-weight: bold;'>&nbsp;Segment class:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', radioButtons('selSegment4', NULL, c("Vowels", "Consonants"), selected = "Vowels", inline = FALSE)),

          uiOutput('selMethod4'),
          uiOutput("selMult4")
        ),

        column
        (
          width=12,

          br(),

          fluidPage(
            align="center",
            style="min-height: 550px; padding: 0; margin: 0;",
            uiOutput("Graph4"),
            busyIndicator(text = NULL, wait = 1000)
          ),

          div(style="height: 10px"),

          splitLayout
          (
            align="center",
            cellWidths = c("180px", "100px", "100px", "100px", "auto"),

            uiOutput('selFont4'),
            uiOutput('selPoint4'),
            uiOutput('selFormat4'),
            uiOutput('selDPI4'),
            downloadButton('downLoad4', 'Graph')
          )
        )
      )
    ),

    navbarMenu("Note",

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-info-sign' style='font-size: 100%'></span>&nbsp;About"),
      value = "about",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        
        br(),
        
        fluidPage(
          style = "min-height: 601px;",

          h5(strong("About")),
          p(HTML("LED-A stands for 'Levenshtein Edit Distance App.' It is a web app for calculating and visualizing linguistic distances among dialect varieties using the Relative Identity Distance, Levenshtein distance, Dynamic Time Warping, and the POS-tag <i>n</i>-gram frequency method. The following people were involved in the development of LED-A: Wilbert Heeringa (implementation), Vincent van Heuven (advice), Hans Van de Velde (project manager). LED-A is still under development. Comments are welcome and can be sent to"), img(src = 'e-mail.png', height = 20, align = "center"),"."),
          br(),

          h5(strong("How to cite this app")),
          p(HTML("Heeringa, Wilbert & Van Heuven, Vincent & Van de Velde, Hans (2023). LED-A: Levenshtein Edit Distance App [computer program]. Retrieved 27 June 2025 from <span style='font-family: \"Lucida Console\", \"Menlo\", \"Monaco\", \"Courier\", monospace;'>https://www.led-a.org/</span>.")),
          br(),

          h5(strong("Implementation")),
          p("LED-A is implemented as a Shiny app. Shiny was developed by RStudio. This web app uses the following R packages:"),
          br(),

          tags$div(tags$ul
          (
            tags$li(tags$span(HTML("<span style='color:blue'>apcluster</span>"), p("Ulrich Bodenhofer, Andreas Kothmeier, and Sepp Hochreiter (2011) APCluster: an R package for affinity propagation clustering Bioinformatics 27:2463-2464.", br(),
                                                                                   "Brendan J. Frey and Delbert Dueck (2007). Clustering by passing messages between data points. Science 315:972-977."))),
            tags$li(tags$span(HTML("<span style='color:blue'>base</span>"), p("R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. URL: https://www.R-project.org/."))),
            tags$li(tags$span(HTML("<span style='color:blue'>Cairo</span>"), p("Urbanek S, Horner J (2022). _Cairo: R Graphics Device using Cairo Graphics Library for Creating High-Quality Bitmap (PNG, JPEG, TIFF), Vector (PDF, SVG, PostScript) and Display (X11 and Win32) Output_. URL: https://CRAN.R-project.org/package=Cairo."))),
            tags$li(tags$span(HTML("<span style='color:blue'>callr</span>"), p("Csárdi G, Chang W (2022). _callr: Call R from R_. URL: https://CRAN.R-project.org/package=callr."))),
            tags$li(tags$span(HTML("<span style='color:blue'>colouR</span>"), p("Inglis A (2023). _colouR: Create Colour Palettes from Images_. URL: https://CRAN.R-project.org/package=colouR."))),
            tags$li(tags$span(HTML("<span style='color:blue'>dbscan</span>"), p("Hahsler M, Piekenbrock M (2024). _dbscan: Density-Based Spatial Clustering of Applications with Noise (DBSCAN) and Related Algorithms_. URL: https://CRAN.R-project.org/package=dbscan.", br(),
                                                                                "Hahsler M, Piekenbrock M, Doran D (2019). “dbscan: Fast Density-Based Clustering with R.” _Journal of Statistical Software_, *91*(1), 1-30."))),
            tags$li(tags$span(HTML("<span style='color:blue'>deldir</span>"), p("Turner R (2023). _deldir: Delaunay Triangulation and Dirichlet (Voronoi) Tessellation_. URL: https://CRAN.R-project.org/package=deldir."))),
            tags$li(tags$span(HTML("<span style='color:blue'>dplyr</span>"), p("Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data Manipulation_. URL: https://CRAN.R-project.org/package=dplyr."))),
            tags$li(tags$span(HTML("<span style='color:blue'>dtw</span>"), p("Giorgino T (2009). “Computing and Visualizing Dynamic Time Warping Alignments in R: The dtw Package. _Journal of Statistical Software_, *31*(7), 1-24. URL: https://doi.org/10.32614/CRAN.package.dtw."))),
            tags$li(tags$span(HTML("<span style='color:blue'>dynamicTreeCut</span>"), p("Langfelder P, Zhang B, Horvath S (2016). _dynamicTreeCut: Methods for Detection of Clusters in Hierarchical Clustering Dendrograms_. URL: https://doi.org/10.32614/CRAN.package.dynamicTreeCut."))),
            tags$li(tags$span(HTML("<span style='color:blue'>fpc</span>"), p("Hennig C (2024). _fpc: Flexible Procedures for Clustering_. URL: https://doi.org/10.32614/CRAN.package.fpc."))),
            tags$li(tags$span(HTML("<span style='color:blue'>fs</span>"), p("Hester J, Wickham H, Csárdi G (2025). _fs: Cross-Platform File System Operations Based on 'libuv'_. URL: https://doi:10.32614/CRAN.package.fs."))),
            tags$li(tags$span(HTML("<span style='color:blue'>geodist</span>"), p("Padgham, Mark (2021). geodist: Fast, Dependency-Free Geodesic Distance Calculations. URL: https://github.com/hypertidy/geodist"))),
            tags$li(tags$span(HTML("<span style='color:blue'>ggdendro</span>"), p("de Vries A, Ripley BD (2022). _ggdendro: Create Dendrograms and Tree Diagrams Using 'ggplot2'_. URL: https://CRAN.R-project.org/package=ggdendro."))),
            tags$li(tags$span(HTML("<span style='color:blue'>ggh4x</span>"), p("van den Brand T (2023). _ggh4x: Hacks for 'ggplot2'_. URL: https://CRAN.R-project.org/package=ggh4x."))),
            tags$li(tags$span(HTML("<span style='color:blue'>ggplot2</span>"), p("H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016. URL: https://ggplot2.tidyverse.org."))),
            tags$li(tags$span(HTML("<span style='color:blue'>ggrepel</span>"), p("Slowikowski K (2023). _ggrepel: Automatically Position Non-Overlapping Text Labels with 'ggplot2'_. URL: https://CRAN.R-project.org/package=ggrepel."))),
            tags$li(tags$span(HTML("<span style='color:blue'>htmlwidgets</span>"), p("Vaidyanathan R, Xie Y, Allaire J, Cheng J, Sievert C, Russell K (2023). _htmlwidgets: HTML Widgets for R_. URL: https://CRAN.R-project.org/package=htmlwidgets."))),
            tags$li(tags$span(HTML("<span style='color:blue'>ipa</span>"), p("Alexander Rossell Hayes (2020). ipa: convert between phonetic alphabets. URL: https://github.com/rossellhayes/ipa."))),
            tags$li(tags$span(HTML("<span style='color:blue'>leaflet</span>"), p("Cheng J, Schloerke B, Karambelkar B, Xie Y (2023). _leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library_. URL: https://CRAN.R-project.org/package=leaflet."))),
            tags$li(tags$span(HTML("<span style='color:blue'>leaflet.extras</span>"), p("Karambelkar B, Schloerke B (2018). _leaflet.extras: Extra Functionality for 'leaflet' Package_. URL: https://CRAN.R-project.org/package=leaflet.extras."))),
            tags$li(tags$span(HTML("<span style='color:blue'>MASS</span>"), p("Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0. URL: https://www.stats.ox.ac.uk/pub/MASS4/."))),
            tags$li(tags$span(HTML("<span style='color:blue'>openxlsx</span>"), p("Schauberger P, Walker A (2023). _openxlsx: Read, Write and Edit xlsx Files_. URL: https://CRAN.R-project.org/package=openxlsx."))),
            tags$li(tags$span(HTML("<span style='color:blue'>pcaPP</span>"), p("Filzmoser P, Fritz H, Kalcher K (2024). _pcaPP: Robust PCA by Projection Pursuit_. URL: https://doi.org/10.32614/CRAN.package.pcaPP."))),
            tags$li(tags$span(HTML("<span style='color:blue'>plyr</span>"), p("Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1), 1-29. URL: https://www.jstatsoft.org/v40/i01/."))),
            tags$li(tags$span(HTML("<span style='color:blue'>proxy</span>"), p("Meyer D, Buchta C (2022). _proxy: Distance and Similarity Measures_. URL: https://doi:10.32614/CRAN.package.proxy."))),
            tags$li(tags$span(HTML("<span style='color:blue'>readr</span>"), p("Wickham H, Hester J, Bryan J (2023). _readr: Read Rectangular Text Data_. URL: https://CRAN.R-project.org/package=readr."))),
            tags$li(tags$span(HTML("<span style='color:blue'>rnaturalearth</span>"), p("Massicotte P, South A (2023). _rnaturalearth: World Map Data from Natural Earth_. URL: https://CRAN.R-project.org/package=rnaturalearth."))),
            tags$li(tags$span(HTML("<span style='color:blue'>rnaturalearthdata</span>"), p("South A (2017). _rnaturalearthdata: World Vector Map Data from Natural Earth Used in 'rnaturalearth'_. URL: https://CRAN.R-project.org/package=rnaturalearthdata."))),
            tags$li(tags$span(HTML("<span style='color:blue'>Rtsne</span>"), p("L.J.P. van der Maaten and G.E. Hinton (2008). Visualizing High-Dimensional Data Using t-SNE. Journal of Machine Learning Research 9(Nov):2579-2605.", br(),
                                                                               "L.J.P. van der Maaten (2014). Accelerating t-SNE using Tree-Based Algorithms.   Journal of Machine Learning Research 15(Oct):3221-3245.", br(),
                                                                               "Jesse H. Krijthe (2015). Rtsne: T-Distributed Stochastic Neighbor Embedding using a Barnes-Hut Implementation. URL: https://github.com/jkrijthe/Rtsne."))),
            tags$li(tags$span(HTML("<span style='color:blue'>shiny</span>"), p("Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2023). _shiny: Web Application Framework for R_. URL: https://CRAN.R-project.org/package=shiny."))),
            tags$li(tags$span(HTML("<span style='color:blue'>shinyBS</span>"), p("Bailey E (2022). _shinyBS: Twitter Bootstrap Components for Shiny_. URL: https://CRAN.R-project.org/package=shinyBS."))),
            tags$li(tags$span(HTML("<span style='color:blue'>shinyjqui</span>"), p("Tang Y (2022). _shinyjqui: 'jQuery UI' Interactions and Effects for Shiny_. URL: https://CRAN.R-project.org/package=shinyjqui."))),
            tags$li(tags$span(HTML("<span style='color:blue'>shinysky</span>"), p("ZJ D (2019). _shinysky: A Set of Shiny Components and Widgets_. URL: https://github.com/AnalytixWare/ShinySky."))),
            tags$li(tags$span(HTML("<span style='color:blue'>shinyWidgets</span>"), p("Perrier V, Meyer F, Granjon D (2023). _shinyWidgets: Custom Inputs Widgets for Shiny_. URL: https://CRAN.R-project.org/package=shinyWidgets."))),
            tags$li(tags$span(HTML("<span style='color:blue'>stringr</span>"), p("Wickham H (2022). _stringr: Simple, Consistent Wrappers for Common String Operations_. URL: https://CRAN.R-project.org/package=stringr."))),
            tags$li(tags$span(HTML("<span style='color:blue'>svglite</span>"), p("Wickham H, Henry L, Pedersen T, Luciani T, Decorde M, Lise V (2023). _svglite: An 'SVG' Graphics Device_. URL: https://CRAN.R-project.org/package=svglite."))),
            tags$li(tags$span(HTML("<span style='color:blue'>tikzDevice</span>"), p("Sharpsteen C, Bracken C (2023). _tikzDevice: R Graphics Output in LaTeX Format_. URL: https://CRAN.R-project.org/package=tikzDevice."))),
            tags$li(tags$span(HTML("<span style='color:blue'>tuneR</span>"), p("Uwe Ligges, Sebastian Krey, Olaf Mersmann, and Sarah Schnackenberg (2023). tuneR: Analysis of Music and Speech. URL: https://CRAN.R-project.org/package=tuneR"))),
            tags$li(tags$span(HTML("<span style='color:blue'>udpipe</span>"), p("Wijffels J (2023). _udpipe: Tokenization, Parts of Speech Tagging, Lemmatization and Dependency Parsing with the UDPipe' 'NLP' Toolkit_. URL: https://doi.org/10.32614/CRAN.package.udpipe."))),
            tags$li(tags$span(HTML("<span style='color:blue'>webshot2</span>"), p("Chang W (2025). _webshot2: Take Screenshots of Web Pages_. URL: https://doi.org/10.32614/CRAN.package.webshot2.")))
          )),

          br(),
          p("In addition, programs implemented in ", a("Free Pascal", href = "https://www.freepascal.org", target = "_blank"), "and", a("C", href = "https://gcc.gnu.org/", target = "_blank"), " are used on the backend, as well as the ", a("Praat", href = "https://www.fon.hum.uva.nl/praat/", target = "_blank"), " program (in particular the 'Change gender' function)."),
          br(),
          p("The icons used in the main menu of this app are glyphs taken from the set of", a("Bootstrap Glyphicons", href = "https://getbootstrap.com/docs/3.3/components/", target = "_blank"), "which includes over 250 glyphs from the", a("Glyphicon", href = "https://glyphicons.com/", target = "_blank"), "Halflings set."),
          br(),

          h5(strong("Gabmap")),
          p("Gabmap offers some functionality that is not available in LED-A. Gabmap is available at", a("gabmap.nl", href = "https://gabmap.nl", target = "_blank"), ". A Docker version can be found", a("here", href = "https://github.com/pebbe/Gabmap", target = "_blank"), "."),
        ),
        
        br()
      )
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-warning-sign' style='font-size: 100%'></span>&nbsp;Disclaimer"),
      value = "disclaimer",
      
      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        
        br(),
        
        fluidPage(
          style = "min-height: 601px;",

          h5(strong("Privacy")),
          p("By uploading data files in LED-A, you give the Fryske Akademy permission to process the data files with LED-A. The data will not be used by the Fryske Akademy for any other purposes. When uploading data files in LED-A, they are temporarily stored on the Fryske Akademy server. The data will not be shared with third parties and is protected against unauthorized access and disclosure. The data files will be deleted from the server after the session is ended by the user."),
          br(),
          p("This app uses cookies that are used to collect data. By using this site you agree to these cookies being set. Google Analytics is used in order to track and report website traffic. See: ", a("How Google uses data when you use our partners' sites or apps", href = "https://www.google.com/policies/privacy/partners/", target = "_blank"),"."),
          br(),
          h5(strong("Liability")),
          p("This app is provided 'as is' without warranty of any kind, either express or implied, including, but not limited to, the implied warranties of fitness for a purpose, or the warranty of non-infringement. Without limiting the foregoing, the Fryske Akademy makes no warranty that: 1) the app will meet your requirements, 2) the app will be uninterrupted, timely, secure or error-free, 3) the results that may be obtained from the use of the app will be effective, accurate or reliable, 4) the quality of the app will meet your expectations, 5) any errors in the app will be corrected."),
          br(),
          p("The app and its documentation could include technical or other mistakes, inaccuracies or typographical errors. The Fryske Akademy may make changes to the app or documentation made available on its web site. The app and its documentation may be out of date, and the Fryske Akademy makes no commitment to update such materials."),
          br(),
          p("The Fryske Akademy assumes no responsibility for errors or ommissions in the app or documentation available from its web site."),
          br(),
          p("In no event shall the Fryske Akademy be liable to you or any third parties for any special, punitive, incidental, indirect or consequential damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of use, data or profits, whether or not the Fryske Akademy has been advised of the possibility of such damages, and on any theory of liability, arising out of or in connection with the use of this software."),
          br(),
          p("The use of the app is done at your own discretion and risk and with agreement that you will be solely responsible for any damage to your computer system or loss of data that results from such activities. No advice or information, whether oral or written, obtained by you from the Fryske Akademy shall create any warranty for the software."),
          br(),
          h5(strong("Other")),
          p("The disclaimer may be changed from time to time."),
        ),
        
        br()
      )
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-download-alt' style='font-size: 100%'></span>&nbsp;Examples"),
      value = "examples",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        
        br(),
        
        fluidPage(
          style = "min-height: 601px;",

          h5("The data sets on this page can freely be downloaded and used to try out LED-A."),

          br(),

          h5(strong("One-word pair example")),

          a("heart",
            href = "datasets1/heart.xlsx",
            target = "_blank"),

          br(), br(),

          p(HTML("The Dutch word <i>hart</i> 'heart' is pronounced as [ærtə] in
            the West Flemish dialect of Oostende (in Belgium) and as [hɑʀt] in the
            Limburg dialect of Meerssen (in The Netherlands). With the example
            file the distance between the two respective realizations can be
            calculated in LED-A. The transcriptions were taken from the <i>Reeks
            Nederlandse Dialectatlassen</i> (see below).")),

          br(),

          h5(strong("Nordavinden og sola -- Norwegian")),

          a("NOS Norwegian",
            href = "datasets1/NorwegianNOS.xlsx",
            target = "_blank"),

          br(),

          a("NOS Norwegian coordinates",
            href = "datasets1/NorwegianNOS_GeoHack.xlsx",
            target = "_blank"),

          br(), br(),

          p(HTML("The data offered here consists of IPA transcriptions of a subset
            of 20 Norwegian varieties out of a database that contains recordings
            and transcriptions of 55 Norwegian varieties. This database was
            compiled by J&oslash;rn Almberg and Kristian Skarb&oslash; (Department
            of Linguistics, University of Trondheim) in the period 1999–2002. As a
            basis the text of the fable ‘The North Wind and the Sun’ was used."),
            "The database is ", a("online", href = "http://www.hf.ntnu.no/nos/",
            target = "_blank"), HTML("available at a website of the Norwegian
            University of Science and Technology.")),

          br(),

          h5(strong("Reeks Nederlands Dialectatlassen -- Dutch")),

          a("RND Dutch",
            href = "datasets1/DutchRND050.xlsx",
            target = "_blank"),

          br(),

          a("RND Dutch coordinates",
            href = "datasets1/DutchRND050_GeoHack.xlsx",
            target = "_blank"),

          br(), br(),

          p(HTML("The <i>Reeks Nederlandse Dialectatlassen</i> (RND) is a series
            of atlasses covering the Dutch dialect area. The Dutch dialect area
            comprises the Netherlands, the northern part of Belgium, a smaller
            northwestern part of France and the German county Bentheim. The atlas
            was compiled by prof. E. Blancquaert and Willem P&eacute;e in the
            period 1925-1982. The RND contains the translations of 139 sentences
            in 1956 local dialects spread over this entire area. The sentences are
            translated and transcribed in phonetic script for each dialect. The
            atlas is "), a("online", href =
            "https://www.dialectzinnen.ugent.be", target = "_blank"),
            HTML("available at a website of Ghent University.")),

          p(HTML("A selection of 360 local dialects is available at the "),
            a("Dutch Language Institute", href =
            "https://taalmaterialen.ivdnt.org/download/rnd-woordenlijsten1-1/",
            target = "_blank"), HTML(". For 136 local dialects transcriptions are
            available for a set of 166 words that were chosen from the 139
            sentences. For 226 local dialects transcriptions are available for a
            set of 125 words. The set of 125 words is a subset of the set of 166
            words. Standard Dutch and Standard German were added having
            transcriptions of 166 words each. The data set that we offer on this
            page includes a subset of 50 local dialects having transcriptions of
            166 words each. Additionally, Standard Dutch (SD) is included.")),

          br(),

          h5(strong("Reeks Nederlandse Dialectatlassen -- Frisian")),

          a("RND Frisian",
            href = "datasets1/FrisianRND.xlsx",
            target = "_blank"),

          br(),

          a("RND Frisian coordinates",
            href = "datasets1/FrisianRND_GeoHack.xlsx",
            target = "_blank"),

          br(), br(),

          p(HTML("This data set is another subset of the set that is hosted by the
            Dutch Language Institute. It contains transcriptions of 48 local
            dialects in the Dutch province of Frysl&#226;n and transcriptions of
            two local dialects in the Dutch province of Groningen in the area
            adjacent to the eastern border of the province of Frysl&#226;n. Per
            local dialect 90 words are selected. For the local dialects of
            Appelscha, Donkerbroek and Tjalleberd transcriptions of two different
            varieties are included.")),
          
          br(),
          
          h5(strong("Text To Speech -- Germanic")),
          
          a("TTS Germanic",
            href = "datasets2/GermanicTTS.zip",
            target = "_blank"),
          
          br(),
          
          a("TTS Germanic genders",
            href = "datasets2/GermanicTTS_Genders.xlsx",
            target = "_blank"),
          
          br(), br(),
          
          p(HTML("This dataset contains acoustic samples of 20 words in 16 
            language varieties. The 20 words are: apple, daughter, earth, egg,
            eye, fish, foot, heart, house, year, knee, milk, name, night, nose,
            stone, wind, water, word, sun. These words were translated into
            Danish, Dutch, English, Icelandic, German, Norwegian and Swedish.
            The translations were entered into"),
            a("ttsMP3.com", href = "https://ttsmp3.com/", target = "_blank"),
            HTML("and pronounced by the following speakers: Mads (Danish), Naja
            (Danish), Lotte (Dutch), Ruben (Dutch), Nicole (Australian English),
            Russell (Australian English), Amy (British English), Brian (British
            English), Matthew (US English), Salli (US English), Hans (German),
            Vicki (German), Dóra (Icelandic), Karl (Icelandic), Liv (Norwegian),
            Astrid (Swedish)."),
            a("ttsMP3.com", href = "https://ttsmp3.com/", target = "_blank"),
            HTML("uses external services such as"),
            a("Amazon Polly", href = "https://docs.aws.amazon.com/polly/", target = "_blank"),
            HTML("for Standard Voices."),
            a("Amazon Polly", href = "https://docs.aws.amazon.com/polly/", target = "_blank"),
            HTML("is known to use concatenative synthesis. See"),
            a("here", href = "https://docs.aws.amazon.com/polly/latest/dg/available-voices.html", target = "_blank"),
            HTML("for the full list of speakers.")),
          
          p(HTML("In order to use this data set in LED-A, download the two
            files. After downloading unzip GermanicTTS.zip. This will result in
            a folder GermanicTTS.zip that contains 16 times 20 is 320 wave
            files. In order to process them, choose ‘Run’ in the main menu of
            LED-A, and subsequently choose ‘Acoustic data.’ Under ‘Upload sound
            files’ browse to the folder GermanicTTS and select all 320 wave
            files at once. The uploading may take some time. Choose as format
            ‘variety_item.wav’. ")),
          
          p(HTML("Upload GermanicTTS_Genders.xlsx under ‘Upload table with
            gender’. This is important since our data set includes both male and
            female speakers. When the genders of the speakers are known, the
            differences between the genders will automatically be eliminated by
            using the function Change Gender in"),
            a("Praat", href = "https://www.fon.hum.uva.nl/praat/", target = "_blank"),
            HTML(".")),
          
          p(HTML("Finally, click on Go! The calculation of the acoustic
            distances among  the speakers may take some time. During this
            process, do not close the browser.")),

          br(),
          
          h5(strong("Universal Dependencies -- European")),
          
          a("UD European xlsx",
            href = "datasets3/EuropeanUDxlsx.zip",
            target = "_blank"),
          
          br(),
          
          a("UD European conllu",
            href = "datasets3/EuropeanUDconllu.zip",
            target = "_blank"),
          
          br(), br(),
          
          p(HTML("This dataset contains translations of the fable 'The North
            Wind and the Sun' in 14 different languages. The English
            orthographic version in"), 
            a("Wikipedia", href = "https://en.wikipedia.org/wiki/The_North_Wind_and_the_Sun", target = "_blank"),
            HTML("was translated into the 14 languages using Google Translate 
            and POS-tagged using the"),
            a("UDPipe", href = "https://lindat.mff.cuni.cz/services/udpipe/", target = "_blank"),
            HTML("web app. For Frisian"),
            a("Frysker", href = "https://frysker.nl/oersetter", target = "_blank"), 
            HTML("was used for translating the text into Frisian, and"),
            a("UDPipe Frysk", href = "https://fryske-akademy.nl/fa-apps/ud-pipe/", target = "_blank"), 
            HTML("was used for adding the POS-tags. We offer two versions of the
            data set. In the first version each POS-tagged text is stored as
            Excel file, and in the second version each POS-tagged text is saved
            as conllu file. In LED-A either format can be used.")),
          
          p(HTML("In order to use these data sets in LED-A, download the two
            files. After downloading unzip the two files. Each file contains 14
            files. In order to process them, choose ‘Run’ in the main menu of
            LED-A, and subsequently choose ‘POS-tag data.’ Under ‘Upload sound
            files’ browse to the folder EuropeanUDxlsx or EuropeanUDconllu and
            select all 14 files at once. Under 'Format of file names' indicate
            the format of the files that you uploaded. Then review the different
            options that you can choose. Normally the options that are chosen by
            default are fine.")),
          
          p(HTML("Finally, click on Go! The calculation of the acoustic
            distances among  the speakers may take some time. During this
            process, do not close the browser."))
        ),

        br()
      )
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-tree-conifer' style='font-size: 100%'></span>&nbsp;Standalone"),
      value = "standalone",
      
      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 235px); background-color: #eeeefe",
        
        br(),
        
        fluidPage(
          style = "border: 1px solid silver; min-height: 601px; max-width: 680px; background-color: #ffffff",

          br(),
        
          h5(style= "margin-left: 46px;", strong("How to install LED-A as a standalone application in Windows")),
          
          tags$div(tags$ul
          (
            tags$li(tags$div(style="padding: 5px;", "The installer provided on this page has been tested under Windows 10.")),
            tags$li(tags$div(style="padding: 5px;", "Click", a(strong("here"), href="standalone/Install LED-A.exe", target = "_blank"), "in order to download the installer.")),
            tags$li(tags$div(style="padding: 5px;", "Save the installer file in the Downloads or Desktop folder and double-click on it. Then follow the installation procedure.")),
            tags$li(tags$div(style="padding: 5px;", "Under", strong('Select Destination Location'), "click on", strong('Next'), ".")),
            tags$li(tags$div(style="padding: 5px;", "Under", strong('Select Additional Tasks'), "check", strong('Create a desktop shortcut'), "(important!)")),
            tags$li(tags$div(style="padding: 5px;", "Under", strong('Ready to Install'), "click on", strong('Install'), ".")),
            tags$li(tags$div(style="padding: 5px;", "Under", strong('Installing'), "just wait.")),
            tags$li(tags$div(style="padding: 5px;", "Under", strong('Completing the LED-A Setup Wizard'), "click on", strong('Finish'), ".")),
            tags$li(tags$div(style="padding: 5px;", "Now the packages are loaded, which may take a while, especially when running the application for the first time.")),
            tags$li(tags$div(style="padding: 5px;", "When loading the packages is completed, your default web browser is openened and LED-A is loaded in the browser.")),
            tags$li(tags$div(style="padding: 5px;", "When you closed the app and want to run it again, double-click on the LED-A icon on the deskop."))
          )),
        
          p(style= "margin-left: 46px;", "We thank wleepang for developing and making available the", 
            a("DesktopDeployR framework", href="https://github.com/wleepang/DesktopDeployR", target = "_blank"), 
            "and Jordan Russell for developing and making available", 
            a("Inno Setup", href="https://jrsoftware.org/isdl.php", target = "_blank"),
            ". We thank", a("SuperShiny", href="https://www.youtube.com/watch?v=t6laYlLdgH8", target = "_blank"), 
            "for explaining how to use both the", 
            a("DesktopDeployR framework", href="https://github.com/wleepang/DesktopDeployR", target = "_blank"), 
            "and",
            a("Inno Setup", href="https://jrsoftware.org/isdl.php", target = "_blank"), "."),

          br()
        ),
        
        br()
      )
    )),
    
    br()
  ),
    
  tags$footer
  (
    tags$table(
      style = "width:100%",
      tags$tr
      (
        tags$td(style = "width: 23%;"),
        tags$td(tags$a(tags$img(src="FA.png", style = "height: 40px; margin-top: -4px;  margin-bottom: 7px; margin-left: 0px;"),
                       href    = "https://www.fryske-akademy.nl/en/"),
                       style   = "width: 54%; text-align: center;",
                       class   = "balk",
                       onclick = "window.open('https://www.fryske-akademy.nl/en/', '_blank'); return false;",
                       target  = "_blank"),
        tags$td(textOutput("heartbeat"))
      )
    )
  )
)

################################################################################

server <- function(input, output, session)
{
  homeDir <- list.dirs("/home", full.names = TRUE, recursive = FALSE)[1]
  homeDir <- paste0(homeDir, "/temp/")

  tempDir <- sub("/tmp/", homeDir, tempdir())
  tempDir <- paste0(tempDir, "/", session$token, "/")
  
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
  Sys.setenv(TMPDIR = tempDir)

  session$onSessionEnded(function()
  {
    tryCatch(
      expr  = if (dir.exists(tempDir))
        unlink(tempDir, recursive = TRUE, force = TRUE),
      error = function(e)
        warning("Failed to delete temp dir: ", conditionMessage(e))
    )
  })

  global <- reactiveValues(
    dataType         = "transcriptions",

    background       = NULL,
    background6      = NULL,

    items            = NULL,
    geoTab           = NULL,
    genTab1          = NULL,
    geoTab1          = NULL,
    geoTab2          = NULL,
    geoTab8          = NULL,
    
    distTab          = NULL,
    segmTab          = NULL,

    IPAset           = NULL,

    finished         = NULL,
    finished6        = NULL,
    idNot            = NULL,

    replyMethod31    = "UPGMA",
    replyMethod32    = "Kruskal's",

    nColGroups       = NULL,
    showLegend       = FALSE,
    legendLabels     = NULL,
    
    replyColors3     = TRUE,

    mdsGeon3         = "2D",
    mdsGeon31        = c("colors", "labels"),
    mdsGeon32        = NULL,

    maxOverlaps      = NULL,

    beamWeight       = 4,
    networkWeight    = 4,

    selR             = "1",
    selG             = "2",
    selB             = "3",
    selInv           = NULL,

    selRef           = NULL,

    dotRadiusBeam    = 3,
    dotRadiusNetwork = 3,
    
    selBorder5       = FALSE,
    selBorder6       = FALSE,
    
    dotRadiusArea5   = 6,
    dotRadiusArea6   = 6,
    dotRadiusRGB     = 6,
    dotRadiusRef     = 6,

    degStab6         = 5,
    minPts6          = NULL,     
    posLegend        = "br",
    selCophenetic    = FALSE,
    partition        = NULL
  )

  ##############################################################################
	
  observeEvent(input$navBar,
  {
    if (getUrlHash() == paste0("#", input$navBar)) return()
    updateQueryString(paste0("#", input$navBar), mode = "push")
  })

  observeEvent(getUrlHash(),
  {
    Hash <- getUrlHash()
    if (Hash == paste0("#", input$navBar)) return()
    Hash <- gsub("#", "", Hash)
    updateNavbarPage(session, "navBar", selected=Hash)
  })

  output$heartbeat <- renderText(
  {
    invalidateLater(5000)
    Sys.time()
  })

  ##############################################################################
  
  onSessionStart = isolate(
  {
    users$count <- users$count + 1
  })
  
  onSessionEnded(function() 
  {
    isolate(
    {
      users$count <- users$count - 1
    })
  })
  
  observeEvent(users$count,
    showNotification(paste0("Currently ", users$count, " user(s) connected."), type = "message", duration = 5)
  )

  ##############################################################################
  
  check_decimal <- function(df)
  {
    if (as.character(Sys.localeconv()[1])==".")
    {
      df[] <- lapply(df, function(x) 
      {
        if (is.numeric(x)) 
        {
          x <- as.numeric(gsub(",", "\\.", as.character(x)))
        }
        
        return(x)
      })
    }
    
    if (as.character(Sys.localeconv()[1])==",")
    {
      df[] <- lapply(df, function(x) 
      {
        if (is.numeric(x)) 
        {
          x <- as.numeric(gsub("\\.", ",", as.character(x)))
        }
        
        return(x)
      })
    }
    
    return(df)  
  }  

  ##############################################################################

  output$geoTab <- renderUI(
  {
    input$dataTab
    global$geoTab <- NULL

    fluidPage(
      align = "center",
      fileInput(
        inputId    = "geoTab",
        label      = NULL,
        multiple   = FALSE,
        accept     = ".xlsx",
        width      = "250px")
    )
  })

  observeEvent(input$geoTab,
  {
    global$geoTab <- input$geoTab
  })
  
  output$segmTab <- renderUI(
  {
    input$dataTab
    global$segmTab <- NULL

    req(input$segmDist)

    if (input$segmDist=="upload my own distances")
    {
      return(tagList(
        fluidPage(
          style = "margin-left: 120px",
          HTML("<p style='font-weight: bold;'>Upload table:</p>"),
          p(style='height: 8px')
        ),

        fluidPage(
          align = "center",
          fileInput(
            inputId    = "segmTab",
            label      = NULL,
            multiple   = FALSE,
            accept     = ".xlsx",
            width      = "250px")
        ),
      ))
    }
    else
      return(NULL)
  })

  observeEvent(input$segmTab,
  {
    global$segmTab <- input$segmTab
  })  
  
  output$extraAli <- renderUI(
  {
    req(input$segmDist)

    if ((input$segmDist!="binary item comparison") & (input$segmDist!="upload my own distances"))
    {
      choices <- "i/j/u/w versus anything"

      if ((input$segmDist=="plain  sub. = 1 indel = 0.5") | (input$segmDist=="plain  cost = 1") | (input$segmDist=="PMI   cost ≤ 1"))
        choices <- c(choices, "ə/ɐ versus sonorant")

      return(tagList(
        fluidPage(
          style = "margin-left: 120px",

          bsButton("butExtraAli", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modExtraAli", "Extra allowed segment alignments", "butExtraAli", size = "large", "Dit is de helptekst"),

          HTML("<span style='font-weight: bold;'>&nbsp;Extra allowed segment alignments:</span>"),
          p(style='height: 8px')
        ),

        fluidPage(
          style = "margin-left: 184px",

          checkboxGroupInput(
            inputId    = 'extraAli',
            label      = NULL,
            choices    = choices,
            selected   =   "",
            inline     = FALSE
          )
        ),

        br()
      ))
    }
  })

  output$selProc <- renderUI(
  {
    if (input$segmDist!="binary item comparison")
    {
      choices <- c("primary stress",
                   "secondary stress",
                   "length",
                   "diacritics")

      return(tagList(
        fluidPage(
          style = "margin-left: 120px",

          bsButton("butSelProc", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modSelProc", "Process", "butSelProc", size = "large",
                   span("For info about processing stress, length and diacritics see "), 
                   a("stress, length and diacritics", href = "docs/Diacr.pdf", target = "_blank"),
                   span(".")),

          HTML("<span style='font-weight: bold;'>&nbsp;Process:</span>"),
          p(style='height: 8px')
        ),

        fluidPage(
          style = "margin-left: 184px",

          checkboxGroupInput(
            inputId    = 'selProc',
            label      = NULL,
            choices    = choices,
            selected   =   "",
            inline     = FALSE
          )
        ),

        br()
      ))
    }
  })

  output$selNorm <- renderUI(
  {
    if (input$segmDist!="binary item comparison")
    {
      return(tagList(
        fluidPage(
          style = "margin-left: 120px",

          bsButton("butSelNorm", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modSelNorm", "Normalization of word pair distances", "butSelNorm", size = "large", "Dit is de helptekst"),

          HTML("<span style='font-weight: bold;'>&nbsp;Normalization of word pair distances:</span>"),
          p(style='height: 8px')
        ),

        fluidPage(
          style = "margin-left: 184px",

          radioButtons(
            inputId    = 'selNorm',
            label      = NULL,
            choices    = c("none", "divide by alignment length"),
            selected   = "divide by alignment length",
            inline     = FALSE
          )
        ),

        br()
      ))
    }
  })

  output$selMeasurements <- renderUI(
  {
    req(input$segmDist)

    if (input$segmDist!="binary item comparison")
    {
      return(tagList(
        fluidPage(
          style = "margin-left: 120px",

          bsButton("butSelMeasurements", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modSelMeasurements", "Measurements are based on", "butSelMeasurements", size = "large", "Dit is de helptekst"),

          HTML("<span style='font-weight: bold;'>&nbsp;Measurements are based on:</span>"),
          p(style='height: 8px')
        ),

        fluidPage(
          style = "margin-left: 184px",

          radioButtons(
            inputId    = 'selMeasurements',
            label      = NULL,
            choices    = c("whole words",
                           "only vowel-vowel pairs",
                           "only vowel indels",
                           "only consonant-consonant pairs",
                           "only consonant indels"),
            selected   =   "whole words",
            inline     = FALSE
          )
        ),

        br()
      ))
    }
  })

  output$selOutput <- renderUI(
  {
    req(input$segmDist)

    choices <- c("aggregated distances", "word pair distances")

    if (input$segmDist!="binary item comparison")
    {
      choices <- c(choices, c("segment distances"))

      if (!((input$segmDist=="plain  sub. = 1 indel = 0.5") & is.element("length", input$selProc)))
        choices <- c(choices, c("alignments"))
    }

    fluidPage(
      style = "margin-left: 184px",

      checkboxGroupInput(
        inputId    = 'selOutput',
        label      = NULL,
        choices    = choices,
        selected   = "",
        inline     = FALSE
      )
    )
  })

  observeEvent(input$goButton,
  {
    global$dataType   <- input$navBar
    global$finished   <- FALSE
    global$background <- NULL
    global$partition  <- NULL

    gc();
    system(paste0("rm -Rf ", tempDir, "*"))

    # check inputs

    if  (length(input$dataTab )==0)
    {
      showNotification("Please upload a file with transcriptions!", type = "error")
      return(NULL)
    }

    if  (length(input$dataForm)==0)
    {
      showNotification("Please indicate the format of the table with transcriptions!", type = "error")
      return(NULL)
    }

    if ((input$segmDist=="upload my own distances") &&
        (length(global$segmTab)==0))
    {
      showNotification("Please upload a file with segment distances!", type = "error")
      return(NULL)
    }

    # upload table with transcriptions

    if (is.null(input$dataTab))
      return(NULL)

    dt <- as.data.frame(read.xlsx(input$dataTab$datapath, rowNames = F, colNames = T, sep.names = " "))
    dt[is.na(dt)] <- ""

    # check duplicates

    if (length(unique(dt[,1])) < length(dt[,1]))
    {
      if (input$dataForm=="rows are varieties, columns are items")
        showNotification("Duplicate variety names are not allowed!", type = "error", duration = NULL)
      else

      if (input$dataForm=="rows are items, columns are varieties")
        showNotification("Duplicate item names are not allowed!"   , type = "error", duration = NULL)
      else {}

      return(NULL)
    }

    if (length(unique(colnames(dt))) < length(colnames(dt)))
    {
      if (input$dataForm=="rows are varieties, columns are items")
        showNotification("Duplicate item names are not allowed!"   , type = "error", duration = NULL)
      else
      
      if (input$dataForm=="rows are items, columns are varieties")
        showNotification("Duplicate variety names are not allowed!", type = "error", duration = NULL)
      else {}

      return(NULL)
    }

    if (input$dataForm=="rows are varieties, columns are items")
    {
      dt <- dt[order(tolower(dt[,1])),]
      rownames(dt) <- dt[,1]
      dt[,1] <- NULL
      dt <- t(dt)
      dt <- cbind(rownames(dt), data.frame(dt, row.names=NULL, check.names = F))
    }
    else
      
    if (input$dataForm=="rows are items, columns are varieties")  
    {
      dt0 <- dt[,2:ncol(dt)]
      dt0 <- dt0[,order(tolower(colnames(dt0)))]
      dt  <- cbind(dt[,1], dt0)
    }
    else {}

    colnames(dt)[1] <- ""

    # check coordinates file

    if (!is.null(global$geoTab))
    {
      varieties1 <- tolower(colnames(dt)[2:ncol(dt)])
      varieties2 <- tolower(geoTab()[,1])

      if ((!setequal(varieties1,varieties2)) | (length(varieties1)!=length(varieties2)))
      {
        showNotification("Varieties in transcriptions file do not match with varieties in coordinates file!", type = "error", duration = NULL)
        return(NULL)
      }
    }

    # check items

    nr <- nrow(dt)
    dt <- dt[rowSums(dt!="") > 2,]
    NR <- nrow(dt)

    if (NR < nr)
      showNotification(paste(nr-NR, "items not found or found in only one variety were removed,", NR, "items are left over."), type = "message", duration = NULL)

    # preprocess transcriptions
    
    global$idNot <- showNotification("Preprocessing transcriptions...", type = "message", duration = NULL)

    if (input$segmDist=="binary item comparison")
    {
      # replace spaces by underscores
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) str_replace_all(x, "(?<!([ ]/))[ ](?!(/[ ]))", "_"))
    }
    else
    {
      # exclude stress if not checked
      if (!is.element("primary stress", input$selProc))
      {
        dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ˈ", "", x))
        dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("'", "", x))
        dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("’", "", x))
      }

      if (!is.element("secondary stress", input$selProc))
        dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ˌ", "", x))

      # remove underscores
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("_", "", x))

      # remove spaces
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) str_replace_all(x, "(?<!(/))[ ](?!(/))", ""))

      # remove tie bars
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("([[:alpha:]])͡([[:alpha:]])", "[\\1\\2]", x))

      # convert obsolete or exceptional characters
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("g", "ɡ" , x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ɼ", "r̝" , x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ɥ", "jʷ", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ɫ", "lˠ", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ɷ", "ʊ" , x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ʍ", "xʷ", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ʁ̍", "ʁ" , x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ʌ̱", "ʌ" , x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ɕ", "ʃ" , x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ʑ", "ʒ" , x))

      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ʦ", "[ts]", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ʧ", "[tʃ]", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ʨ", "[tʃ]", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ʣ", "[dz]", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ʤ", "[dʒ]", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ʥ", "[dʒ]", x))

      # get set of unique segments in dataset
      global$IPAset <- c()

      for (i in 2:ncol(dt))
      {
        global$IPAset <- unique(c(global$IPAset, unique(unlist(strsplit(dt[,i], '')))))
      }

      global$IPAset <- gsub("ʰ", "h", global$IPAset)
      global$IPAset <- gsub("ʷ", "w", global$IPAset)
      global$IPAset <- gsub("ʲ", "j", global$IPAset)
      global$IPAset <- gsub("ˠ", "ɣ", global$IPAset)
      global$IPAset <- gsub("ˤ", "ʕ", global$IPAset)
      global$IPAset <- gsub("̃ ", "n", global$IPAset)
      
      global$IPAset <- unique(global$IPAset)
      
      # convert to X-SAMPA
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) ipa(x, to="xsampa"))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ⱱ", "b\\", x))

      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("b_<"    , "b<"    , x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("d_<"    , "d<"    , x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("J\\\\_<", "J\\\\<", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("g_<"    , "g<"    , x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("G\\\\_<", "G\\\\<", x))

      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("ˈ", '"', x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("'", '"', x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("’", '"', x))

      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("x\\\\", "[Sx]", x))

      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("~", "_~", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("͂", "_~", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("=", "_=", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("`", "_`", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("̊" , "_0", x))
      
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("t_`", "t`", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("d_`", "d`", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("n_`", "n`", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("r_`", "r`", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("s_`", "s`", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("z_`", "z`", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("r\\\\_`", "r\\\\`", x))
      dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("l_`", "l`", x))
    }

    # put # in empty cells
    dt[,2:ncol(dt)] <- apply(dt[,2:ncol(dt)], 2, function(x) gsub("^$", "#", x))

    # write each column as a file
    fileName <- rep("", ncol(dt)-1)
    nDigits  <- floor(log10(ncol(dt)-1)) + 1

    for (i in 2:ncol(dt))
    {
      fileName[i] <- gsub(" ", "0", format(i-1, width = nDigits))
      write_tsv(as.data.frame(dt[,i]),
                file = paste0(tempDir, fileName[i]), quote = "none", escape = "none", col_names = F)
    }
    
    # write names.txt
    write_tsv(as.data.frame(colnames(dt)[2:ncol(dt)]),
              file = paste0(tempDir, "names.txt"), col_names = F)

    # write files.txt
    write_tsv(as.data.frame(fileName[2:ncol(dt)]),
              file = paste0(tempDir, "files.txt"), col_names = F)
    
    # write items.txt
    write_tsv(data.frame(dt[,1]),
              file = paste0(tempDir, "items.txt"), col_names = F)

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

    if (input$segmDist!="binary item comparison")
    {
      # move stress in front of a vowel, process length and diacritics

      if (is.element("length"    , input$selProc))
        length     <- 1
      else
        length     <- 0

      if (is.element("diacritics", input$selProc))
        diacritics <- 1
      else
        diacritics <- 0

      messages <- system2(
        command = "./phon",
        args    = c("names.txt files.txt", "items.txt", length, diacritics, tempDir),
        stdout  = TRUE,
        stderr  = TRUE
      )

      if (length(messages)>0)
      {
        showNotification(HTML(paste(messages, sep = "", collapse = "<br>")), type = "error", duration = NULL)
        return(NULL)
      }
    }

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

    removeNotification(global$idNot)
    global$idNot <- showNotification("Getting segment distances...", type = "message", duration = NULL)

    # generate or load segment distances

    getSegmDist1 <- function(featureTable)
    {
      # Create distance matrix

      distMat <- matrix(99999, nrow=nrow(featureTable), ncol=nrow(featureTable))

      # Calculate vowel distance and consonant distance

      vIndex <- which(colnames(featureTable)=="vowel"    )
      cIndex <- which(colnames(featureTable)=="consonant")

      sDist <- function(featureTable, i, j)
      {
        if (featureTable$phone[i]==featureTable$phone[j])
          return(0)
        else
          return(1)
      }

      # Assign distances to distance table

      for (i in 1:nrow(featureTable))
      {
        for (j in 1:nrow(featureTable))
        {
          if ((featureTable$vowel    [i]==1) & (featureTable$vowel    [j]==1))
            distMat[i,j] <- sDist(featureTable,i,j)
          else

          if ((featureTable$consonant[i]==1) & (featureTable$consonant[j]==1))
            distMat[i,j] <- sDist(featureTable,i,j)
          else

          if ((featureTable$semivowel[i]==1) & (featureTable$semivowel[j]==1) & is.element("i/j/u/w versus anything", input$extraAli))
            distMat[i,j] <- sDist(featureTable,i,j)
          else

          if ((featureTable$vowel    [i]==1) & (featureTable$semivowel[j]==1) & is.element("i/j/u/w versus anything", input$extraAli))
            distMat[i,j] <- sDist(featureTable,i,j)
          else

          if ((featureTable$semivowel[i]==1) & (featureTable$vowel    [j]==1) & is.element("i/j/u/w versus anything", input$extraAli))
            distMat[i,j] <- sDist(featureTable,i,j)
          else

          if ((featureTable$consonant[i]==1) & (featureTable$semivowel[j]==1) & is.element("i/j/u/w versus anything", input$extraAli))
            distMat[i,j] <- sDist(featureTable,i,j)
          else

          if ((featureTable$semivowel[i]==1) & (featureTable$consonant[j]==1) & is.element("i/j/u/w versus anything", input$extraAli))
            distMat[i,j] <- sDist(featureTable,i,j)
          else

          if ((featureTable$sonorant [i]==1) & (featureTable$sonorant [j]==1) & is.element("ə/ɐ versus sonorant"    , input$extraAli))
            distMat[i,j] <- sDist(featureTable,i,j)
          else {}
        }
      }

      # Set indel distances

      if  (input$segmDist=="plain  sub. = 1 indel = 0.5")
      {
        distMat[1, 2:ncol(distMat)] <- 0.5
        distMat[2:nrow(distMat), 1] <- 0.5
      }
      else

      if ((input$segmDist=="plain  cost = 1") | (input$segmDist=="PMI   cost ≤ 1"))
      {
        distMat[1, 2:ncol(distMat)] <- 1.0
        distMat[2:nrow(distMat), 1] <- 1.0
      }

      # Set row names and column names

      rownames(distMat) <- featureTable$phone
      colnames(distMat) <- featureTable$phone

      return(distMat)
    }

    getSegmDist2 <- function(featureTable)
    {
      # Create distance matrix

      distMat <- matrix(99999, nrow=nrow(featureTable), ncol=nrow(featureTable))

      # Calculate vowel distance and consonant distance

      vIndex <- which(colnames(featureTable)=="vowel"    )
      cIndex <- which(colnames(featureTable)=="consonant")
      sIndex <- which(colnames(featureTable)=="semivowel")

      vDist <- function(featureTable, i, j)
      {
        return(sum(abs(featureTable[i, (vIndex+1):(cIndex-1)] - featureTable[j, (vIndex+1):(cIndex-1)])))
      }

      cDist <- function(featureTable, i, j)
      {
        return(sum(abs(featureTable[i, (cIndex+1):(sIndex-1)] - featureTable[j, (cIndex+1):(sIndex-1)])))
      }

      # Find maximum vowel distance and maximum consonant distance

      viMax <- 0
      ciMax <- 0

      vsMax <- 0
      csMax <- 0

      for (i in 1:nrow(featureTable))
      {
        for (j in 1:nrow(featureTable))
        {
          if (((featureTable$phone    [i]=="0") & (featureTable$vowel    [j]== 1 )) |
              ((featureTable$vowel    [i]== 1 ) & (featureTable$phone    [j]=="0")))
          {
            distV <- vDist(featureTable,i,j)
            viMax <- max(viMax, distV)
          }
          else

          if (((featureTable$phone    [i]=="0") & (featureTable$consonant[j]== 1 )) |
              ((featureTable$consonant[i]== 1 ) & (featureTable$phone    [j]=="0")))
          {
            distC <- cDist(featureTable,i,j)
            ciMax <- max(ciMax, distC)
          }
          else

          if  ((featureTable$vowel    [i]== 1 ) & (featureTable$vowel    [j]== 1 ))
          {
            distV <- vDist(featureTable,i,j)
            vsMax <- max(vsMax, distV)
          }
          else

          if  ((featureTable$consonant[i]== 1 ) & (featureTable$consonant[j]== 1 ))
          {
            distC <- cDist(featureTable,i,j)
            csMax <- max(csMax, distC)
          }
          else

          if  ((featureTable$semivowel[i]== 1 ) & (featureTable$semivowel[j]== 1 ) & is.element("i/j/u/w versus anything", input$extraAli))
          # i vs. j, i vs. w, u vs. j, u vs. w
          {
           distV <- vDist(featureTable,i,j)
           vsMax <- max(vsMax, distV)

           distC <- cDist(featureTable,i,j)
           csMax <- max(csMax, distC)
          }
          else

          if  ((featureTable$vowel    [i]== 1 ) & (featureTable$semivowel[j]== 1 ) & is.element("i/j/u/w versus anything", input$extraAli))
          {
            distV <- vDist(featureTable,i,j)
            vsMax <- max(vsMax, distV)
          }
          else

          if  ((featureTable$semivowel[i]== 1 ) & (featureTable$vowel    [j]== 1 ) & is.element("i/j/u/w versus anything", input$extraAli))
          {
            distV <- vDist(featureTable,i,j)
            vsMax <- max(vsMax, distV)
          }
          else

          if  ((featureTable$consonant[i]== 1 ) & (featureTable$semivowel[j]== 1 ) & is.element("i/j/u/w versus anything", input$extraAli))
          {
            distC <- cDist(featureTable,i,j)
            csMax <- max(csMax, distC)
          }
          else

          if  ((featureTable$semivowel[i]== 1 ) & (featureTable$consonant[j]== 1 ) & is.element("i/j/u/w versus anything", input$extraAli))
          {
            distC <- cDist(featureTable,i,j)
            csMax <- max(csMax, distC)
          }
          else {}
        }
      }

      # Assign normalized distances to distance table

      if (input$segmDist=="A&B  sub. ≤ 1 indel ≤ 0.5")
        indelFactor <- 0.5

      if (input$segmDist=="A&B  cost ≤ 1")
      {
        indelFactor <- 1

        viMax <- max(viMax, vsMax)
        vsMax <- viMax

        ciMax <- max(ciMax, csMax)
        csMax <- ciMax
      }

      for (i in 1:nrow(featureTable))
      {
        for (j in 1:nrow(featureTable))
        {
          if (((featureTable$phone    [i]=="0") & (featureTable$vowel    [j]== 1 )) |
              ((featureTable$vowel    [i]== 1 ) & (featureTable$phone    [j]=="0")))
          {
            distV <- vDist(featureTable,i,j)
            distMat[i,j] <- (distV / viMax) * indelFactor
          }
          else

          if (((featureTable$phone    [i]=="0") & (featureTable$consonant[j]== 1 )) |
              ((featureTable$consonant[i]== 1 ) & (featureTable$phone    [j]=="0")))
          {
            distC <- cDist(featureTable,i,j)
            distMat[i,j] <- (distC / ciMax) * indelFactor
          }
          else

          if  ((featureTable$vowel    [i]== 1 ) & (featureTable$vowel    [j]== 1 ))
          {
            distV <- vDist(featureTable,i,j)
            distMat[i,j] <- distV / vsMax
          }
          else

          if  ((featureTable$consonant[i]== 1 ) & (featureTable$consonant[j]== 1 ))
          {
            distC <- cDist(featureTable,i,j)
            distMat[i,j] <- distC / csMax
          }
          else

          if  ((featureTable$semivowel[i]== 1 ) & (featureTable$semivowel[j]== 1 ) & is.element("i/j/u/w versus anything", input$extraAli))
          # i vs. j, i vs. w, u vs. j, u vs. w
          {
            distV <- vDist(featureTable,i,j) / vsMax
            distC <- cDist(featureTable,i,j) / csMax

            distMat[i,j] <- (distV + distC)/2
          }
          else

          if  ((featureTable$vowel    [i]== 1 ) & (featureTable$semivowel[j]== 1 ) & is.element("i/j/u/w versus anything", input$extraAli))
          {
            distV <- vDist(featureTable,i,j)
            distMat[i,j] <- distV / vsMax
          }
          else

          if  ((featureTable$semivowel[i]== 1 ) & (featureTable$vowel    [j]== 1 ) & is.element("i/j/u/w versus anything", input$extraAli))
          {
            distV <- vDist(featureTable,i,j)
            distMat[i,j] <- distV / vsMax
          }
          else

          if  ((featureTable$consonant[i]== 1 ) & (featureTable$semivowel[j]== 1 ) & is.element("i/j/u/w versus anything", input$extraAli))
          {
            distC <- cDist(featureTable,i,j)
            distMat[i,j] <- distC / csMax
          }
          else

          if  ((featureTable$semivowel[i]== 1 ) & (featureTable$consonant[j]== 1 ) & is.element("i/j/u/w versus anything", input$extraAli))
          {
            distC <- cDist(featureTable,i,j)
            distMat[i,j] <- distC / csMax
          }
          else {}
        }
      }

      # Set row names and column names

      rownames(distMat) <- featureTable$phone
      colnames(distMat) <- featureTable$phone

      return(distMat)
    }

    # Set weights for stress

    setWeightsStress <- function(distMat)
    {
      maxIndel <- max(distMat[1, 2:nrow(featureTable)])

      if  ( is.element("primary stress", input$selProc) & !is.element("secondary stress", input$selProc))
      {
        distMat <- rbind(distMat, rep(99999, ncol(distMat)))
        distMat <- cbind(distMat, rep(99999, nrow(distMat)))

        distMat[nrow(distMat)  , ncol(distMat)  ] <- 0
        distMat[nrow(distMat)  , 1              ] <- maxIndel
        distMat[1              , ncol(distMat)  ] <- maxIndel

        rownames(distMat)[ncol(distMat)] <- "ˈ"
        colnames(distMat)[nrow(distMat)] <- "ˈ"
      }

      if  (!is.element("primary stress", input$selProc) &  is.element("secondary stress", input$selProc))
      {
        distMat <- rbind(distMat, rep(99999, ncol(distMat)))
        distMat <- cbind(distMat, rep(99999, nrow(distMat)))

        distMat[nrow(distMat)  , ncol(distMat)  ] <- 0
        distMat[nrow(distMat)  , 1              ] <- maxIndel * 0.5
        distMat[1              , ncol(distMat)  ] <- maxIndel * 0.5

        rownames(distMat)[ncol(distMat)] <- "ˌ"
        colnames(distMat)[nrow(distMat)] <- "ˌ"
      }

      if ( is.element("primary stress", input$selProc) &  is.element("secondary stress", input$selProc))
      {
        distMat <- rbind(distMat, rep(99999, ncol(distMat)))
        distMat <- cbind(distMat, rep(99999, nrow(distMat)))

        distMat[nrow(distMat)  , ncol(distMat)  ] <- 0
        distMat[nrow(distMat)  , 1              ] <- maxIndel
        distMat[1              , ncol(distMat)  ] <- maxIndel

        rownames(distMat)[ncol(distMat)] <- "ˈ"
        colnames(distMat)[nrow(distMat)] <- "ˈ"

        distMat <- rbind(distMat, rep(99999, ncol(distMat)))
        distMat <- cbind(distMat, rep(99999, nrow(distMat)))

        distMat[nrow(distMat)  , ncol(distMat)  ] <- 0
        distMat[nrow(distMat)  , 1              ] <- maxIndel * 0.5
        distMat[1              , ncol(distMat)  ] <- maxIndel * 0.5

        rownames(distMat)[ncol(distMat)] <- "ˌ"
        colnames(distMat)[nrow(distMat)] <- "ˌ"

        distMat[nrow(distMat)-1, ncol(distMat)  ] <- maxIndel * 0.5
        distMat[nrow(distMat)  , ncol(distMat)-1] <- maxIndel * 0.5
      }

      return(distMat)
    }

    # Upload segment distances

    uploadSeg <- function(inFile)
    {
      if (is.null(inFile))
        return(NULL)

      dt <- check_decimal(read.xlsx(inFile$datapath, rowNames = T))
      dt[is.na(dt)] <- 99999

      return(dt)
    }

    # Save distances

    saveDist <- function(distMat, fileName)
    {
      Content <- as.character(nrow(distMat))

      for (i in 1:nrow(distMat))
      {
        if (rownames(distMat)[i]=="ⱱ")
          phone <- "b\\"
        else

        if (rownames(distMat)[i]=="ˈ")
          phone <- '"'
        else
        {
          phone <- ipa(rownames(distMat)[i], to="xsampa")
          phone <- gsub("b_<"    , "b<"    , phone)
          phone <- gsub("d_<"    , "d<"    , phone)
          phone <- gsub("J\\\\_<", "J\\\\<", phone)
          phone <- gsub("g_<"    , "g<"    , phone)
          phone <- gsub("G\\\\_<", "G\\\\<", phone)
        }

        Content <- c(Content, phone)
      }

      for (i in 2:ncol(distMat))
      {
        for (j in 1:(i-1))
        {
          Content <- c(Content, as.character(distMat[i,j]))
        }
      }

      write_tsv(data.frame(Content), file = fileName, col_names = F, quote = "none", escape = "none")
    }

    if ((input$segmDist=="plain  sub. = 1 indel = 0.5") | (input$segmDist=="plain  cost = 1") | (input$segmDist=="PMI   cost ≤ 1"))
    {
      featureTable <- read.xlsx("feattabs/featureTableIPA1.xlsx")
      featureTable <- subset(featureTable, is.element(featureTable$phone, c("0", global$IPAset)))
      saveDist(setWeightsStress(getSegmDist1(featureTable)), paste0(tempDir, "sounddists.txt"))
    }

    if ((input$segmDist=="A&B  sub. ≤ 1 indel ≤ 0.5") | (input$segmDist=="A&B  cost ≤ 1"))
    {
      featureTable <- read.xlsx("feattabs/featureTableIPA2.xlsx")
      featureTable <- subset(featureTable, is.element(featureTable$phone, c("0", global$IPAset)))
      saveDist(setWeightsStress(getSegmDist2(featureTable)), paste0(tempDir, "sounddists.txt"))
    }

    if  (input$segmDist=="upload my own distances")
    {
      segmDist <- uploadSeg(global$segmTab)
      saveDist(segmDist, paste0(tempDir, "sounddists.txt"))
    }

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

    removeNotification(global$idNot)
    global$idNot <- showNotification(HTML("Calculating distances. This can take up to several hours..."), type = "message", duration = NULL)

    # Calculate binary item distances

    Levenshtein1 <- function()
    {
      saveIn  <- "1"
      saveAg  <- "1"

      System2 <- function(saveIn, saveAg, tempDir)
      {
        system2(
          command = "./leven1",
          args    = c("names.txt", "files.txt", "items.txt", saveIn, saveAg, tempDir),
          stdout  = TRUE,
          stderr  = TRUE
        )
      }

      global$background <- r_bg(
        func = System2,
        args = list(saveIn, saveAg, tempDir)
      )
    }

    # Calculate Levenshtein distances

    Levenshtein2 <- function()
    {
      if (input$segmDist=="PMI   cost ≤ 1")
        levMeth <- "2"
      else
        levMeth <- "1"

      if (input$selNorm=="divide by alignment length")
        normAli <- "1"
      else
        normAli <- "0"

      if (input$selMeasurements=="only consonant indels")
        selPart <- "4"
      else
      if (input$selMeasurements=="only consonant-consonant pairs")
        selPart <- "3"
      else
      if (input$selMeasurements=="only vowel indels")
        selPart <- "2"
      else
      if (input$selMeasurements=="only vowel-vowel pairs")
        selPart <- "1"
      else
        selPart <- "0"

      if (is.element("segment distances", input$selOutput))
        saveSO  <- "1"
      else
        saveSO  <- "1"

      if (is.element("alignments"       , input$selOutput))
        saveAl  <- "1"
      else
        saveAl  <- "0"

        saveIn  <- "1"
        saveAg  <- "1"

      System2 <- function(levMeth, normAli, selPart, saveSO, saveAl, saveIn, saveAg, tempDir)
      {
        system2(
          command = "./leven2",
          args    = c("names.txt", "files.txt", "items.txt", "sounddists.txt", levMeth, normAli, selPart, saveSO, saveAl, saveIn, saveAg, tempDir),
          stdout  = TRUE,
          stderr  = TRUE
        )
      }

      global$background <- r_bg(
        func = System2,
        args = list(levMeth, normAli, selPart, saveSO, saveAl, saveIn, saveAg, tempDir)
      )
    }

    if (input$segmDist=="binary item comparison")
      Levenshtein1()
    else
      Levenshtein2()
  })

  observe(
  {
    req(global$background)
    status <- global$background$poll_io(0)["process"]

    if  (status == "timeout")
      invalidateLater(1000)

    if ((status == "ready") & !global$finished)
    {
      messages <- global$background$get_result()

      if (length(messages)>0)
      {
        showNotification(HTML(paste(messages, sep = "", collapse = "<br>")), type = "message", duration = NULL)
      }

      # Calculate Cronbach's α

      messages <- system2(
        command = "./cron",
        args    = c("files.txt", "items.txt", "individual.tsv", tempDir),
        stdout  = TRUE,
        stderr  = TRUE
      )

      removeNotification(global$idNot)
      
      if (length(messages)>0)
      {
        showNotification(HTML(paste0(messages, ".", sep = "", collapse = "<br>")), type = "message", duration = NULL)
      }

      # Finish

      insertUI(
        selector = "#goButton",
        where    = "afterEnd",
        ui       = tags$audio(src = "ready.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
      )

      showNotification("Ready!", type = "message", duration = NULL)
      global$finished   <- TRUE
      global$background <- NULL
    }
  })

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

  # Read segment distances

  segmMat <- eventReactive(global$finished,
  {
    if (file.exists(paste0(tempDir, "sounddists.csv")) && (file.size(paste0(tempDir, "sounddists.csv"))!=0))
    {
      df <- read.csv(paste0(tempDir, "sounddists.csv"), quote="")

      segmList <- df[2:nrow(df),1]

      segmList <- lapply(segmList, function(x) gsub("b<"    , "b_<"    , x))
      segmList <- lapply(segmList, function(x) gsub("d<"    , "d_<"    , x))
      segmList <- lapply(segmList, function(x) gsub("J\\\\<", "J\\\\_<", x))
      segmList <- lapply(segmList, function(x) gsub("g<"    , "g_<"    , x))
      segmList <- lapply(segmList, function(x) gsub("G\\\\<", "G\\\\_<", x))

      segmList <- xsampa(segmList, to = "ipa")
      segmList <- gsub("b\\\\", "ⱱ", segmList)

      df[,1] <- NULL

      rownames(df) <- c(0, segmList)
      colnames(df) <- c(0, segmList)

      df <- as.matrix(df)

      return(df)
    }
    else
      return(NULL)
  })

  segmMatV <- eventReactive(segmMat(),
  {
    vowels     <- c("0", "i", "y", "ɨ", "ʉ", "ɯ", "u", "ɪ", "ʏ", "ʊ", "e", "ø", "ɘ", "ɵ", "ɤ", "o", "ə", "ɛ", "œ", "ɜ", "ɞ", "ʌ", "ɔ", "æ", "ɐ", "a", "ɶ", "ɑ", "ɒ")
    segmList   <- row.names(segmMat())
    index      <- which(segmList %in% vowels)

    return(segmMat()[index, index])
  })

  segmMatC <- eventReactive(segmMat(),
  {
    consonants <- c("0", "ɓ", "ɗ", "ʄ", "ɠ", "ʛ", "p", "b", "t", "d", "ʈ", "ɖ", "c", "ɟ", "k", "ɡ", "q", "ɢ", "ʔ", "m", "ɱ", "n", "ɳ", "ɲ", "ŋ", "ɴ", "ʙ", "r", "ʀ", "ⱱ", "ɾ", "ɽ", "ɸ", "β", "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "ʂ", "ʐ", "ç", "ʝ", "x", "ɣ", "χ", "ʁ", "ħ", "ʕ", "h", "ɦ", "ɬ", "ɮ", "w", "ʋ", "ɹ", "ɻ", "j", "ɰ", "l", "ɭ", "ʎ", "ʟ")
    segmList   <- row.names(segmMat())
    index      <- which(segmList %in% consonants)

    return(segmMat()[index, index])
  })

  aggrMat <- eventReactive(global$finished,
  {
    if  (global$dataType=="transcriptions")
    {
      if (file.exists(paste0(tempDir, "aggregated.csv")) && (file.size(paste0(tempDir, "aggregated.csv"))!=0))
      {
        files <- read.delim(paste0(tempDir, "names.txt"), header=FALSE, quote="")$V1
        
        df <- data.matrix(read.table(paste0(tempDir, "aggregated.csv"),
                                     header=FALSE,
                                     row.names=as.character(files),
                                     col.names=as.character(files),
                                     check.names = FALSE,
                                     sep=",",
                                     fill = TRUE,
                                     comment.char = ""))
        
        df[upper.tri(df)] <- t(df)[upper.tri(df)]
        df <- as.matrix(df)
        global$maxOverlaps <- nrow(df)
        
        return(df)
      }
    }
    else

    if ((global$dataType=="acousticdata") | (global$dataType=="postaggeddata"))
    {
      if (file.exists(paste0(tempDir, "aggregated.csv")) && (file.size(paste0(tempDir, "aggregated.csv"))!=0))
      {
        files <- read.delim(paste0(tempDir, "files.txt"), header=FALSE, quote="")$V1
        
        df <- data.matrix(read.table(paste0(tempDir, "aggregated.csv"),
                                     header=FALSE,
                                     row.names=as.character(files),
                                     col.names=as.character(files),
                                     check.names = FALSE,
                                     sep=",",
                                     fill = TRUE,
                                     comment.char = ""))
        
        df <- as.matrix(df)
        global$maxOverlaps <- nrow(df)
        
        return(df)
      }
    }
    else

    if  (global$dataType=="distancetable")  
    {
      req(input$dataTab8, global$distTab)
      
      df <- global$distTab

      for (i in 1:ncol(df)) 
      {
        df[,i] <- as.numeric(as.character(df[,i]))
      }

      for (i in 2:nrow(df))
      {
        for (j in 1:(i-1))
        {
          if (!is.na(df[i,j]) &  is.na(df[j,j]))
            d <- df[i,j]
          else
            
          if ( is.na(df[i,j]) & !is.na(df[j,j]))
            d <- df[j,i]
          else
            
          if (!is.na(df[i,j]) & !is.na(df[j,j]))
            d <- (df[i,j] + df[j,i])/2
          else
            d <- NA
            
          df[i,j] <- d
          df[j,i] <- d
        }
      }

      df <- as.matrix(df)
      global$maxOverlaps <- nrow(df)

      return(df)
    }
    else  
      return(NULL)
  })

  # Read genders
  
  genTab <- eventReactive(global$genTab1,
  {
    if (global$dataType=="acousticdata")
    {
      req(global$genTab1)
      inFile <- global$genTab1
    }
    else
      return(NULL)
                            
    dt <- as.data.frame(read.xlsx(inFile$datapath, rowNames = F, sep.names = " "))
    dt[is.na(dt)] <- ""
    dt <- dt[order(tolower(dt[,1])),]
    dt[,2] <- tolower(dt[,2])

    return(dt)
  })
  
  # Read coordinates

  observeEvent(input$navBar,
  {
    if ((!is.null(global$geoTab )) && (global$dataType=="transcriptions"))
    {
      global$geoTab  <- NULL
      global$geoTab  <- input$geoTab
    }
    else
                   
    if ((!is.null(global$geoTab1)) && (global$dataType=="acousticdata"))
    {
      global$geoTab1 <- NULL
      global$geoTab1 <- input$geoTab1
    }
    else
             
    if ((!is.null(global$geoTab2)) && (global$dataType=="postaggeddata"))
    {
      global$geoTab2 <- NULL
      global$geoTab2 <- input$geoTab2
    }
    else

    if ((!is.null(global$geoTab8)) && (global$dataType=="distancetable"))  
    {
      global$geoTab8 <- NULL
      global$geoTab8 <- input$geoTab8
    }
    else {}
  })

  geoTab <- eventReactive(c(global$geoTab, global$geoTab1, global$geoTab2, global$geoTab8),
  {
    if (global$dataType=="transcriptions")
    {
      req(global$geoTab)
      inFile <- global$geoTab
    }
    else
      
    if (global$dataType=="acousticdata")
    {
      req(global$geoTab1)
      inFile <- global$geoTab1
    }
    else

    if (global$dataType=="postaggeddata")
    {
      req(global$geoTab2)
      inFile <- global$geoTab2
    }
    else

    if  (global$dataType=="distancetable")  
    {
      req(global$geoTab8)
      inFile <- global$geoTab8
    }
    else
      return(NULL)

    dt <- check_decimal(as.data.frame(read.xlsx(inFile$datapath, rowNames = F, sep.names = " ")))
    dt[is.na(dt)] <- ""
    dt <- dt[order(tolower(dt[,1])),]
    
    return(dt)
  })

  # Make downloads available

  output$download01  <- downloadHandler(filename = "aggregated.xlsx", content = function(file)
    write.xlsx(as.data.frame(aggrMat()), file = file, rowNames = T, colNames = T, sheetName = "aggregated distances", headerStyle = createStyle(textDecoration = "BOLD"))
  )

  output$download02  <- downloadHandler(filename = "individual.tsv", content = function(file)
  {
    file.copy(paste0(tempDir, "individual.tsv"), file, overwrite = T)
  },
  contentType = "text/csv")
  
  output$download03  <- downloadHandler(filename = "sounddists.xlsx", content = function(file)
    write.xlsx(as.data.frame(segmMat ()), file = file, rowNames = T, colNames = T, sheetName = "sound distances", headerStyle = createStyle(textDecoration = "BOLD"))
  )

  output$download03a <- downloadHandler(filename = "voweldists.xlsx", content = function(file)
    write.xlsx(as.data.frame(segmMatV()), file = file, rowNames = T, colNames = T, sheetName = "vowel distances", headerStyle = createStyle(textDecoration = "BOLD"))
  )

  output$download03b <- downloadHandler(filename = "consonantdists.xlsx", content = function(file)
    write.xlsx(as.data.frame(segmMatC()), file = file, rowNames = T, colNames = T, sheetName = "consonant distances", headerStyle = createStyle(textDecoration = "BOLD"))
  )

  output$download04  <- downloadHandler(filename = "alignments.txt", content = function(file)
  {
    file.copy(paste0(tempDir, "alignments.txt"), file, overwrite = T)
  },
  contentType = "text/plain")

  output$showResults <- renderUI(
  {
    req(global$finished, input$selOutput)

    tl <- fluidPage(
      style = "margin-left: 106px",
      align = "left",

      br(), br(),

      bsButton("butDownloads", label = NULL, icon = icon("info"), size = "extra-small"),
      bsModal ("modDownloads", "Output", "butDownloads", size = "large", "Dit is de helptekst"),

      HTML("<span style='font-weight: bold;'>&nbsp;Downloads:</span>"),
      p(style='height: 8px')
    )

    if (is.element("aggregated distances", input$selOutput) & file.exists(paste0(tempDir, "aggregated.csv")) && (file.size(paste0(tempDir, "aggregated.csv"))!=0))
      tl <- tagList(tl, downloadButton('download01' , class = 'dlButton', 'aggregated distances'),
                    br(), br())
    
    if (is.element("word pair distances" , input$selOutput) & file.exists(paste0(tempDir, "individual.tsv")))
      tl <- tagList(tl, downloadButton('download02' , class = 'dlButton', 'word pair distances') ,
                    br(), br())

    if (is.element("segment distances"   , input$selOutput) && file.exists(paste0(tempDir, "sounddists.csv")) && (file.size(paste0(tempDir, "sounddists.csv"))!=0))
    {
      tl <- tagList(tl, downloadButton('download03' , class = 'dlButton', 'segment distances') ,
                    br(), br())
      tl <- tagList(tl, downloadButton('download03a', class = 'dlButton', 'segment distances (vowels)') ,
                    br(), br())
      tl <- tagList(tl, downloadButton('download03b', class = 'dlButton', 'segment distances (consonants)') ,
                    br(), br())
    }

    if (is.element("alignments"          , input$selOutput) & file.exists(paste0(tempDir, "alignments.txt")) && (file.size(paste0(tempDir, "alignments.txt"))!=0))
      tl <- tagList(tl, downloadButton('download04' , class = 'dlButton', 'alignments')          ,
                    br(), br())

    return(tl)
  })

  ##############################################################################

  output$genTab1 <- renderUI(
  {
    input$dataTab1
    global$genTab1 <- NULL
      
    fluidPage(
      align = "center",
      fileInput(
        inputId    = "genTab1",
        label      = NULL,
        multiple   = FALSE,
        accept     = ".xlsx",
        width      = "250px")
    )
  })  
  
  observeEvent(input$genTab1,
  {
    global$genTab1 <- input$genTab1
  })
  
  output$geoTab1 <- renderUI(
  {
    input$dataTab1
    global$geoTab1 <- NULL
      
    fluidPage(
      align = "center",
      fileInput(
        inputId    = "geoTab1",
        label      = NULL,
        multiple   = FALSE,
        accept     = ".xlsx",
        width      = "250px")
    )
  })
  
  observeEvent(input$geoTab1,
  {
    global$geoTab1 <- input$geoTab1
  })

  output$selOutput1 <- renderUI(
  {
    req(input$segmDist)
      
    choices <- c("aggregated distances", "word pair distances")
      
    fluidPage(
      style = "margin-left: 184px",
        
      checkboxGroupInput(
        inputId    = 'selOutput1',
        label      = NULL,
        choices    = choices,
        selected   = "",
        inline     = FALSE
      )
    )
  })  

  standardize <- function(m)
  {
    for (i in 1:ncol(m))
    {
      m[,i] <- (m[,i]-mean(m[,i]))/sd(m[,i])
    }
    
    return(m)
  }
  
  snd2MFCC <- function(s0, s)
  {
    w <- readWave(s0)
    m <- na.omit(melfcc(w, numcep =  9, minfreq = 50, maxfreq = 5000, nbands = 32))
    m <- standardize(m)
    write.table(m, file = paste0(tempDir, s), quote = F, row.names = F, col.names = T, sep=",")
  }
  
  filePrep <- function(varieties, items, genNum)
  {
    for (i in 1:length(varieties))
    {
      for (k in 1:length(items))
      {
        if (input$dataForm1=="1")
          s <- paste0(varieties[i], "_", items[k], ".wav")
        
        if (input$dataForm1=="2")
          s <- paste0(varieties[i], ".", items[k])
        
        index <- which(input$dataTab1$name==s)
        
        if (length(index)==1)
        {
          s0 <- input$dataTab1$datapath[index]
          
          if ((!is.null(global$genTab1)) && (genNum==2) && (genTab()[i,2]=="m"))
          {
            if (input$trim)
            {
              system(paste('./praat --run gender.praat', s0, tempDir, "yes", "yes"))
              s0 <- paste0(tempDir, "tmp.wav")
            }
            else
            {
              system(paste('./praat --run gender.praat', s0, tempDir, "no" , "yes"))
              s0 <- paste0(tempDir, "tmp.wav")
            }
          }
          else
          {
            if (input$trim)
            {
              system(paste('./praat --run gender.praat', s0, tempDir, "yes", "no" ))
              s0 <- paste0(tempDir, "tmp.wav")
            }
            else {}
          }
          
          snd2MFCC(s0, sub("\\.wav$", "\\.csv", s))
        }
      }
      
      if (!is.null(global$idNot)) removeNotification(global$idNot)
      global$idNot <- showNotification(HTML(paste0("Preprocessing, ", round((i/length(varieties))*100), "% done.")), type = "message", duration = NULL)
    }
  }
  
  cos.sim <- function(A,B)
  { 
    return((sum(A*B))/sqrt((sum(A^2))*(sum(B^2))))
  }

  dtw1 <- function(x,y)
  {
    # Calculate distances between vectors of x and vectors of y
    distance_matrix <- proxy::dist(x=x, y=y, method = function(x, y){1 - cor(x, y)})
    
    # Perform dynamic time warping
    dtw_result <- dtw::dtw(x=distance_matrix, keep.internals=TRUE, step.pattern = symmetric1)
    
    # Get normalized distance
    return(dtw_result$distance/length(dtw_result$index1))
  }
  
  dtw2 <- function(x,y)
  {
    nx <- nrow(x) + 1
    ny <- nrow(y) + 1
    
    m <- matrix(Inf, nrow=nx, ncol=ny)
    p <- matrix("" , nrow=nx, ncol=ny)
    m[1,1] <- 0

    for (i in 2:nx)
    {
      for (j in 2:ny)
      {
        if (input$selDTW=="DTW using Cosine θ"   )
          cost <- 1 - cos.sim(x[i-1,], y[j-1,])
        
        if (input$selDTW=="DTW using Pearson's r")
          cost <- 1 - cor    (x[i-1,], y[j-1,])

        mincell <- min( m[i-1, j  ], # insertion
                        m[i-1, j-1], # match
                        m[i  , j-1]) # deletion
        
        m[i,j] <- cost + mincell
        
        if (mincell==m[i-1, j  ]) p[i,j] <- "I"
        if (mincell==m[i-1, j-1]) p[i,j] <- "M"
        if (mincell==m[i  , j-1]) p[i,j] <- "D"
      }
    }
    
    i <- nx
    j <- ny
    l <- 0
    
    repeat
    {
      if (p[i,j]=="I") { i <- i - 1              } else
      if (p[i,j]=="M") { i <- i - 1;  j <- j - 1 } else
      if (p[i,j]=="D") {              j <- j - 1 } else {}
      
      l <- l + 1
      if ((i==1) & (j==1)) break
    }
    
    return(m[nx,ny] / l)
  }
  
  writeLine <- function(indFile, s)
  {
    indConn <- file(indFile, "a")
    writeLines(s, indConn) 
    close(indConn)
  }
  
  calcDist1 <- function(varieties, items, gends)
  {
    indFile <- paste0(tempDir, "individual.tsv")
    writeLines("var1\tvar2\titem\tdist", con = indFile)
    
    dis <- matrix(0, nrow=length(varieties), ncol=length(varieties))
    
    ij <- 0
    nn <- (length(varieties) * (length(varieties)-1))/2
    
    for (i in 2:length(varieties))
    {
      for (j in 1:(i-1))
      {
        ij <- ij + 1
        
        sum <- 0
        num <- 0
        
        for (k in 1:length(items))
        {
          if (input$dataForm1=="1")
          {
            s1 <- paste0(tempDir, varieties[i], "_", items[k], ".csv")
            s2 <- paste0(tempDir, varieties[j], "_", items[k], ".csv")
          }
            
          if (input$dataForm1=="2")
          {
            s1 <- paste0(tempDir, varieties[i], ".", items[k])
            s2 <- paste0(tempDir, varieties[j], ".", items[k])
          }

          if (file.exists(s1) & file.exists(s2))
          {
            m1 <- as.matrix(read.csv(s1 , quote=""))
            m2 <- as.matrix(read.csv(s2 , quote=""))
            
            d   <- dtw1(x=m1, y=m2)
            sum <- sum + d
            num <- num + 1
            
            writeLine(indFile, paste0(varieties[i], "\t", varieties[j], "\t", items[k], "\t", as.character(d)))
          }
          else
            writeLine(indFile, paste0(varieties[i], "\t", varieties[j], "\t", items[k], "\t")) 
        }
        
        dis[i,j] <- sum/num
        dis[j,i] <- sum/num
        
        removeNotification(global$idNot)
        global$idNot <- showNotification(HTML(paste0("Calculating, ", round((ij/nn)*100), "% done.")), type = "message", duration = NULL)
      }
    }
    
    write.table(dis, file = paste0(tempDir, "aggregated.csv"), quote = F, row.names = F, col.names = F, sep=",")
  }

  observeEvent(input$goButton1,
  {
    global$dataType  <- input$navBar
    global$finished  <- FALSE
    global$partition <- NULL

    system(paste0("rm -Rf ", tempDir, "*"))
    
    # check inputs
    
    if  (length(input$dataTab1)==0)
    {
      showNotification("Please upload sound files!", type = "error")
      return(NULL)
    }
    
    if  (length(input$dataForm1)==0)
    {
      showNotification("Please indicate the format of the sound files!", type = "error")
      return(NULL)
    }
    
    # get list of varieties and list of items
    
    if (input$dataForm1=="1")
    {
      res <- unlist(strsplit(input$dataTab1$name, '_', fixed = T))
        
      varieties <- unique(res[c(TRUE, FALSE)])
      items     <- unique(res[c(FALSE, TRUE)])
      items     <- gsub(".wav", "", items)
    }
      
    if (input$dataForm1=="2")
    {
      res <- unlist(strsplit(input$dataTab1$name, '.', fixed = T))
        
      varieties <- unique(res[c(TRUE, FALSE)])
      items     <- unique(res[c(FALSE, TRUE)])
    }
    
    varieties <- sort(varieties)

    write.table(varieties, file = paste0(tempDir, "files.txt"), quote = F, row.names = F, col.names = F, sep=",")
    write.table(items    , file = paste0(tempDir, "items.txt"), quote = F, row.names = F, col.names = F, sep=",")

    # check gender file
    
    genNum <- 1
    
    if (!is.null(global$genTab1))
    {
      varieties1 <- tolower(varieties)
      varieties2 <- tolower(genTab()[,1])
      
      if ((!setequal(varieties1,varieties2)) | (length(varieties1)!=length(varieties2)))
      {
        showNotification("Variety labels of sound files do not match with variety labels in gender file!", type = "error", duration = NULL)
        return(NULL)
      }
      
      genNum <- length(unique(genTab()[,2]))
    }

    # check coordinates file
    
    if (!is.null(global$geoTab1))
    {
      varieties1 <- tolower(varieties)
      varieties2 <- tolower(geoTab()[,1])
      
      if ((!setequal(varieties1,varieties2)) | (length(varieties1)!=length(varieties2)))
      {
        showNotification("Varieties in distances file do not match with varieties in coordinates file!", type = "error", duration = NULL)
        return(NULL)
      }
    }

    # convert sound files to MFCC representations
    
    filePrep(varieties, items, genNum)

    # calculate distances
    
    calcDist1(varieties, items)
    
    # Calculate Cronbach's α
    
    messages <- system2(
      command = "./cron",
      args    = c("files.txt", "items.txt", "individual.tsv", tempDir),
      stdout  = TRUE,
      stderr  = TRUE
    )
    
    removeNotification(global$idNot)
    
    if (length(messages)>0)
    {
      showNotification(HTML(paste(messages, sep = "", collapse = "<br>")), type = "message", duration = NULL)
    }

    # Finish
    
    insertUI(
      selector = "#goButton",
      where    = "afterEnd",
      ui       = tags$audio(src = "ready.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
    )
    
    showNotification("Ready!", type = "message", duration = NULL)
    global$finished <- TRUE
  })
  
  # Make downloads available

  output$download11 <- downloadHandler(filename = "aggregated.xlsx", content = function(file)
    write.xlsx(as.data.frame(aggrMat()), file = file, rowNames = T, colNames = T, sheetName = "aggregated distances", headerStyle = createStyle(textDecoration = "BOLD"))
  )

  output$download12 <- downloadHandler(filename = "individual.tsv", content = function(file)
  {
    file.copy(paste0(tempDir, "individual.tsv"), file, overwrite = T)
  },
  contentType = "text/csv")

  output$showResults1 <- renderUI(
  {
    req(global$finished, input$selOutput1)
      
    tl <- fluidPage(
      style = "margin-left: 106px",
      align = "left",
        
      br(), br(),
        
      bsButton("butDownloads1", label = NULL, icon = icon("info"), size = "extra-small"),
      bsModal ("modDownloads1", "Output", "butDownloads1", size = "large", "Dit is de helptekst"),
      
      HTML("<span style='font-weight: bold;'>&nbsp;Downloads:</span>"),
      p(style='height: 8px')
    )
  
    if (is.element("aggregated distances", input$selOutput1) & file.exists(paste0(tempDir, "aggregated.csv")) && (file.size(paste0(tempDir, "aggregated.csv"))!=0))
      tl <- tagList(tl, downloadButton('download11' , class = 'dlButton', 'aggregated distances'),
                    br(), br())
    
    if (is.element("word pair distances" , input$selOutput1) & file.exists(paste0(tempDir, "individual.tsv")))
      tl <- tagList(tl, downloadButton('download12' , class = 'dlButton', 'word pair distances') ,
                    br(), br())

    return(tl)
  })  
  
  ##############################################################################
  
  output$geoTab2 <- renderUI(
  {
    input$dataTab2
    global$geoTab2 <- NULL
    
    fluidPage(
      align = "center",
      fileInput(
        inputId    = "geoTab2",
        label      = NULL,
        multiple   = FALSE,
        accept     = ".xlsx",
        width      = "250px")
    )
  })
  
  observeEvent(input$geoTab2,
  {
    global$geoTab2 <- input$geoTab2
  })
  
  output$selOutput2 <- renderUI(
  {
    req(input$segmDist)
    
    choices <- c("aggregated distances", "n-gram frequencies")
    
    fluidPage(
      style = "margin-left: 184px",
      
      checkboxGroupInput(
        inputId    = 'selOutput2',
        label      = NULL,
        choices    = choices,
        selected   = "",
        inline     = FALSE
      )
    )
  })  
  
  # Read file with upos tags
  
  read.upos <- function(variety)
  {
    if (input$dataForm2==1)
    {
      variety <- paste0(variety, ".xlsx")
      index <- which(input$dataTab2$name==variety)
      df <- read.xlsx         (input$dataTab2$datapath[index])
    }
    
    if (input$dataForm2==2)
    {
      variety <- paste0(variety, ".conllu")
      index <- which(input$dataTab2$name==variety)
      df <- udpipe_read_conllu(input$dataTab2$datapath[index])
      
      df <- data.frame(
        sentence_id = df$sentence_id,
        token       = df$token,
        upos        = df$upos
      )
    }
    
    df <- subset(df, !is.na(df[,3]) & (df[,3]!="PUNCT") & (df[,3]!="INTJ"))
    
    return(df)
  }

  # Make inventory of POS tags
  
  all.upos <- function(varieties)
  {
    uposAll <- c("EMPTY")
    
    for (i in 1:length(varieties))
    {
      uposAll <- unique(c(uposAll, read.upos(varieties[i])[,3]))
    }
    
    return(uposAll)  
  }
  
  # Count 2-grams in file
  
  process2 <- function(uposAll, df, variety, frqFile)
  {
    freq <- array(0, c(length(uposAll), length(uposAll)))
    
    dimNames <- list(
      dim1 <- uposAll,
      dim2 <- uposAll
    )
    dimnames(freq) <- dimNames
    
    sentences <- unique(df[,1])
    for (i in 1:length(sentences))
    {
      upos <- df[df[,1]==sentences[i],3]
      
      if (length(upos) >= 2)
      {
        for (j in 0:length(upos))
        {
          if (j==0)
            t1 <- "EMPTY"
          else
            t1 <- upos[j]
          
          if (j==length(upos))
            t2 <- "EMPTY"
          else
            t2 <- upos[j+1]
          
          freq[t1,t2] <- freq[t1,t2] + 1
          
          if (is.element("n-gram frequencies" , input$selOutput2) && (variety!=""))
            writeLine(frqFile, paste0(variety, "\t", t1, "\t", t2, "\t", "1"))
        }
      }
    }
    
    if (input$selScale=="linear")
      return(c(freq))
    if (input$selScale=="logarithmic")
      return(c(log(freq[]+1)))
  }
  
  # Count 3-grams in file
  
  process3 <- function(uposAll, df, variety, frqFile)
  {
    freq <- array(0, c(length(uposAll), length(uposAll), length(uposAll)))
    
    dimNames <- list(
      dim1 <- uposAll,
      dim2 <- uposAll,
      dim3 <- uposAll
    )
    dimnames(freq) <- dimNames
    
    sentences <- unique(df[,1])
    for (i in 1:length(sentences))
    {
      upos <- df[df[,1]==sentences[i],3]
      
      if (length(upos) >= 3)
      {
        for (j in 0:(length(upos)-1))
        {
          if (j==0)
            t1 <- "EMPTY"
          else
            t1 <- upos[j]
          
          t2 <- upos[j+1]
          
          if (j==(length(upos)-1))
            t3 <- "EMPTY"
          else
            t3 <- upos[j+2]
          
          freq[t1,t2,t3] <- freq[t1,t2,t3] + 1
          
          if (is.element("n-gram frequencies" , input$selOutput2) && (variety!=""))
            writeLine(frqFile, paste0(variety, "\t", t1, "\t", t2, "\t", t3, "\t", "1"))
        }
      }
    }
    
    if (input$selScale=="linear")
      return(c(freq))
    if (input$selScale=="logarithmic")
      return(c(log(freq[]+1)))
  }
  
  # Count 4-grams in file
  
  process4 <- function(uposAll, df, variety, frqFile)
  {
    freq <- array(0, c(length(uposAll), length(uposAll), length(uposAll), length(uposAll)))
    
    dimNames <- list(
      dim1 <- uposAll,
      dim2 <- uposAll,
      dim3 <- uposAll,
      dim4 <- uposAll
    )
    dimnames(freq) <- dimNames
    
    sentences <- unique(df[,1])
    for (i in 1:length(sentences))
    {
      upos <- df[df[,1]==sentences[i],3]
      
      if (length(upos) >= 4)
      {
        for (j in 0:(length(upos)-2))
        {
          if (j==0)
            t1 <- "EMPTY"
          else
            t1 <- upos[j]
          
          t2 <- upos[j+1]
          t3 <- upos[j+2]
          
          if (j==(length(upos)-2))
            t4 <- "EMPTY"
          else
            t4 <- upos[j+3]
          
          freq[t1,t2,t3,t4] <- freq[t1,t2,t3,t4] + 1
          
          if (is.element("n-gram frequencies" , input$selOutput2) && (variety!=""))
            writeLine(frqFile, paste0(variety, "\t", t1, "\t", t2, "\t", t3, "\t", t4, "\t", "1"))
        }
      }
    }
    
    if (input$selScale=="linear")
      return(c(freq))
    if (input$selScale=="logarithmic")
      return(c(log(freq[]+1)))
  }
  
  # Count 5-grams in file
  
  process5 <- function(uposAll, df, variety, frqFile)
  {
    freq <- array(0, c(length(uposAll), length(uposAll), length(uposAll), length(uposAll), length(uposAll)))
    
    dimNames <- list(
      dim1 <- uposAll,
      dim2 <- uposAll,
      dim3 <- uposAll,
      dim4 <- uposAll,
      dim5 <- uposAll
    )
    dimnames(freq) <- dimNames
    
    sentences <- unique(df[,1])
    for (i in 1:length(sentences))
    {
      upos <- df[df[,1]==sentences[i],3]
      
      if (length(upos) >= 5)
      {
        for (j in 0:(length(upos)-3))
        {
          if (j==0)
            t1 <- "EMPTY"
          else
            t1 <- upos[j]
          
          t2 <- upos[j+1]
          t3 <- upos[j+2]
          t4 <- upos[j+3]
          
          if (j==(length(upos)-3))
            t5 <- "EMPTY"
          else
            t5 <- upos[j+4]
          
          freq[t1,t2,t3,t4,t5] <- freq[t1,t2,t3,t4,t5] + 1
          
          if (is.element("n-gram frequencies" , input$selOutput2) && (variety!=""))
            writeLine(frqFile, paste0(variety, "\t", t1, "\t", t2, "\t", t3, "\t", t4, "\t", t5, "\t", "1"))
        }
      }
    }
    
    if (input$selScale=="linear")
      return(c(freq))
    if (input$selScale=="logarithmic")
      return(c(log(freq[]+1)))
  }
  
  process0 <- function(uposAll, df, variety, frqFile)
  {
    if (input$selGrams=="2-gram")
      return(process2(uposAll, df, variety, frqFile))
    
    if (input$selGrams=="3-gram")
      return(process3(uposAll, df, variety, frqFile))
    
    if (input$selGrams=="4-gram")
      return(process4(uposAll, df, variety, frqFile))
    
    if (input$selGrams=="5-gram")
      return(process5(uposAll, df, variety, frqFile))
  }
  
  calcDist2 <- function(varieties)
  {
    indFile <- paste0(tempDir, "individual.tsv")
    writeLines("var1\tvar2\titem\tdist", con = indFile)

    frqFile <- paste0(tempDir, "frequencies.tsv")
    
    selGrams <- as.numeric(substr(input$selGrams, 1,1))
    header   <- "var\tpos"
    
    if (selGrams >= 2)
      header <- paste0(header, "\tpos")
    if (selGrams >= 3)
      header <- paste0(header, "\tpos")
    if (selGrams >= 4)
      header <- paste0(header, "\tpos")
    if (selGrams >= 5)
      header <- paste0(header, "\tpos")
   
    writeLines(paste0(header, "\tfreq"), con = frqFile)  

    uposAll <- all.upos(varieties)
    dis <- matrix(0, nrow=length(varieties), ncol=length(varieties))
    
    ij <- 0
    nn <- (length(varieties) * (length(varieties)-1))/2
    
    freq1 <- process0(uposAll, read.upos(varieties[1]), varieties[1], frqFile) 
    
    for (i in 2:length(varieties))
    {
      freq1 <- process0(uposAll, read.upos(varieties[i]), varieties[i], frqFile) 
      
      for (j in 1:(i-1))
      {
        ij <- ij + 1
        
        freq2 <- process0(uposAll, read.upos(varieties[j]), "", NULL)
        
        if (input$selComp=="Cosine θ")
          dis[i,j] <- 1 - cos.sim(freq1, freq2)
        
        if (input$selComp=="Pearson's r")
          dis[i,j] <- 1 - cor    (freq1, freq2, method = "pearson" )
        
        if (input$selComp=="Spearman's ρ")
          dis[i,j] <- 1 - cor    (freq1, freq2, method = "spearman")
        
        if (input$selComp=="Kendall's τ")
          dis[i,j] <- 1 - cor.fk (freq1, freq2)
        
        dis[j,i] <- dis[i,j]
        
        writeLine(indFile, paste0(varieties[i], "\t", varieties[j], "\t", "all", "\t", as.character(dis[i,j])))
        
        removeNotification(global$idNot)
        global$idNot <- showNotification(HTML(paste0("Calculating, ", round((ij/nn)*100), "% done.")), type = "message", duration = NULL)
      }
    }
    
    write.table(dis, file = paste0(tempDir, "aggregated.csv"), quote = F, row.names = F, col.names = F, sep=",")
  }
  
  observeEvent(input$goButton2,
  {
    global$dataType  <- input$navBar
    global$finished  <- FALSE
    global$partition <- NULL

    system(paste0("rm -Rf ", tempDir, "*"))
    
    # check inputs
    
    if  (length(input$dataTab2)==0)
    {
      showNotification("Please upload files with POS tags!", type = "error")
      return(NULL)
    }
    
    if  (length(input$dataForm2)==0)
    {
      showNotification("Please indicate the format of the files with POS tags!", type = "error")
      return(NULL)
    }

    # get list of varieties
    
    if (input$dataForm2=="1")
    {
      varieties <- gsub(".xlsx", "", input$dataTab2$name)
    }

    if (input$dataForm2=="2")
    {
      varieties <- gsub(".conllu", "", input$dataTab2$name)
    }

    varieties <- sort(varieties)
    items     <- "all"
    
    write.table(varieties, file = paste0(tempDir, "files.txt"), quote = F, row.names = F, col.names = F, sep=",")
    write.table(items    , file = paste0(tempDir, "items.txt"), quote = F, row.names = F, col.names = F, sep=",")
    
    # check coordinates file
    
    if (!is.null(global$geoTab2))
    {
      varieties1 <- tolower(varieties)
      varieties2 <- tolower(geoTab()[,1])
      
      if ((!setequal(varieties1,varieties2)) | (length(varieties1)!=length(varieties2)))
      {
        showNotification("Varieties in distances file do not match with varieties in coordinates file!", type = "error", duration = NULL)
        return(NULL)
      }
    }

    # calculate distances
    
    calcDist2(varieties)
    
    # Finish
    
    removeNotification(global$idNot)
    
    insertUI(
      selector = "#goButton",
      where    = "afterEnd",
      ui       = tags$audio(src = "ready.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
    )
    
    showNotification("Ready!", type = "message", duration = NULL)
    global$finished <- TRUE
  })
  
  # Make downloads available
  
  output$download21 <- downloadHandler(filename = "aggregated.xlsx", content = function(file)
    write.xlsx(as.data.frame(aggrMat()), file = file, rowNames = T, colNames = T, sheetName = "aggregated distances", headerStyle = createStyle(textDecoration = "BOLD"))
  )
  
  output$download22 <- downloadHandler(filename = "frequencies.xlsx", content = function(file)
  {
    frq <- read.delim(paste0(tempDir, "frequencies.tsv"))
    agg <- aggregate(freq~., data=frq, FUN=sum)
    agg <- agg[order(agg$var, -agg$freq),]
    write.xlsx(agg, file = file, rowNames = F, colNames = T, sheetName = "frequencies", headerStyle = createStyle(textDecoration = "BOLD"))
  })

  output$showResults2 <- renderUI(
  {
    req(global$finished, input$selOutput2)
    
    tl <- fluidPage(
      style = "margin-left: 106px",
      align = "left",
      
      br(), br(),
      
      bsButton("butDownloads2", label = NULL, icon = icon("info"), size = "extra-small"),
      bsModal ("modDownloads2", "Output", "butDownloads2", size = "large", "Dit is de helptekst"),
      
      HTML("<span style='font-weight: bold;'>&nbsp;Downloads:</span>"),
      p(style='height: 8px')
    )
    
    if (is.element("aggregated distances", input$selOutput2) & file.exists(paste0(tempDir, "aggregated.csv")) && (file.size(paste0(tempDir, "aggregated.csv"))!=0))
      tl <- tagList(tl, downloadButton('download21' , class = 'dlButton', 'aggregated distances'),
                    br(), br())
    
    if (is.element("n-gram frequencies" , input$selOutput2) & file.exists(paste0(tempDir, "frequencies.tsv")))
      tl <- tagList(tl, downloadButton('download22' , class = 'dlButton', 'n-gram frequencies') ,
                    br(), br())

    return(tl)
  })  
  
  ##############################################################################
  
  output$geoTab8 <- renderUI(
  {
    input$dataTab8
    global$geoTab8 <- NULL
      
    fluidPage(
      align = "center",
      fileInput(
        inputId    = "geoTab8",
        label      = NULL,
        multiple   = FALSE,
        accept     = ".xlsx",
        width      = "250px")
    )
  })
  
  observeEvent(input$geoTab8,
  {
    global$geoTab8 <- input$geoTab8
  })

  observeEvent(input$goButton8,
  {
    global$dataType  <- input$navBar
    global$finished  <- FALSE
    global$distTab   <- NULL
    global$partition <- NULL
    
    system(paste0("rm -Rf ", tempDir, "*"))
    
    # check inputs
    
    if  (length(input$dataTab8)==0)
    {
      showNotification("Please upload a file with distances!", type = "error")
      return(NULL)
    }
    
    # upload table with distances

    if (is.null(input$dataTab8))
      return(NULL)
    
    inFile <- input$dataTab8
    df <- check_decimal(as.data.frame(read.xlsx(inFile$datapath, rowNames = F, sep.names = " ")))
    rownames(df) <- df[,1]
    df[,1] <- NULL
    colnames(df) <- colnames(df)
    
    df <- df[,order(tolower(colnames(df)))]
    df <- df[order(tolower(rownames(df))),]
   
    # check if row names and column names are the same
    
    cmp <- unique((tolower(rownames(df))==tolower(colnames(df))))
    
    if ((length(cmp)>1) || (cmp==F))
    {
      showNotification("Row names and column names are not the same!", type = "error", duration = NULL)
      return(NULL)
    }
  
    # check duplicates
    
    if (length(unique(rownames(df))) < length(rownames(df)))
    {
      showNotification("Duplicate variety names are not allowed!", type = "error", duration = NULL)
      return(NULL)
    }
    
    if (length(unique(colnames(df))) < length(colnames(df)))
    {
      showNotification("Duplicate variety names are not allowed!", type = "error", duration = NULL)
      return(NULL)
    }
  
    # check coordinates file
         
    if (!is.null(global$geoTab8))
    {
      varieties1 <- tolower(rownames(df))
      varieties2 <- tolower(geoTab()[,1])

      if ((!setequal(varieties1,varieties2)) | (length(varieties1)!=length(varieties2)))
      {
        showNotification("Varieties in distances file do not match with varieties in coordinates file!", type = "error", duration = NULL)
        return(NULL)
      }
    }

    # finish

    showNotification("Ready!", type = "message", duration = NULL)
    global$distTab  <- df  
    global$finished <- TRUE
  })

  ##############################################################################

  output$selItem7 <- renderUI(
  {
    req(global$dataType=="transcriptions", input$dataTab, input$dataForm, input$geoTab)
    
    if (input$dataForm=="rows are varieties, columns are items")
      globalitems <- colnames(read.xlsx(input$dataTab$datapath, rowNames = F, colNames = T, sep.names = " ")) 
    else
    
    if (input$dataForm=="rows are items, columns are varieties")
      globalitems <-          read.xlsx(input$dataTab$datapath, rowNames = F, colNames = T, sep.names = " ")[,1]
    else {}

    globalitems[1] <- "location"
    global$items   <- globalitems
    
    globalitems[1] <- ""
    globalitems    <- globalitems[order(tolower(globalitems))]
    globalitems[1] <- "location"
    
    return(tagList(
      br(),
      
      bsButton("butReplyItem7", label = NULL, icon = icon("info"), size = "extra-small"),
      bsModal ("modReplyItem7", "Select item", "butReplyItem7", size = "large", "Dit is de helptekst"),
        
      HTML("<span style='font-weight: bold;'>&nbsp;Select item:</span>"),
      div(style='height: 12px'),
      div(style='margin-left: 60px; width:200px', selectInput(inputId   = "replyItem7", 
                                                              label     = NULL, 
                                                              choices   = globalitems,
                                                              selected  = "location", 
                                                              multiple  = F, 
                                                              selectize = F)
         ),
      
      br()
    ))
  })
    
  plotDisplay7 <- function()
  {
    req(input$replyItem7, input$marginX7, input$marginY7)
    
    if (input$replyItem7=="location")
      index <- 1
    else
      index <- which(global$items==input$replyItem7)
      
    if ((length(index)>0) && (input$dataForm=="rows are varieties, columns are items"))
    { 
      df <- data.frame(
        vars = read.xlsx(input$dataTab$datapath, rowNames = F, colNames = T, sep.names = " ")[,1],
        labs = read.xlsx(input$dataTab$datapath, rowNames = F, colNames = T, sep.names = " ")[,index]
      )
        
      df <- df[order(tolower(df$vars)),]
    }
    else
      
    if ((length(index)>0) && (input$dataForm=="rows are items, columns are varieties"))
    {
      df <- data.frame(
        vars = colnames(read.xlsx(input$dataTab$datapath, rowNames = F, colNames = T, sep.names = " ")),
        labs =      c(t(read.xlsx(input$dataTab$datapath, rowNames = F, colNames = T, sep.names = " ")[index,]))
      )

      df <- df[2:nrow(df),]
      df <- df[order(tolower(df$vars)),]
    }
    else 
      return(NULL)

    varieties1 <- tolower(df$vars)
    varieties2 <- tolower(geoTab()[,1])

    if ((!setequal(varieties1,varieties2)) | (length(varieties1)!=length(varieties2)))
    {
      if (index==1)
      {
        showNotification("Varieties in transcriptions file do not match with varieties in coordinates file!", type = "error", duration = NULL)
      }
      
      return(NULL)
    }
    
    df$labs[is.na(df$labs)] <- ""
    
    if ("group" %in% colnames(geoTab()))
    {
      ngroups <- length(unique(geoTab()$group))
      Colors  <- rainbow(ngroups)
      textCol <- "black"
    }
    else
    {
      Colors  <- "black"
      textCol <- "darkblue" 
    }

    world <- ne_countries(scale = "large", returnclass = "sf")
    class(world)

    fs <- as.numeric(input$replyPoint7)/2.8346438836889
    
    gp <- ggplot(data = world) +
      geom_sf() +
      geom_point(data=geoTab(), aes(x=long, y=lat, color=geoTab()$group), size = input$dotRadius7) +
      geom_text_repel(data=geoTab(), aes(x=long, y=lat, label=df$labs),
                      color=textCol, max.overlaps = nrow(geoTab()),
                      family=input$replyFont7, size = fs) +
      scale_color_manual(values=Colors, name = 'group') +
      xlab("") + ylab("") +
      coord_sf(xlim = c(min(geoTab()$long)-input$marginX7, max(geoTab()$long)+input$marginX7), 
               ylim = c(min(geoTab()$lat )-input$marginY7, max(geoTab()$lat )+input$marginY7), expand = T) +
      theme_void() +
      theme(axis.text.x      = element_blank(),
            axis.ticks.x     = element_blank(),
            axis.text.y      = element_blank(),
            axis.ticks.y     = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background  = element_rect(fill = 'white'  , colour = 'white'),
            panel.background = element_rect(fill = '#d5e3e9', colour = 'white'),
            legend.title     = element_text(size=max(as.numeric(input$replyPoint7),11), face="bold"),
            legend.text      = element_text(size=max(as.numeric(input$replyPoint7),11)),
            legend.key.size  = unit(1.5,'lines'))
    
    print(gp)
  }

  plotGraph7 <- function()
  {
    req(global$dataType=="transcriptions", input$dataTab, input$dataForm, input$geoTab)
    plotDisplay7()
  }
  
  output$graph7 <- renderPlot(res = 72,
  {
    plotGraph7()
  })
  
  output$Graph7 <- renderUI(
  {
    plotOutput("graph7", width = 0.6244378*input$winWidth, height = max(550, input$winHeight - 350))
  })
  
  output$selFont7 <- renderUI(
  {
    selectInput('replyFont7', label=NULL, choices=Fonts, selected = "FreeSans", selectize=FALSE, multiple=FALSE)
  })
  
  output$selPoint7 <- renderUI(
  {
    options <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48,54,60)
    selectInput('replyPoint7', label=NULL, options, selected = 12, selectize=FALSE, multiple=FALSE)
  })
  
  output$selFormat7 <- renderUI(
  {
    options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
    selectInput('replyFormat7', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
  })
  
  output$selDPI7 <- renderUI(
  {
    options <- c("150 dpi","300 dpi","600 dpi")
    selectInput('replyDPI7', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
  })
  
  dpi7 <- function()
  {
    req(input$replyDPI7)
    
    if (input$replyDPI7=="150 dpi")
      return(150)
    if (input$replyDPI7=="300 dpi")
      return(300)
    if (input$replyDPI7=="600 dpi")
      return(600)
  }
  
  fileName7 <- function()
  {
    if (!is.null(global$items))
      return(paste0(input$replyItem7, "_plot.", input$replyFormat7))
    else
      return(paste0(           "display_plot.", input$replyFormat7)) 
  }
  
  output$downLoad7 <- downloadHandler(filename = fileName7, content = function(file)
  {
    grDevices::pdf(NULL)
    
    width  <- 0.6244378*input$winWidth
    height <- max(550, input$winHeight - 350)
    
    width  <- convertUnit(x=unit(width , "pt"), unitTo="in", valueOnly=TRUE)
    height <- convertUnit(x=unit(height, "pt"), unitTo="in", valueOnly=TRUE)
    
    if (!is.null(global$items))
      plot <- plotGraph7()
    else
      plot <- ggplot()+theme_bw()
    
    if (input$replyFormat7=="JPG")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi7(), limitsize=F, device="jpeg")
    else
    if (input$replyFormat7=="PNG")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi7(), limitsize=F, device="png" )
    else
    if (input$replyFormat7=="SVG")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi7(), limitsize=F, device="svg" )
    else
    if (input$replyFormat7=="EPS")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi7(), limitsize=F, device=grDevices::cairo_ps )
    else
    if (input$replyFormat7=="PDF")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi7(), limitsize=F, device=grDevices::cairo_pdf)
    else
    if (input$replyFormat7=="TEX")
    {
      tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
      print(plot)
    }
    else {}
    
    grDevices::graphics.off()
  })

  ##############################################################################
  
  aggrMatDist <- eventReactive(aggrMat(),
  {
    matAggr <- aggrMat()
    matAggr[as.numeric(matAggr)==0] <- 1e-20
    return(stats::as.dist(matAggr))
  })

  clusObj3 <- reactive(
  {
    if (length(input$replyMethod31)==0)
      method <- "average"
    else
      
    if (input$replyMethod31==  "Single-Linkage")
      method <- "single"
    else
      
    if (input$replyMethod31=="Complete-Linkage")
      method <- "complete"
    else
      
    if (input$replyMethod31=="UPGMA")
      method <- "average"
    else
      
    if (input$replyMethod31=="WPGMA")
      method <- "mcquitty"
    else
      
    if (input$replyMethod31=="Ward's")
      method <- "ward.D2"
    else {}
    
    clus <- hclust(aggrMatDist(), method=method)

    explVar <- formatC(x=round2((cor(aggrMatDist(), cophenetic(clus))^2)*100, n=1), digits = 1, format = "f")
    contentOfMessage <- paste0("Variance explained by ", input$replyMethod31, " clustering: ", explVar, "%.")
    showNotification(contentOfMessage, type = "message", duration = NULL)

    return(clus)
  })

  getPerplexity <- function()
  {
    if (nrow(aggrMat()) < 91)
      return((nrow(aggrMat())-1) %/% 3)
    else
      return(30)
  }

  nGroups <- function(clusObj)
  {
    maxDiff <- 0
    ii      <- 0

    for (i in 1:length(clusObj$height))
    {
      if (i==1)
        d <- clusObj$height[i]
      else
        d <- clusObj$height[i] - clusObj$height[i-1]

      if (d >= maxDiff)
      {
        maxDiff <- d
        ii      <- i
      }
    }

    if (ii==1)
      return(1)
    else
      return(length(clusObj$labels) - ii + 1)
  }

  sammonMDS <- function(d, k = 2, max_iter = 100) 
  {
    tryCatch(
    {
      # Initial configuration (recommended)
      init <- cmdscale(d, k = k)
      
      result <- sammon(d, y = init, k = k, niter = max_iter, trace = FALSE)
      
      # Check for suspicious stress value
      if (!is.finite(result$stress))
      {
        showNotification(      "High stress!", type = "warning", duration = NULL)
        return(NULL)
      }
      else
        
      if (result$stress > 1)
      {
        showNotification("Non-finite result!", type = "warning", duration = NULL)
        return(NULL)
      }
      else
        return(result)
      
    }, 
    error = function(e) 
    {
      showNotification(paste("Sammon mapping failed:", conditionMessage(e), "."), type = "warning", duration = NULL)
      return(NULL)
    })
  }

  multObj3 <- reactive(
  {
    req(nrow(aggrMatDist())>3)
    
    if (input$replyMethod32=="Classical")
    {
      fit <- cmdscale(aggrMatDist(), eig=TRUE, k=2)
      coords <- as.data.frame(fit$points)
      cat("stress: ", sqrt(sum((aggrMatDist() - dist(coords))^2) / sum(aggrMatDist()^2)), "\n")
    }

    if (input$replyMethod32=="Kruskal's")
    {
      fit <- isoMDS(aggrMatDist(), k=2)
      coords <- as.data.frame(fit$points)
    }

    if (input$replyMethod32=="Sammon's")
    {
      fit <- sammonMDS(aggrMatDist(), k=2)
      coords <- as.data.frame(fit$points)
    }

    if (input$replyMethod32=="t-SNE")
    {
      fit <- Rtsne(aggrMatDist(), check_duplicates=FALSE, pca=TRUE, perplexity=getPerplexity(), theta=0.5, dims=2)
      coords <- as.data.frame(fit$Y)
    }

    if (!is.null(fit))
    {
      explVar <- formatC(x=round2((cor(aggrMatDist(), mdsDist(coords))^2)*100, n=1), digits = 1, format = "f")
      contentOfMessage <- paste0("Variance explained by ", input$replyMethod32, " 2D MDS: ", explVar, "%.")
      showNotification(contentOfMessage, type = "message", duration = NULL)
      return(coords)      
    }
  })

  output$selMethod3 <- renderUI(
  {
    req(aggrMat(), input$selClass3)

    if ((nrow(aggrMat()) >= 3) && (input$selClass3=="Cluster analysis"))
      tagList(
        br(),

        bsButton("butReplyMethod31", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modReplyMethod31", "Cluster method", "butReplyMethod31", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;Cluster method:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', radioButtons('replyMethod31',
                                       NULL,
                                       c("Single-Linkage","Complete-Linkage","UPGMA","WPGMA","Ward's"),
                                       selected=isolate(global$replyMethod31),
                                       inline = FALSE))
      )
    else

    if ((nrow(aggrMat()) >= 3) && (input$selClass3=="Multidimensional scaling"))
      tagList(
        br(),

        bsButton("butReplyMethod32", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modReplyMethod32", "MDS method", "butReplyMethod32", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;MDS method:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', radioButtons('replyMethod32',
                                       NULL,
                                       c("Classical", "Kruskal's", "Sammon's", "t-SNE"),
                                       selected=isolate(global$replyMethod32),
                                       inline = FALSE))
      )
    else {}
  })

  observeEvent(input$replyMethod31,
  {
    global$replyMethod31 <- input$replyMethod31
    global$legendLabels  <- NULL
  })

  observeEvent(input$replyMethod32,
  {
    global$replyMethod32 <- input$replyMethod32
  })

  output$selClus3 <- renderUI(
  {
    req(aggrMat(), input$selClass3)

    if ((nrow(aggrMat()) >= 3) && (input$selClass3=="Cluster analysis"))
      tagList(
        br(),

        bsButton("butNColGroups", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modNColGroups", "Number of color groups", "butNColGroups", size = "large",
                 "By default the number of natural groups is given. You can change this number if you wish."),

        HTML("<span style='font-weight: bold;'>&nbsp;Number of color groups:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(
                                       inputId = "nColGroups",
                                       label   = NULL,
                                       value   = isolate(global$nColGroups),
                                       min     = 1,
                                       max     = nrow(aggrMat()),
                                       step    = 1,
                                       width   = "100px"))
      )

    else
      return(NULL)
  })

  observeEvent(c(input$replyMethod31, aggrMatDist()),
  {
    req(input$replyMethod31)
    global$nColGroups <- nGroups(clusObj3())

    updateNumericInput(
      session = session,
      inputId = 'nColGroups',
      value   = global$nColGroups
    )
  })

  observeEvent(input$nColGroups,
  {
    req(input$nColGroups)

    global$nColGroups   <- input$nColGroups
    global$legendLabels <- NULL

    if (global$nColGroups==1)
      global$showLegend <- FALSE
  })

  output$selMult3 <- renderUI(
  {
    req(aggrMat(), input$selClass3)

    if ((nrow(aggrMat()) >= 3) && (input$selClass3=="Multidimensional scaling"))
    {
      if (global$mdsGeon3=="2D")
      {
        tl1 <- tagList(
          br(),

          bsButton("butMdsGeon31", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modMdsGeon31", "Show colors, shapes, labels", "butMdsGeon31", size = "large", "Dit is de helptekst"),

          HTML("<span style='font-weight: bold;'>&nbsp;Show:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', checkboxGroupInput("mdsGeon31",
                                                            NULL,
                                                            choices =c("colors", "shapes", "labels", "3D"),
                                                            selected=isolate(global$mdsGeon31),
                                                            inline=TRUE))
        )
        
        tl2 <- tagList(
          br(),
          
          bsButton("butMdsGeon32", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modMdsGeon32", "Axes", "butMdsGeon32", size = "large", "Dit is de helptekst"),
          
          HTML("<span style='font-weight: bold;'>&nbsp;Axes:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', checkboxGroupInput("mdsGeon32",
                                                            NULL,
                                                            choices=c("X⇄Y", "inv. X", "inv. Y"),
                                                            selected=isolate(global$mdsGeon32),
                                                            inline=TRUE))
        )
      }
      
      if (global$mdsGeon3=="3D")
      {
        tl1 <- tagList(
          br(),
        
          bsButton("butMdsGeon31", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modMdsGeon31", "Show labels", "butMdsGeon31", size = "large", "Dit is de helptekst"),
        
          HTML("<span style='font-weight: bold;'>&nbsp;Show:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', checkboxGroupInput("mdsGeon31",
                                                            NULL,
                                                            choices =c("labels", "2D"),
                                                            selected=isolate(global$mdsGeon31),
                                                            inline=TRUE))
        )
        
        tl2 <- tagList(
          br(),
          
          bsButton("butMdsGeon32", label = NULL, icon = icon("info"), size = "extra-small"),
          bsModal ("modMdsGeon32", "Axes", "butMdsGeon32", size = "large", "Dit is de helptekst"),
          
          HTML("<span style='font-weight: bold;'>&nbsp;Axes:</span>"),
          div(style='height: 12px'),
          div(style='margin-left: 60px', checkboxGroupInput("mdsGeon32",
                                                            NULL,
                                                            choices=c("X⇄Y", "inv. X", "inv. Y", "inv. Z"),
                                                            selected=isolate(global$mdsGeon32),
                                                            inline=TRUE))
        )
      }

      return(tagList(tl1, tl2))
    }
    else
      return(NULL)
  })

  observeEvent(input$mdsGeon31, ignoreNULL = FALSE, ignoreInit = TRUE,
  {
    global$mdsGeon31 <- input$mdsGeon31
  })
  
  observeEvent(global$mdsGeon31,
  {
    if (is.element("2D", global$mdsGeon31))
    {
      updateCheckboxGroupInput(session,
                               "mdsGeon31", 
                               NULL,
                               choices=c("colors", "shapes", "labels", "3D"),
                               selected=c("colors", "labels"),
                               inline=TRUE)

      global$mdsGeon3 <- "2D"
    }

    if (is.element("3D", global$mdsGeon31))
    {
      updateCheckboxGroupInput(session,
                               "mdsGeon31", 
                               NULL,
                               choices=c("labels", "2D"),
                               selected=c("labels"),
                               inline=TRUE)
      
      global$mdsGeon3 <- "3D"
    }
    else {}
  })
  
  observeEvent(input$mdsGeon32,
  {
    global$mdsGeon32 <- input$mdsGeon32
  })

  output$maxOverlaps <- renderUI(
  {
    req(aggrMat())
    req(input$selClass3)

    if ((nrow(aggrMat()) >= 3) && (input$selClass3=="Multidimensional scaling"))
      tagList(
        br(),

        bsButton("butMaxOverlaps", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modMaxOverlaps", "Max. label overlaps", "butMaxOverlaps", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;Max. label overlaps:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(inputId = 'maxOverlaps',
                                                    label   = NULL,
                                                    min     = 0,
                                                    max     = nrow(aggrMat()),
                                                    value   = isolate(global$maxOverlaps),
                                                    step    = 1,
                                                    width   = "100px"))
    )
    else
      return(NULL)
  })

  observeEvent(input$maxOverlaps,
  {
    global$maxOverlaps <- input$maxOverlaps
  })

  round2 <- function(x, n=0)
  {
    scale<-10^n
    return(trunc(x*scale+sign(x)*0.5)/scale)
  }

  fontSize3 <- reactive(
  {
    req(input$selClass3)

    nv <- attr(aggrMatDist(), "Size")

    if (input$selClass3=="Cluster analysis")
    {
      if (nv>190) fs <-  1 else
      if (nv>126) fs <-  2 else
      if (nv> 95) fs <-  3 else
      if (nv> 75) fs <-  4 else
      if (nv> 62) fs <-  5 else
      if (nv> 53) fs <-  6 else
      if (nv> 46) fs <-  7 else
      if (nv> 40) fs <-  8 else
      if (nv> 36) fs <-  9 else
      if (nv> 33) fs <- 10 else
      if (nv> 30) fs <- 11 else
      if (nv> 27) fs <- 12 else
      if (nv> 25) fs <- 13 else
      if (nv> 23) fs <- 14 else
      if (nv> 22) fs <- 15 else
      if (nv> 20) fs <- 16 else
      if (nv> 19) fs <- 17 else
      if (nv>  1) fs <- 18 else {}
    }

    if (input$selClass3=="Multidimensional scaling")
    {
      if (nv> 45) fs <-  9 else
      if (nv> 30) fs <- 11 else
      if (nv> 10) fs <- 14 else
      if (nv>  1) fs <- 17 else {}
    }

    return(fs)
  })

  output$selColors3 <- renderUI(
  {
    req(aggrMat(), input$nColGroups)
      
    if ((nrow(aggrMat()) >= 3) && (input$selClass3=="Cluster analysis"))
      tagList(
        div(style='margin-left: 60px', checkboxInput(inputId = "replyColors3",
                                                     label   = "rainbow colors",
                                                     value   = isolate(global$replyColors3)))
      )
  })

  observeEvent(input$replyColors3,
  {
    global$replyColors3 <- input$replyColors3
  })

  output$showLegend <- renderUI(
  {
    req(input$nColGroups)

    if ((input$selClass3=="Cluster analysis") &(input$nColGroups>1))
      tagList(
        div(style='margin-left: 60px', checkboxInput(inputId = "showLegend",
                                                     label   = "legend",
                                                     value   = isolate(global$showLegend)))
      )
  })

  observeEvent(input$showLegend,
  {
    global$showLegend <- input$showLegend
  })

  output$setLabels <- renderUI(
  {
    req(input$selClass3, input$nColGroups, input$showLegend)

    if ((input$selClass3=="Cluster analysis" ) & (input$nColGroups>1) & input$showLegend)
      tagList(
        div(
          style='margin-left: 60px',
          shiny::actionButton("setLabels", "Enter labels")
        ),

        br()
      )
  })

  observeEvent(input$setLabels,
  {
    if (length(multCol5())==0)
      return(NULL)
    
    labList <- tagList()

    groupings <- cutree(clusObj3(), input$nColGroups)

    df <- data.frame(hex = multCol5(), group = groupings)
    df <- colouR::avgHex(df, hex_col = "hex", group_col = "group")
    
    if (global$replyColors3)
      Colors <- compHex(df$avg_color, groupings)
    else
      Colors <-         df$avg_color

    for (i in 1:input$nColGroups)
    {
      id <- paste0("labField", as.character(i))

      field <- splitLayout(
        style = "margin-left: -40px",
        cellWidths = c("40px", "auto"),
        div(style = paste0("font-size: 20px; color: ", Colors[i]), "●"),
        textInput(id, NULL, "")
      )

      labList <- tagList(labList, field)
    }

    showModal(list(
      draggableModalDialog(
        easyClose = FALSE,
        fade      = FALSE,
        style     = "background-color: #f9f7fc",
        title     = "Enter legend labels",

        fluidPage(
          style = "font-size: 14px;",
          align = "center",
          labList
        ),

        footer = fluidPage(align="center", actionButton("labelsSet", label = "OK")),
      ),

      tags$script("$('.modal-backdrop').css('display', 'none');")
    ))
  })

  observeEvent(input$labelsSet,
  {
    removeModal()
  })

  observeEvent(input$labelsSet,
  {
    ll <- c()

    for (i in 1:input$nColGroups)
    {
      id <- paste0("labField", as.character(i))
      ll <- c(ll, as.character(input[[id]]))
    }

    global$legendLabels <- ll
  })

  distHex <- function(h1,h2)
  {
    Rdif <- strtoi(paste0("0x", substr(h1, 2,3))) - strtoi(paste0("0x", substr(h2, 2,3)))
    Gdif <- strtoi(paste0("0x", substr(h1, 4,5))) - strtoi(paste0("0x", substr(h2, 4,5)))
    Bdif <- strtoi(paste0("0x", substr(h1, 6,7))) - strtoi(paste0("0x", substr(h2, 6,7)))
    
    return(abs(Rdif)+abs(Gdif)+abs(Bdif))
  }
  
  compHex <- function(v1,groupings)
  {
    v2   <- rainbow(length(unique(groupings)))
    v3   <- c()
    used <- c()
    
    for (i in 1:length(v1))
    {
      smallest <- Inf
      
      for (j in 1:length(v2))
      {
        if (!is.element(j, used))
        {
          d <- distHex(v1[i], v2[j])
          
          if (d<smallest)
          {
            smallest <- d
            v3[i] <- v2[j]
            jj <- j
          }
        }
      }
      
      used <- c(used, jj)
    }
    
    return(v3)
  }

  shpPalette3 <- function(n)
  {
    if (n<=11)
      return(c(19,1,17,2,15,0,18,5,3,4,8)[1:n])
    else
      return(rep(19, input$nColGroups))
  }

  plotClus3 <- function()
  {
    req(input$nColGroups)
    
    if (length(multCol5())==0)
      return(NULL)

    dendro <- dendro_data(as.dendrogram(clusObj3()), type = "rectangle")

    gp <- ggplot(dendro$segments) +
          geom_segment(aes(x = x, y = y, xend = xend, yend = yend))

    groupings <- cutree(clusObj3(), input$nColGroups)

    fs <- as.numeric(input$replyPoint3)/2.8346438836889

    if (input$nColGroups >  1)
    {
      df <- data.frame(hex = multCol5(), group = groupings)
      df <- colouR::avgHex(df, hex_col = "hex", group_col = "group")
      
      if (global$replyColors3)
        Colors <- compHex(df$avg_color, groupings)
      else
        Colors <-         df$avg_color
      
      groupings <- as.character(groupings[clusObj3()$order])
      groupings <- factor(groupings, levels = as.factor(sort(as.numeric(unique(groupings)))))
    }
    else
      Colors <- rep("#000000", length(unique(groupings)))
    
    dendro$labels$label <- paste0("  ", dendro$labels$label)

    if (input$nColGroups == 1)
      gp <- gp + geom_text (data = dendro$labels,
                            aes(x, y, label = label),
                            hjust = 0,
                            angle = 0,
                            family=input$replyFont3,
                            size = fs)
    else
      gp <- gp + geom_text (data = dendro$labels,
                            aes(x, y, label = label, colour=groupings),
                            hjust = 0,
                            angle  = 0,
                            family=input$replyFont3,
                            size = fs, 
                            show.legend = F) +

                 geom_point(data = dendro$labels,
                            aes(x, y, colour=groupings),
                            size  = 0,
                            stroke = 0,
                            show.legend = (global$showLegend && (any(global$legendLabels!=""))))

    if (global$showLegend && (any(global$legendLabels!="")))
      Guide <- guide_stringlegend()
    else
      Guide <- NULL
    
    gp <- gp +
          ylim(max(clusObj3()$height), -0.33*max(clusObj3()$height)) +
          scale_x_continuous(expand = c(0, 0), limits = c(-1, length(dendro$labels$label) + 1)) +
          scale_color_manual(labels=global$legendLabels, values=Colors, guide = Guide, name = "group") +
          labs(colour=NULL) +
          coord_flip() +
          xlab(NULL) + ylab(NULL) +
          theme_bw() +
          theme(text            =element_text(size=as.numeric(input$replyPoint3), family=input$replyFont3),
                axis.line.x     =element_line(color="black"),
                axis.text.x     =element_text(size=as.numeric(input$replyPoint3)),
                axis.text.y     =element_blank(),
                axis.ticks.y    =element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                panel.border    =element_blank(),
                legend.title    =element_text(size=max(as.numeric(input$replyPoint3),13), face="bold"),
                legend.text     =element_text(size=max(as.numeric(input$replyPoint3),13)),
                legend.key.size =unit(1.5,'lines'))

    print(gp)
  }

  plotMult32 <- function()
  {
    req(input$nColGroups)

    if (length(multObj3()) >0)
      coords <- multObj3()
    else
      return(NULL)

    if (length(multCol5())==0)
      return(NULL)
    
    if (!is.element("X⇄Y", input$mdsGeon32))
    {
      Xlab <- "dimension 1"
      Ylab <- "dimension 2"
    }
    else
    {
      colnames(coords)[1] <- "V0"
      colnames(coords)[2] <- "V1"
      colnames(coords)[1] <- "V2"

      Xlab <- "dimension 2"
      Ylab <- "dimension 1"
    }

    if (is.element("inv. X", input$mdsGeon32))
      coords$V1 <- -1 * coords$V1

    if (is.element("inv. Y", input$mdsGeon32))
      coords$V2 <- -1 * coords$V2

    groupings <- as.character(cutree(clusObj3(), input$nColGroups))
    groupings <- factor(groupings, levels = as.factor(sort(as.numeric(unique(groupings)))))

    fs <- as.numeric(input$replyPoint3)/2.8346438836889

    Labels <- row.names(as.matrix(aggrMatDist()))

    gp <- ggplot(coords, aes(V1, V2, label = Labels, color=groupings, shape=groupings)) +
          geom_point(size = fs, show.legend=global$showLegend)

    if (is.element("colors", input$mdsGeon31) & (input$nColGroups>  1))
    {
      df <- data.frame(hex = multCol5(), group = groupings)
      df <- colouR::avgHex(df, hex_col = "hex", group_col = "group")
      
      if (global$replyColors3)
        Colors <- compHex(df$avg_color, groupings)
      else
        Colors <-         df$avg_color
    }
    else
      Colors <- rep("#000000", length(unique(groupings)))

    if (is.element("shapes", input$mdsGeon31) & (input$nColGroups>  1))
      Shapes <- shpPalette3   (length(unique(groupings)))
    else
      Shapes <- rep(19       , length(unique(groupings)))

    if (is.element("labels", input$mdsGeon31))
      gp <- gp + geom_text_repel(family=input$replyFont3, colour="black", size = fs, show.legend=FALSE, max.overlaps = 2 * input$maxOverlaps, min.segment.length = 0.5, point.padding = unit(2 * fs, "points"), box.padding = unit(2 * fs, "points"))

    gp <- gp +
          scale_x_continuous(breaks = 0, limits = c(min(coords$V1-0.01,coords$V2-0.01), max(coords$V1+0.01,coords$V2+0.01))) +
          scale_y_continuous(breaks = 0, limits = c(min(coords$V1-0.01,coords$V2-0.01), max(coords$V1+0.01,coords$V2+0.01))) +
          scale_color_manual(labels=global$legendLabels, values=Colors, name = 'group') +
          scale_shape_manual(labels=global$legendLabels, values=Shapes, name = 'group') +
          xlab(Xlab) + ylab(Ylab) +
          theme_bw() +
          theme(text           =element_text(size=as.numeric(input$replyPoint3), family=input$replyFont3),
                axis.title     =element_text(size=max(as.numeric(input$replyPoint3),13), face="bold"),
                legend.title   =element_text(size=max(as.numeric(input$replyPoint3),13), face="bold"),
                legend.text    =element_text(size=max(as.numeric(input$replyPoint3),13)),
                legend.key.size=unit(1.5,'lines'),
                aspect.ratio   =1)

    print(gp)
  }

  plotMult33 <- function()
  {
    req(input$nColGroups)

    if (length(multObj5()) >0)
      coords <- multObj5()
    else
      return(NULL)

    if (length(multCol5())==0)
      return(NULL)
    
    if (!is.element("X⇄Y", input$mdsGeon32))
    {
      Xlab <- "dimension 1"
      Ylab <- "dimension 2"
    }
    else
    {
      colnames(coords)[1] <- "V0"
      colnames(coords)[2] <- "V1"
      colnames(coords)[1] <- "V2"

      Xlab <- "dimension 2"
      Ylab <- "dimension 1"
    }

    if (is.element("inv. X", input$mdsGeon32))
      coords$V1 <- -1 * coords$V1

    if (is.element("inv. Y", input$mdsGeon32))
      coords$V2 <- -1 * coords$V2

    if (is.element("inv. Z", input$mdsGeon32))
      coords$V3 <- -1 * coords$V3

    fs <- as.numeric(input$replyPoint3)/2.8346438836889

    Labels <- row.names(as.matrix(aggrMatDist()))

    max_val <- max(abs(coords$V3))
    
    gp <- ggplot(coords, aes(V1, V2, label = Labels, fill=V3)) +
          geom_point(size = fs, shape = 21, color = "black", stroke = 0.3)

    if (is.element("labels", input$mdsGeon31))
      gp <- gp + geom_text_repel(family=input$replyFont3, colour="black", size = fs, show.legend=FALSE, max.overlaps = 2 * input$maxOverlaps, min.segment.length = 0.5, point.padding = unit(2 * fs, "points"), box.padding = unit(2 * fs, "points"))

    gp <- gp +
          scale_x_continuous(breaks = 0, limits = c(min(coords$V1-0.01,coords$V2-0.01), max(coords$V1+0.01,coords$V2+0.01))) +
          scale_y_continuous(breaks = 0, limits = c(min(coords$V1-0.01,coords$V2-0.01), max(coords$V1+0.01,coords$V2+0.01))) +
          
          scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +

          xlab(Xlab) + ylab(Ylab) +
          theme_bw() +
          theme(text           =element_text(size=as.numeric(input$replyPoint3), family=input$replyFont3),
                axis.title     =element_text(size=max(as.numeric(input$replyPoint3),13), face="bold"),
                legend.position= "none",
                aspect.ratio   =1)

    print(gp)
  }

  plotGraph3 <- function()
  {
    req(aggrMat(), input$selClass3)

    if ((length(input$replyMethod31)>0) && (input$selClass3=="Cluster analysis"))
      plotClus3 ()
    else

    if ((length(input$replyMethod32)>0) && (input$selClass3=="Multidimensional scaling") && (global$mdsGeon3=="2D"))
      plotMult32()
    else
      
    if ((length(input$replyMethod32)>0) && (input$selClass3=="Multidimensional scaling") && (global$mdsGeon3=="3D"))
      plotMult33()
    else {}
  }

  plotHeight <- reactive(
  {
    req(input$selClass3, input$replyPoint3)

    if (input$selClass3=="Cluster analysis")
    {
      nv <- attr(aggrMatDist(), "Size") + 3
      ph <- round2(1.4 * nv * as.numeric(input$replyPoint3))
    }

    if  (input$selClass3=="Multidimensional scaling")
    {
      ph <- 550
    }

    return(paste0(as.character(ph), "px"))
  })

  output$graph3 <- renderPlot(res = 72,
  {
    plotGraph3()
  })

  output$Graph3 <- renderUI(
  {
    plotOutput("graph3", width = 0.6244378*input$winWidth, height = max(as.numeric(sub("px", "", plotHeight())), input$winHeight - 350))
  })

  mdsDist <- function(coords)
  {
    nf <- nrow(coords)
    dist <- matrix(0, nrow = nf, ncol = nf)

    nd <- ncol(coords)

    for (i in 2:nf)
    {
      for (j in 1:(i-1))
      {
        sum <- 0

        for (k in 1:nd)
        {
          sum <- sum + (coords[i,k] - coords[j,k])^2
        }

        dist[i,j] <- sqrt(sum)
        dist[j,i] <- sqrt(sum)
      }
    }

    for (i in 1:nf)
      dist[i,i] <- 0

    return(stats::as.dist(dist, diag=FALSE, upper=FALSE))
  }

  output$selFont3 <- renderUI(
  {
    selectInput('replyFont3', label=NULL, choices=Fonts, selected = "DejaVu Sans", selectize=FALSE, multiple=FALSE)
  })

  output$selPoint3 <- renderUI(
  {
    options <- c(0.1, 0.2, 0.3, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48,54,60)
    selectInput('replyPoint3', label=NULL, options, selected = fontSize3(), selectize=FALSE, multiple=FALSE)
  })

  output$selFormat3 <- renderUI(
  {
    options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
    selectInput('replyFormat3', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
  })

  output$selDPI3 <- renderUI(
  {
    options <- c("150 dpi","300 dpi","600 dpi")
    selectInput('replyDPI3', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
  })

  dpi3 <- function()
  {
    req(input$replyDPI3)

    if (input$replyDPI3=="150 dpi")
      return(150)
    if (input$replyDPI3=="300 dpi")
      return(300)
    if (input$replyDPI3=="600 dpi")
      return(600)
  }

  fileName3 <- function()
  {
    if (input$selClass3=="Cluster analysis")
      type <- "dendrogram"
    if (input$selClass3=="Multidimensional scaling")
      type <- "multidimensional_scaling_plot"

    return(paste0(type, ".", input$replyFormat3))
  }

  output$downLoad3 <- downloadHandler(filename = fileName3, content = function(file)
  {
    grDevices::pdf(NULL)

    width  <- 0.6244378*input$winWidth
    height <- max(as.numeric(sub("px", "", plotHeight())), input$winHeight - 350)

    width  <- convertUnit(x=unit(width , "pt"), unitTo="in", valueOnly=TRUE)
    height <- convertUnit(x=unit(height, "pt"), unitTo="in", valueOnly=TRUE)

    if (!is.null(aggrMat()))
      plot <- plotGraph3()
    else
      plot <- ggplot()+theme_bw()

    if (input$replyFormat3=="JPG")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi3(), limitsize=F, device="jpeg")
    else
    if (input$replyFormat3=="PNG")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi3(), limitsize=F, device="png" )
    else
    if (input$replyFormat3=="SVG")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi3(), limitsize=F, device="svg" )
    else
    if (input$replyFormat3=="EPS")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi3(), limitsize=F, device=grDevices::cairo_ps )
    else
    if (input$replyFormat3=="PDF")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi3(), limitsize=F, device=grDevices::cairo_pdf)
    else
    if (input$replyFormat3=="TEX")
    {
      tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
      print(plot)
    }
    else {}

    grDevices::graphics.off()
  })

  ##############################################################################

  output$beamWeight <- renderUI(
  {
    req(input$selClass5)
      
    if (input$selClass5 == "Beam map")
      tagList(
        bsButton("butBeamWeight", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modBeamWeight", "Thickness of beams", "butBeamWeight", size = "large", "Dit is de helptekst"),
          
        HTML("<span style='font-weight: bold;'>&nbsp;Thickness of beams:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(
          inputId = "beamWeight",
          label   = NULL,
          value   = isolate(global$beamWeight),
          min     = 1,
          max     = 20,
          step    = 1,
          width   = "100px")),
          
        br()
      )
  })

  output$networkWeight <- renderUI(
  {
    req(input$selClass5)

    if (input$selClass5 == "Network map")
      tagList(
        bsButton("butNetworkWeight", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modNetworkWeight", "Thickness of beams", "butNetworkWeight", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;Thickness of beams:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(
          inputId = "networkWeight",
          label   = NULL,
          value   = isolate(global$networkWeight),
          min     = 1,
          max     = 20,
          step    = 1,
          width   = "100px")),

        br()
      )
  })

  observeEvent(input$beamWeight,
  {
    global$beamWeight <- input$beamWeight
  })

  observeEvent(input$networkWeight,
  {
    global$networkWeight <- input$networkWeight
  })

  output$selRGB <- renderUI(
  {
    req(input$selClass5)

    if ((input$selClass5 == "Area map") | (input$selClass5 == "RGB map"))
      tagList(
        bsButton("butSelRGB", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modSelRGB", "Choose RGB colors", "butSelRGB", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;Choose RGB colors:</span>"),
        div(style='height: 12px'),

        splitLayout(
          style      ='margin-left: 15px; width: 210px;',
          cellWidths = c("42px","50px","50px","50px","50px"),

          column(
            width = 1,
            div(style='height: 25px', "1"),
            div(style='height: 25px', "2"),
            div(style='height: 25px', "3")
          ),

          radioButtons(
            'selR',
            NULL,
            choiceNames  = c("R", "R", "R"),
            choiceValues = c("1", "2", "3"),
            selected = isolate(global$selR)
          ),

          radioButtons(
            'selG',
            NULL,
            choiceNames  = c("G", "G", "G"),
            choiceValues = c("1", "2", "3"),
            selected = isolate(global$selG)
          ),

          radioButtons(
            'selB',
            NULL,
            choiceNames  = c("B", "B", "B"),
            choiceValues = c("1", "2", "3"),
            selected = isolate(global$selB)),

          checkboxGroupInput(
            'selInv',
            NULL,
            choiceNames  = c("inv.","inv.","inv."),
            choiceValues = c("1"   , "2"  , "3"  ),
            selected = isolate(global$selInv))
        ),

        br()
      )
  })

  observeEvent(input$selR,
  {
    global$selR   <- input$selR
  })

  observeEvent(input$selG,
  {
    global$selG   <- input$selG
  })

  observeEvent(input$selB,
  {
    global$selB   <- input$selB
  })

  observeEvent(input$selInv,
  {
    global$selInv <- input$selInv
  })

  output$selRef <- renderUI(
  {
    req(input$selClass5)

    if (input$selClass5 == "Reference point map")
      tagList(
        bsButton("butSelRef", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modSelRef", "Reference point", "butSelRef", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;Reference point:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', selectizeInput(
          inputId   = "selRef",
          label     = NULL,
          choices   = row.names(aggrMat()),
          selected  = isolate(global$selRef),
          width     = "170px",
          options   = list(dropdownParent = 'body'))),

        br()
      )
  })

  observeEvent(input$selRef,
  {
    global$selRef <- input$selRef
  })

  output$dotRadiusBeam <- renderUI(
  {
    req(input$selClass5)
      
    if (input$selClass5 == "Beam map")
      tagList(
        bsButton("butDotRadiusBeam", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modDotRadiusBeam", "Radius of dot", "butDotRadiusBeam", size = "large", "Dit is de helptekst"),
          
        HTML("<span style='font-weight: bold;'>&nbsp;Radius of dot:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(
          inputId = "dotRadiusBeam",
          label   = NULL,
          value   = isolate(global$dotRadiusBeam),
          min     = 0,
          max     = 20,
          step    = 1,
          width   = "100px")),
          
        br()
      )
  })
  
  output$dotRadiusNetwork <- renderUI(
  {
    req(input$selClass5)

    if (input$selClass5 == "Network map")
      tagList(
        bsButton("butDotRadiusNetwork", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modDotRadiusNetwork", "Radius of dot", "butDotRadiusNetwork", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;Radius of dot:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(
          inputId = "dotRadiusNetwork",
          label   = NULL,
          value   = isolate(global$dotRadiusNetwork),
          min     = 0,
          max     = 20,
          step    = 1,
          width   = "100px")),

        br()
      )
  })

  output$dotRadiusArea5 <- renderUI(
  {
    req(input$selClass5)

    if (input$selClass5 == "Area map")
      tagList(
        bsButton("butDotRadiusArea5", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modDotRadiusArea5", "Radius of dot", "butDotRadiusArea5", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;Radius of dot:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(
          inputId = "dotRadiusArea5",
          label   = NULL,
          value   = isolate(global$dotRadiusArea5),
          min     = 1,
          max     = 20,
          step    = 1,
          width   = "100px"))
      )
  })

  output$dotRadiusRGB <- renderUI(
  {
    req(input$selClass5)

    if (input$selClass5 == "RGB map")
      tagList(
        bsButton("butDotRadiusRGB", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modDotRadiusRGB", "Radius of dot", "butDotRadiusRGB", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;Radius of dot:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(
          inputId = "dotRadiusRGB",
          label   = NULL,
          value   = isolate(global$dotRadiusRGB),
          min     = 1,
          max     = 20,
          step    = 1,
          width   = "100px"))
      )
  })

  output$dotRadiusRef <- renderUI(
  {
    req(input$selClass5)

    if (input$selClass5 == "Reference point map")
      tagList(
        bsButton("butDotRadiusRef", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modDotRadiusRef", "Radius of dot", "butDotRadiusRef", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;Radius of dot:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(
          inputId = "dotRadiusRef",
          label   = NULL,
          value   = isolate(global$dotRadiusRef),
          min     = 1,
          max     = 20,
          step    = 1,
          width   = "100px"))
      )
  })

  observeEvent(input$dotRadiusBeam,
  {
    global$dotRadiusBeam <- input$dotRadiusBeam
  })
  
  observeEvent(input$dotRadiusNetwork,
  {
    global$dotRadiusNetwork <- input$dotRadiusNetwork
  })

  observeEvent(input$dotRadiusArea5,
  {
    global$dotRadiusArea5 <- input$dotRadiusArea5
  })

  observeEvent(input$dotRadiusRGB,
  {
    global$dotRadiusRGB  <- input$dotRadiusRGB
  })

  observeEvent(input$dotRadiusRef,
  {
    global$dotRadiusRef  <- input$dotRadiusRef
  })

  output$selBorder5 <- renderUI(
  {
    req(input$selClass5)
      
    if ((input$selClass5 != "Beam map") & (input$selClass5 != "Network map"))
      tagList(
        div(style='margin-left: 60px', checkboxInput(inputId = "selBorder5",
                                                     label   = "show border",
                                                     value   = isolate(global$selBorder5))),
        br()
      )
  })
  
  observeEvent(input$selBorder5,
  {
    global$selBorder5 <- input$selBorder5
  })

  output$posLegend <- renderUI(
  {
    req(input$selClass5, input$showLegend, global$legendLabels)

    if ((input$selClass5=="Area map") && input$showLegend && (length(global$legendLabels) > 1))
      tagList(
        bsButton("butPosLegend", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modPosLegend", "Position of legend", "butPosLegend", size = "large", "Dit is de helptekst"),

        HTML("<span style='font-weight: bold;'>&nbsp;Position of legend:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', radioButtons(
            inputId    = 'posLegend',
            label      = NULL,
            choices    = c("tl", "bl", "tr", "br" ),
            selected   = isolate(global$posLegend),
            inline     = TRUE)),

        br()
      )
  })

  observeEvent(input$posLegend,
  {
    global$posLegend  <- input$posLegend
  })

  output$selCophenetic <- renderUI(
  {
    req(input$selClass5)
      
    if (input$selClass5 == "RGB map")
      tagList(
        bsButton("butselCophenetic", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modselCophenetic", "Choose distances", "butselCophenetic", size = "large", "Dit is de helptekst"),
        
        HTML("<span style='font-weight: bold;'>&nbsp;Choose distances:</span>"),
        div(style='height: 4px'),
        div(style='margin-left: 60px', checkboxInput(inputId = "selCophenetic",
                                                     label   = "Cophenetic",
                                                     value   = isolate(global$selCophenetic))),
        br()
      )
  })
  
  observeEvent(input$selCophenetic,
  {
    global$selCophenetic <- input$selCophenetic
  })
  
  beamGeo5 <- reactive(
  {
    req((input$navBar == "maps") & (input$selClass5 == "Beam map"))
 
    n <- (nrow(geoTab()) * (nrow(geoTab())-1)) / 2
      
    df <- data.frame(lat1  = rep( 0, n),
                     lat2  = rep( 0, n),
                     long1 = rep( 0, n),
                     long2 = rep( 0, n),
                     dist  = rep( 0, n),
                     geo   = rep( 0, n),
                     opac  = rep( 1, n),
                     color = rep("", n))
      
    k <- 0
    for (i in (2:nrow(geoTab())))
    {
      for (j in 1:(i-1))
      {
        k <- k + 1
          
        df$lat1 [k] <- geoTab()$lat [i]
        df$lat2 [k] <- geoTab()$lat [j]
          
        df$long1[k] <- geoTab()$long[i]
        df$long2[k] <- geoTab()$long[j]
          
        df$dist [k] <- as.numeric(aggrMat()[i,j])
          
        x <- data.frame(lon=c(geoTab()$long[i], geoTab()$long[j]),
                        lat=c(geoTab()$lat [i], geoTab()$lat [j]))
        df$geo[k] <- geodist(x, sequential = TRUE, measure = "geodesic") / 1000
      }
    }
    
    return(df)
  })

  networkGeo5 <- reactive(
  {
    req((input$navBar == "maps") & (input$selClass5 == "Network map"))

    dup <- duplicatedxy(geoTab()$lat, geoTab()$long)
    indices <- which(dup == F)
    
    aggrMat <- aggrMat()[indices,indices]
    geoTab  <- geoTab ()[indices,]
    
    n  <- (nrow(geoTab) * (nrow(geoTab)-1)) / 2
    rw <- c(min(geoTab$lat), max(geoTab$lat), min(geoTab$long), max(geoTab$long))
    nb <- getNbrs(deldir(geoTab$lat, geoTab$long, rw=rw))

    df <- data.frame(lat1  = rep( 0, n),
                     lat2  = rep( 0, n),
                     long1 = rep( 0, n),
                     long2 = rep( 0, n),
                     dist  = rep( 0, n),
                     geo   = rep( 0, n),
                     opac  = rep( 1, n),
                     color = rep("", n))

    k <- 0
    for (i in (2:nrow(geoTab)))
    {
      for (j in 1:(i-1))
      {
        if (is.element(j, nb[[i]]))
        {
          k <- k + 1
          
          df$lat1 [k] <- geoTab$lat [i]
          df$lat2 [k] <- geoTab$lat [j]
          
          df$long1[k] <- geoTab$long[i]
          df$long2[k] <- geoTab$long[j]
          
          df$dist [k] <- as.numeric(aggrMat[i,j])
          
          x <- data.frame(lon=c(geoTab$long[i], geoTab$long[j]),
                          lat=c(geoTab$lat [i], geoTab$lat [j]))
          df$geo[k] <- geodist(x, sequential = TRUE, measure = "geodesic") / 1000 
        }
      }
    }

    return(df[1:k,])
  })

  beamObj5 <- reactive(
  {
    req(beamGeo5())
      
    df <- beamGeo5()
      
    df       <- df[order(-df$dist),]
    df$dist  <- df$dist - min(df$dist)
      
    df$dist  <- df$dist / max(df$dist)
    df$dist  <- round2(df$dist * 255)
      
    colorX   <- format(as.hexmode(df$dist), width = 2)
    df$color <- paste0("#", colorX, colorX, colorX)
    
    return(df)
  })
  
  networkObj5 <- reactive(
  {
    req(networkGeo5())

    df <- networkGeo5()

    df       <- df[order(-df$dist),]
    df$dist  <- df$dist - min(df$dist)

    df$dist  <- df$dist / max(df$dist)
    df$dist  <- round2(df$dist * 255)

    colorX   <- format(as.hexmode(df$dist), width = 2)
    df$color <- paste0("#", colorX, colorX, colorX)

    return(df)
  })

  clusObj5 <- reactive(
  {
    if (length(input$replyMethod31)==0)
      method <- "average"
    else
      
    if (input$replyMethod31==  "Single-Linkage")
      method <- "single"
    else
        
    if (input$replyMethod31=="Complete-Linkage")
      method <- "complete"
    else
          
    if (input$replyMethod31=="UPGMA")
      method <- "average"
    else
            
    if (input$replyMethod31=="WPGMA")
      method <- "mcquitty"
    else
              
    if (input$replyMethod31=="Ward's")
      method <- "ward.D2"
    else {}
              
    clus <- hclust(aggrMatDist(), method=method)

    return(clus)
  })

  cophMatDist <- eventReactive(clusObj5(),
  {
    matCoph <- as.matrix(cophenetic(clusObj5()))
    matCoph[as.numeric(matCoph)==0] <- 1e-20
    return(stats::as.dist(matCoph)) 
                                   
  })

  multObj5 <- reactive(
  {
    req(nrow(aggrMatDist())>3)
    
    if (global$replyMethod32=="Classical")
    {
      if (!global$selCophenetic)
        fit <- cmdscale(aggrMatDist(), eig=TRUE, k=3)
      else
        fit <- cmdscale(cophMatDist(), eig=TRUE, k=3)
      
      coords <- as.data.frame(fit$points)
    }

    if (global$replyMethod32=="Kruskal's")
    {
      if (!global$selCophenetic)
        fit <- isoMDS(aggrMatDist(), k=3)
      else
        fit <- isoMDS(cophMatDist(), k=3)
        
      coords <- as.data.frame(fit$points)
    }

    if (global$replyMethod32=="Sammon's")
    {
      if (!global$selCophenetic)
        fit <- sammonMDS(aggrMatDist(), k=3)
      else
        fit <- sammonMDS(cophMatDist(), k=3)
      
      coords <- as.data.frame(fit$points)
    }

    if (global$replyMethod32=="t-SNE")
    {
      if (!global$selCophenetic)
        fit <- Rtsne(aggrMatDist(), check_duplicates=FALSE, pca=TRUE, perplexity=getPerplexity(), theta=0.5, dims=3)
      else
        fit <- Rtsne(cophMatDist(), check_duplicates=FALSE, pca=TRUE, perplexity=getPerplexity(), theta=0.5, dims=3)
      
      coords <- as.data.frame(fit$Y)
    }

    if (!is.null(fit))
    {
      explVar <- formatC(x=round2((cor(aggrMatDist(), mdsDist(coords))^2)*100, n=1), digits = 1, format = "f")
      contentOfMessage <- paste0("Variance explained by ", global$replyMethod32, " 3D MDS: ", explVar, "%.")
      showNotification(contentOfMessage, type = "message", duration = NULL)
      return(coords)      
    }
  })

  multCol5 <- reactive(
  {
    req(multObj5())
    
    if (length(multObj5()) >0)
      coords <- multObj5()
    else
      return(NULL)
    
    coords$V1 <- scaleCoords(coords$V1)
    coords$V2 <- scaleCoords(coords$V2)
    coords$V3 <- scaleCoords(coords$V3)
    
    if ((length(input$selInv)>0) && is.element("1", input$selInv))
      coords$V1 <- max(coords$V1) - coords$V1
    
    if ((length(input$selInv)>0) && (is.element("2", input$selInv)))
      coords$V2 <- max(coords$V2) - coords$V2
    
    if ((length(input$selInv)>0) && (is.element("3", input$selInv)))
      coords$V3 <- max(coords$V3) - coords$V3
    
    if ((length(input$selR)==0) || (input$selR=="1"))
      colorR <- format(as.hexmode(coords$V1), width = 2)
    if ((length(input$selR) >0) && (input$selR=="2"))
      colorR <- format(as.hexmode(coords$V2), width = 2)
    if ((length(input$selR) >0) && (input$selR=="3"))
      colorR <- format(as.hexmode(coords$V3), width = 2)
    
    if ((length(input$selG) >0) && (input$selG=="1"))
      colorG <- format(as.hexmode(coords$V1), width = 2)
    if ((length(input$selG)==0) || (input$selG=="2"))
      colorG <- format(as.hexmode(coords$V2), width = 2)
    if ((length(input$selG) >0) && (input$selG=="3"))
      colorG <- format(as.hexmode(coords$V3), width = 2)
    
    if ((length(input$selB) >0) && (input$selB=="1"))
      colorB <- format(as.hexmode(coords$V1), width = 2)
    if ((length(input$selB) >0) && (input$selB=="2"))
      colorB <- format(as.hexmode(coords$V2), width = 2)
    if ((length(input$selB)==0) || (input$selB=="3"))
      colorB <- format(as.hexmode(coords$V3), width = 2)
    
    return(paste0("#", colorR, colorG, colorB))
  })
  
  selProvider5 <- function()
  {
    if (input$selMap5 == "CartoDB Positron")
      return(providers$CartoDB.Positron)

    if (input$selMap5 == "CartoDB Positron No Labels")
      return(providers$CartoDB.PositronNoLabels)

    if (input$selMap5 == "CartoDB Voyager")
      return(providers$CartoDB.Voyager)

    if (input$selMap5 == "CartoDB Voyager No Labels")
      return(providers$CartoDB.VoyagerNoLabels)

    if (input$selMap5 == "Esri World Terrain")
      return(providers$Esri.WorldTerrain)

    if (input$selMap5 == "Esri World Gray Canvas")
      return(providers$Esri.WorldGrayCanvas)
  }

  showPoints  <- function(df, Colors, selBorder, radius, selProvider)
  {
  # labOpts <- labelOptions(noHide = T, textOnly = T, direction = "left", offset = c(-9, 0))
    labOpts <- NULL

    m <- leaflet() %>%
      fitBounds(min(df$long), min(df$lat), max(df$long), max(df$lat)) %>%
      addProviderTiles(provider     = do.call(selProvider, args = list())) %>%

      addCircleMarkers(lng          = df$long,
                       lat          = df$lat,
                       label        = df[,1],
                       labelOptions = labOpts,
                       radius       = radius,
                       fillColor    = Colors,
                       fillOpacity  = 1,
                       stroke       = selBorder,
                       color        = "#000000",
                       weight       = 1,
                       opacity      = 1)

    return(m)
  }

  plotBeam5 <- function()
  {
    req(input$dotRadiusBeam)
    
  # labOpts <- labelOptions(noHide = T, textOnly = T, direction = "left", offset = c(-9, 0))
    labOpts <- NULL

    lines_list <- lapply(1:nrow(beamObj5()), function(i) 
    {
      st_linestring(matrix(c(
        beamObj5()$long1[i], beamObj5()$lat1[i],
        beamObj5()$long2[i], beamObj5()$lat2[i]
      ), ncol = 2, byrow = TRUE))
    })
    
    lines_sf <- st_sf(
      geometry = st_sfc(lines_list, crs = 4326),
      color    = beamObj5()$color,
      opac     = beamObj5()$opac
    )

    m <- leaflet() %>%
      fitBounds(min(geoTab()$long), min(geoTab()$lat), max(geoTab()$long), max(geoTab()$lat)) %>%
      addProviderTiles(provider = selProvider5()) %>%
      addPolylines(data = lines_sf, color = ~color, opacity = ~opac, weight = global$beamWeight, noClip = TRUE)

    if (input$dotRadiusBeam > 0)
      m <- m %>% addCircleMarkers(lng          = geoTab()$long,
                                  lat          = geoTab()$lat,
                                  label        = geoTab()[,1],
                                  labelOptions = labOpts,
                                  radius       = input$dotRadiusBeam,
                                  color        = "blue",
                                  fillOpacity  = 1,
                                  stroke       = FALSE)
    
    return(m)
  }
  
  plotNetwork5 <- function()
  {
    req(input$dotRadiusNetwork)

  # labOpts <- labelOptions(noHide = T, textOnly = T, direction = "left", offset = c(-9, 0))
    labOpts <- NULL

    lines_list <- lapply(1:nrow(networkObj5()), function(i) 
    {
      st_linestring(matrix(c(
        networkObj5()$long1[i], networkObj5()$lat1[i],
        networkObj5()$long2[i], networkObj5()$lat2[i]
      ), ncol = 2, byrow = TRUE))
    })
    
    lines_sf <- st_sf(
      geometry = st_sfc(lines_list, crs = 4326),
      color    = networkObj5()$color,
      opac     = networkObj5()$opac
    )
    
    m <- leaflet() %>%
         fitBounds(min(geoTab()$long), min(geoTab()$lat), max(geoTab()$long), max(geoTab()$lat)) %>%
         addProviderTiles(provider = selProvider5()) %>%
         addPolylines(data = lines_sf, color = ~color, opacity = ~opac, weight = global$beamWeight, noClip = TRUE)

    if (input$dotRadiusNetwork > 0)
      m <- m %>% addCircleMarkers(lng          = geoTab()$long,
                                  lat          = geoTab()$lat,
                                  label        = geoTab()[,1],
                                  labelOptions = labOpts,
                                  radius       = input$dotRadiusNetwork,
                                  color        = "blue",
                                  fillOpacity  = 1,
                                  stroke       = FALSE)

    return(m)
  }

  plotClus5 <- function()
  {
    req(input$dotRadiusArea5)

    if (length(multCol5())==0)
      return(NULL)
    
    if (length(input$nColGroups) == 0)
      nColGroups <- nGroups(clusObj5())
    else
      nColGroups <- input$nColGroups

    groupings <- cutree(clusObj5(), nColGroups)
    
    partfile <- data.frame(variety=geoTab()[,1], group=as.character(groupings))
    write.table(partfile, file = paste0(tempDir, "area_map.tsv"), quote = F, row.names=F, col.names=T, sep="\t")
    
    if (nColGroups >  1)
    {
      df <- data.frame(hex = multCol5(), group = groupings)
      df <- colouR::avgHex(df, hex_col = "hex", group_col = "group")
    
      if (global$replyColors3)
        df$avg_color <- compHex(df$avg_color, groupings)
      else {}

      Colors <- rep(NA, length(groupings))

      for (i in 1:nrow(df))
      {
        index <- which(groupings==i)
        Colors[index] <- df$avg_color[i]
      }
    }
    else
      Colors <- rep("#000000", length(groupings))

    m <- showPoints(geoTab(), Colors, global$selBorder5, input$dotRadiusArea5, "selProvider5")

    if ((length(input$showLegend)>0) && input$showLegend && (length(global$legendLabels) > 1) && (any(global$legendLabels!="")))
    {
      if (global$posLegend=="tl")
        posLegend <- "topleft"
      if (global$posLegend=="bl")
        posLegend <- "bottomleft"
      if (global$posLegend=="tr")
        posLegend <- "topright"
      if (global$posLegend=="br")
        posLegend <- "bottomright"

      m <- m %>% addLegend(position = posLegend,
                           colors   = df$avg_color,
                           labels   = global$legendLabels,
                           opacity  = 1,
                           title    = NULL)
    }

    return(m)
  }

  scaleCoords <- function(coords)
  {
    coords <- coords+abs(min(coords))
    return(round2((coords/max(coords)) * 255))
  }

  plotMult5 <- function()
  {
    req( input$selR,  input$selG,  input$selB)
    req(global$selR, global$selG, global$selB)

    req(input$dotRadiusRGB)

    if (length(multCol5())==0)
      return(NULL)
    
    m <- showPoints(geoTab(), multCol5(), global$selBorder5, input$dotRadiusRGB, "selProvider5")

    return(m)
  }

  plotRef5 <- function()
  {
    req(input$selRef)

    req(input$dotRadiusRef)

    selRef <- which(row.names(aggrMat())==input$selRef)

    if ((length(selRef)==0) || (selRef > nrow(aggrMat())))
      selRef <- 1

    df <- data.frame(dist = as.numeric(aggrMat()[,selRef]))

    df$dist <- df$dist/max(df$dist)
    df$dist <- round2(df$dist * 100)

    pal <- colorRampPalette(c("#ff0000", "#f88208", "#f8d408", "#bef808", "#08f85b", "#08f8df", "#08b3f8", "#080ef8"))
    pal <- pal(101)

    Colors <- rep("", nrow(df))

    for (i in 1:nrow(df))
    {
      Colors[i] <- pal[df$dist[i]+1]
    }

    m <- showPoints(geoTab(), Colors, global$selBorder5, input$dotRadiusRef, "selProvider5")

    return(m)
  }

  plotGraph5 <- function()
  {
    req(aggrMat(), geoTab(), input$selClass5, plotGraph5)

    if (input$selClass5=="Beam map")
      plotBeam5()
    else

    if (input$selClass5=="Network map")
      plotNetwork5()
    else

    if ((nrow(aggrMat()) >= 3) && (input$selClass5=="Area map"))
      plotClus5()
    else

    if ((nrow(aggrMat()) >= 3) && (input$selClass5== "RGB map"))
      plotMult5()
    else

    if ((input$selClass5=="Reference point map"))
      plotRef5()
    else {}
  }

  output$graph5 <- renderLeaflet(
  {
    plotGraph5()
  })

  observe({
    updateNumericInput(session, 'selSize5', NULL, value = max(550, input$winHeight - 350), min = max(550, input$winHeight - 350), step = 50)
  })

  output$Graph5 <- renderUI(
  {
    leafletOutput("graph5", width = 0.6244378*input$winWidth, height = input$selSize5)
  })

  output$selFormat5 <- renderUI(
  {
    options <- c("JPG","PNG","PDF")
    
    if (input$selClass5=="Area map")
      options <- c(options, "TSV")
    
    selectInput('replyFormat5', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
  })

  output$selDPI5 <- renderUI(
  {
    options <- c("low","medium","high")
    selectInput('replyDPI5', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
  })

  dpi5 <- function()
  {
    req(input$replyDPI5)

    if (input$replyDPI5=="low")
      return(0.5)
    if (input$replyDPI5=="medium")
      return(1.0)
    if (input$replyDPI5=="high")
      return(2.0)
  }

  fileName5 <- function()
  {
    if (input$selClass5=="Beam map")
      type <- "beam"
    if (input$selClass5=="Network map")
      type <- "network"
    if (input$selClass5=="Area map")
      type <- "area"
    if (input$selClass5=="RGB map")
      type <- "RGB"
    if (input$selClass5=="Reference point map")
      type <- "reference_point"

    return(paste0(type, "_map.",input$replyFormat5))
  }

  output$downLoad5 <- downloadHandler(filename = fileName5, content = function(file)
  {
    grDevices::pdf(NULL)

    if (!is.null(aggrMat()))
    {
      if (input$replyFormat5!="TSV")
      {
        map5 <- plotGraph5()
        map5$x$options <- append(map5$x$options, list("zoomControl" = FALSE))
        saveWidget(map5, paste0(tempDir, "map.html"), selfcontained = FALSE)
        webshot2::webshot(paste0(tempDir, "map.html"), file = file, zoom = dpi5(), vwidth = 0.6244378*input$winWidth, vheight = input$selSize5)
      }
      else
        file.copy(paste0(tempDir, "area_map.tsv"), file, overwrite = T)
    }
    else {}

    grDevices::graphics.off()
  })

  ##############################################################################

  silhouette_min_dist <- function(dist_matrix, labels, noise_label = 0) 
  {
    # Convert dist object to matrix if needed
    if (!inherits(dist_matrix, "dist") && !is.matrix(dist_matrix)) 
    {
      stop("dist_matrix must be a matrix or 'dist' object")
    }
    if (inherits(dist_matrix, "dist")) 
    {
      dist_matrix <- as.matrix(dist_matrix)
    }
  
    n <- nrow(dist_matrix)
    
    # Exclude noise points (label 0)
    valid_idx <- labels != noise_label
    
    if (length(unique(valid_idx)) == 0)
      return(NULL)
    
    dist_matrix <- dist_matrix[valid_idx, valid_idx]
    labels <- labels[valid_idx]
    n_valid <- length(labels)
    
    sil_scores <- numeric(n_valid)
    
    for (i in 1:n_valid) 
    {
      own_cluster <- labels[i]
      
      # Intra-cluster distances (excluding self)
      same_cluster <- which(labels == own_cluster & seq_len(n_valid) != i)
      if (length(same_cluster) > 0) 
      {
        a_i <- min(dist_matrix[i, same_cluster])
      } 
      else 
      {
        a_i <- 0  # singleton cluster
      }

      # Inter-cluster minimum distance
      other_clusters <- unique(labels[labels != own_cluster])
      b_i <- Inf
 
      for (cl in other_clusters) 
      {
        cl_points <- which(labels == cl)
        b_i <- min(b_i, min(dist_matrix[i, cl_points]))
      }

      # Silhouette formula
      sil_scores[i] <- ifelse(max(a_i, b_i) == 0, 0, (b_i - a_i) / max(a_i, b_i))
    }

    return(round2(mean(sil_scores), 4))
  }

  silhouette_mean_dist <- function(dist_matrix, labels, noise_label = 0) 
  {
    # Convert dist object to matrix if needed
    if (!inherits(dist_matrix, "dist") && !is.matrix(dist_matrix)) 
    {
      stop("dist_matrix must be a matrix or 'dist' object")
    }
    if (inherits(dist_matrix, "dist")) 
    {
      dist_matrix <- as.matrix(dist_matrix)
    }
  
    n <- nrow(dist_matrix)
  
    # Exclude noise points (label 0)
    valid_idx <- labels != noise_label
    
    if (length(unique(valid_idx)) == 0)
      return(NULL)
    
    dist_matrix <- dist_matrix[valid_idx, valid_idx]
    labels <- labels[valid_idx]
    n_valid <- length(labels)
  
    sil_scores <- numeric(n_valid)
  
    for (i in 1:n_valid) 
    {
      own_cluster <- labels[i]

      # Intra-cluster distances (excluding self)
      same_cluster <- which(labels == own_cluster & seq_len(n_valid) != i)
      if (length(same_cluster) > 0) 
      {
        a_i <- mean(dist_matrix[i, same_cluster])
      } 
      else 
      {
        a_i <- 0  # singleton cluster
      }
  
      # Inter-cluster mean distances
      other_clusters <- unique(labels[labels != own_cluster])
      b_i <- Inf

      for (cl in other_clusters) 
      {
        cl_points <- which(labels == cl)
        b_i <- min(b_i, mean(dist_matrix[i, cl_points]))
      }

      # Silhouette formula
      sil_scores[i] <- ifelse(max(a_i, b_i) == 0, 0, (b_i - a_i) / max(a_i, b_i))
    }

    return(round2(mean(sil_scores), 4))
  }

  observeEvent(c(input$replyMethod31, input$replyMethod6, aggrMat()),
  {
    global$minPts6 <- NULL          
  })
  
  setMinPts <- function()
  {
    req(input$replyMethod31, input$replyMethod6, aggrMat())
    
    if (((input$replyMethod6=="Dynamic tree cut") | (input$replyMethod6=="HDBSCAN")) & (length(aggrMat()) > 0))
    {
      if (length(global$minPts6)==0)
      {
        n <- ncol(aggrMat())
        
        best_score     <- 0
        best_minPts    <- NA
        best_partition <- c()
        
        for (minPts in 2:(n-1)) 
        {
          if (input$replyMethod6=="Dynamic tree cut")
            partition <- cutreeDynamic(dendro         = clusObj3(),
                                       distM          = as.matrix(aggrMat()),
                                       method         = "hybrid",
                                       deepSplit      = 2,
                                       minClusterSize = minPts)
          
          if (input$replyMethod6=="HDBSCAN")
          {
            result <- hdbscan(as.dist(aggrMat()), minPts = minPts)
            partition <- result$cluster
          }
          
          partition0 <- partition        
          partition  <- partition[partition!=0]
          
          if (length(unique(partition)) > 0)
          {
            if (input$replyMethod6=="Dynamic tree cut")
            {
              if ((length(input$replyMethod31)>0) && (input$replyMethod31=="Single-Linkage"))
                score <- silhouette_min_dist (as.dist(aggrMat()), partition, 0) 
              else          
                score <- silhouette_mean_dist(as.dist(aggrMat()), partition, 0)
            }
            
            if (input$replyMethod6=="HDBSCAN")
            {
              sizes <- as.numeric(table(partition))           
              score <- weighted.mean(result$cluster_scores, sizes)
            }
            
            if (is.na(score))
              next
            
            if (score > best_score) 
            {
              best_score     <- score
              best_minPts    <- minPts
              best_partition <- partition0
            }
          }  
        }
        
        if (!is.na(best_minPts))
          global$minPts6 <- best_minPts
        else
          global$minPts6 <- 2
      }
      else
        global$minPts6 <- isolate(input$minPts6)        
    }
  }

  observeEvent(global$minPts6,
    updateNumericInput(
      session = session,
      inputId = 'minPts6',
      value   = isolate(global$minPts6)
    )
  )
  
  observeEvent(input$goButton6,
  {
    req(global$finished, input$replyMethod31)

    global$finished6   <- FALSE
    global$background6 <- NULL

    removeNotification(global$idNot)
    global$idNot <- showNotification(HTML("Running. This can take up to several hours..."), type = "message", duration = NULL)

    if (global$dataType=="distancetable")
    {
      # write files.txt
      write_tsv(data.frame(rownames(aggrMat())),
                file = paste0(tempDir, "files.txt"), col_names = F)
      
      # write items.txt
      write_tsv(data.frame("none"),
                file = paste0(tempDir, "items.txt"), col_names = F)
      
      # write individual.tsv
      
      n <- (nrow(aggrMat()) * (nrow(aggrMat()) - 1)) / 2
      
      df <- data.frame(
        var1 = rep("", n),
        var2 = rep("", n),
        item = rep("", n),
        dist = rep(NA, n)
      )
     
      k <- 0
      for (i in 2:nrow(aggrMat()))
      {
        for (j in 1:(i-1))
        {
          k <- k + 1
          
          df$var1[k] <- row.names(aggrMat())[i]
          df$var2[k] <- row.names(aggrMat())[j]
          df$item[k] <- "none"
          df$dist[k] <- aggrMat()[i,j]
        }
      }

      write_tsv(df, file = paste0(tempDir, "individual.tsv"), col_names = T, quote = "none", escape = "none")
    }

    Partition1 <- function()
    {
      if (global$replyMethod31==  "Single-Linkage")
        methodc <- 1

      if (global$replyMethod31=="Complete-Linkage")
        methodc <- 2

      if (global$replyMethod31=="UPGMA")
        methodc <- 3

      if (global$replyMethod31=="WPGMA")
        methodc <- 4

      if (global$replyMethod31=="Ward's")
        methodc <- 7

      if ( input$replyMethod6 =="Bootstrap")
        methodr <- 1

      if ( input$replyMethod6 =="Noise")
        methodr <- 2

      if (file.exists(paste0(   tempDir, "partition.csv")))
        system(paste0("rm -f ", tempDir, "partition.csv"))

      System2 <- function(methodc, methodr, degStab, tempDir)
      {
        system2(
          command = "./robust",
          args    = c("files.txt", "items.txt", "individual.tsv", methodc, methodr, degStab, tempDir),
          stdout  = TRUE,
          stderr  = TRUE
        )
      }

      global$background6 <- r_bg(
        func = System2,
        args = list(methodc, methodr, input$degStab6, tempDir)
      )
    }

    Partition2 <- function()
    {
      if (file.exists(paste0(   tempDir, "partition.csv")))
        system(paste0("rm -f ", tempDir, "partition.csv"))

      result <- apcluster(s=max(aggrMat()) - aggrMat())
      
      df <-  data.frame(
        cluster = 0,  
        variety = colnames(aggrMat())
      )
      
      for (i in 1:nrow(df))
      {
        for (j in 1:length(result@clusters))
        {
          if (is.element(i, result@clusters[[j]]))
            df$cluster[i] <- j
        }
      }
      
      write_csv(df, paste0(tempDir, "partition.csv"), col_names = F)
      
      global$background6 <- r_bg(
        func = function(){return(NULL)}
      )
    }
    
    Partition3 <- function()
    {
      setMinPts()
      
      if (file.exists(paste0(   tempDir, "partition.csv")))
        system(paste0("rm -f ", tempDir, "partition.csv"))

      if (input$replyMethod6=="Dynamic tree cut")
        partition <- cutreeDynamic(dendro         = clusObj3(),
                                   distM          = as.matrix(aggrMat()),
                                   method         = "hybrid",
                                   deepSplit      = 2,
                                   minClusterSize = global$minPts6)

      if  (input$replyMethod6=="HDBSCAN")
        partition <- hdbscan(as.dist(aggrMat()), minPts = global$minPts6)$cluster

      df <-  data.frame(
        cluster = partition,
        variety = colnames(aggrMat())
      )

      write_csv(df, paste0(tempDir, "partition.csv"), col_names = F)
      
      global$background6 <- r_bg(
        func = function(){return(NULL)}
      )
    }

    if ((input$replyMethod6=="Bootstrap") | 
        (input$replyMethod6=="Noise"))
      Partition1()  
    else
      
    if  (input$replyMethod6=="Affinity propagation")
      Partition2()
    else 
      
    if ((input$replyMethod6=="Dynamic tree cut") | 
        (input$replyMethod6=="HDBSCAN"))
      Partition3()
    else {}
  })

  observe(
  {
    req(global$background6)
    status <- global$background6$poll_io(0)["process"]

    if  (status == "timeout")
      invalidateLater(1000)

    if ((status == "ready") & !global$finished6)
    {
      messages <- global$background6$get_result()

      if (length(messages)>0)
      {
        showNotification(HTML(paste(messages, sep = "", collapse = "<br>")), type = "message", duration = NULL)
      }

      removeNotification(global$idNot)

      if (file.exists(paste0(tempDir, "partition.csv")))
        global$partition <- read.csv(paste0(tempDir, "partition.csv"), header = FALSE, quote="")
      else
        return(NULL)
      
      nAll    <- nrow(global$partition)
      nClass  <- length(global$partition$V1[global$partition$V1>0])
      nGroups <- max(global$partition$V1)

      showNotification(paste(nClass, "varieties out of", nAll, "are classified into", nGroups, "groups."), type = "message", duration = NULL)

      silhMin  <- silhouette_min_dist (dist_matrix=as.dist(aggrMat()), labels=global$partition$V1, noise_label=0)
      silhMean <- silhouette_mean_dist(dist_matrix=as.dist(aggrMat()), labels=global$partition$V1, noise_label=0)
      
      showNotification(paste0("Silhouette: ", silhMin, " (min. dist.), ", silhMean, " (mean dist.)"), type = "message", duration = NULL)

      insertUI(
        selector = "#goButton",
        where    = "afterEnd",
        ui       = tags$audio(src = "ready.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;")
      )

      showNotification("Ready!", type = "message", duration = NULL)
      global$finished6   <- TRUE
      global$background6 <- NULL
    }
  })

  output$degStab6 <- renderUI(
  {
    req(input$replyMethod6)
    
    if ((input$replyMethod6=="Bootstrap") | (input$replyMethod6=="Noise"))
    {
      tagList(
        bsButton("butDegStab6", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modDegStab6", "Degree of stability", "butDegStab6", size = "large", "Dit is de helptekst"),
      
        HTML("<span style='font-weight: bold;'>&nbsp;Degree of stability:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(
          inputId = "degStab6",
          label   = NULL,
          value   = isolate(global$degStab6),
          min     = 1,
          max     = 5,
          step    = 1,
          width   = "100px")),
        
        br()
      )
    }
  })
  
  observeEvent(input$degStab6,
  {
    global$degStab6 <- input$degStab6
  })

  output$minPts6 <- renderUI(
  {
    req(input$replyMethod6)
      
    if ((input$replyMethod6=="Dynamic tree cut") | (input$replyMethod6=="HDBSCAN"))
    {
      if (length(isolate(global$minPts6)) > 0)
        v <- isolate(global$minPts6)
      else
        v <- NULL
      
      tagList(
        bsButton("butMinPts6", label = NULL, icon = icon("info"), size = "extra-small"),
        bsModal ("modMinPts6", "Minimum cluster size", "butMinPts6", size = "large", "Dit is de helptekst"),
          
        HTML("<span style='font-weight: bold;'>&nbsp;Minimum cluster size:</span>"),
        div(style='height: 12px'),
        div(style='margin-left: 60px', numericInput(
          inputId = "minPts6",
          label   = NULL,
          value   = v,
          min     = 2,
          max     = NA,
          step    = 1,
          width   = "100px")),
          
        br()
      )
    }
  })
  
  output$dotRadiusArea6 <- renderUI(
  {
    tagList(
      bsButton("butDotRadiusArea6", label = NULL, icon = icon("info"), size = "extra-small"),
      bsModal ("modDotRadiusArea6", "Radius of dot", "butDotRadiusArea6", size = "large", "Dit is de helptekst"),

      HTML("<span style='font-weight: bold;'>&nbsp;Radius of dot:</span>"),
      div(style='height: 12px'),
      div(style='margin-left: 60px', numericInput(
        inputId = "dotRadiusArea6",
        label   = NULL,
        value   = isolate(global$dotRadiusArea6),
        min     = 1,
        max     = 20,
        step    = 1,
        width   = "100px"))
    )
  })

  observeEvent(input$dotRadiusArea6,
  {
    global$dotRadiusArea6 <- input$dotRadiusArea6
  })

  output$selBorder6 <- renderUI(
  {
    tagList(
      div(style='margin-left: 60px', checkboxInput(inputId = "selBorder6",
                                                   label   = "show border",
                                                   value   = isolate(global$selBorder6))),
      br()
    )
  })
  
  observeEvent(input$selBorder6,
  {
    global$selBorder6 <- input$selBorder6
  })
  
  selProvider6 <- function()
  {
    if (input$selMap6 == "CartoDB Positron")
      return(providers$CartoDB.Positron)

    if (input$selMap6 == "CartoDB Positron No Labels")
      return(providers$CartoDB.PositronNoLabels)

    if (input$selMap6 == "CartoDB Voyager")
      return(providers$CartoDB.Voyager)

    if (input$selMap6 == "CartoDB Voyager No Labels")
      return(providers$CartoDB.VoyagerNoLabels)

    if (input$selMap6 == "Esri World Terrain")
      return(providers$Esri.WorldTerrain)

    if (input$selMap6 == "Esri World Gray Canvas")
      return(providers$Esri.WorldGrayCanvas)
  }

  plotGraph6 <- function()
  {
    req(aggrMat(), geoTab(), input$selMap6, global$partition, input$dotRadiusArea6, multCol5())

    groupings <- global$partition$V1

    partfile <- data.frame(variety=geoTab()[,1], group=as.character(groupings))
    write.table(partfile, file = paste0(tempDir, "partition_map.tsv"), quote = F, row.names=F, col.names=T, sep="\t")

    df <- data.frame(hex = multCol5(), group = groupings)
    df <- colouR::avgHex(df, hex_col = "hex", group_col = "group")
    
    if (global$replyColors3)
      df$avg_color <- compHex(df$avg_color, groupings)
    else {}

    Colors <- rep(NA, length(groupings))
    
    for (i in 0:nrow(df))
    {
      index <- which(groupings==i)
      
      if (length(index) > 0)
      {
        if (i>0)
          Colors[index] <- df$avg_color[i]
        else
          Colors[index] <- "#ffffff"
      }
    }

    if (!global$selBorder6)
      index <- which(groupings > 0)
    if ( global$selBorder6 | (length(index)==0))
      index <- 1:length(groupings)
    
    m <- showPoints(geoTab()[index,], Colors[index], global$selBorder6, input$dotRadiusArea6, "selProvider6")

    return(m)
  }

  output$graph6 <- renderLeaflet(
  {
    plotGraph6()
  })

  observe({
    updateNumericInput(session, 'selSize6', NULL, value = max(550, input$winHeight - 350), min = max(550, input$winHeight - 350), step = 50)
  })

  output$Graph6 <- renderUI(
  {
    leafletOutput("graph6", width = 0.6244378*input$winWidth, height = input$selSize6)
  })

  output$selFormat6 <- renderUI(
  {
    options <- c("JPG","PNG","PDF","TSV")
    selectInput('replyFormat6', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
  })

  output$selDPI6 <- renderUI(
  {
    options <- c("low","medium","high")
    selectInput('replyDPI6', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
  })

  dpi6 <- function()
  {
    req(input$replyDPI6)

    if (input$replyDPI6=="low")
      return(0.5)
    if (input$replyDPI6=="medium")
      return(1.0)
    if (input$replyDPI6=="high")
      return(2.0)
  }

  fileName6 <- function()
  {
    return(paste0("partition_map.",input$replyFormat6))
  }

  output$downLoad6 <- downloadHandler(filename = fileName6, content = function(file)
  {
    grDevices::pdf(NULL)

    if (!is.null(global$partition))
    {
      if (input$replyFormat6!="TSV")
      {
        map6 <- plotGraph6()
        map6$x$options <- append(map6$x$options, list("zoomControl" = FALSE))
        saveWidget(map6, paste0(tempDir, "map.html"), selfcontained = FALSE)
        webshot2::webshot(paste0(tempDir, "map.html"), file = file, zoom = dpi6(), selector = "#htmlwidget_container", vwidth = 0.6244378*input$winWidth, vheight = input$selSize6)
      }
      else
        file.copy(paste0(tempDir, "partition_map.tsv"), file, overwrite = T)
    }
    else {}

    grDevices::graphics.off()
  })

  ##############################################################################

  removeNA <- function(dist)
  {
    dist[dist==1] <- NA

    repeat
    {
      nna <- apply(dist, 1, function(x) sum(is.na(x)))
      nnaMax <- max(nna)

      if (nnaMax > 0)
      {
        index <- as.numeric(which(nna==nnaMax)[1])

        dist <- dist[ -index,]
        dist <- dist[,-index ]
      }
      else
        break
    }

    return(dist)
  }

  segmMatVDist <- eventReactive(segmMatV(),
  {
    matSegm <- apply(segmMatV(), 2, function(x) as.numeric(x))
    matSegm[matSegm==0] <- 1e-20
  # matSegm <- removeNA(matSegm)
    return(stats::as.dist(matSegm))
  })

  segmMatCDist <- eventReactive(segmMatC(),
  {
    matSegm <- apply(segmMatC(), 2, function(x) as.numeric(x))
    matSegm[matSegm==0] <- 1e-20
  # matSegm <- removeNA(matSegm)
    return(stats::as.dist(matSegm))
  })

  segmMatDist <- reactive(
  {
    if (input$selSegment4=="Vowels")
      return(segmMatVDist())
    else

    if (input$selSegment4=="Consonants")
      return(segmMatCDist())
    else {}
  })

  getPerplexity4 <- function()
  {
    if (attributes(segmMatDist())$Size < 91)
      return((attributes(segmMatDist())$Size-1) %/% 3)
    else
      return(30)
  }
  
  multObj4 <- reactive(
  {
    req(nrow(aggrMatDist())>2) 
    
    if (input$replyMethod42=="Classical")
    {
      fit <- cmdscale(segmMatDist(), eig=TRUE, k=2)
      coords <- as.data.frame(fit$points)
    }

    if (input$replyMethod42=="Kruskal's")
    {
      fit <- isoMDS(segmMatDist(), k=2)
      coords <- as.data.frame(fit$points)
    }

    if (input$replyMethod42=="Sammon's")
    {
      fit <- sammonMDS(segmMatDist(), k=2)
      coords <- as.data.frame(fit$points)
    }

    if (input$replyMethod42=="t-SNE")
    {
      fit <- Rtsne(segmMatDist(), check_duplicates=FALSE, pca=TRUE, perplexity=getPerplexity4(), theta=0.5, dims=2)
      coords <- as.data.frame(fit$Y)
    }

    if (!is.null(fit))
    {
      explVar <- formatC(x=round2((cor(segmMatDist(), mdsDist(coords))^2)*100, n=1), digits = 1, format = "f")
      contentOfMessage <- paste0("Variance explained by ", input$replyMethod42, " 2D MDS: ", explVar, "%.")
      showNotification(contentOfMessage, type = "message", duration = NULL)
      return(coords)      
    }
  })

  output$selMethod4 <- renderUI(
  {
    tagList(
      br(),

      bsButton("butReplyMethod42", label = NULL, icon = icon("info"), size = "extra-small"),
      bsModal ("modReplyMethod42", "MDS method", "butReplyMethod42", size = "large", "Dit is de helptekst"),

      HTML("<span style='font-weight: bold;'>&nbsp;MDS method:</span>"),
      div(style='height: 12px'),
      div(style='margin-left: 60px', radioButtons('replyMethod42',
                                     NULL,
                                     c("Classical", "Kruskal's", "Sammon's", "t-SNE"),
                                     selected="Kruskal's",
                                     inline=FALSE))
    )
  })

  output$selMult4 <- renderUI(
  {
    tagList(
      br(),

      bsButton("butMdsGeon42", label = NULL, icon = icon("info"), size = "extra-small"),
      bsModal ("modMdsGeon42", "Axes", "butMdsGeon42", size = "large", "Dit is de helptekst"),

      HTML("<span style='font-weight: bold;'>&nbsp;Axes:</span>"),
      div(style='height: 12px'),
      div(style='margin-left: 60px', checkboxGroupInput("mdsGeon42",
                                                        NULL,
                                                        c("X⇄Y","inv. X","inv. Y"),
                                                        selected=NULL,
                                                        inline=TRUE))
    )
  })

  fontSize4 <- reactive(
  {
    req(input$selSegment4)

    nv <- attr(segmMatDist(), "Size")

    if (nv> 45) fs <-  9 else
    if (nv> 30) fs <- 11 else
    if (nv> 10) fs <- 14 else
    if (nv>  1) fs <- 17 else {}

    return(fs)
  })

  plotMult4 <- function()
  {
    if (length(multObj4()) >0)
      coords <- multObj4()
    else
      return(NULL)
    
    req(nrow(multObj4())==nrow(segmMatDist()))
    
    if ((length(input$mdsGeon42)==0) || (!is.element("X⇄Y", input$mdsGeon42)))
    {
      Xlab <- "dimension 1"
      Ylab <- "dimension 2"
    }
    else
    {
      colnames(coords)[1] <- "V0"
      colnames(coords)[2] <- "V1"
      colnames(coords)[1] <- "V2"

      Xlab <- "dimension 2"
      Ylab <- "dimension 1"
    }

    if (is.element("inv. X", input$mdsGeon42))
      coords$V1 <- -1 * coords$V1

    if (is.element("inv. Y", input$mdsGeon42))
      coords$V2 <- -1 * coords$V2

    fs <- as.numeric(input$replyPoint4)/2.8346438836889

    gp <- ggplot(coords, aes(V1, V2, label = row.names(as.matrix(segmMatDist())))) +
          geom_text (family=input$replyFont4, size = fs, family=input$replyFont4, show.legend=FALSE)

    gp <- gp +
          scale_x_continuous(breaks = 0, limits = c(min(coords$V1-0.01,coords$V2-0.01), max(coords$V1+0.01,coords$V2+0.01))) +
          scale_y_continuous(breaks = 0, limits = c(min(coords$V1-0.01,coords$V2-0.01), max(coords$V1+0.01,coords$V2+0.01))) +
          xlab(Xlab) + ylab(Ylab) +
          theme_bw() +
          theme(text           =element_text(size=as.numeric(input$replyPoint4), family=input$replyFont4),
                plot.title     =element_text(face="bold", hjust = 0.5),
                axis.title     =element_text(face="bold"),
                legend.title   =element_blank(),
                legend.text    =element_text(size=as.numeric(input$replyPoint4)),
                legend.key.size=unit(1.5,'lines'),
                aspect.ratio   =1)

    print(gp)
  }

  plotGraph4 <- function()
  {
    req(segmMatDist())

    if ((input$segmDist!="plain  sub. = 1 indel = 0.5") & (input$segmDist!="plain  cost = 1") & (attr(segmMatDist(),"Size") >= 3))
      plotMult4()
    else
      return(NULL)
  }

  output$graph4 <- renderPlot(height = "auto", res = 72,
  {
    plotGraph4()
  })

  output$Graph4 <- renderUI(
  {
    plotOutput("graph4", width = 0.6244378*input$winWidth, height = max(550, input$winHeight - 350))
  })

  output$selFont4 <- renderUI(
  {
    selectInput('replyFont4', label=NULL, choices=Fonts, selected = "DejaVu Sans", selectize=FALSE, multiple=FALSE)
  })

  output$selPoint4 <- renderUI(
  {
    options <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48,54,60)
    selectInput('replyPoint4', label=NULL, options, selected = fontSize4(), selectize=FALSE, multiple=FALSE)
  })

  output$selFormat4 <- renderUI(
  {
    options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
    selectInput('replyFormat4', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
  })

  output$selDPI4 <- renderUI(
  {
    options <- c("150 dpi","300 dpi","600 dpi")
    selectInput('replyDPI4', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
  })

  dpi4 <- function()
  {
    req(input$replyDPI4)

    if (input$replyDPI4=="150 dpi")
      return(150)
    if (input$replyDPI4=="300 dpi")
      return(300)
    if (input$replyDPI4=="600 dpi")
      return(600)
  }

  fileName4 <- function()
  {
    if (input$selSegment4=="Vowels")
      type <- "vowel"
    if (input$selSegment4=="Consonants")
      type <- "consonant"

    return(paste0(type, "_plot.", input$replyFormat4))
  }

  output$downLoad4 <- downloadHandler(filename = fileName4, content = function(file)
  {
    grDevices::pdf(NULL)

    width  <- 0.6244378*input$winWidth
    height <- max(550, input$winHeight - 350)

    width  <- convertUnit(x=unit(width , "pt"), unitTo="in", valueOnly=TRUE)
    height <- convertUnit(x=unit(height, "pt"), unitTo="in", valueOnly=TRUE)

    if (!is.null(aggrMat()))
      plot <- plotGraph4()
    else
      plot <- ggplot()+theme_bw()

    if (input$replyFormat4=="JPG")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi4(), limitsize=F, device="jpeg")
    else
    if (input$replyFormat4=="PNG")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi4(), limitsize=F, device="png" )
    else
    if (input$replyFormat4=="SVG")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi4(), limitsize=F, device="svg" )
    else
    if (input$replyFormat4=="EPS")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi4(), limitsize=F, device=grDevices::cairo_ps )
    else
    if (input$replyFormat4=="PDF")
      ggsave(filename=file, plot=plot, scale=1, width=width, height=height, units="in", dpi=dpi4(), limitsize=F, device=grDevices::cairo_pdf)
    else
    if (input$replyFormat4=="TEX")
    {
      tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
      print(plot)
    }
    else {}

    grDevices::graphics.off()
  })
}

################################################################################

options(shiny.sanitize.errors = TRUE)
options(shiny.usecairo=FALSE)
options(shiny.maxRequestSize=100*1024^2)

shinyApp(ui = ui, server = server)

################################################################################
