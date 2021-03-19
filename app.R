source('lib/config.R')

source('lib/db_functions.R')
source('lib/UsefulFunctions.R')

# for connection from windows computer, ran locally
#pool <- dbPool(drv = odbc::odbc(), dsn = 'SFT_64', encoding = 'windows-1252')

pool<-dbConnect(RPostgres::Postgres(), dbname = postgres_database, user=postgres_user)

spdat <- getSpeciesData(pool)

# get matching species
speciesMatch <- getMatchSpecies(pool)
speciesMatchScientificNames <- getListBirdsUrl(bird_list_id)
#speciesMatchScientificNames <- getListSpeciesBirdsScientificName(pool)

#spdat <<- getSpeciesDataMongo()

library(readxl)
rangedat <- as.data.frame(read_excel('SpeciesWithShorterTimePeriods.xls', sheet = 'N vs S', skip = 3))
rangedat$Latlimit <- as.numeric(gsub('[[:alpha:]]|[[:punct:]]|[[:blank:]]', '', rangedat$Latitudgräns))
rangedat$smaller <- grepl('S', rangedat$Latitudgräns)
startyr <- as.data.frame(read_excel('SpeciesWithShorterTimePeriods.xls', sheet = 'StartYear'))
startyr$Delprogram[startyr$Delprogram=='SomPKT'] <- 'totalsommar_pkt'
startyr$Delprogram[startyr$Delprogram=='Standard'] <- 'totalstandard'
startyr$Delprogram[startyr$Delprogram=='VinPKT'] <- 'totalvinter_pkt'


 ## Not  sure this is needed (see https://shiny.rstudio.com/articles/pool-basics.html)
onStop(function() {
  poolClose(pool)
})


ui <- fluidPage(theme = 'flatly',
                tags$head(
                  tags$style(
                    HTML(
                      "
                            .multicol6 {
                                height:auto;
                                -webkit-column-count: 6;
                                -moz-column-count: 6;
                                column-count: 6;
                                -moz-column-fill: balanced;
                                column-fill: balanced;
                            }
                            .multicol8 {
                                height:auto;
                                -webkit-column-count: 8;
                                -moz-column-count: 8;
                                column-count: 8;
                                -moz-column-fill: balanced;
                                column-fill: balanced;
                            }  
                            .checkbox {
                                margin-top: 0px;
                                -webkit-margin-after: 1px;
                                margin-after: 0px;
                            }
                            .checkbox input, .checkbox label {
                                vertical-align: middle;
                                padding-bottom: 1px;
                                padding-top: 1px;
                                line-height: normal;
                            }
                            # .checkbox label{
                                #line-height: normal;
                                # padding-bottom: 1px;
                            #    # text-align: center;
                            # }
                            "
                    )
                  )
                ),
                titlePanel(title = div(img(style = 'display: inline-block;', src = "fageltaxering-logo2x.png", height = 80 , width = 240),
                                       p(style = 'display: inline-block; margin: auto; width: 60%; text-align: center; font-size: 1.5em;',
                                         'Åkes superTRIMprogram'))),
                # titlePanel(title = div(style = 'margin: auto; padding-bottom: 50px', p(style = 'display: block; text-align: center; font-size: 1.5em;',
                #                          img(style = 'float: left; margin-bottom: 100px;', src = "fageltaxering-logo2x.png", height = 80 , width = 240),
                #                          'Åkes superTRIMprogram'))),
                tabsetPanel(
                  tabPanel('Get data',
                           radioButtons('databasechoice', label = 'Select the database',
                                        choices = list(`Good old sft database on PSQL` = 'psql',
                                                       `Brand new mongoDB` = 'mongodb'),
                                        selected = 'mongodb'),
                           radioButtons('tabsel', label = 'Select monitoring scheme',
                                        choices = list(Standardrutter = 'totalstandard',
                                                       Sommarpunktrutter = 'totalsommar_pkt',
                                                       Vinterpunktrutter =  'totalvinter_pkt',
                                                       `Sjöfågeltaxering Vår` = 'totalvatmark',
                                                       `IWC Januari` = 'total_iwc_januari',
                                                       `IWC September` = 'total_iwc_september',
                                                       `Miscellaneous system` = 'misc_census'),
                                        selected = 'totalstandard', inline = FALSE, width = NULL),
                           conditionalPanel(condition = 'input.tabsel == "totalstandard"',
                                            radioButtons('linepoint', label = 'Select subscheme',
                                                         choices = list(Lines = TRUE,
                                                                        Points = FALSE),
                                                         selected = TRUE, inline = TRUE)
                                            ),
                           conditionalPanel(condition = 'input.tabsel == "totalvinter_pkt"',
                                            checkboxGroupInput('specper', label = 'Select period(s)',
                                                               choices = list(`Period 1`= 1,
                                                                              `Period 2`= 2,
                                                                              `Period 3`= 3,
                                                                              `Period 4`= 4,
                                                                              `Period 5`= 5),
                                                               selected = 3, inline = TRUE)
                                            ),
                           hr(),
                           uiOutput('yrSlider'),
                           hr(),
                           radioButtons('specsp', label = 'Select species set',
                                        choices = list(`All available bird species` = 'all',
                                                       `All available mammal species` = 'mammals',
                                                       `Farmland Bird Index` = 'FBI',
                                                       `Environmental Objective 13` = 'eo13',
                                                       `Fredriks urval IWC Januari` = 'iwcjan',
                                                       `Individual species` = 'ind'),
                                        selected = 'all'),
                           conditionalPanel(condition = 'input.specsp == "ind"',
                                            uiOutput('specCheckbox')),
                           hr(),
                           fluidRow(column(6,
                                           checkboxGroupInput('savedat',
                                              label = 'Data output to (will always be available in app)?', 
                                              choices = list(`R Workspace` = 1,
                                                             `.csv-file` = 2,
                                                             `.csv-file (xls-friendly)` = 3,
                                                             `.rdata-file` = 4),
                                              selected = 1, inline = TRUE)
                                           ),
                                    column(6,
                                           textInput('filenameDat', label = 'Enter filename:', value = 'Dataextraction_All')
                                           )
                                    ),
                           hr(),
                           actionButton("sendquery", "Submit query"),
                           hr(),
                           #verbatimTextOutput('testtext'),
                           withSpinner(DT::dataTableOutput("dataTable"), proxy.height = '150px')
                  ),
                  tabPanel('Analyze data',
                           radioButtons('modeltype', label = 'Modeltype',
                                        choices = list(`Model 1` = 1,
                                                       `Model 2` = 2,
                                                       `Model 3` = 3),
                                        selected = 2, inline = TRUE),
                           hr(),
                           checkboxGroupInput('trimset', label = 'Trim settings',
                                        choices = list(Overdispersion = 'od',
                                                       `Serial correlation` = 'sc',
                                                       `Autodelete` = 'ad'),
                                        selected = c('od', 'sc', 'ad'), inline = TRUE),
                           hr(),
                           fluidRow(column(6,
                                           checkboxGroupInput('saveresult',
                                                              label = 'Result output to (will always be available in app)?', 
                                                              choices = list(`R Workspace (named as trimOutput)` = 1,
                                                                             `.rdata-file` = 2),
                                                              selected = c(1, 2), inline = TRUE)
                                          ),
                                    column(6,
                                            textInput('filenameRes', label = 'Enter filename:', value = 'trimOutput')
                                    )
                                  ),
                           hr(),
                           withSpinner(uiOutput('yrSliderAnalyze'), proxy.height = '100px'),
                           verbatimTextOutput('testtext'),
                           hr(),
                           fluidRow(column(6,
                                           radioButtons('specspAnalyze', label = 'Select species set',
                                                choices = list(`All available species` = 'all',
                                                              # `All + northern/southern` = 'allNS',
                                                               `Farmland Bird Index` = 'FBI',
                                                               `Environmental Objective 13` = 'eo13',
                                                               `Fredriks urval IWC Januari` = 'iwcjan',
                                                               `Individual species` = 'ind'),
                                                selected = 'all')),
                                    column(4,
                                           checkboxInput('addNS', label = 'Add northern/southern/species groups?',
                                                         value = FALSE))),
                           conditionalPanel(condition = 'input.specspAnalyze == "ind"',
                                            withSpinner(uiOutput('specCheckboxAnalyze'), proxy.height = '100px')),
                           hr(),
                           conditionalPanel(condition = 'input.tabsel == "totalstandard"',
                                            fluidRow(column(4,
                                                            radioButtons('specrtAnalyze', label = 'Select routes to include',
                                                                         choices = list(`All availble routes` = 'all',
                                                                                        `Counties (län)` = 'lan',
                                                                                        `Province (landskap)` = 'lsk',
                                                                                        `Mountains (Fjällen) n=104` = 'fjl104',
                                                                                        `Mountains (Fjällen) n=142` = 'fjl142',
                                                                                        `Southern routes (<60 N)` = 'S',
                                                                                        `Northern routes (>60 N)` = 'N',
                                                                                        `Individual routes` = 'ind'),
                                                                         selected = 'all')),
                                                     column(8,
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "lan"',
                                                                             uiOutput('lanCheckboxAnalyze')),
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "lsk"',
                                                                             uiOutput('lskCheckboxAnalyze')),
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "fjl104" || input.specrtAnalyze == "fjl142"',
                                                                             uiOutput('fjlCheckboxAnalyze'))
                                                     )
                                            ),
                                            fluidRow(column(12,
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "ind"',
                                                                             uiOutput('indrtCheckboxAnalyze')))
                                            )
                           ),
                           conditionalPanel(condition = 'input.tabsel == "total_iwc_januari" || input.tabsel == "total_iwc_september"',
                                            fluidRow(column(4,
                                                            radioButtons('specrtIWCAnalyze', label = 'Select sites to include',
                                                                         choices = list(`All availble sites` = 'all',
                                                                                        `Eastern coastal` = 'east',
                                                                                        `Western coastal` = 'west'),
                                                                         selected = 'all'))
                                            )
                           ),
                           hr(),
                           fluidRow(column(8,
                                           checkboxInput('makepdf', label = 'Do you want graphs as pdf?',
                                                         value = TRUE),
                                           textInput('filenamepdf', label = 'Enter filename:', value = 'TrimGrafer')
                                           ),
                                    column(4,
                                           actionButton("sendanalysis", "Run analysis"))
                                    ),
                           #actionButton("sendanalysis", "Run analysis"),
                           withSpinner(verbatimTextOutput('testtext2'), proxy.height = '100px'),
                           hr()
                  ),
                  tabPanel('Display results',
                           plotOutput('plot')
                  )
                  # tabPanel('Handle results',
                  #          selectInput('displaysp', label = 'Species to display',
                  #                      choices = as.list(names(resultout())))
                  # )
                )
)

server <- function(input, output, session) {

  specart <- reactive({
    switch(input$specsp,
           all = c(1:699),
           mammals = c(700:799),
           FBI = c(75, 155, 157, 163, 189, 206, 219, 226, 229, 230, 235, 249, 251, 258),
           eo13 = c(75, 86, 155, 157, 188, 189, 206, 226, 229, 230, 235, 249, 258),
           iwcjan = c(1, 2, 3, 4, 5, 7, 8, 9, 12, 13, 16, 17, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 35, 36, 73),
           ind = as.integer(input$indspecsp)
           )
  })
  
  specartAnalyze <- reactive({
    switch(input$specspAnalyze,
           all = if(input$addNS){
                                   specs <- sort(unique(data()$species))  
                                   as.integer(sort(c(specs,
                                                     rangedat$art[rangedat$speciesmain%in%specs],
                                                     '645'[any(c('243', '244', '245')%in%specs)])))
                                 } else {
                                   as.integer(sort(unique(data()$species)))
                                 },
           FBI = c(75, 155, 157, 163, 189, 206, 219, 226, 229, 230, 235, 249, 251, 258),
           eo13 = c(75, 86, 155, 157, 188, 189, 206, 226, 229, 230, 235, 249, 258),
           iwcjan = c(1, 2, 3, 4, 5, 7, 8, 9, 12, 13, 16, 17, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 35, 36, 73),
           ind = as.integer(input$indspecspAnalyze)
           )
  })
  
  specrouteAnalyze <- reactive({
    switch(input$specrtAnalyze,
           all = regStdat$karta,
           lan = regStdat$karta[regStdat$lan%in%input$lanspecrtAnalyze],
           lsk = regStdat$karta[regStdat$lsk%in%input$lskspecrtAnalyze],
           fjl104 = regStdat$karta[regStdat$fjall104==input$fjlspecrtAnalyze],
           fjl142 = regStdat$karta[regStdat$fjall142==input$fjlspecrtAnalyze],
           S = regStdat$karta[regStdat$karta%in%rcdat$site[rcdat$lat < 60]],
           N = regStdat$karta[regStdat$karta%in%rcdat$site[rcdat$lat > 60]],
           ind = input$indspecrtAnalyze)
  })
  

  regIWCdat <<- getIWCData(pool)

  specrouteIWCAnalyze <- reactive({
    switch(input$specrtIWCAnalyze,
           all = regIWCdat$site,
           east = regIWCdat$site[regIWCdat$ki=='K' & regIWCdat$ev=='E'],
           west = regIWCdat$site[regIWCdat$ki=='K' & regIWCdat$ev=='V'])
  })
  
  data <- eventReactive(input$sendquery,{

    if (input$databasechoice == "mongodb") {
      rcdat <<- getSitesMongo()
      #print(rcdat)

      sitesMatchMongo <- getMatchSitesMongo()

      #regStdat <<- getBiotopSites(pool)
      regStdat <<- getBiotopSitesMongo()

      print(Sys.time())
      dataMerge <<- getTotalStandardData (speciesMatch = speciesMatch, speciesMatchSN = speciesMatchScientificNames, sitesMatchMongo = sitesMatchMongo, years = input$selyrs)

      #output$downloadData <- downloadHandler(
      #  content = function(file) {
      #    write.csv(dataMerge, file = paste0('extract/', input$filenameDat, '_', "totalstd", '_', gsub('[ :]', '_', Sys.time()), '.csv'),
      #      row.names = FALSE)
      #  }
      #)

      exportSaveData(dataMerge, savedat = input$savedat, filename = input$filenameDat)
    }
    else {

      rcdat <<- getSites(pool)

      regStdat <<- getBiotopSites(pool)

      DoQuery(pool = pool, tab = input$tabsel, spec=specart(),
            specper = input$specper, selyrs = input$selyrs, line = input$linepoint,
            savedat = input$savedat, filename = input$filenameDat)
    } 
    
  })
  
  resultout <- eventReactive(input$sendanalysis, {
    if (input$addNS){
      dat <- AddNSspecies(data = data(), rangedata = rangedat, coorddata = rcdat)
    } else {
      dat <- data()
    }
    sprtA <<- specrouteIWCAnalyze()
    tix <- dat$time%in%(input$selyrsAnalyze[1]:input$selyrsAnalyze[2])
    if(input$tabsel=='totalstandard'){
      rix <- dat$site%in%specrouteAnalyze()
    } else if (input$tabsel=='total_iwc_januari' | input$tabsel=='total_iwc_september'){
      rix <- dat$site%in%specrouteIWCAnalyze()
    } else {
      rix <- !logical(nrow(dat))
    }
    dat <- subset(dat, tix & rix)
    dat2 <<- dat
    spartA <<- specartAnalyze()
    spAix <- specartAnalyze()%in%as.integer(unique(dat$species))
    styr <- if(input$tabsel=='total_iwc_januari' | input$tabsel=='total_iwc_september'){
                NULL
              } else {
                startyr[startyr$Delprogram==input$tabsel, c('Art', 'StartYear')]
              }
    
    RunTRIMmodel(dat = dat, modeltype = as.integer(input$modeltype), sp_to_run = specartAnalyze()[spAix],
                 odisp = 'od'%in%input$trimset, sercor = 'sc'%in%input$trimset,
                 autodel = 'ad'%in%input$trimset,  speciesdat = spdat,
                 startyr = styr, tabell = input$tabsel,
                 saveresult = input$saveresult, filename = input$filenameRes)
  })
  
  output$yrSlider <- renderUI({
    queryyr <- sprintf("select min(yr) as minyr, max(yr) as maxyr
              from %s", input$tabsel)
    yrs <- dbGetQuery(pool, queryyr)
    sliderInput(inputId = 'selyrs', label = 'Set years',
                min = yrs$minyr, max = yrs$maxyr, value = c(2019, yrs$maxyr),
                step = 1, sep = NULL)
  })

  output$yrSliderAnalyze <- renderUI({
    yrs <- range(data()$time)
    sliderInput(inputId = 'selyrsAnalyze', label = 'Set years',
                min = yrs[1], max = yrs[2], value = c(yrs[1], yrs[2]),
                step = 1, sep = NULL)
  })
    
  output$specCheckbox <- renderUI({
    queryspec <- sprintf("select distinct art
                          from %s
                          where art>'000'
                          order by art", input$tabsel)
    specs <- dbGetQuery(pool, queryspec)
    specnames <- spdat$arthela[match(specs$art,spdat$art)]
    speclist <- as.list(specs$art)
    names(speclist) <- specnames
    tags$div(tags$div(strong(p("Select species"))),
             tags$div(align = 'left',
                      class = 'multicol6',
                      checkboxGroupInput(inputId = 'indspecsp', label = NULL,
                                         choices = speclist,
                                         selected = NULL)
             )
    )
  })
  
  output$specCheckboxAnalyze <- renderUI({
    specs <- sort(unique(data()$species))
    if(input$addNS){
      specs <- sort(c(specs,
                      rangedat$art[rangedat$speciesmain%in%specs],
                      '645'[any(c('243', '244', '245')%in%specs)]))
    }
    specnames <- spdat$arthela[match(specs, spdat$art)]
    speclist <- as.list(specs)
    names(speclist) <- specnames
    tags$div(tags$div(strong(p("Select species"))),
             tags$div(align = 'left',
                      class = 'multicol6',
                      checkboxGroupInput(inputId = 'indspecspAnalyze', label = NULL,
                                         choices = speclist,
                                         selected = NULL)
             )
    )
  })
  
  output$lanCheckboxAnalyze <- renderUI({
    lans <- sort(unique(regStdat$lan))
    lanlist <- as.list(lans)
    tags$div(tags$div(strong(p("Select county(ies)"))),
             tags$div(align = 'left',
                      class = 'multicol8',
                      checkboxGroupInput(inputId = 'lanspecrtAnalyze', label = NULL,
                                         choices = lanlist,
                                         selected = NULL)
             )
    )
  })
  
  output$lskCheckboxAnalyze <- renderUI({
    lsks <- sort(unique(regStdat$lsk))
    lsklist <- as.list(lsks)
    tags$div(tags$div(strong(p("Select province(s)"))),
             tags$div(align = 'left',
                      class = 'multicol8',
                      checkboxGroupInput(inputId = 'lskspecrtAnalyze', label = NULL,
                                         choices = lsklist,
                                         selected = NULL)
             )
    )
  })
  
  # output$fjlCheckboxAnalyze <- renderUI({
  #   fjls <- sort(unique(regStdat$fjall))
  #   fjllist <- as.list(fjls)
  #   tags$div(tags$div(strong(p("Select Mountains (Fjäll) or not (Nej)"))),
  #            tags$div(align = 'left',
  #                     #class = 'multicol8',
  #                     checkboxGroupInput(inputId = 'fjlspecrtAnalyze', label = NULL,
  #                                        choices = fjllist,
  #                                        selected = NULL)
  #            )
  #   )
  # })
  output$fjlCheckboxAnalyze <- renderUI({
    # fjls <- sort(unique(regStdat$fjall))
    # fjllist <- as.list(fjls)
    tags$div(tags$div(strong(p("Select Mountains (Yes) or not (No)"))),
             tags$div(align = 'left',
                      #class = 'multicol8',
                      radioButtons(inputId = 'fjlspecrtAnalyze', label = NULL,
                                         choices = list(Yes = '1',
                                                        No = '0'),
                                         selected = '1')
             )
    )
  })
  
  output$indrtCheckboxAnalyze <- renderUI({
    rts <- paste(regStdat$karta, regStdat$namn)
    tags$div(tags$div(strong(p("Select route(s)"))),
             tags$div(align = 'left',
                      class = 'multicol8',
                      checkboxGroupInput(inputId = 'indspecrtAnalyze', label = NULL,
                                         choiceNames = as.list(rts),
                                         choiceValues = as.list(regStdat$karta),
                                         selected = NULL)
             )
    )
  })
  
  # output$testtext <- renderText({
  #   as.character('od'%in%input$trimset)})
  output$testtext2 <- renderPrint({
    resultout()[[1]]})
  # output$testtext <- renderPrint({
  #   as.integer(input$modeltype)})
  output$testtext <- renderPrint({
    session$clientData$output_plot_width})
    
  output$dataTable <- DT::renderDataTable({
    DT::datatable(data(), filter='top')
    })
  
  output$plot <- renderPlot({
    worked <- sapply(resultout(), function(x) inherits(x$value,'trim'))
    restoplot <- resultout()[worked]
    styr <- if(input$tabsel=='total_iwc_januari' | input$tabsel=='total_iwc_september'){
      NULL
    } else {
      startyr[startyr$Delprogram==input$tabsel, c('Art', 'StartYear')]
    }
    byr <- ifelse(isolate(input$selyrsAnalyze[1])>1998, isolate(input$selyrsAnalyze[1]), 1998) 
    indexplot(restoplot, base = byr, ncol = 3, speciesdat = spdat, startyr = styr, makepdf = input$makepdf, filename = paste0('extract/', input$filenamepdf, '.pdf'))
  }, height = function() {
    nr <- ceiling(sum(sapply(resultout(), function(x) inherits(x$value, 'trim')))/3)
    px <- session$clientData$output_plot_width*nr/3
    return(px)
  })
  
}

shinyApp(ui = ui, server = server)


