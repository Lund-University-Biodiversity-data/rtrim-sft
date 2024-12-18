source('lib/config.R')

source('lib/db_functions.R')
source('lib/UsefulFunctions.R')
source('lib/SummarizeFunctions.R')

# for connection from windows computer, ran locally
#pool <- dbPool(drv = odbc::odbc(), dsn = 'SFT_64', encoding = 'windows-1252')

pool<-dbConnect(RPostgres::Postgres(), dbname = postgres_database, user=postgres_user, password = postgres_password)
poolParams<-dbConnect(RPostgres::Postgres(), dbname = postgres_database_parameters, user=postgres_user, password = postgres_password)

#spdat2 <- getListsFromAla(poolParams)
#spdat <- getSpeciesData(pool)


spdat <- getSpeciesDataParams(poolParams)
#spdat <<- getSpeciesDataMongo()
speciesMatch <- getMatchSpecies(poolParams)

rangedat <- getLimitNorthSouth(poolParams)
rangedat$Latlimit <- as.numeric(gsub('[[:alpha:]]|[[:punct:]]|[[:blank:]]', '', rangedat$Latitudgräns))
rangedat$smaller <- grepl('S', rangedat$Latitudgräns)

startyr <- getStartYear(poolParams)
#startyr$Delprogram[startyr$Delprogram=='SomPKT'] <- 'totalsommar_pkt'
#startyr$Delprogram[startyr$Delprogram=='Standard'] <- 'totalstandard'
#startyr$Delprogram[startyr$Delprogram=='VinPKT'] <- 'totalvinter_pkt'

# data frame to match county (län) codes with the county's full names
counties <<- data.frame(code = c("AB", "C", "D", "E", "F", "G", "H", "I", "K", "M", "N", "O", "S", "T", "U", "W", "X", "Y", "Z", "AC", "BD"),
                       name = c("Stockholms län", "Uppsala län", "Södermanlands län", "Östergötlands län", "Jönköpings län", "Kronobergs län",
                                "Kalmar län", "Gotlands län", "Blekinge län", "Skåne län", "Hallands län", "Västra Götalands län", "Värmlands län",
                                "Örebro län", "Västmanlands län", "Dalarnas län", "Gävleborgs län", "Västernorrlands län", "Jämtlands län", "Västerbottens län", "Norrbottens län"))

 ## Not  sure this is needed (see https://shiny.rstudio.com/articles/pool-basics.html)
onStop(function() {
  poolClose(pool)
  poolClose(poolParams)
  mongoConnection$disconnect()
})


print(paste("Ready ", Sys.time()))

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
                                margin-top: 10px;
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
                  ),
                  tags$title("Åkes superTRIMprogram")
                ),
                titlePanel(title = div(img(style = 'display: inline-block;', src = "fageltaxering-logo2x.png", height = 80 , width = 240),
                                       p(style = 'display: inline-block; margin: auto; width: 60%; text-align: center; font-size: 1.5em;',
                                         'Åkes superTRIMprogram - MongoDB version'))),
                # titlePanel(title = div(style = 'margin: auto; padding-bottom: 50px', p(style = 'display: block; text-align: center; font-size: 1.5em;',
                #                          img(style = 'float: left; margin-bottom: 100px;', src = "fageltaxering-logo2x.png", height = 80 , width = 240),
                #                          'Åkes superTRIMprogram'))),
                tabsetPanel(selected="Get data",
                  tabPanel('Parameters', 
                    tabsetPanel(
                      tabPanel('SpeciesList',
                        hr(),
                        fluidRow( div(paste0("The species list here is obtained from the API requested at the url ", species_list_api_url) )),
                        hr(),
                        actionButton("regenerateSpecies", "Request the API"),
                        hr(),
                        withSpinner(verbatimTextOutput('resultGenerateSpecies'))),
                      tabPanel('StartYear',
                        withSpinner(uiOutput("dtTableStartYear"))),
                      tabPanel('NorthSouth',
                        withSpinner(uiOutput("dtNorthSouth")))
                    )
                  ),
                  tabPanel('Get data', 
                           radioButtons('databasechoice', label = 'Select the database',
                                        choices = list(
                                            #`Good old sft database on PSQL` = 'psql',
                                                       `Brand new mongoDB` = 'mongodb'),
                                        selected = 'mongodb'),
                           radioButtons('tabsel', label = 'Select monitoring scheme',
                                        choices = list(Standardrutter = 'totalstandard', 
                                                       Sommarpunktrutter = 'totalsommar_pkt',
                                                       Vinterpunktrutter =  'totalvinter_pkt',
                                                       #`Sjöfågeltaxering Vår` = 'totalvatmark',
                                                       `IWC Januari` = 'total_iwc_januari',
                                                       `IWC September` = 'total_iwc_september'#,
                                                       #`Miscellaneous system` = 'misc_census'
                                                       ),
                                        selected = 'totalstandard', inline = FALSE, width = NULL),
                           conditionalPanel(condition = 'input.tabsel == "totalstandard"',
                                            radioButtons('linepoint', label = 'Select subscheme',
                                                         choices = list(Lines = TRUE, Points = FALSE ),
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
                           withSpinner(uiOutput('yrSlider')),
                           hr(),
                           radioButtons('specsp', label = 'Select species set',
                                        choices = list(`All available bird species` = 'all',
                                                       #`All available mammal species` = 'mammals',
                                                       `Farmland Bird Index` = 'FBI',
                                                       #`Environmental Objective 13` = 'eo13',
                                                       #`Fredriks urval IWC Januari` = 'iwcjan',
                                                       `Individual species` = 'ind'
                                                       ),
                                        selected = 'all'),
                           conditionalPanel(condition = 'input.specsp == "ind"',
                                            withSpinner(uiOutput('specCheckbox'))),
                           hr(),
                           checkboxGroupInput('specifCorrections', label = 'Specific corrections',
                                        choices = list(#'RödGlada#43 > 30 = 30' = 'fixArt43',
                                                       'Tallbit#242 > 0 = 0 (1984/1986/1996 only)' = 'fixArt242',
                                                       'Bergfink#248 > 50000 = 50000' = 'fixArt248'
                                                ),
                                        selected = c(), inline = TRUE),
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
                                                               `Farmland Bird Index` = 'FBI',
                                                               #`Environmental Objective 13` = 'eo13',
                                                               #`Fredriks urval IWC Januari` = 'iwcjan',
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
                                                                         choices = list(`All available routes` = 'all',
                                                                                        `Counties (län)` = 'lan',
                                                                                        `Province (landskap)` = 'lsk',
                                                                                        `Mountains (Fjällen) n=104` = 'fjl104',
                                                                                        `Mountains (Fjällen) n=142` = 'fjl142',
                                                                                        `Southern routes (<60 N)` = 'S',
                                                                                        `Northern routes (>60 N)` = 'N',
                                                                                        `Individual routes` = 'ind'
                                                                                        ),
                                                                         selected = 'all')),
                                                     column(8,
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "lan"',
                                                                             uiOutput('lanCheckboxAnalyze')),
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "lsk"',
                                                                             uiOutput('lskCheckboxAnalyze')),
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "fjl104" || input.specrtAnalyze == "fjl142" || input.specrtAnalyze == "fjl104_inv" || input.specrtAnalyze == "fjl142_inv"',
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
                                                                                        `Coasts only (ki=K)` = 'coast',
                                                                                        `Inland only (ki=I)` = 'inland',
                                                                                        `Eastern coastal (ev=E & ki=K)` = 'east',
                                                                                        `Western coastal (ev=V & ki=K)` = 'west',
                                                                                        `Counties (län)` = 'lan'),
                                                                         selected = 'all')),
                                                     column(8,
                                                            conditionalPanel(condition = 'input.specrtIWCAnalyze == "lan"',
                                                                             uiOutput('lanIWCCheckboxAnalyze')))
                                            )
                           ),
                           conditionalPanel(condition = 'input.tabsel == "totalvinter_pkt" || input.tabsel == "totalsommar_pkt"',
                                            fluidRow(column(4,
                                                            radioButtons('specrtPKTAnalyze', label = 'Select sites to include',
                                                                         choices = list(`All availble sites` = 'all',
                                                                                        `Counties (län)` = 'lan'),
                                                                         selected = 'all')),
                                                     column(8,
                                                            conditionalPanel(condition = 'input.specrtPKTAnalyze == "lan"',
                                                                             uiOutput('lanPKTCheckboxAnalyze')))
                                            )
                           ),
                           hr(),
                           fluidRow(column(8,
                                           checkboxInput('makepdf', label = 'Save graphs as pdf',
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
                           #plotOutput('plot' )
                           hr(),
                           textInput('displaysize', label = 'Result size (XX%):', value = '50%'),
                           hr(),
                           withSpinner(uiOutput("plotResultsDisplay")),
                           hr()
                  ),
                  tabPanel('Summarize results',
                           hr(),
                           textInput('filenameResSumm', label = 'Enter filename:', value = 'trimOutput'),
                           #textInput('yearBaseSumm', label = 'Base year:', value = '2002'),
                           uiOutput('yearBaseSummAuto'),
                           hr(),
                           p('What are the name(s) of the "monitoring systems" (tables in SFT) that you want summaries for. Can be 1 or several systems.'),
                           checkboxGroupInput('tableSumm', label = 'Select table(s)',
                                                               choices = list(`totalstandard`= "totalstandard",
                                                                              `totalsommar_pkt`= "totalsommar_pkt",
                                                                              `totalvinter_pkt`= "totalvinter_pkt",
                                                                              #`totalvatmark`= "totalvatmark"),
                                                                              `IWC Januari` = "total_iwc_januari",
                                                                              `IWC September` = "total_iwc_september"),
                                                               selected = "totalstandard", inline = TRUE),
                           hr(),
                           p('Do you want single files (trimv201x...) for graph making (each system separately)? For example, do you also want Winter.'),
                           checkboxInput('singleSumm', label = 'Single files', value = TRUE),
                           hr(),
                           p('Do you want "homepage" files (you will get one for each system)? This is the "overview data" file.'),
                           checkboxInput('homepageSumm', label = 'Homepage files', value = TRUE),
                           hr(),
                           p('Do you want to use shorter time periods for some species? (i.e. use the information in "SpeciesWithShorterTimePeriods.xls")<br> If the system(s) you are running does not have such information in the xls-file it does not matter how you specify this.'),
                           checkboxInput('shorterPeriodSumm', label = 'Shorter time periods', value = TRUE),
                           hr(),
                           radioButtons('langSumm', label = 'Language of species name',
                                        choices = list(`SE` = "SE",
                                                       `EN` = "EN",
                                                       `World` = "WD"),
                                        selected = "SE", inline = TRUE),
                           hr(),
                           fluidRow(column(6,
                                           actionButton("sendquerysumm", "Generate excel files")
                                           ),
                                    column(6,
                                           p('The generated files can be found => .'),
                                           tags$a("Download files folder", href=url_extract, target="_blank", rel="noopener noreferrer")
                                           )
                                    ),
                           hr(),
                           withSpinner(verbatimTextOutput('rtSumm'), proxy.height = '100px')
                                            
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
           #FBI = c(75, 155, 157, 163, 189, 206, 219, 226, 229, 230, 235, 249, 251, 258),
           FBI = c(57, 75, 155, 157, 163, 189, 206, 219, 226, 229, 230, 235, 249, 251, 258),
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
           all = regStdat$internalSiteId,
           lan = regStdat$internalSiteId[regStdat$lan%in%input$lanspecrtAnalyze],
           lsk = regStdat$internalSiteId[regStdat$lsk%in%input$lskspecrtAnalyze],
           fjl104 = regStdat$internalSiteId[regStdat$fjall104==input$fjlspecrtAnalyze],
           fjl142 = regStdat$internalSiteId[regStdat$fjall142==input$fjlspecrtAnalyze],
           S = regStdat$internalSiteId[regStdat$internalSiteId%in%rcdat$site[rcdat$lat < 60]],
           N = regStdat$internalSiteId[regStdat$internalSiteId%in%rcdat$site[rcdat$lat > 60]],
           ind = input$indspecrtAnalyze)
  })
  

  #regIWCdat <<- getIWCData(pool)
  regIWCdat <<- getIWCDataMongo(project_id_iwc)

  specrouteIWCAnalyze <- reactive({
    switch(input$specrtIWCAnalyze,
           all = regIWCdat$site,
           coast = regIWCdat$site[regIWCdat$ki=='K'],
           inland = regIWCdat$site[regIWCdat$ki=='I'],
           east = regIWCdat$site[regIWCdat$ki=='K' & regIWCdat$ev=='E'],
           west = regIWCdat$site[regIWCdat$ki=='K' & regIWCdat$ev=='V'],
           lan = regIWCdat$site[regIWCdat$lan%in%input$lanspecrtIWCAnalyze])
  })
  
  # get site data for sommarpunktrutter and vinterpunktrutter
  regPKTdat <<- getPKTDataMongo(project_id_punkt)
  
  specroutePKTAnalyze <- reactive({
    switch(input$specrtPKTAnalyze,
           all = regPKTdat$site,
           lan = regPKTdat$site[regPKTdat$lan%in%input$lanspecrtPKTAnalyze])
  })
  
  data <- eventReactive(input$sendquery,{
  
    # error message in case no species were selected
    shiny::validate(
      need(length(specart()) > 0, "Please select at least one species.")
    )

    correctionsArt <- data.frame(FALSE, FALSE, FALSE)
    colnames(correctionsArt) <- c("s043", "s242", "s248")

    if ("fixArt43"%in%input$specifCorrections) {
        correctionsArt$s043 = TRUE
    }
    if ("fixArt242"%in%input$specifCorrections) {
        correctionsArt$s242 = TRUE
    }
    if ("fixArt248"%in%input$specifCorrections) {
        correctionsArt$s248 = TRUE
    }

    if (input$databasechoice == "mongodb") {

      linepoint <- ""
      selectedPeriod <- ""

      if (input$tabsel == "totalstandard") {
        projectId <- project_id_std
        projectActivityId <- project_activity_id_std

        if (input$linepoint) {
          linepoint <- "line"
        }
        else {
          linepoint <- "point" 
        }
      }
      else if (input$tabsel == "total_iwc_januari") {
        projectId <- project_id_iwc
        projectActivityId <- project_activity_id_iwc
        selectedPeriod <- '"Januari"'
      }
      else if (input$tabsel == "total_iwc_september") {
        projectId <- project_id_iwc
        projectActivityId <- project_activity_id_iwc

        selectedPeriod <- '"September"'
      }
      else if (input$tabsel == "totalsommar_pkt") {
        projectId <- project_id_punkt
        projectActivityId <- project_activity_id_summer
      }
      else if (input$tabsel == "totalvinter_pkt") {
        projectId <- project_id_punkt
        projectActivityId <- project_activity_id_winter
        
        # error message in case no period was selected
        shiny::validate(
          need(length(input$specper) > 0, "Please select a monitoring period.")
        )

        selectedPeriod <- paste0('"', paste0(input$specper, collapse = '","'), '"')

      }


      rcdat <<- getSitesMongo(projectId)

      sitesMatchMongo <- getMatchSitesMongo(projectId)

      #regStdat <<- getBiotopSites(pool)
      regStdat <<- getBiotopSitesMongo(projectId)

      print(Sys.time())
      

      # get matching species
      #speciesMatch <- getMatchSpecies(poolParams, specart())
      #speciesMatchScientificNames <- getListBirdsUrl(bird_list_id, specart())
      speciesMatchScientificNames <- getMatchSpeciesSN(poolParams, specart())

      dataMerge <<- getCountData (projectActivityId = projectActivityId, speciesMatch = speciesMatch, speciesMatchSN = speciesMatchScientificNames, sitesMatchMongo = sitesMatchMongo, yearsSel = input$selyrs, linepoint = linepoint, selectedPeriod = selectedPeriod, correctionsArt = correctionsArt)

      #output$downloadData <- downloadHandler(
      #  content = function(file) {
      #    write.csv(dataMerge, file = paste0('extract/', input$filenameDat, '_', "totalstd", '_', gsub('[ :]', '_', Sys.time()), '.csv'),
      #      row.names = FALSE)
      #  }
      #)

      exportSaveData(dataMerge, savedat = input$savedat, filename = input$filenameDat, input$tabsel)
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
    
    # error message in case no species were selected
    shiny::validate(
      need(length(specartAnalyze()) > 0, "Please select at least one species.")
    )

    print(paste("start analysis ", Sys.time()))

    if (input$addNS){
      dat <- AddNSspecies(data = data(), rangedata = rangedat, coorddata = rcdat)
    } else {
      dat <- data()
    }
    sprtA <<- specrouteIWCAnalyze()
    tix <- dat$time%in%(input$selyrsAnalyze[1]:input$selyrsAnalyze[2])
    if(input$tabsel=='totalstandard'){
      rix <- dat$site%in%specrouteAnalyze()
    } 
    else if (input$tabsel=='total_iwc_januari' | input$tabsel=='total_iwc_september'){
      rix <- dat$site%in%specrouteIWCAnalyze()
    }
    else if (input$tabsel=='totalvinter_pkt' | input$tabsel=='totalsommar_pkt') {
      rix <- dat$site%in%specroutePKTAnalyze()
    }
    else {
      rix <- !logical(nrow(dat))
    }
    dat <- subset(dat, tix & rix)
    # error message in case the subset contains no data
    shiny::validate(
      need(nrow(dat) > 0, "There was no data found that matches your selection.")
    )
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



  summarizeRt <- eventReactive(input$sendquerysumm, {

    useShorterPeriods <- input$shorterPeriodSumm
    tables <- input$tableSumm

        ## Get info on species specific startyear (has been used in the app analyses as well)
    if (useShorterPeriods & any(tables%in%c('totalsommar_pkt', 'totalvinter_pkt', 'totalstandard'))){
      # startyr <- read.xlsx('SpeciesWithShorterTimePeriods.xls', sheetName = 'StartYear', encoding = 'UTF-8',
      #                      stringsAsFactors = F)
      startyr <- read_excel(paste0(path_project, 'SpeciesWithShorterTimePeriods.xls'), sheet = 'StartYear')
      startyr$Delprogram[startyr$Delprogram=='SomPKT'] <- 'totalsommar_pkt'
      startyr$Delprogram[startyr$Delprogram=='Standard'] <- 'totalstandard'
      startyr$Delprogram[startyr$Delprogram=='VinPKT'] <- 'totalvinter_pkt'
    } else {
      startyr <- NULL
    }

    DoSummarizeResult(filenames=input$filenameResSumm, tables=c(input$tableSumm), base=strtoi(input$yearBaseSumm), spdat=spdat, startyr=startyr, homepage=input$homepageSumm, single=input$singleSumm, lang=input$langSumm) 


  })

  getspecies <- eventReactive(input$regenerateSpecies, {

    spdat <<- getListsFromAla(poolParams)

    #print(spdat)
  })


  output$resultGenerateSpecies <- renderPrint({
    getspecies()})



  output$yrSlider <- renderUI({
    queryyr <- sprintf("select min(yr) as minyr, max(yr) as maxyr
              from %s", input$tabsel)
    yrs <- dbGetQuery(pool, queryyr)
    sliderInput(inputId = 'selyrs', label = 'Set years',
                min = yrs$minyr, max = yrs$maxyr, value = c(2017, yrs$maxyr),
                step = 1, sep = NULL)
  })

  output$yrSliderAnalyze <- renderUI({
    yrs <- range(data()$time)
    sliderInput(inputId = 'selyrsAnalyze', label = 'Set years',
                min = yrs[1], max = yrs[2], value = c(yrs[1], yrs[2]),
                step = 1, sep = NULL)
  })

  output$yearBaseSummAuto <- renderUI({
    yrs <- range(data()$time)
    tags$div(textInput('yearBaseSumm', label = 'Base year:', value = yrs[1]))
  })
    
  output$specCheckbox <- renderUI({
    if (input$tabsel == "totalstandard") {
      projectId <- project_id_std
      projectActivityId <- project_activity_id_std
    }
    else if (input$tabsel == "totalsommar_pkt") {
      projectId <- project_id_punkt
      projectActivityId <- project_activity_id_summer
    } 
    else if (input$tabsel == "totalvinter_pkt") {
      projectId <- project_id_punkt
      projectActivityId <- project_activity_id_winter
    } 
    else if (input$tabsel == "total_iwc_januari" || input$tabsel == "total_iwc_september") {
      projectId <- project_id_iwc
      projectActivityId <- project_activity_id_iwc
    } 
    else if (input$tabsel == "totalvinter_pkt") { # what is this else if?
      projectId <- project_id_punkt
      projectActivityId <- project_activity_id_iwc
    } 

    specsSN <- getUniquesSpeciesFromScheme(projectActivityId, speciesMatch)
    
    nbSp <- nrow(specsSN)

    vSpecies <- vector()
    for (iSp  in 1:nbSp) {
      # check if species name exists in both objects. If not, print validation error
      shiny::validate(
        need(str_trim(specsSN$name[iSp]) %in% attributes(speciesMatch)$names, paste0("ERROR in retrieving the species items. This item can't be found in the lists module: ", specsSN$name[iSp]))
      )
      vSpecies[iSp] <- speciesMatch[[str_trim(specsSN$name[iSp])]]
    }
    vSpecies <- sort(vSpecies)

    specnames <- spdat$arthela[match(vSpecies,spdat$art)]
    
    speclist <- as.list(vSpecies)
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
    lans <- sort(unique(regStdat$lan[nchar(regStdat$lan) > 0]))
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
    lsks <- sort(unique(regStdat$lsk[nchar(regStdat$lsk) > 0]))
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
    rts <- paste(regStdat$internalSiteId, regStdat$namn)
    tags$div(tags$div(strong(p("Select route(s)"))),
             tags$div(align = 'left',
                      class = 'multicol8',
                      checkboxGroupInput(inputId = 'indspecrtAnalyze', label = NULL,
                                         choiceNames = as.list(rts),
                                         choiceValues = as.list(regStdat$internalSiteId),
                                         selected = NULL)
             )
    )
  })
  
  output$lanIWCCheckboxAnalyze <- renderUI({
    lans <- sort(unique(regIWCdat$lan[nchar(regIWCdat$lan) > 0]))
    lanlist <- as.list(lans)
    tags$div(tags$div(strong(p("Select county(ies)"))),
             tags$div(align = 'left',
                      class = 'multicol8',
                      checkboxGroupInput(inputId = 'lanspecrtIWCAnalyze', label = NULL,
                                         choices = lanlist,
                                         selected = NULL)
             )
    )
  })
  
  output$lanPKTCheckboxAnalyze <- renderUI({
    lans <- sort(unique(regPKTdat$lan[nchar(regPKTdat$lan) > 0]))
    lanlist <- as.list(lans)
    tags$div(tags$div(strong(p("Select county(ies)"))),
             tags$div(align = 'left',
                      class = 'multicol6',
                      checkboxGroupInput(inputId = 'lanspecrtPKTAnalyze', label = NULL,
                                         choices = lanlist,
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

  output$rtSumm <- renderPrint({
    summarizeRt()})


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
    indexplot(restoplot, base = byr, ncol = 3, speciesdat = spdat, startyr = styr, makepdf = input$makepdf, filename = paste0(path_project_extract,input$filenamepdf, '.pdf'))
  }, height = function() {
    n_plots <- sum(sapply(resultout(), function(x) inherits(x$value, 'trim')))
    # number of rows
    nr <- ceiling(n_plots/3)
    # account for case of having less than 3 plots to avoid distortion of plots
    #px <- session$clientData$output_plot_width*nr/3
    if (n_plots < 4) {
      px <- session$clientData$output_plot_width*nr/n_plots
    } else {
      px <- session$clientData$output_plot_width*nr/3
    }
    
    # output helpful error message if input for plot width given is out of range
    if (n_plots == 1) {
      w <- session$clientData$output_plot_width*1
      shiny::validate(
        need(200 < w & 1500 > w, "The result cannot be displayed, because the value entered for result size is either too small or too large.")
      )
    } else if (n_plots == 2) {
      w <- session$clientData$output_plot_width*2
      shiny::validate(
        need(540 < w & 4000 > w, "The result cannot be displayed, because the value entered for result size is either too small or too large.")
      )
    } else {
      w <- session$clientData$output_plot_width*3
      shiny::validate(
      need(800 < w & 6000 > w, "The result cannot be displayed, because the value entered for result size is either too small or too large.")
      )
    }

    return(px)
  }
  )

  # get the plot size
  plotWidth <- reactive(input$displaysize)
  # render it
  output$plotResultsDisplay <- renderUI({
    # output helpful error message if no data available to be displayed
    worked <- sapply(resultout(), function(x) inherits(x$value,'trim'))
    shiny::validate(
      need(TRUE %in% worked, "No data to be displayed.")
    )
    plotOutput("plot", width = plotWidth())
  })





  dtTSY.insert.callback <- function(data, row) {


    query <- paste0("INSERT INTO species_start_year (species_id, species_sw_name, scheme, year, comment) VALUES ( ",
      "'", data[row,]$Art, "', ",
      "'", data[row,]$Arthela, "', ",
      "'", data[row,]$Delprogram, "', ",
      data[row,]$StartYear, ", ",
      "'", data[row,]$Extra, "' ",
      ")")
    #print(query) # For debugging
    dbSendQuery(poolParams, query)

    getid <- paste0("select id FROM species_start_year WHERE ",
        "species_id = '", data[row,]$Art, "' AND ",
        "species_sw_name = '", data[row,]$Arthela, "' AND ",
        "scheme = '", data[row,]$Delprogram, "' AND ",
        "year = ", data[row,]$StartYear)
    #print(getid) # For debugging
    newrow <- dbGetQuery(poolParams, getid)
    data[row,]$id <- newrow$id

    #startyr <- rbind(data, rowColName)
    return(data)
  }

  dtTSY.update.callback <- function(data, olddata, row) {

    query <- paste0("UPDATE species_start_year SET ",
          "species_id = '", data[row,]$Art, "', ",
          "species_sw_name = '", data[row,]$Arthela, "', ",
          "scheme = '", data[row,]$Delprogram, "', ",
          "year = ", data[row,]$StartYear, ", ",
          "comment = '", data[row,]$Extra, "' ",
          "WHERE id = ", data[row,]$id)
    #print(query) # For debugging
    dbSendQuery(poolParams, query)

    startyr[row,] <- data[row,]
    return(startyr)
  }

  dtTSY.delete.callback <- function(data, row) {

      query <- paste0("DELETE FROM species_start_year WHERE id = ", data[row,]$id)
      #print(query) # For debugging
      dbSendQuery(poolParams, query)

      startyr[row,] <- NULL
      return(startyr)
  }

  DTedit::dtedit(input, output,
                 name = 'dtTableStartYear',
                 thedata = startyr,
                 edit.cols = c('Art', 'Arthela', 'Delprogram', 'StartYear', 'Extra'),
                 edit.label.cols = c('Art', 'Arthela', 'Delprogram', 'StartYear', 'Extra'),
                 input.types = c(Art='textInput', Arthela='textInput', Delprogram='textInput', StartYear='textInput', Extra='textAreaInput'),
                 view.cols = c('Art', 'Arthela', 'Delprogram', 'StartYear', 'Extra'),
                 callback.update = dtTSY.update.callback,
                 callback.insert = dtTSY.insert.callback,
                 callback.delete = dtTSY.delete.callback,
                 show.copy = FALSE,
                 show.delete = FALSE)
  
  dtNS.insert.callback <- function(data, row) {

    query <- paste0("INSERT INTO species_limit_north_south (species_id, species_id_main, species_sw_name, species_latin_name, species_en_name, latitude_limit) VALUES ( ",
      "'", data[row,]$art, "', ",
      "'", data[row,]$speciesmain, "', ",
      "'", data[row,]$arthela, "', ",
      "'", data[row,]$latin, "' ",
      "'", data[row,]$englishname, "' ",
      "'", data[row,]$Latitudgräns, "' ",
      ")")
    #print(query) # For debugging
    dbSendQuery(poolParams, query)

    getid <- paste0("select id FROM species_limit_north_south WHERE ",
        "species_id = '", data[row,]$art, "' AND ",
        "species_id_main = '", data[row,]$speciesmain, "' ")
    #print(getid) # For debugging
    newrow <- dbGetQuery(poolParams, getid)
    data[row,]$id <- newrow$id



    #rangedat <- rbind(data, rowColName)
    return(data)
  }

  dtNS.update.callback <- function(data, olddata, row) {

    query <- paste0("UPDATE species_limit_north_south SET ",
          "species_id = '", data[row,]$art, "', ",
          "species_id_main = '", data[row,]$speciesmain, "', ",
          "species_sw_name = '", data[row,]$arthela, "', ",
          "species_latin_name = '", data[row,]$latin, "', ",
          "species_en_name = '", data[row,]$englishname, "', ",
          "latitude_limit = '", data[row,]$Latitudgräns, "' ",
          "WHERE id = ", data[row,]$id)
    print(query) # For debugging
    dbSendQuery(poolParams, query)

    rangedat[row,] <- data[row,]

    rangedat$Latlimit <- as.numeric(gsub('[[:alpha:]]|[[:punct:]]|[[:blank:]]', '', rangedat$Latitudgräns))
    rangedat$smaller <- grepl('S', rangedat$Latitudgräns)

    return(rangedat)

  }

  dtNS.delete.callback <- function(data, row) {
    rangedat[row,] <- NULL
    return(rangedat)
  }

  #mydata2 <- rangedat

  DTedit::dtedit(input, output,
                 name = 'dtNorthSouth',
                 thedata = rangedat,
                 edit.cols = c('art', 'speciesmain', 'arthela', 'latin', 'englishname', 'Latitudgräns'),
                 edit.label.cols = c('art', 'speciesmain', 'arthela', 'latin', 'englishname', 'Latitudgräns'),
                 view.cols = c('art', 'speciesmain', 'arthela', 'latin', 'englishname', 'Latitudgräns'),
                 callback.update = dtNS.update.callback,
                 callback.insert = dtNS.insert.callback,
                 callback.delete = dtNS.delete.callback,
                 show.copy = FALSE,
                 show.delete = FALSE)


}

shinyApp(ui = ui, server = server)


