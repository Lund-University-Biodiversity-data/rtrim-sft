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

# data frame with full names of schemes and their abbreviations
tabShorts <<- data.frame(table = c('totalstandard', 'totalsommar_pkt', 'totalvinter_pkt', 'totalvatmark', 'totalkustfagel200', 'total_iwc_januari', 'total_iwc_september', 'misc_census'),
                         short = c('T', 'S', 'V', 'VAT', 'K', 'IWCjan', 'IWCsep', 'M'))

# data frame with start years of the schemes
yrStart <<- data.frame(table = c('totalstandard', 'totalsommar_pkt', 'totalvinter_pkt', 'totalvatmark', 'totalkustfagel200', 'total_iwc_januari', 'total_iwc_september', 'misc_census'),
                       year = c(1996, 1975, 1975, 0, 2015, 1966, 1973, 0))

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
                            .keyButton {
                                color: white;
                                background-color: #00308F;
                            }
                            "
                    )
                  ),
                  tags$title("Åkes superTRIMprogram")
                ),
                shinyjs::useShinyjs(),
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
                                        choices = list(`Good old sft database on PSQL` = 'psql',
                                                       `Brand new mongoDB` = 'mongodb'),
                                        selected = 'mongodb'),
                           radioButtons('tabsel', label = 'Select monitoring scheme',
                                        choices = list(`Standardrutter` = 'totalstandard', 
                                                       `Sommarpunktrutter` = 'totalsommar_pkt',
                                                       `Vinterpunktrutter` =  'totalvinter_pkt',
                                                       `Kustfagelrutor` = 'totalkustfagel200',
                                                       #`Sjöfågeltaxering Vår` = 'totalvatmark',
                                                       `IWC Januari` = 'total_iwc_januari',
                                                       `IWC September` = 'total_iwc_september',
                                                       `Miscellaneous system` = 'misc_census'
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
                           conditionalPanel(condition = 'input.tabsel == "misc_census"',
                                            p(em("Please make sure your data is in the required format. You can find a template below.")),
                                            downloadLink("misc_template", "Download excel template"),
                                            fileInput("misc_data", "Choose CSV, XLSX or XLS file", accept = c(".csv", ".xlsx", ".xls")),
                                            verbatimTextOutput("misc_data_contents"),
                                            p(em('Below you will see the first six rows of the imported data. If you find it has not been recognized correctly, please check the format of your data and try uploading it again.')),
                                            tableOutput("contents"),
                                            uiOutput("reminder")
                                            ),
                           hr(),
                           withSpinner(uiOutput('yrSlider')),
                           hr(),
                           fluidRow(column(6,
                                           radioButtons('specsp', label = 'Select species set',
                                           choices = list(`All available bird species` = 'all',
                                                       #`All available mammal species` = 'mammals',
                                                       `Farmland Bird Index` = 'FBI',
                                                       #`Environmental Objective 13` = 'eo13',
                                                       `Fredriks urval IWC Januari` = 'iwcjan',
                                                       `Individual species` = 'ind',
                                                       `Custom set of species from file upload` = 'custom' 
                                                       ),
                                           selected = 'all')),
                                    column(6,
                                           conditionalPanel(condition = 'input.specsp == "custom"',
                                                            p(em('Please upload an excel or csv file listing the art numbers of the species you want to select. You can find a template below for the required format.')),
                                                            downloadLink('specTemplate', 'Download template'),
                                                            fileInput("specset", "Choose xlsx, xls or csv File", accept = c(".csv", ".xlsx", ".xls")),
                                                            verbatimTextOutput("specset_contents"))
                                           )
                           ),
                           withSpinner(uiOutput('specCheckbox')),
                           hr(),
                           conditionalPanel(condition = 'input.tabsel == "totalvinter_pkt"',
                                            checkboxGroupInput('specifCorrections', label = 'Specific corrections',
                                                               choices = list(#'RödGlada#43 > 30 = 30' = 'fixArt43',
                                                               'Tallbit#242 > 0 = 0 (1984/1986/1996 only)' = 'fixArt242',
                                                               'Bergfink#248 > 50000 = 50000' = 'fixArt248'),
                                                               selected = c(), inline = TRUE)),
                           hr(),
                           fluidRow(column(6,
                                           checkboxGroupInput('savedat',
                                              label = 'Data output to (will always be available in app)?', 
                                              choices = list(#`R Workspace` = 1,
                                                             `.csv-file` = 2,
                                                             `.csv-file (xls-friendly)` = 3,
                                                             `.rdata-file` = 4),
                                              selected = 1, inline = TRUE)
                                           ),
                                    column(6,
                                           textInput('filenameDat', label = 'Choose a name for your files:', value = 'Dataextraction_All')
                                           )
                                    ),
                           hr(),
                           actionButton("sendquery", "Submit query", class = "keyButton"),
                           hr(),
                           p('Download the generated files:'),
                           fluidRow(column(4,
                                           downloadButton("downloadCSV", "Download csv")
                                           ),
                                    column(4,
                                           downloadButton("downloadCSV2", "Download csv (xls-friendly)")
                                           ),
                                    column(4,
                                           downloadButton("downloadRDATA", "Download rdata")
                                           ),
                                    ),
                           hr(),
                           withSpinner(DT::dataTableOutput("dataTable"), proxy.height = '150px'),
                           verbatimTextOutput('duplicates'),
                           verbatimTextOutput('querytime'),
                           verbatimTextOutput("querytimetable") 
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
                           withSpinner(uiOutput('yrSliderAnalyze'), proxy.height = '100px'),
                           verbatimTextOutput('testtext'),
                           hr(),
                           fluidRow(column(6,
                                           radioButtons('specspAnalyze', label = 'Select species set',
                                                choices = list(`All available species` = 'all',
                                                               `Farmland Bird Index` = 'FBI',
                                                               #`Environmental Objective 13` = 'eo13',
                                                               `Fredriks urval IWC Januari` = 'iwcjan',
                                                               `Individual species` = 'ind'),
                                                selected = 'all')),
                                    column(4,
                                           checkboxInput('addNS', label = 'Add northern/southern species groups',
                                                         value = TRUE))),
                           withSpinner(uiOutput('specCheckboxAnalyze'), proxy.height = '100px'),
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
                                                                                        `Individual routes` = 'ind',
                                                                                        `Select on map` = 'karta'
                                                                                        ),
                                                                         selected = 'all'),
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "karta"',
                                                                             p('Define your area of interest by clicking on the map to set the vertices of a polygon. You can turn off the existing point data layers at the top right of the map.
                                                                               To start over and remove all points, click "Clear polygon". To remove only the last point you set, click "Undo one". When you are done, confirm your selection by clicking "Select routes".'))
                                                            ),
                                                     column(8,
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "lan"',
                                                                             uiOutput('lanCheckboxAnalyze')),
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "lsk"',
                                                                             uiOutput('lskCheckboxAnalyze')),
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "fjl104" || input.specrtAnalyze == "fjl142" || input.specrtAnalyze == "fjl104_inv" || input.specrtAnalyze == "fjl142_inv"',
                                                                             uiOutput('fjlCheckboxAnalyze')),
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "karta"',
                                                                             leafletOutput("map"),
                                                                             fluidRow(column(2,
                                                                                             actionButton('clearMap', 'Clear polygon')),
                                                                                      column(2,
                                                                                             actionButton('clearOne', 'Undo one')),
                                                                                      column(2,
                                                                                             actionButton('selectmap', 'Select routes')),
                                                                                      column(6,
                                                                                             withSpinner(verbatimTextOutput('mapsel'), proxy.height = '100px'))
                                                                                      ))
                                                     )
                                            ),
                                            fluidRow(column(12,
                                                            conditionalPanel(condition = 'input.specrtAnalyze == "ind"',
                                                                            uiOutput('indrtCheckboxAnalyze')
                                                                            ))
                                            )
                           ),
                           conditionalPanel(condition = 'input.tabsel == "total_iwc_januari" || input.tabsel == "total_iwc_september"',
                                            fluidRow(column(4,
                                                            radioButtons('specrtIWCAnalyze', label = 'Select sites to include',
                                                                         choices = list(`All available sites` = 'all',
                                                                                        `Coasts only (ki=K)` = 'coast',
                                                                                        `Inland only (ki=I)` = 'inland',
                                                                                        `Eastern coastal (ev=E & ki=K)` = 'east',
                                                                                        `Western coastal (ev=V & ki=K)` = 'west',
                                                                                        `Counties (län)` = 'lan',
                                                                                        `Select on map` = 'karta'
                                                                                        ),
                                                                         selected = 'all'),
                                                            conditionalPanel(condition = 'input.specrtIWCAnalyze == "karta"',
                                                                             p('Define your area of interest by clicking on the map to set the vertices of a polygon. You can turn off the existing point data layers at the top right of the map.
                                                                               To start over and remove all points, click "Clear polygon". To remove only the last point you set, click "Undo one". When you are done, confirm your selection by clicking "Select routes".'))
                                                            ),
                                                     column(8,
                                                            conditionalPanel(condition = 'input.specrtIWCAnalyze == "lan"',
                                                                             uiOutput('lanIWCCheckboxAnalyze')),
                                                            conditionalPanel(condition = 'input.specrtIWCAnalyze == "karta"',
                                                                             leafletOutput("mapIWC"),
                                                                             fluidRow(column(2,
                                                                                             actionButton('clearMap', 'Clear polygon')),
                                                                                      column(2,
                                                                                             actionButton('clearOne', 'Undo one')),
                                                                                      column(2,
                                                                                             actionButton('selectmap', 'Select routes')),
                                                                                      column(6,
                                                                                             withSpinner(verbatimTextOutput('mapselIWC'), proxy.height = '100px'))
                                                                             ))
                                                            )
                                            )
                           ),
                           conditionalPanel(condition = 'input.tabsel == "totalvinter_pkt" || input.tabsel == "totalsommar_pkt"',
                                            fluidRow(column(4,
                                                            radioButtons('specrtPKTAnalyze', label = 'Select sites to include',
                                                                         choices = list(`All availble sites` = 'all',
                                                                                        `Counties (län)` = 'lan',
                                                                                        `Select on map` = 'karta'),
                                                                         selected = 'all'),
                                                            conditionalPanel(condition = 'input.specrtPKTAnalyze == "karta"',
                                                                             p('Define your area of interest by clicking on the map to set the vertices of a polygon. You can turn off the existing point data layers at the top right of the map.
                                                                               To start over and remove all points, click "Clear polygon". To remove only the last point you set, click "Undo one". When you are done, confirm your selection by clicking "Select routes".'))
                                                     ),
                                                     column(8,
                                                            conditionalPanel(condition = 'input.specrtPKTAnalyze == "lan"',
                                                                             uiOutput('lanPKTCheckboxAnalyze')),
                                                            conditionalPanel(condition = 'input.specrtPKTAnalyze == "karta"',
                                                                             leafletOutput("mapPKT"),
                                                                             fluidRow(column(2,
                                                                                             actionButton('clearMap', 'Clear polygon')),
                                                                                      column(2,
                                                                                             actionButton('clearOne', 'Undo one')),
                                                                                      column(2,
                                                                                             actionButton('selectmap', 'Select routes')),
                                                                                      column(6,
                                                                                             withSpinner(verbatimTextOutput('mapselPKT'), proxy.height = '100px'))
                                                                             ))
                                                            )
                                            )
                           ),
                           conditionalPanel(condition = 'input.tabsel == "totalkustfagel200"',
                                            fluidRow(column(4,
                                                            radioButtons('specrtKustAnalyze', label = 'Select routes to include',
                                                                         choices = list(`All available routes` = 'all',
                                                                                        `Counties (län)` = 'lan',
                                                                                        #`Province (landskap)` = 'lsk',
                                                                                        `Individual routes` = 'ind',
                                                                                        `Select on map` = 'karta'),
                                                                         selected = 'all'),
                                                     conditionalPanel(condition = 'input.specrtKustAnalyze == "karta"',
                                                                      p('Define your area of interest by clicking on the map to set the vertices of a polygon. You can turn off the existing point data layers at the top right of the map.
                                                                               To start over and remove all points, click "Clear polygon". To remove only the last point you set, click "Undo one". When you are done, confirm your selection by clicking "Select routes".'))
                                                     ),
                                                     column(8,
                                                            conditionalPanel(condition = 'input.specrtKustAnalyze == "lan"',
                                                                             uiOutput('lanKustCheckboxAnalyze')),
                                                            # conditionalPanel(condition = 'input.specrtKustAnalyze == "lsk"',
                                                            #                  uiOutput('lskKustCheckboxAnalyze')),
                                                            conditionalPanel(condition = 'input.specrtKustAnalyze == "karta"',
                                                                             leafletOutput("mapKust"),
                                                                             fluidRow(column(2,
                                                                                             actionButton('clearMap', 'Clear polygon')),
                                                                                      column(2,
                                                                                             actionButton('clearOne', 'Undo one')),
                                                                                      column(2,
                                                                                             actionButton('selectmap', 'Select routes')),
                                                                                      column(6,
                                                                                             withSpinner(verbatimTextOutput('mapselKust'), proxy.height = '100px'))
                                                                             ))
                                                     )
                                            ),
                                            fluidRow(column(12,
                                                            conditionalPanel(condition = 'input.specrtKustAnalyze == "ind"',
                                                                             uiOutput('indrtKustCheckboxAnalyze')))
                                            )
                           ),
                           conditionalPanel(condition = 'input.tabsel == "misc_census"',
                                            fluidRow(column(4,
                                                            radioButtons('specrtMiscAnalyze', label = 'Select sites to include',
                                                                         choices = list(`All available sites` = 'all',
                                                                                        `Filter by 'extra'` = 'extra'
                                                                         ),
                                                                         selected = 'all')),
                                                     column(8,
                                                            conditionalPanel(condition = 'input.specrtMiscAnalyze == "extra"',
                                                                             uiOutput('extraCheckboxAnalyze'))
                                                     )
                                            )
                           ),
                           hr(),
                           fluidRow(column(6,
                                           checkboxGroupInput('saveresult',
                                                              label = 'Result output to (will always be available in app)?', 
                                                              choices = list(#`R Workspace (named as trimOutput)` = 1,
                                                                             `.rdata-file` = 2,
                                                                             `Save graphs as pdf` = 3),
                                                              selected = c(1, 2, 3), inline = TRUE)
                                          ),
                                    column(6,
                                           textInput('filenameRes', label = 'Choose a name for your files:', value = 'trimOutput'))
                           ),
                           hr(),
                           fluidRow(column(6,
                                           actionButton("sendanalysis", "Run analysis", class = "keyButton")),
                                    column(6,
                                           downloadButton("downloadAnalysis", "Download rdata")
                                    )
                           ),
                           withSpinner(verbatimTextOutput('testtext2'), proxy.height = '100px'),
                           hr()
                  ),
                  tabPanel('Display results',
                           #plotOutput('plot' )
                           hr(),
                           fluidRow(column(6,
                                           textInput('displaysize', label = 'Result size (XX%):', value = '50%'),
                                           ),
                                    column(6,
                                           downloadButton("downloadPDF", "Download pdf")
                                           )
                                    ),
                           hr(),
                           withSpinner(uiOutput("plotResultsDisplay")),
                           hr()
                  ),
                  tabPanel('Summarize results',
                           hr(),
                           textInput('filenameResSumm', label = 'Filename used in analysis:', value = 'trimOutput'),
                           p(em('NOTE: This has to be the same filename as used in tab "Analyze data" for all of the monitoring systems you want summaries for.')),
                           hr(),
                           #textInput('yearBaseSumm', label = 'Base year:', value = '2002'),
                           uiOutput('yearBaseSummAuto'),
                           hr(),
                           p('What are the name(s) of the "monitoring systems" (tables in SFT) that you want summaries for. Can be 1 or several systems.'),
                           checkboxGroupInput('tableSumm', label = 'Select table(s)',
                                                               choices = list(`totalstandard`= "totalstandard",
                                                                              `totalsommar_pkt`= "totalsommar_pkt",
                                                                              `totalvinter_pkt`= "totalvinter_pkt",
                                                                              `Kustfagelrutor` = "totalkustfagel200",
                                                                              #`totalvatmark`= "totalvatmark"),
                                                                              `IWC Januari` = "total_iwc_januari",
                                                                              `IWC September` = "total_iwc_september",
                                                                              `Miscellaneous system` = "misc_census"),
                                                               selected = "totalstandard", inline = TRUE),
                           # hr(),
                           # p('Do you want single files (trimv201x...) for graph making (each system separately)? For example, do you also want Winter.'),
                           # checkboxInput('singleSumm', label = 'Single files', value = TRUE),
                           hr(),
                           p('Do you want "homepage" files (you will get one for each system)? This is the "overview data" file.'),
                           checkboxInput('homepageSumm', label = 'Homepage files', value = TRUE),
                           hr(),
                           p('Do you want to use shorter time periods for some species? (i.e. use the information in "SpeciesWithShorterTimePeriods.xls") If the system(s) you are running does not have such information in the xls-file it does not matter how you specify this.'),
                           checkboxInput('shorterPeriodSumm', label = 'Shorter time periods', value = TRUE),
                           hr(),
                           radioButtons('langSumm', label = 'Language of species name',
                                        choices = list(`SE` = "SE",
                                                       `EN` = "EN",
                                                       `World` = "WD"),
                                        selected = "SE", inline = TRUE),
                           hr(),
                           actionButton("sendquerysumm", "Generate excel files", class = "keyButton"),
                           hr(),
                           p('Download the generated files:'),
                           fluidRow(column(4,
                                           downloadButton("downloadComb", "Download combined table (Trimcombined Figurritning)")
                                           ),
                                    column(4,
                                           downloadButton("downloadSingle", "Download individual table (Figurritning)")
                                           ),
                                    column(4,
                                           downloadButton("downloadHomepage", "Download overview tables (Tabeller)")
                                           )
                                    ),
                           hr(),
                           p('You can find all the downloadable files here:'),
                           tags$a("Download files folder", href=url_extract, target="_blank", rel="noopener noreferrer"),
                           hr(),
                           verbatimTextOutput('listSites'),
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
           #ind = as.integer(input$indspecsp)
           ind = character(0),
           custom = character(0)
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
           #ind = as.integer(input$indspecspAnalyze)
           ind = character(0)
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
           ind = input$indspecrtAnalyze,
           karta = regStdat$internalSiteId[regStdat$internalSiteId%in%routeselKarta()]
           )
  })
  

  specrouteIWCAnalyze <- reactive({
    switch(input$specrtIWCAnalyze,
           all = regIWCdat$site,
           coast = regIWCdat$site[regIWCdat$ki=='K'],
           inland = regIWCdat$site[regIWCdat$ki=='I'],
           east = regIWCdat$site[regIWCdat$ki=='K' & regIWCdat$ev=='E'],
           west = regIWCdat$site[regIWCdat$ki=='K' & regIWCdat$ev=='V'],
           lan = regIWCdat$site[regIWCdat$lan%in%input$lanspecrtIWCAnalyze],
           karta = regIWCdat$site[regIWCdat$site%in%routeselKarta()])
  })
  

  specroutePKTAnalyze <- reactive({
    switch(input$specrtPKTAnalyze,
           all = regPKTdat$site,
           lan = regPKTdat$site[regPKTdat$lan%in%input$lanspecrtPKTAnalyze],
           karta = regPKTdat$site[regPKTdat$site%in%routeselKarta()])
  })
  
  
  specrouteKustAnalyze <- reactive({
    switch(input$specrtKustAnalyze,
           all = regKustdat$site,
           lan = regKustdat$site[regKustdat$lan%in%input$lanspecrtKustAnalyze],
           ind = input$indspecrtKustAnalyze,
           karta = regKustdat$site[regKustdat$site%in%routeselKarta()])
  })
  
  
  # spatially filter misc data for analysis
  specrouteMiscAnalyze <- reactive({
    switch(input$specrtMiscAnalyze,
           all = data()$site,
           extra = data()$site[data()$extra%in%input$extraspecrtAnalyze])
  })
  
  
  # get observed species data
  data <- eventReactive(input$sendquery,{
  
    startTime <- Sys.time()
    
    # error message in case no species were selected
    shiny::validate(
      need(length(input$indspecsp) > 0, "Please select at least one species.")
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

    if (input$tabsel == "misc_census") {
      
      # select time period by excluding rows of years > or < than selected period
      excl <- c()
      miscData2 <- miscData()
      for (ob in 1:nrow(miscData())) {
        if (miscData()$yr[ob] < input$selyrs[1] || miscData()$yr[ob] > input$selyrs[2]) {
          excl <- c(excl, ob)
        }
      }
      if (length(excl > 0)) {
        miscData2 <- miscData2[-excl,]
      }
      
      # apply species specific corrections
      # need columns "count" and "species"
      names(miscData2) <- c("extra", "site", "time", "species", "count")
      miscData3 <- applySpecificCorrections(miscData2, correctionsArt)
      
      # select species
      excl <- c()
      miscData4 <- miscData3
      for (ob in 1:nrow(miscData3)) {
        if (!as.integer(miscData3$species[ob]) %in% input$indspecsp) { 
          excl <- c(excl, ob)
        }
      }

      if (length(excl > 0)) {
        miscData4 <- miscData4[-excl,]
      }

      # remove duplicate values and pick the largest one (max)
      miscData5 <- aggregate(miscData4$count, by=list(extra=miscData4$extra, site=miscData4$site, time=miscData4$time, species=miscData4$species), FUN=max)
      colnames(miscData5) <- c("extra", "site", "time", "species", "count")
      if ((nrow(miscData4)-nrow(miscData5)) > 0) {
        output$duplicates <- renderPrint({
          print(paste(nrow(miscData4)-nrow(miscData5), 'duplicate values were removed. In these instances, the max values were kept.'))
        })
      }


        # Create the minus1 matrix
        # unique site-species
        miscDataTempUniqueSiteSpecies <- unique(cbind(miscData5$site, miscData5$species))
        #print(miscDataTemp)
        # merge with the unique times
        merge_minus1 <- merge(x= miscDataTempUniqueSiteSpecies, y=unique(miscData5$time))
        #print(merge_minus1)
        #write.csv(merge_minus1, file = 'test_merge_merge_minus1.csv', row.names = FALSE)
        colnames(merge_minus1) <- c("site", "species", "time")
        #print(merge_minus1)
        # fill with -1
        merge_minus2 <- merge(x= merge_minus1, y=(-1))
        #print(merge_minus2)
        colnames(merge_minus2) <- c("site", "species", "time", "count")

        #write.csv(merge_minus2, file = 'test_merge_minus.csv', row.names = FALSE)

        # create the zeros matrix
        #print(unique(miscData2$species))
        #print(miscData2)
        #print("onlyspecies0:")
        # get only the rows with species = 000
        onlySpecies0 = subset(miscData2, species=='000')
        #print("onlySpecies0")
        #print(onlySpecies0)
        # get the unique rows of site-time for art=000
        miscDataTempZeros <- unique(cbind(onlySpecies0$site, onlySpecies0$time))
        colnames(miscDataTempZeros) <- c("site", "time")
        #print(miscDataTempZeros)
        colnames(miscDataTempUniqueSiteSpecies) <- c("site", "species")
        # merge with the unique species
        merge_zeros <- merge(x= miscDataTempZeros, y=unique(miscDataTempUniqueSiteSpecies), by.x="site", by.y="site", all.x=TRUE, all.y=TRUE)
        #merge_zeros <- left_join(miscDataTempZeros, unique(miscDataTempUniqueSiteSpecies), by = "species")
        #print(merge_zeros)
        # fill with count=0
        merge_zeros_2 <- merge(x= merge_zeros, y=(0))
        colnames(merge_zeros_2) <- c("site", "time", "species", "count")

        #write.csv(merge_zeros_2, file = 'test_merge_zeros.csv', row.names = FALSE)

        # merge all of this 
      miscData6 <- mergeTabs(minus1 = merge_minus2, zeros = merge_zeros_2, stdcount = miscData5)
    
      # export and save data
      dataMerge <- miscData6
      exportSaveData(miscData6, savedat = input$savedat, filename = input$filenameDat, input$tabsel)
    }
    
    else if (input$databasechoice == "mongodb") {

      linepoint <- ""
      selectedPeriod <- ""

      if (input$tabsel == "totalstandard") {
        projectId <- project_id_std
        projectActivityId <- project_activity_id_std
        
        regStdat <<- getBiotopSitesMongo(projectId)

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
        
        regIWCdat <<- getIWCDataMongo(projectId)
      }
      else if (input$tabsel == "total_iwc_september") {
        projectId <- project_id_iwc
        projectActivityId <- project_activity_id_iwc

        selectedPeriod <- '"September"'
        
        regIWCdat <<- getIWCDataMongo(projectId)
      }
      else if (input$tabsel == "totalsommar_pkt") {
        projectId <- project_id_punkt
        projectActivityId <- project_activity_id_summer

        regPKTdat <<- getPKTDataMongo(projectId)
      }
      else if (input$tabsel == "totalvinter_pkt") {
        projectId <- project_id_punkt
        projectActivityId <- project_activity_id_winter
        
        regPKTdat <<- getPKTDataMongo(projectId)
        
        # error message in case no period was selected
        shiny::validate(
          need(length(input$specper) > 0, "Please select a monitoring period.")
        )

        selectedPeriod <- paste0('"', paste0(input$specper, collapse = '","'), '"')

      }
      else if (input$tabsel == "totalkustfagel200") {
        projectId <- project_id_kust
        projectActivityId <- project_activity_id_kust
        
        # get site data for coastal bird scheme
        # can use same function as for punktrutter schemes, because same columns required in output and similar structure in db
        regKustdat <<- getPKTDataMongo(project_id_kust)
        
      }


      rcdat <<- getSitesMongo(projectId)

      sitesMatchMongo <- getMatchSitesMongo(projectId)

      # get matching species
      #speciesMatch <- getMatchSpecies(poolParams, specart())
      #speciesMatchScientificNames <- getListBirdsUrl(bird_list_id, specart())
      speciesMatchScientificNames <- getMatchSpeciesSN(poolParams, input$indspecsp)

      result <- getCountData (projectActivityId = projectActivityId, speciesMatch = speciesMatch, speciesMatchSN = speciesMatchScientificNames, sitesMatchMongo = sitesMatchMongo, yearsSel = input$selyrs, linepoint = linepoint, selectedPeriod = selectedPeriod, correctionsArt = correctionsArt)

      # aggregate by getting the maximum value in case of doublon
      # (works as well for iwc when boat/land can be done the same year)
      dataMerge <- aggregate(result$count, by=list(site=result$site, species=result$species, time=result$time), FUN=max)
      colnames(dataMerge) <- c("site", "species", "time", "count")
      
      if ((nrow(result)-nrow(dataMerge)) > 0) {
        output$duplicates <- renderPrint({
          print(paste(nrow(result)-nrow(dataMerge), 'duplicate values were removed. In these instances, the max values were kept.'))
        })      
      }

      endTime <- Sys.time()
      Coords$queryTime <- diff(c(startTime, endTime))
      
      exportSaveData(dataMerge, savedat = input$savedat, filename = input$filenameDat, input$tabsel)
      
    }
    else if (input$databasechoice == "psql") {

      rcdat <<- getSites(pool)

      if (input$tabsel == "totalstandard") {
        regStdat <<- getBiotopSites(pool)
      }
      else if (input$tabsel == "total_iwc_januari" | input$tabsel == "total_iwc_september") {
        regIWCdat <<- getIWCData(pool)
      }
      else if (input$tabsel == "totalsommar_pkt" | input$tabsel == "totalvinter_pkt") {
        regPKTdat <<- getPKTData(pool)
      }
      else if (input$tabsel == "totalkustfagel200") {
        regKustdat <<- getKustData(pool)
      }

      dataMerge <- DoQuery(pool = pool, tab = input$tabsel, spec=input$indspecsp,
            specper = input$specper, selyrs = input$selyrs, line = input$linepoint,
            savedat = input$savedat, filename = input$filenameDat)
    } 
    
    endTime <- Sys.time()
    Coords$queryTime <- diff(c(startTime, endTime))
    
    return(dataMerge)
    
  })
  
  resultout <- eventReactive(input$sendanalysis, {
    
    # error message in case no species were selected
    shiny::validate(
      need(length(input$indspecspAnalyze) > 0, "Please select at least one species.")
    )

    print(paste("start analysis ", Sys.time()))

    if (input$addNS){
      dat <- AddNSspecies(data = data(), rangedata = rangedat, coorddata = rcdat)
    } else {
      dat <- data()
    }
    
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
    else if (input$tabsel == 'totalkustfagel200') {
      rix <- dat$site%in%specrouteKustAnalyze()
    }
    else if (input$tabsel=='misc_census') {
      rix <- dat$site%in%specrouteMiscAnalyze()
    } 
    else {
      rix <- !logical(nrow(dat))
    }
    dat <- subset(dat, tix & rix)
    # error message in case the subset contains no data
    shiny::validate(
      need(nrow(dat) > 0, "There was no data found that matches your selection.")
    )
    spAix <- input$indspecspAnalyze %in% as.integer(unique(dat$species))
    styr <- if(input$tabsel=='total_iwc_januari' | input$tabsel=='total_iwc_september'){
                NULL
              } else {
                startyr[startyr$Delprogram==input$tabsel, c('Art', 'StartYear')]
              }

    RunTRIMmodel(dat = dat, modeltype = as.integer(input$modeltype), sp_to_run = input$indspecspAnalyze[spAix],
                 odisp = 'od'%in%input$trimset, sercor = 'sc'%in%input$trimset,
                 autodel = 'ad'%in%input$trimset,  speciesdat = spdat,
                 startyr = styr, tabell = input$tabsel,
                 saveresult = input$saveresult, filename = input$filenameRes)
  })



  summarizeRt <- eventReactive(input$sendquerysumm, {
    
    # error message in case no monitoring scheme was selected
    shiny::validate(
      need(length(input$tableSumm) > 0, "ERROR: Please select at least one monitoring scheme.")
    )
    # error message in case entered base year is out of range of the data
    shiny::validate(
      need(input$yearBaseSumm >= input$selyrsAnalyze[1] & input$yearBaseSumm <= input$selyrsAnalyze[2], paste0("ERROR: Base year must be in range: ", input$selyrsAnalyze[1], " - ", input$selyrsAnalyze[2]))
    )

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

    # parameters/settings sheet
    db <- input$databasechoice
    tab <- input$tabsel
    lp <- input$linepoint
    if (!input$tabsel == 'totalstandard') {
      lp <- 'NOT APPLICABLE'
    }
    else if (lp[1] == TRUE) {
      lp <- 'lines'
    }
    else if (lp[2] == TRUE) {
      lp <- 'points'
    }
    per <- input$specper
    if (!input$tabsel == 'totalvinter_pkt') {
      per <- 'NOT APPLICABLE'
    }
    yrs <- c(input$selyrsAnalyze)
    # divide species into ones that gave results and ones that didn't
    worked <- c()
    not <- c()
    for (s in 1:length(resultout())) {
      if ('trim' %in% class(resultout()[[s]]$value)) {
        worked <- c(worked, gsub('Art_', '', names(resultout())[s]))
      } else {
        not <- c(not, gsub('Art_', '', names(resultout())[s]))
      }
    }
    if (is.null(worked)) {
      worked <- 'NONE'
    }
    if (is.null(not)) {
      not <- 'NONE'
    }
    N_S <- input$addNS
    corr <- c(input$specifCorrections)
    if (is.null(corr)) {
      corr <- 'NOT APPLICABLE'
    }
    mod <- input$modeltype
    sett <- c(input$trimset)
    sett[sett == 'od'] <- 'overdispersion'
    sett[sett == 'sc'] <- 'serial correlation'
    sett[sett == 'ad'] <- 'autodelete'
    # spatial filter
    filter <- 0
    sel <- 0
    sites <- 0
    if (input$tabsel == 'totalstandard') {
      filter <- input$specrtAnalyze
      # all selected sites
      sites <- specrouteAnalyze()
      if (filter == 'lan') {
        sel <- input$lanspecrtAnalyze
      }
      else if (filter == 'lsk') {
        sel <- input$lskspecrtAnalyze
      }
      else if (filter == 'fjl104' | filter == 'fjl142') {
        sel <- input$fjlspecrtAnalyze
      }
      else if (filter == 'ind') {
        sel <- input$indspecrtAnalyze
      }
      else if (filter == 'karta') {
        # coordinates of polygon
        sel <- c()
        for (co in 1:nrow(Coords$aoi[[1]])) {
          sel <- c(sel, Coords$aoi[[1]][co,])
        }
      }
    }
    else if (input$tabsel == 'total_iwc_januari' | input$tabsel == 'total_iwc_september') {
      filter <- input$specrtIWCAnalyze
      # all selected sites
      sites <- specrouteIWCAnalyze()
      if (filter == 'lan') {
        sel <- input$lanspecrtIWCAnalyze
      }
      else if (filter == 'karta') {
        # coordinates of polygon
        sel <- c()
        for (co in 1:nrow(Coords$aoi[[1]])) {
          sel <- c(sel, Coords$aoi[[1]][co,])
        }
      }
    }
    else if (input$tabsel == 'totalvinter_pkt' | input$tabsel == 'totalsommar_pkt') {
      filter <- input$specrtPKTAnalyze
      # all selected sites
      sites <- specroutePKTAnalyze()
      if (filter == 'lan') {
        sel <- input$lanspecrtPKTAnalyze
      }
      else if (filter == 'karta') {
        # coordinates of polygon
        sel <- c()
        for (co in 1:nrow(Coords$aoi[[1]])) {
          sel <- c(sel, Coords$aoi[[1]][co,])
        }
      }
    }
    else if (input$tabsel == 'totalkustfagel200') {
      filter <- input$specrtKustAnalyze
      # all selected sites
      sites <- specrouteKustAnalyze()
      if (filter == 'lan') {
        sel <- input$lanspecrtKustAnalyze
      }
      else if (filter == 'ind') {
        sel <- input$indspecrtKustAnalyze
      }
      else if (filter == 'karta') {
        # coordinates of polygon
        sel <- c()
        for (co in 1:nrow(Coords$aoi[[1]])) {
          sel <- c(sel, Coords$aoi[[1]][co,])
        }
      }
    }
    else if (input$tabsel == 'misc_census') {
      filter <- input$specrtMiscAnalyze
      # all selected sites
      sites <- specrouteMiscAnalyze()
      if (filter == 'extra') {
        sel <- input$extraspecrtAnalyze
      }
    }
    if (sel[1] == 0) {
      sel <- 'NOT APPLICABLE'
    }
    
    
    values <- c(db, tab,lp, per, list(yrs), list(worked), list(not), N_S, list(corr), mod, list(sett), filter, list(sel), list(sites))
    mat <- sapply(values, '[', seq(max(sapply(values, length))))
    mat[is.na(mat)] <- ''
    params <- data.frame(mat)
    colnames(params) <- list('database', 'scheme', 'subscheme', 'period', 'time range', 'species (with result)', 'species (without result)', 'North-South', 'corrections', 'modeltype', 'trimsettings', 'spatial filter', 'selected', 'sites selected')
    
    # use data frame 'params' in DoSummarizeResult() function to add it as a sheet in the excel output 'Tabeller'
    # create excel files
    DoSummarizeResult(filenames=input$filenameResSumm, tables=c(input$tableSumm), base=strtoi(input$yearBaseSumm), spdat=spdat, startyr=startyr, homepage=input$homepageSumm, lang=input$langSumm, params = params) 

  })

  getspecies <- eventReactive(input$regenerateSpecies, {

    spdat <<- getListsFromAla(poolParams)

    #print(spdat)
  })


  # provide excel template for misc census data
  output$misc_template <- downloadHandler(
    filename = "misc_data_upload_template.xlsx",
    content = function(file) {
      template <- read_xlsx(paste0(path_project_templates, "misc_data_upload_template.xlsx"), col_names = TRUE, col_types = c("text", "text", "numeric", "text", "numeric"))
      write_xlsx(template, file)
    }
  )
  
  # read data file uploaded by user 
  output$misc_data_contents <- renderPrint(input$misc_data)
  
  output$contents <- renderTable({
    file <- input$misc_data
    req(file)

    ext <- tools::file_ext(file$datapath)

    if (ext == "csv") {
      miscData <- read.csv(file$datapath, header = TRUE, colClasses = c("extra" = "character", "karta" = "character", "yr" = "integer", "art" = "character", "ind" = "integer"))
    }
    else if (ext == "xlsx" | ext == "xls") {
      miscData <- read_excel(file$datapath, col_names = TRUE, col_types = c("text", "text", "numeric", "text", "numeric"))
    }
    else {
      print("ERROR: Please upload a csv file or an excel file")
    }
    
    head(miscData)
  })
  

  miscData <- eventReactive(input$misc_data,{
    file <- input$misc_data
    req(file)
    
    ext <- tools::file_ext(file$datapath)
    
    if (ext == "csv") {
      miscData <- read.csv(file$datapath, header = TRUE, colClasses = c("extra" = "character", "karta" = "character", "yr" = "integer", "art" = "character", "ind" = "integer"))
    }
    else if (ext == "xlsx" | ext == "xls") {
      miscData <- read_excel(file$datapath, col_names = TRUE, col_types = c("text", "text", "numeric", "text", "numeric"))
    }
    else {
      print("ERROR: Please upload a csv file or an excel file")
    }
  })
  

  observeEvent(input$misc_data, {
    output$reminder <- renderUI({
      strong(em('The file is uploaded, but remember to still run the query!'))})
  })

  
  output$resultGenerateSpecies <- renderPrint({
    getspecies()})


  # update filename in tab 'Summarize results' if filename in tab 'Analyze data' is edited
  observe({
    input$filenameRes
    updateTextInput(inputId = 'filenameResSumm', value = input$filenameRes)
  })
  
  # update preselected monitoring scheme in tab 'Summarize results' if different scheme is selected in tab 'Get data'
  # and reset species selection buttons
  observe({
    input$tabsel
    updateCheckboxGroupInput(inputId = 'tableSumm', selected = input$tabsel)
    updateRadioButtons(inputId = 'specsp', selected = 'all')
    updateRadioButtons(inputId = 'specspAnalyze', selected = 'all')
  })
  observe({
    input$sendquery
    updateRadioButtons(inputId = 'specspAnalyze', selected = 'all')
  })
  observe({
    input$databasechoice
    updateRadioButtons(inputId = 'specsp', selected = 'all')
    updateRadioButtons(inputId = 'specspAnalyze', selected = 'all')
  })
  
  # show slider ranging from first to last (current) year of selected scheme
  observe({input$misc_data
    output$yrSlider <- renderUI({
      if (input$tabsel == "misc_census") {
        shiny::validate(
          need(!is.null(input$misc_data), "Please upload a csv file or an excel file containing your data.")
        )
        
        sliderInput(inputId = 'selyrs', label = 'Set years',
                  min = min(miscData()$yr), max = max(miscData()$yr), value = c(min(miscData()$yr), max(miscData()$yr)),
                  step = 1, sep = NULL)
      }
      else {
        sliderInput(inputId = 'selyrs', label = 'Set years',
                    min = yrStart$year[yrStart$table == input$tabsel], max = as.integer(format(Sys.Date(), '%Y')), value = c(2017, as.integer(format(Sys.Date(), '%Y'))),
                    step = 1, sep = NULL)
      }
    })
  })

  # year range for slider in tab 'analyze data' based on acquired data
  output$yrSliderAnalyze <- renderUI({
    yrs <- range(data()$time)
    sliderInput(inputId = 'selyrsAnalyze', label = 'Set years',
                min = yrs[1], max = yrs[2], value = c(yrs[1], yrs[2]),
                step = 1, sep = NULL)
  })

  # set default base year as first year of selected time span
  output$yearBaseSummAuto <- renderUI({
    #yrs <- range(data()$time)
    tags$div(textInput('yearBaseSumm', label = 'Base year:', value = input$selyrsAnalyze[1]))
  })
    
  # show species list from selected choice of database
  observe({ input$misc_data
    output$specCheckbox <- renderUI({
      if (input$tabsel == "misc_census") {
        shiny::validate(
          need(!is.null(input$misc_data), "Please upload a csv file or an excel file containing your data.")
        )
        
        species <- c(as.integer(unique(miscData()$art)))
        species_string <- paste0(species, collapse = ",")
        spdat <- getSpeciesNames(poolParams, species_string)
        
        # get a sorted list of the ranks of the species that exist in both objects
        specranks <- spdat$rank
        specranks <- sort(specranks)
        speclist <- as.list(specranks)
      }
  
      else if (input$databasechoice == 'mongodb') {
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
        # else if (input$tabsel == "totalvinter_pkt") { # what is this else if?
        #   projectId <- project_id_punkt
        #   projectActivityId <- project_activity_id_iwc
        # } 
        else if (input$tabsel == "totalkustfagel200") {
          projectId <- project_id_kust
          projectActivityId <- project_activity_id_kust
        }
    
        specsSN <- getUniquesSpeciesFromScheme(projectActivityId)
        
        nbSp <- nrow(specsSN)
    
        vSpecies <- vector()
        for (iSp  in 1:nbSp) {
          # check if species name exists in both objects. If not, print validation error
          shiny::validate(
            need(str_trim(specsSN$name[iSp]) %in% attributes(speciesMatch)$names, paste0("ERROR in retrieving the species items. This item can't be found in the lists module: ", specsSN$name[iSp]))
          )
          vSpecies[iSp] <- speciesMatch[[str_trim(specsSN$name[iSp])]]
        }
        
        # get a sorted list of the ranks of the species that exist in both objects
        specranks <- spdat$rank[match(vSpecies,spdat$art)]
        specranks <- sort(specranks)
        speclist <- as.list(specranks)
      }
      
      else {
        queryart <- sprintf("select distinct art
                  from %s", input$tabsel)
        arts <- dbGetQuery(pool, queryart)
        
        # get a sorted list of the ranks of the species that exist in both objects
        specranks <- spdat$rank[match(arts$art,spdat$art)]
        specranks <- sort(subset(specranks, specranks < 3880))
        speclist <- as.list(specranks)
      }
      
      # assign the associated Swedish species names to the ranks 
      specnames <- c()
      specarts <- c()
      for (spec in 1:length(speclist)) {
        rank <- speclist[spec]
        specnames[spec] <- spdat$arthela[spdat$rank == rank]
        specarts[spec] <- as.integer(spdat$art[spdat$rank == rank])
      }
      names(speclist) <- specnames
      attr(speclist, 'art') <- specarts
      
      tags$div(tags$div(strong(p("Select species"))),
               tags$div(align = 'left',
                        class = 'multicol6',
                        checkboxGroupInput(inputId = 'indspecsp', label = NULL,
                                           choiceNames = attributes(speclist)$names,
                                           choiceValues = attr(speclist, 'art'),
                                           selected = attr(speclist, 'art'))
               )
      )
    })
  })
  
  # update species selection based on selected preset option
  observe({
    input$specsp
    updateCheckboxGroupInput(inputId = 'indspecsp', selected = specart())
  })
  
  # provide excel template for custom species set data
  output$specTemplate <- downloadHandler(
    filename = "species_set_template.xlsx",
    content = function(file) {
      template <- read_xlsx(paste0(path_project_templates, "species_set_template.xlsx"), sheet = "species_set", col_names = TRUE, col_types = c("text", "text"))
      instructions <- read_xlsx(paste0(path_project_templates, "species_set_template.xlsx"), sheet = "README")
      write_xlsx(list('species_set' = template, 'README' = instructions), file)
    }
  )
  
  # read in custom species set from uploaded file
  output$specset_contents <- renderPrint({
    file <- input$specset
    req(file)
    
    ext <- tools::file_ext(file$datapath)
    shiny::validate(need(ext %in% c("csv", "xlsx", "xls"), "Please upload an excel or csv file"))
    
    if (ext == "csv") {
      customSpecs <- read.csv(file$datapath)
    }
    else {
      customSpecs <- read_excel(file$datapath)
    }
    
    vcustomSpecs <- as.vector(as.integer(customSpecs$art))
    
    updateCheckboxGroupInput(inputId = 'indspecsp', selected = vcustomSpecs)
    
    print(paste('Your set contains the following species:', list(vcustomSpecs)))
    
    notfound <- c()
    for (s in 1:length(vcustomSpecs)) {
      if (!vcustomSpecs[s] %in% input$indspecsp) {
        notfound <- c(notfound, vcustomSpecs[s])
      }
    }
    if (length(notfound) > 0) {
      print(paste('These species of your set were not found in the species list:', list(notfound)))
    }
    
  })
  
  # output species selection in tab 'Analyze data'
  output$specCheckboxAnalyze <- renderUI({
    specs <- sort(unique(data()$species))
    if(input$addNS){
      specs <- sort(c(specs,
                      rangedat$art[rangedat$speciesmain%in%specs],
                      '645'[any(c('243', '244', '245')%in%specs)]))
    }
    # get a sorted list of the ranks of the species
    specranks <- spdat$rank[match(specs,spdat$art)]
    specranks <- sort(specranks)
    speclist <- as.list(specranks)
    
    # assign the associated Swedish species names and art to the ranks 
    specnames <- c()
    specarts <- c()
    for (spec in 1:length(speclist)) {
      rank <- speclist[spec]
      specnames[spec] <- spdat$arthela[spdat$rank == rank]
      specarts[spec] <- as.integer(spdat$art[spdat$rank == rank])
    }
    names(speclist) <- specnames
    attr(speclist, 'art') <- specarts
    
    tags$div(tags$div(strong(p("Select species"))),
             tags$div(align = 'left',
                      class = 'multicol6',
                      checkboxGroupInput(inputId = 'indspecspAnalyze', label = NULL,
                                         choiceNames = attributes(speclist)$names,
                                         choiceValues = attr(speclist, 'art'),
                                         selected = attr(speclist, 'art'))
             )
    )
  })
  # update species selection based on selected preset option
  observe({
    input$specspAnalyze
    updateCheckboxGroupInput(inputId = 'indspecspAnalyze', selected = specartAnalyze())
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
  

  # define outputs for filter checkboxes for kustfagel in tab 'analyze data'
  output$lanKustCheckboxAnalyze <- renderUI({
    lans <- sort(unique(regKustdat$lan[nchar(regKustdat$lan) > 0]))
    lanlist <- as.list(lans)
    tags$div(tags$div(strong(p("Select county(ies)"))),
             tags$div(align = 'left',
                      class = 'multicol6',
                      checkboxGroupInput(inputId = 'lanspecrtKustAnalyze', label = NULL,
                                         choices = lanlist,
                                         selected = NULL)
             )
    )
  })
  
  output$indrtKustCheckboxAnalyze <- renderUI({
    tags$div(tags$div(strong(p("Select route(s)"))),
             tags$div(align = 'left',
                      class = 'multicol8',
                      checkboxGroupInput(inputId = 'indspecrtKustAnalyze', label = NULL,
                                         choices = as.list(regKustdat$site),
                                         selected = NULL)
             )
    )
  })

                                         
  # filter for misc census
  output$extraCheckboxAnalyze <- renderUI({
    extras <- sort(unique(data()$extra))
    extralist <- as.list(extras)
    tags$div(tags$div(strong(p("Select"))),
             tags$div(align = 'left',
                      class = 'multicol8',
                      checkboxGroupInput(inputId = 'extraspecrtAnalyze', label = NULL,
                                         choices = extralist,
                                         selected = NULL)
             )
    )
  })


  # map for selection of individual routes/sites
  observe({
    input$sendquery
    output$map <- renderLeaflet({ 
      leaflet() |> 
        addTiles() |> 
        setView(15, 63, zoom = 4) |>
        addCircleMarkers(data = rcdat, lng = rcdat$lon, lat = rcdat$lat, color = 'blue', radius = 2, group = 'all routes') |>
        addCircleMarkers(data = rcdat, lng = rcdat$lon[rcdat$site %in% data()$site], lat = rcdat$lat[rcdat$site %in% data()$site], color = 'red' , radius = 2, group = 'routes with observations') |>
        addLegend(position = c('topleft'), colors = c('blue', 'red', 'black'), labels = c('routes of the scheme', 'routes present in your data', 'area of interest')) |>
        addLayersControl(overlayGroups = c('all routes', 'routes with observations'))
    }) 
  })
  
  observe({
    input$sendquery
    output$mapIWC <- renderLeaflet({ 
      leaflet() |> 
        addTiles() |> 
        setView(15, 63, zoom = 4) |>
        addCircleMarkers(data = rcdat, lng = rcdat$lon, lat = rcdat$lat, color = 'blue', radius = 2, group = 'all routes') |>
        addCircleMarkers(data = rcdat, lng = rcdat$lon[rcdat$site %in% data()$site], lat = rcdat$lat[rcdat$site %in% data()$site], color = 'red' , radius = 2, group = 'routes with observations') |>
        addLegend(position = c('topleft'), colors = c('blue', 'red', 'black'), labels = c('routes of the scheme', 'routes present in your data', 'area of interest')) |>
        addLayersControl(overlayGroups = c('all routes', 'routes with observations'))
    })
  })
  
  observe({
    input$sendquery
    output$mapPKT <- renderLeaflet({ 
      leaflet() |> 
        addTiles() |> 
        setView(15, 63, zoom = 4) |>
        addCircleMarkers(data = rcdat, lng = rcdat$lon, lat = rcdat$lat, color = 'blue', radius = 2, group = 'all routes') |>
        addCircleMarkers(data = rcdat, lng = rcdat$lon[rcdat$site %in% data()$site], lat = rcdat$lat[rcdat$site %in% data()$site], color = 'red' , radius = 2, group = 'routes with observations') |>
        addLegend(position = c('topleft'), colors = c('blue', 'red', 'black'), labels = c('routes of the scheme', 'routes present in your data', 'area of interest')) |>
        addLayersControl(overlayGroups = c('all routes', 'routes with observations'))
    })
  })
  
  observe({
    input$sendquery
    output$mapKust <- renderLeaflet({ 
      leaflet() |> 
        addTiles() |> 
        setView(15, 63, zoom = 4) |>
        addCircleMarkers(data = rcdat, lng = rcdat$lon, lat = rcdat$lat, color = 'blue', radius = 2, group = 'all routes') |>
        addCircleMarkers(data = rcdat, lng = rcdat$lon[rcdat$site %in% data()$site], lat = rcdat$lat[rcdat$site %in% data()$site], color = 'red' , radius = 2, group = 'routes with observations') |>
        addLegend(position = c('topleft'), colors = c('blue', 'red', 'black'), labels = c('routes of the scheme', 'routes present in your data', 'area of interest')) |>
        addLayersControl(overlayGroups = c('all routes', 'routes with observations'))
    })
  })
  
  # Create reactive values to store the coordinates of the polygon for access from multiple functions in local environment
  Coords <- reactiveValues(lng = NULL, lat = NULL, aoi = NULL, queryTime = NULL)
  
  # draw polygon on map
  observe({
    if (input$tabsel == 'totalstandard') {
      req(input$map_click)
      mapevent <- input$map_click
      map <- 'map'
    }
    else if (input$tabsel == 'total_iwc_januari' | input$tabsel == 'total_iwc_september') {
      req(input$mapIWC_click)
      mapevent <- input$mapIWC_click
      map <- 'mapIWC'
    }
    else if (input$tabsel == 'totalsommar_pkt' | input$tabsel == 'totalvinter_pkt') {
      req(input$mapPKT_click)
      mapevent <- input$mapPKT_click
      map <- 'mapPKT'
    }
    else if (input$tabsel == 'totalkustfagel200') {
      req(input$mapKust_click)
      mapevent <- input$mapKust_click
      map <- 'mapKust'
    }
    else {
      req(input$mapXXX_click)
    }
    
    observeEvent(mapevent, { 
      if (is.null(Coords$lng)) {
        Coords$lng <- c(mapevent$lng)
        Coords$lat <- c(mapevent$lat)
        coords <- data.frame(lng=Coords$lng, lat=Coords$lat)
        
        proxy <- leafletProxy(map)
        proxy %>% addCircles(data = coords, lng = coords$lng, lat = coords$lat, color = 'black')
      }
      else {
        Coords$lng <- c(Coords$lng, mapevent$lng)
        Coords$lat <- c(Coords$lat, mapevent$lat)
        coords <- data.frame(lng=Coords$lng, lat=Coords$lat)
        
        proxy <- leafletProxy(map)
        proxy %>% addCircles(data = coords, lng = coords$lng, lat = coords$lat, color = 'black')
        proxy %>% addPolygons(layerId = 'area', data = coords, lng = coords$lng, lat = coords$lat, color = 'black')
      }
    })
  })
  
  # clear polygon from map
  observe({
    input$clearMap
    input$sendquery
    if (input$tabsel == 'totalstandard') {
      map <- 'map'
    }
    else if (input$tabsel == 'total_iwc_januari' | input$tabsel == 'total_iwc_september') {
      map <- 'mapIWC'
    }
    else if (input$tabsel == 'totalsommar_pkt' | input$tabsel == 'totalvinter_pkt') {
      map <- 'mapPKT'
    }
    else if (input$tabsel == 'totalkustfagel200') {
      map <- 'mapKust'
    }
    else {
      req(input$mapXXX)
    }
    proxy <- leafletProxy(map)
    proxy %>% clearShapes()
    Coords$lng <- NULL
    Coords$lat <- NULL
  })
  
  # clear last drawn point of polygon from map
  observeEvent(input$clearOne, {
    req(Coords$lat, Coords$lng)
    Coords$lng <- Coords$lng[1:(length(Coords$lng)-1)]
    Coords$lat <- Coords$lat[1:(length(Coords$lat)-1)]
    coords <- data.frame(lng=Coords$lng, lat=Coords$lat)
    if (input$tabsel == 'totalstandard') {
      map <- 'map'
    }
    else if (input$tabsel == 'total_iwc_januari' | input$tabsel == 'total_iwc_september') {
      map <- 'mapIWC'
    }
    else if (input$tabsel == 'totalsommar_pkt' | input$tabsel == 'totalvinter_pkt') {
      map <- 'mapPKT'
    }
    else if (input$tabsel == 'totalkustfagel200') {
      map <- 'mapKust'
    }
    proxy <- leafletProxy(map)
    proxy %>% clearShapes()
    proxy %>% addCircles(data = coords, lng = coords$lng, lat = coords$lat, color = 'black')
    proxy %>% addPolygons(layerId = 'area', data = coords, lng = coords$lng, lat = coords$lat, color = 'black')
  })
  
  # select routes based on polygon the user defined
  routeselKarta <- eventReactive(input$selectmap,{
    # error message for if no area was defined
    shiny::validate(
      need(length(Coords$lng) > 2  & length(Coords$lat) > 2, 'There are not enough points set on the map to create a polygon.')
    )
    # create polygon
    polyCoords <- c()
    for (p in 1:length(Coords$lng)) {
      polyCoords <- rbind(polyCoords, c(Coords$lng[p], Coords$lat[p]))
    }
    polyCoords <- rbind(polyCoords, c(Coords$lng[1], Coords$lat[1]))
    Coords$aoi <- st_polygon(list(polyCoords))
    # create multipoint object
    pointcoords <- data.frame(rcdat[, 2:3])
    routes <- st_multipoint(as.matrix(pointcoords))
    # use created polygon to select sites
    routesel <- st_intersection(routes, Coords$aoi)
    sitesel <- rcdat$site[rcdat$lon %in% routesel[,1] & rcdat$lat %in% routesel[,2]]
  })
  
  # give the user feedback about route selection after they clicked the action button
  output$mapsel <- renderPrint({
    paste0('A total of ', length(routeselKarta()), ' routes have been selected.')
  })
  
  output$mapselIWC <- renderPrint({
    paste0('A total of ', length(routeselKarta()), ' routes have been selected.')
  })
  
  output$mapselPKT <- renderPrint({
    paste0('A total of ', length(routeselKarta()), ' routes have been selected.')
  })

  output$mapselKust <- renderPrint({
    paste0('A total of ', length(routeselKarta()), ' routes have been selected.')
  })
  
  
  output$querytime <- renderText({
    req(data())
    print(paste0('Query executed in ', Coords$queryTime, ' seconds.'))
    })
  output$querytimetable <- renderPrint({
    if (input$databasechoice == 'mongodb' & input$tabsel != 'misc_census') {
      req(data())
      req(times)
      times
    }})
  output$testtext2 <- renderPrint({
    resultout()[[1]]})
  # output$testtext <- renderPrint({
  #   as.integer(input$modeltype)})
  output$testtext <- renderPrint({
    session$clientData$output_plot_width})
  
  output$listSites <- renderPrint({
    sites_df <- data.frame(art=c(rep(0, length(resultout()))), sites = c(rep(0, length(resultout()))))
    for (art in 1:length(resultout())) {
      sites_df$art[art] <- names(resultout())[art]
      obj <- resultout()[[art]]
      sites_df$sites[art] <- list(obj$value$site_id)
    }
    sites_df
  })

  output$rtSumm <- renderPrint({
    summarizeRt()})


  output$dataTable <- DT::renderDataTable({
    DT::datatable(data(), filter='top')
    })
  
  
  # configure download buttons
  observe({
    input$sendquery
    req(input$sendquery)
    Sys.sleep(1)
    # enable the download buttons
    if (2 %in% input$savedat) {
      shinyjs::enable("downloadCSV")
    }
    if (3 %in% input$savedat) {
      shinyjs::enable("downloadCSV2")
    }
    if (4 %in% input$savedat) {
      shinyjs::enable("downloadRDATA")
    }

    output$downloadCSV <- downloadHandler(
      filename = paste0(input$filenameDat, '_', gsub('[ :]', '_', round(Sys.time(),0)), '.csv'),
      content = function(file) {
        write.csv(data(), file, row.names = FALSE)
      }
    )
    
    output$downloadCSV2 <- downloadHandler(
      filename = paste0(input$filenameDat, '_', gsub('[ :]', '_', round(Sys.time(),0)), '.csv'),
      content = function(file) {
        write.csv2(data(), file, row.names = FALSE)
      }
    )
    
    output$downloadRDATA <- downloadHandler(
      filename = paste0(input$filenameDat, '_', gsub('[ :]', '_', round(Sys.time(),0)), '.rdata'),
      content = function(file) {
        save(data(), file = file)
      }
    )
  })

  observe({
    input$sendanalysis
    req(input$sendanalysis)
    Sys.sleep(1)
    # enable the download buttons
      if (2 %in% input$saveresult) {
          shinyjs::enable("downloadAnalysis")
          result <- resultout()
        }
      if (3 %in% input$saveresult) {
          shinyjs::enable("downloadPDF")
        }
  
    output$downloadAnalysis <- downloadHandler(
      filename = paste0(input$filenameRes, '_', gsub('[ :]', '_', round(Sys.time(),0)), '.rdata'),
      content = function(file) {
        save(result, file = file)
      }
    )
    
    output$downloadPDF <- downloadHandler(
      filename = paste0(input$filenameRes, '.pdf'),
      content = function(file) {
        file.copy(from=paste0(path_project_extract,input$filenameRes, '.pdf'), to=file)
      }
    )
  })
  
  observe({
    input$sendquerysumm
    req(input$sendquerysumm)
    Sys.sleep(1)
    # enable the download buttons
    if (length(input$tableSumm) > 1) {
        shinyjs::enable("downloadComb")
      }
    if (length(input$tableSumm) == 1) {
        shinyjs::enable("downloadSingle")
      }
    if (input$homepageSumm) {
        shinyjs::enable("downloadHomepage")
      }
  
    # download file reporting on all selected schemes next to each other
    output$downloadComb <- downloadHandler(
      filename = paste0('Trimcombined_', 'Figurritning_', input$filenameResSumm, '_', gsub('[ :]', '_', round(Sys.time(),0)), '.xlsx'),
      content = function(file) {
        write_xlsx(summarizeRt()[[1]], file, format_headers = TRUE)
      }
    )
    
    # download file reporting on all selected schemes individually
    output$downloadSingle <- downloadHandler(
      filename = paste0('Trim_', 'Figurritning_', input$filenameResSumm, '_', gsub('[ :]', '_', round(Sys.time(),0)), '.xlsx'),
      content = function(file) {
        # if (length(input$tableSumm) == 1) {
            write_xlsx(summarizeRt()[[1]], file, format_headers = TRUE)
        # }
          # else if (length(input$tableSumm) > 1) {
          #   write_xlsx(summarizeRt()[[2]], file, format_headers = TRUE)
          # }
      }
    )
    
    # download overview data file on all selected schemes individually
    output$downloadHomepage <- downloadHandler(
      filename = paste0('Trim_', 'Tabeller_', input$filenameResSumm, '_', gsub('[ :]', '_', round(Sys.time(),0)), '.xlsx'),
      content = function(file) {
        # if (length(input$tableSumm) == 1) {
        write_xlsx(summarizeRt()[[2]], file, format_headers = TRUE)
        # }
          # else if (length(input$tableSumm) > 1) {
          #   write_xlsx(summarizeRt()[[3]], file, format_headers = TRUE)
          # }
        }
    )
  })
  
  shinyjs::disable("downloadCSV")
  shinyjs::disable("downloadCSV2")
  shinyjs::disable("downloadRDATA")
  shinyjs::disable("downloadAnalysis")
  shinyjs::disable("downloadPDF")
  shinyjs::disable("downloadComb")
  shinyjs::disable("downloadSingle")
  shinyjs::disable("downloadHomepage")
  

  output$plot <- renderPlot({
    worked <- sapply(resultout(), function(x) inherits(x$value,'trim'))
    restoplot <- resultout()[worked]
    styr <- if(input$tabsel=='total_iwc_januari' | input$tabsel=='total_iwc_september'){
      NULL
    } else {
      startyr[startyr$Delprogram==input$tabsel, c('Art', 'StartYear')]
    }
    byr <- ifelse(isolate(input$selyrsAnalyze[1])>1998, isolate(input$selyrsAnalyze[1]), 1998) 
    indexplot(restoplot, base = byr, ncol = 3, speciesdat = spdat, startyr = styr, makepdf = 3 %in% input$saveresult, filename = paste0(path_project_extract,input$filenameRes, '.pdf'))
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


