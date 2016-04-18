library(shiny)
library(markdown)

# Define the overall UI
shinyUI(
  fluidPage(
    includeCSS("styles.css"),
    img(src="logo_adira.png", align = "right"),
    img(src="logo-mantis.png", align = "left"),
    br(),
    br(),
    titlePanel("MANTIS Automated application for Adira's machine logs file"),
    p("This is a temporary application to clean the log file and remove duplicate records. First select the file in the Read log section and wait until the application finishes the processing. For now, don't change any option in the Read file section and only select the log file. After seeing a table in the bottom of the page, you can download the clean CSV by pressing the Download button in the Download section. Finally, you can see the report if you select the Report tab."),
    #p("First select the file and wait until the application finishes the processing,"),
    #p("For now, don't change any option in the left side and only select the log file,"),
    #p("After seeing a table in the right side of the window, you can download the clean CSV by pressing the Download button"),
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(3,
             h3("Read Headers"),
             p('You can upload a CSV file which contains the headers. Please specify the separator.'),
             radioButtons('sep2', 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ','),
             fileInput('file2', 'Choose csv file for headers',
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv'
                       )
             )
      ),
      column(3,
             h3("Read Log"),
             checkboxInput('header', 'Header', FALSE),
             radioButtons('sep', 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ';'),
             radioButtons('quote', 'Quote',
                          c(None='',
                            'Double Quote'='"',
                            'Single Quote'="'"),
                          '"'),
             fileInput('file1', 'Choose file to upload',
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv'
                       )
             )
      ),
      column(3,
             h3("Download Clean Data"),
             p('You can download clean data at the end!'),
             downloadButton('downloadData', 'Download')
      ),
      column(3,
             h3("Produce and Download Report"),
             p("Press the following button if you don't see the tables in the following report to refresh it!"),
             #checkboxInput('Wantout', 'Do you want to produce report?', TRUE),
             #tags$hr(),
             actionButton("goButton", "Refresh Report!"),
             tags$hr(),
             p("For now, only html report is accessible!"),
             radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), 'HTML', inline = TRUE),
             downloadButton('downloadReport')
      )
    ),
    # Create a new row for the table.
    fluidRow(
      tabsetPanel(
        tabPanel("Headers", div(DT::dataTableOutput('headers'), style = "font-size:75%")),
        tabPanel("Dataset", div(DT::dataTableOutput('contents'), style = "font-size:75%")),
        tabPanel("Report", uiOutput("page1"))
        #HTML('<footer> "By Mohammad Nozari, INESC Porto" </footer>')
        #p("By Mohammad Nozari, INESC Porto, on ", format(Sys.time(), '%d %B, %Y'))
      )
    ),
    fluidRow(
      absolutePanel(
        bottom = 0, left = 0, right = 0,
        fixed = TRUE,
        div(
          align = "center", style="padding: 8px; color:green; font-family: 'times'; border-bottom: 1px solid #CCC; background: #FFFFEE;",
          HTML(markdownToHTML(fragment.only=TRUE, text=c(
            "By Mohammad Nozari, INESC Porto"
          )))
        )
      )
    )
  )
)