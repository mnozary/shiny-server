library(png)
#shiny::runGitHub("shiny-phyloseq","joey711")
library(shiny)
#library(shinythemes)
library(DT)

#runApp("C:/Dropbox/Postdoc/Project/MANTIS/Documents/Adira/New Data/Apps")
options(shiny.maxRequestSize = 50*1024^2)

outputDir <- "awesome_app"
ESA.col.names <- c("ax0.ra4","ax1.ra4","ax2.ra4","ax3.ra4","ax4.ra4","ax6.ra4","ax7.ra4","LazerSafe.PCSSConditionCode","ax0.ra5","ax1.ra5","ax2.ra5","ax3.ra5","ax4.ra5","ax6.ra5","ax7.ra5","C60.0","C60.1","M5.4","M5.5","np","IO1.15","M2.9","M2.10","IO2.14","LazerSafe.CncOperationalModb.6","C70.21","C70.16","C70.17","C70.18","C70.19","C62.14","LazerSafe.PCSSReplay.2","pargene.dwgn12","IO100","IO52","IO53")
Adira.col.names <- c("Position.Y1","Position.Y2","Position.X1","Position.X2","Position.R","Position.Z1","Position.Z2","Lazersafe.Error","Speed.Y1","Speed.Y2","Speed.X1","Speed.X2","Speed.R1","Speed.Z1","Speed.Z2","Pedal.Down","Pedal.Up","Start.NC","Stop.NC","N.Quinagem","Seq.Quinagem","Ciclo.Automatico","Ciclo.de.Quinagem","DNC.Manual","Programacao.NC","Axis.in.Position","PMS","PCV","PCL","PMI","MUTE","Comunicacao.LZS.CNC","Horas.de.trabalho","Pressure.DNC","Pressure.Machine.Y1","Pressure.Machine.Y2")


col.names <- c("time", "ax0ra4", "ax1ra4", "ax2ra4", "ax3ra4", "ax4ra4", "ax6ra4", "ax7ra4", 
               "C6221", "ax0ra5", "ax1ra5", "ax2ra5", "ax3ra5", "ax4ra5", "ax6ra5", "ax7ra5", 
               "C600", "C601", "M54", "M55", "np", "IO115", "M29",
               "M210", "IO214", "lazer6", "C7021", "C7016",
               "C7017", "C7018", "C7019", "C6214", "lazer2", "dwgn12",
               "last1", "last2", "last3", "last4", "last5", "last6",
               "last7")

headers.df <- data.frame(Attribute=character())

#col.names <- Adira.col.names

no.columns <- length(col.names)
new.col.add <- 0

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

readMulti <- function(x, sep, replace)
{
  dat <- readLines(x)
  dat <- gsub(sep, replace, dat)
  last.line <- length(dat)
  
  #remove the replace at the end of each line
  for(i in 1:(last.line-1)){
    if(substrRight(dat[i],1) == replace){
      dat[i] <- substr(dat[i], 1, nchar(dat[i])-1)
    }
  }
  
  #remove the last line which is most probably incomplete
  dat <- dat[-last.line]
  
  write(dat, file =paste0(outputDir, "/temp.csv"), ncolumns = 1)
  
  return(paste0(outputDir, "/temp.csv"))
}

isDuplicate <- function(r1, r2){
  res <- TRUE
  for(k in 1:ncol(r1)){
    if(r1[,k]!=r2[,k]) {
      res <- FALSE
      break
    }
  }
  return(res)
}

remove.duplicates <- function(no.row, kk, pp, new_DFD){
  #for test: (0, 1, 1, new_DFD): no.row=0;kk=1;pp=1
  new_DFD$datetime <- as.character(new_DFD$datetime)
  new_DFD$date <- as.character(new_DFD$date)
  new_DFD$time <- as.character(new_DFD$time)
  
  new_DFD2 <- subset(new_DFD, FALSE)
  
  withProgress(message = 'Please wait...', value = 0, {
    while(kk<=nrow(new_DFD)){
      incProgress(kk/nrow(new_DFD), detail = paste( round(kk/nrow(new_DFD)*100, 0),"% done"))
      no.dup <- 1
      no.row <- no.row + 1
      kk <- pp
      pp <- kk + 1
      while(pp<=nrow(new_DFD)){
        if(!isDuplicate(new_DFD[kk,2:no.columns], new_DFD[pp,2:no.columns])){
          
          break
          
        }else{
          
          no.dup <- no.dup + 1
          pp <- pp + 1
          
        }
        
      }
      
      #Add unique row
      if(kk<=nrow(new_DFD) || pp<=nrow(new_DFD)){
        for(u in 2:no.columns){new_DFD2[no.row,u] <- new_DFD[kk, u]}
        new_DFD2$noDup[no.row] <- no.dup
        new_DFD2$datetime[no.row] <- new_DFD$datetime[kk]
        new_DFD2$date[no.row] <- new_DFD$date[kk]
        new_DFD2$time[no.row] <- as.character(new_DFD$time[kk])
        new_DFD2$timeDiff[no.row] <- as.numeric(difftime(new_DFD$datetime[pp-1],new_DFD$datetime[kk],units="secs"))
        #new_DFD2$timeDiff[no.row] <- as.numeric(difftime(new_DFD[pp-1, (no.columns+new.col.add+2)],new_DFD[kk, (no.columns+new.col.add+2)],units="secs"))
      }
    }
  })
  return(new_DFD2)
}

shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    
    print("enter")
    #for test: headers.spec <- headers.df
    headers.spec <- headersInput()
    
    print(str(headers.spec))
    
    col.names <- colnames(headers.spec)[2:length(colnames(headers.spec))]
    no.columns <- length(col.names)
    
    #for test: log.file <- "C:/Dropbox/Postdoc/Project/MANTIS/Documents/ADIRA/New Data/Latest/04042016.log"
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #for test: output22 <- readMulti(log.file, sep = "  ", replace = ";")
    output22 <- readMulti(inFile$datapath, sep = "  ", replace = input$sep)
    
    #for test: output <- read.csv(output22, header = FALSE, sep = ";", col.names = col.names, fileEncoding = "latin1")
    output <- read.csv(output22, header = input$header, sep = input$sep, quote = input$quote, col.names = col.names, fileEncoding = "latin1")
    
    new.col.add <- 0
    for(h in 1:ncol(output)){
      if(headers.spec[3,col.names[h]]=="1"){
        output[,h] <- output[,h]/1000
      }
      if(headers.spec[5,col.names[h]]!=""){
        output[,h] <- round(output[,h], as.integer(headers.spec[5,col.names[h]]))
      }
#       if(headers.spec[4,col.names[h]]=="1"){
#         col.name.new <- col.names[h]
#         col.name.new <- paste0(col.name.new, ".hex")
#         output[,col.name.new] <- as.character(as.hexmode(output[,h]))
#         new.col.add <- new.col.add + 1
#       }
    }
    
    #print(str(output))
    
    ## Codes for data cleanig
    new_DFD <- output
    #for test: new_DFD$date <- rep("04042016", nrow(output))
    new_DFD$date <- rep(strsplit(as.character(input$file1[1]), "\\.")[[1]][1], nrow(output))
    
    x <- paste(new_DFD$date, new_DFD[,c(1)], sep = " ")
    
    new_datetime <- strptime(x, "%d%m%Y %H:%M:%S")
    new_DFD$datetime <- new_datetime
    new_DFD$datetime<-as.POSIXct(new_DFD$datetime)
    
    #Sort the dataframe with the date and time
    new_DFD <- new_DFD[ order(new_DFD[,(no.columns+new.col.add+1)], new_DFD[,1]), ]
    
    #Remove duplicates
    #for test: new_DFD2
    new_DFD2 <- remove.duplicates(0, 1, 1, new_DFD)
    
    #write csv for download
    isolate(write.csv(new_DFD2, file =paste0(outputDir, "/output.csv"), row.names=F, fileEncoding = "latin1"))
    
    #write dataframe for report
    isolate(save(new_DFD2, file = paste0(outputDir, "/report.RData")))
    
    new_DFD2$time <- as.character(new_DFD2$time)
    new_DFD2$date <- as.character(new_DFD2$date)
    new_DFD2$datetime <- as.character(new_DFD2$datetime)
    #new_DFD2$ax7ra4.hex <- as.character(new_DFD2$ax7ra4.hex)
    
    #print(nrow(new_DFD2))
    
    return(new_DFD2)
    
  })
  
  headersInput <- reactive({
    withProgress(message = 'Please wait...', value = 1, {
      #for test: inFile <- "C:/Dropbox/Postdoc/Project/MANTIS/Documents/ADIRA/New Data/Latest/headers.csv"
      inFile <- input$file2
      
      if (is.null(inFile)){
        dat <- readLines("headers.csv", encoding = "latin1")
        #return(NULL)
      }else{
        #for test: dat <- readLines(inFile)
        dat <- readLines(inFile$datapath, encoding = "latin1")
      }
      
      dat <- gsub(" ", ".", dat)
      for(d in 1:length(dat)){
        if(substrRight(dat[d],1)==","){
          dat[d] <- paste0(dat[d], ",")
        }
      }
      
      #print(dat)
      
      #for test: ESA.col.names <- strsplit(dat[2], ",")
      ESA.col.names <- strsplit(dat[2], input$sep2)
      ESA.col.names <- ESA.col.names[[1]][2:length(ESA.col.names[[1]])]
      
      #for test: Adira.col.names <- strsplit(dat[3], ",")
      Adira.col.names <- strsplit(dat[3], input$sep2)
      Adira.col.names <- Adira.col.names[[1]][2:length(Adira.col.names[[1]])]
      
      #for test: div.cols <- strsplit(dat[4], ",")
      div.cols <- strsplit(dat[4], input$sep2)
      div.cols <- div.cols[[1]][2:length(div.cols[[1]])]
      
      #for test: hex.cols <- strsplit(dat[5], ",")
      hex.cols <- strsplit(dat[5], input$sep2)
      hex.cols <- hex.cols[[1]][2:length(hex.cols[[1]])]
      
      #for test: deci.cols <- strsplit(dat[6], ",")
      deci.cols <- strsplit(dat[6], input$sep2)
      deci.cols <- deci.cols[[1]][2:length(deci.cols[[1]])]
      
      headers.df <- as.data.frame(setNames(replicate(length(Adira.col.names)+1,numeric(0), simplify = F), letters[1:length(Adira.col.names)+1]))
      #headers.df$attributes <- c("Adira names", "ESA names", "Needs dividing by 1000", "Needs hex", "Number of decimal")
      headers.df[1,1] <- c("Adira names")
      headers.df[1,1:length(Adira.col.names)+1] <- Adira.col.names
      
      headers.df[2,1] <- c("ESA names")
      headers.df[2,1:length(Adira.col.names)+1] <- ESA.col.names
      
      headers.df[3,1] <- c("Needs dividing by 1000")
      headers.df[3,1:length(Adira.col.names)+1] <- div.cols
      
      headers.df[4,1] <- c("Needs hex")
      headers.df[4,1:length(Adira.col.names)+1] <- hex.cols
      
      headers.df[5,1] <- c("Number of decimal")
      headers.df[5,1:length(Adira.col.names)+1] <- deci.cols
      
      colnames(headers.df) <- c("attributes", Adira.col.names)
      
      #print(headers.df)
    })
    return(headers.df)
  })
  
  CreateReport <- reactive({
    input$goButton
    #if(input$Wantout) {
      return(includeMarkdown(rmarkdown::render("Rep_html.Rmd")))
    #}
  })  
  
  RefreshReport <- eventReactive(input$goButton, {
    withProgress(message = 'Please wait...', value = 1, {
      includeMarkdown(rmarkdown::render("Rep_html.Rmd"))
    })
  })
  
  output$page1 <- renderUI({
    withProgress(message = 'Please wait...', value = 1, {
      CreateReport()
    })
  })
  
  output$contents <- DT::renderDataTable(
    datasetInput(), options = list(lengthMenu = c(50, 100, 200, 400), autoWidth = TRUE, pageLength = 200), server=TRUE
  )

  output$headers <- DT::renderDataTable(
    headersInput(), options = list(lengthMenu = c(10, 20, 30, 40, 50), autoWidth = TRUE, pageLength = 40), server=TRUE
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('mantis-report', sep = '.', switch(
        input$format, PDF = 'PDF', HTML = 'HTML', Word = 'Word'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('Rep_html.Rmd')

      out <- HTML = render('Rep_html.Rmd', html_document())
      
      file.rename(out, file)
    }
  )
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste0(outputDir, "/output.csv")
    },

    content = function(file) {
      sep <- input$sep
      
      write.table(datasetInput(), file, sep = sep,
                  row.names = FALSE)
    }
  )
})