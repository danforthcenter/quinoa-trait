library(shiny)
library(d3heatmap)
library(gplots)
library(reshape)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(shinythemes)

# load shoot data
cq.shoot <- read.csv(file="data/cqShootMean.csv", sep=",", header=TRUE, stringsAsFactors=FALSE) #cq_mean_data
raw.data <- read.csv(file="data/cqShootRaw.csv", sep=",", header=TRUE, stringsAsFactors=FALSE) #cqshoot_data_20170114
images.list <- read.csv(file="data/cqShootImages.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
# load panicle data
panicle <- read.csv(file="data/cqPanicle.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
# load seed data
seed.0 <- read.csv(file="data/cqSeedGen0_area.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
seed.1 <- read.csv(file="data/cqSeedGen1_area.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
seed.biomass <- read.csv(file="data/cqSeedBiomass.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

#### user interface
ui <- navbarPage("Quinoa Phenotype Explorer",
                 theme = shinytheme("spacelab"),
                 tabPanel("About",
                          fluidRow(
                            column(6,
                                   h4(strong("Germplasm:")),
                                   p("A total of 383 quinoa lines were obtained from the USDA Germplasm Research Information Network National Plant Germplasm System,
                                      from Leibniz-Institut Fur Pflanzengenetik Und Kulturpflanzenforschung (IPK) seed bank, and from collaborators at Washington State 
                                      University (Kevin Murphy) and Brigham Young University (Jeff Maughan and Rick Jellen). All seeds from each line were imaged using 
                                      a Nikon Coolpix L830 camera. Images were then processed and analyzed for seed area and color using PlantCV (Fahlgren et al. 2015).")), 
                            column(1),
                            column(4,
                                   img(src="example-images/20160822_115928.jpg",
                                       height="100%", width="100%"),
                                   tags$small(
                                     em("Chenopodium quinoa."),
                                     "Source: Steven Callen, Donald Danforth Plant Science Center"))),hr(),
                          fluidRow(
                            column(6,
                                   h4(strong("F0 Seed:")),
                                   p(" Seeds from each of the 383 quinoa lines were imaged using a Nikon Coolpix L830 camera. Images were then processed and analyzed for seed area 
                                     and color using PlantCV (Fahlgren et al. 2015)."),
                                   p("Fifty of these accessions were selected as a diversity panel to examine differences in shoot growth and seed production among quinoa
                                     varieties. Accessions were selected based on country of origin, elevation, and seed color and size. After germination, six seedlings 
                                     per accession were transplanted into 4-in pots containing a Pro-Mix FPX soil medium and then grown in a greenhouse at 24-28C, 20-80% 
                                     RH, and ~14.5-hr day length."), 
                                  wellPanel(h4("Download F0 Seed Data"),
                                            h5("Includes normalized area of each seed from each accession.", style="margin-left:10px"),
                                            downloadButton('downloadSeedG0', 'F0 Seed Data'))),br(), br(),
                            column(1),
                            column(4,
                                   img(src="example-images/Cq0268.00.jpg",
                                       height="100%", width="100%"),
                                   tags$small(
                                     em("Chenopodium quinoa."),
                                     "Source: Michael Miller and Monica Tessman, Donald Danforth Plant Science Center"))), hr(),
                          fluidRow(
                            column(6,
                                   h4(strong("Shoot Data (50 Accessions):")),
                                   p("Plants were imaged using RaspberryPis about every other day for seven days. Images of quinoa shoots were taken simultaneously from four 
                                      camera angles: three side views and one top view. Images were processed and analyzed for area and height above bound using PlantCV. 
                                      Above-ground fresh- and dry-weight biomass (g) were estimated from linear regression models of shoot area (cm^2) and height (cm). 
                                      Model for fresh-weight (adjusted R^2 = 0.776): FW = 1.278e-04(all_area) + all_ht, where 'all_area' is the sum of above-ground plant 
                                      pixel area from all four camera angles and 'all_ht' is the mean of above-ground plant pixel height from all camera angles. Model for 
                                      dry-weight (adjusted R^2 = 0.742): DW = 8.684e-06(sides_area) + 2.723e-05(area_top), where 'sides_area' is the sum area of all three 
                                      sides and 'area-top' is the area from top camera angle only."),
                                   p("The heatmap presented is generated from data averaged over all replicates per time point and scaled by row."), 
                                   wellPanel(h4("Download Raw or Mean Shoot Data"),
                                             h5("Raw data includes area and height for all replicates of each accession.
                                                Mean data is averaged over replicates of each accession per day.", style="margin-left:15px"),
                                             downloadButton('downloadRawData', 'Raw Shoot Data'),
                                             downloadButton('downloadData', 'Mean Shoot Data'))),br(), br(),
                            column(1),
                            column(4,
                                   img(src="example-images/IMG_2812.jpg",
                                       height="100%", width="100%"),
                                   tags$small(
                                     em("Chenopodium quinoa."),
                                     "Source: Malia Gehan, Donald Danforth Plant Science Center"))), hr(),
                          fluidRow(
                            column(6,
                                   h4(strong("Panicle Data (50 Accessions):")),
                                   p("Panicles were collected at 111-146 days after germination, according to maturity. Panicles were imaged using a Nikon Coolpix L830 
                                     camera."),
                                   p("Panicle shape and density were scored using the descriptors for quinoa established by Bioversity International et al. 2013."), 
                                  wellPanel(h4("Download Panicle Data"),
                                            h5("Includes panicle density and shape for each accession.", style="margin-left:15px"),
                                            downloadButton('downloadPan', 'Panicle Data'))),br(), br(),
                            column(1),
                            column(4,
                                   img(src="example-images/IMG_4387.jpg",
                                       height="100%", width="100%"),
                                   tags$small(
                                     em("Chenopodium quinoa."),
                                     "Source: Elizabeth Castillo, Donald Danforth Plant Science Center"))), hr(),
                          fluidRow(
                            column(6,
                                   h4(strong("F1 Seed Data (50 Accessions):")),
                                   p("Seeds were harvested by rubbing panicles across a 1/8-in mesh screen placed atop an Almaco Air Blast Seed Cleaner. Total seed weight 
                                      (yield, g) was recorded for each plant. A subset of seeds from each plant was weighed and imaged. The image background consisted of white 
                                      paper with a 1.27-cm diameter Tough-Spot for use as a size marker. Seed images were processed and analyzed for seed size and color
                                      using PlantCV, and the number of seeds per image was calculated using R v3.3.3 (2017-03-06)."),
                                   p("Seed size was normalized by dividing the area of each seed by the area of the size marker. Average seed weight (g/seed) was estimated for each 
                                      plant by dividing the total weight of the subset of seeds by the number of seeds in the subset. The total number of seeds per plant 
                                      was then estimated by dividing the yield (g) by the average seed weight (g/seed)."), 
                                  wellPanel(h4("Download F1 Seed Data"),
                                            h5("Includes seed number, normalized area, and weight for each accession.", style="margin-left:15px"),
                                            downloadButton('downloadSeedG1', 'F1 Seed Data'))),br(), br(),
                            column(1),
                            column(4,
                                   img(src="example-images/IMG_0010.jpg",
                                       height="100%", width="100%"),
                                   tags$small(
                                     em("Chenopodium quinoa."),
                                     "Source: Steven Callen, Donald Danforth Plant Science Center"))), br()),
                 tabPanel("Shoot",
                          fluidRow(
                            column(12,
                                   wellPanel(
                                     h4("Datasets"),
                                     selectInput('data.choice', 'Select:', c("Height (cm)", "Area (cm^2)", "Modeled fresh weight (g)", "Modeled dry weight (g)"),selectize = FALSE))),
                            column(12,
                                   wellPanel(
                                     h4("Select Dates:"),
                                     h5("Note: Must select >1 date to display heatmap.", style="margin-left:15px"),
                                     h5("Note: Must unselect 'All' to display data of specific dates.", style="margin-left:15px"),
                                     checkboxGroupInput("dates", "Dates", c("All", levels(as.factor(unique(cq.shoot$Date)))), selected = "All", inline=TRUE))),
                            column(12,   
                                   wellPanel(
                                     h4("Select Accessions:"),
                                     h5("Note: Must select >1 genotype to display heatmap.", style="margin-left:15px"),
                                     h5("Note: Must unselect 'All' to display data of specific accessions.", style="margin-left:15px"),
                                     checkboxGroupInput("accessions", "Accessions", c("All", unique(cq.shoot$Genotype)), selected = "All", inline=TRUE))),
                            column(12,
                                   tabsetPanel(
                                     tabPanel("Heatmap", column(9, d3heatmapOutput('heatmap', height="700px")), br(),
                                              column(3, wellPanel(h4("Heatmap Colors"),
                                                                  selectInput("palette", "Palette:", c("Greens", "Blues", "Purples", "YlGn", "YlOrRd", "RdYlBu", "RdYlGn")))),
                                              column(3, wellPanel(h4("Download Heatmap"),
                                                                  radioButtons('plotType', 'Select file type:', choices= c("png", "pdf"), inline=TRUE),
                                                                  downloadButton('downloadPlot', 'Download')))),
                                     tabPanel("Table", fluidRow(column(6, h4("Means Data Table:"), DT::dataTableOutput('table'))),
                                                       fluidRow(br())),
                                     tabPanel("Images", br(),
                                              column(2,wellPanel(h4("Camera Angle"),
                                                       checkboxGroupInput("angle", "Select angle:", choices = c("side", "top"), selected=c("side", "top"), inline=TRUE))),
                                              br(), br(),
                                              conditionalPanel(condition = "input.accessions == 'All'", p("NOTE: Select at least one accession to display images.")),
                                              conditionalPanel(condition = "input.dates == 'All'", p("NOTE: Select at least one date to display images.")),
                                              conditionalPanel(condition = "input.accessions != 'All' & input.dates == ''", p("NOTE: Select at least one date.")),
                                              conditionalPanel(condition = "input.dates != 'All' & input.accessions == ''", p("NOTE: Select one accession.")),
                                              conditionalPanel(condition = "input.accessions != 'All' & input.dates != 'All'", uiOutput('images'))))))),
                        tabPanel("Panicle",
                          fluidRow(
                           column(4,
                                 img(src="panicle-images/panicleShape_vert.png",
                                     height="100%", width="100%"),
                                 tags$small("Example", em("Chenopodium quinoa"),
                                   "panicle shape images. Courtesy of Elizabeth Castillo, 
                                   Donald Danforth Plant Science Center.")),
                           column(4,
                                  wellPanel(
                                    h4("Panicle Data Table:"),
                                    DT::dataTableOutput('panicle'))),
                           column(4,
                                  img(src="panicle-images/panicleDens_vert.png",
                                      height="100%", width="100%"),
                                  tags$small("Example", em("Chenopodium quinoa"),
                                             "panicle density images. Courtesy of Elizabeth Castillo, 
                                              Donald Danforth Plant Science Center.")))),
                       tabPanel("Seed",
                                fluidRow(
                                  column(6,
                                         p("Quinoa seed data here: boxplots, images below")))),
                       tabPanel("Data Comparisons",
                                fluidRow(
                                  column(3,
                                         wellPanel(
                                          h4("Regressions"),
                                          selectInput("xcol", "X Variable", choices=c("All Seed - Est. Number" = "SeedNum_EstAll",
                                                                                      "All Seed - Yield (g)" = "SeedWT_All_g",
                                                                                      "Est. Fresh Weight" = "FWbiomass",
                                                                                      "Est. Dry Weight" = "DWbiomass",
                                                                                      "Seed Subset - Number" = "SeedNum_Subset",
                                                                                      "Seed Subset - Weight (g)" = "SeedWT_Subset_g",
                                                                                      "Seed Subset - Mean Norm. Area" = "MeanSeedNormArea"), 
                                                      selected = "SeedNum_EstAll", selectize = FALSE),
                                          selectInput("ycol", "Y Variable", choices=c("All Seed - Est. Number" = "SeedNum_EstAll",
                                                                                      "All Seed - Yield (g)" = "SeedWT_All_g",
                                                                                      "Est. Fresh Weight" = "FWbiomass",
                                                                                      "Est. Dry Weight" = "DWbiomass",
                                                                                      "Seed Subset - Number" = "SeedNum_Subset",
                                                                                      "Seed Subset - Weight (g)" = "SeedWT_Subset_g",
                                                                                      "Seed Subset - Mean Norm. Area" = "MeanSeedNormArea"), 
                                                      selected = "SeedWT_All_g", selectize = FALSE)),
                                         wellPanel(h4("Download Plot"),
                                                   radioButtons('plotType2', 'Select file type:', choices= c("png", "pdf"), inline=TRUE),
                                                   downloadButton('regressPlot', 'Download'))),
                                  column(5, 
                                         div(style = "position:relative", 
                                         plotOutput("plot1", height = 500, click = "plot1_click", brush = brushOpts(id = "plot1_brush"), 
                                                    hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                                         uiOutput("hover_info"))),
                                  column(4, div(style="height:100px;",
                                         h4("Points near click"),
                                         verbatimTextOutput("click_info"),
                                         h4("Brushed points"),
                                         verbatimTextOutput("brush_info")))),
                                fluidRow(
                                  column(5, offset = 3,
                                         h4("Correlation Summary"),
                                         verbatimTextOutput("corSummary"),
                                         h4("Regression Model Summary"),
                                         verbatimTextOutput("modelSummary")))))


#### server
server <- function(input, output) {

################################### SHOOT ###########################################
  #Table
  #select data
  cq.data.table <- reactive({
    if (input$data.choice == "Modeled fresh weight (g)") {
      x.choice <- cq.shoot[order(cq.shoot$Genotype), 1:3]
    } else if (input$data.choice == "Modeled dry weight (g)") {
      x.choice <- cq.shoot[order(cq.shoot$Genotype), c(1:2,4)]
    } else if (input$data.choice == "Height (cm)") {
      x.choice <- cq.shoot[order(cq.shoot$Genotype), c(1:2,5)]
    } else if (input$data.choice == "Area (cm^2)") {
      x.choice <- cq.shoot[order(cq.shoot$Genotype), c(1:2,6)]
    }
  })
  
  #output table
  output$table <- DT::renderDataTable(DT::datatable({
    data <- cq.data.table()
    if(input$accessions != "All"){
      data <- cq.data.table()[which(cq.data.table()$Genotype %in% input$accessions),]
    } 
    if(input$dates != "All"){
      data <- data[which(data$Date %in% input$dates),]
    }
    data
  }, rownames = FALSE, options=list(lengthMenu = list(c(10,25,50,-1), c('10','25','50','All')), 
                                    pageLength = 10)))
  
  #download means table
  tableDL <- reactive({
    data <- cq.data.table()
    if(input$accessions != "All"){
      data <- cq.data.table()[which(cq.data.table()$Genotype %in% input$accessions),]
    } 
    if(input$dates != "All"){
      data <- data[which(data$Date %in% input$dates),]
    }
    data
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$data.choice, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(tableDL(), file, row.names=F)
    }
  )
  
  #download raw data
  output$downloadRawData <- downloadHandler(
    filename = "cqShoot_rawData.csv",
    content = function(file) {
      write.csv(raw.data, file, row.names=F)
    }
  )
  
  #download panicle data
  output$downloadPan <- downloadHandler(
    filename = "cqPanicle.csv",
    content = function(file) {
      write.csv(panicle, file, row.names=F)
    }
  )

  #download G0 data
  output$downloadSeedG0 <- downloadHandler(
    filename = "cqSeedGen0_area.csv",
    content = function(file) {
      write.csv(seed.0, file, row.names=F)
    }
  )
  
  #download G1 data
  output$downloadSeedG1 <- downloadHandler(
    filename = "cqSeedGen1_area.csv",
    content = function(file) {
      write.csv(seed.1, file, row.names=F)
    }
  )
  

  #Heatmap
  heatmap <- reactive({
    data <- cq.data.table()
    if(input$accessions != "All"){
      data <- data[which(data$Genotype %in% input$accessions),]
    } 
    if(input$dates != "All"){
      data <- data[which(data$Date %in% input$dates),]
    }
    data
    cq.explode <- cast(data, Date~Genotype, mean, value=colnames(data[3]))
    cq.t <- t(cq.explode)
  })
  
  #display heatmap
  output$heatmap <- renderD3heatmap({
    d3heatmap(heatmap(), dendrogram ="none", colors=input$palette,
              xaxis_font_size = "9pt", yaxis_font_size = "9pt")
  })
  
  #downloading heatmap
  output$downloadPlot <- downloadHandler(
    filename = function() { 
      paste(input$data.choice, input$plotType, sep=".") 
    },
    content = function(file) {
      if (input$plotType == "png")
        png(file, width=8, height=5.5, units="in", res=300, pointsize = 10)
      else
        pdf(file, width=8, height=5.5, paper='special', pointsize = 10)
      heatmap.2(heatmap(), dendrogram = "none", key=FALSE, Rowv=FALSE, Colv=FALSE, 
                lwid=c(0.1,4), lhei=c(0.1,4), col=brewer.pal(9,input$palette), trace="none",adjCol=c(0,0),margins=c(5,10), 
                srtCol=-45,cexCol=1,cexRow=1,sepwidth=c(0,0), sepcolor="floralwhite", colsep=1:ncol(heatmap()), 
                rowsep=1:nrow(heatmap()),labRow=paste("-  ", rownames(heatmap())), labCol=paste("- ", colnames(heatmap())))
      
      dev.off()
    })
  
  
  # For displaying pre-rendered images
  nameinfo <- cq.shoot[order(cq.shoot$Genotype), c(1:2,7)]

  selectedAngle <- reactive({
    validate(need(input$angle, 'Choose one angle or even two!'))
    views <- subset(images.list, grepl(paste(c(input$angle), collapse="|"), images.list$imageName))
    views <- droplevels(views)
    return(views)
  })

    nameList.ID <- reactive({
    validate(need(input$accessions, 'Check at least one box!'))
    nameinfo.select <- subset(nameinfo, nameinfo$Genotype %in% input$accessions)
    nameinfo.select <- droplevels(nameinfo.select)
    return(nameinfo.select)
  })
  
  nameList <- reactive({
    validate(need(input$dates, 'Check at least one box!'))
    dateinfo_select <- subset(nameList.ID(), nameinfo$Date %in% input$dates)
    dateinfo_select <- na.omit(droplevels(dateinfo_select))
    toMatch_byName <- c(unique(dateinfo_select$Genotype))
    imagenames_Name <- paste(grep(paste(toMatch_byName, collapse="|"), selectedAngle()[,1], value=TRUE), ".png", sep="")
    toMatch_byDate <- c(unique(dateinfo_select$Date))
    imagenames <- grep(paste(toMatch_byDate, collapse="|"), imagenames_Name, value=TRUE)
    return(imagenames)
  })
  

  n.col <- 1
  output$images <- renderUI({
    col.width <- round(12/n.col) # Calculate bootstrap column width
    n.row <- ceiling(length(nameList())/n.col) # calculate number of rows
    cntr <<- 0 # Counter variable
    
    # Create row with columns
    rows  <- lapply(1:n.row, function(row.num){
      cols  <- lapply(1:n.col, function(i) {
        cntr    <<- cntr + 1
        image_names <- paste("./www/", nameList()[cntr], sep="")
        column(col.width, imageOutput(image_names, height = "100%", width = "100%"))
      }) 
      fluidRow( br(), do.call(tagList, cols))
    })
    do.call(tagList, rows)
  })
  
  
  for (i in 1:length(isolate(nameList()))){
    local({
      my_i <- i
      image_names <- paste("./www/", isolate(nameList()[my_i]), sep="")
      output[[image_names]] <- renderImage({
        filename <- normalizePath(file.path(image_names), winslash="/")
        list(src = filename,
             filetype = "image/png",
             height = "20%", width = "100%",
             alt = "quinoaShoots" )}, deleteFile = FALSE)
    })
  }


#####################################################################################

####################################### SEED ######################################## 
# Malia
#####################################################################################

###################################### Panicle ###################################### 

  #output panicle table
  output$panicle <- DT::renderDataTable(DT::datatable({
    panicle[,c(1,3,5,4)]}, rownames = FALSE,
    options=list(lengthMenu = list(c(10,25,50,-1), c('10','25','50','All')),
                 pageLength = 10, scrollX = TRUE)))
  
#####################################################################################
#################################### Comparisons #################################### 
#####################################################################################

  #output plot
  currdata <- reactive({ seed.biomass[c(colnames(seed.biomass[1]),input$xcol,input$ycol)] })

  output$plot1 <- renderPlot({
    ggplot(currdata(), aes(currdata()[,2], currdata()[,3])) + geom_point(size=2) +
      geom_smooth(method = lm, fullrange = TRUE, color = "black") +
      labs(x = input$xcol, y = input$ycol) + theme(axis.title=element_text(size=15), axis.text = element_text(size=12))
    })
  
  cq.mod1<- reactive({
    lm(currdata()[,3] ~ currdata()[,2])
  })
  
  cq.Pearson <- reactive({
    cor.test(~currdata()[,3] + currdata()[,2], alternative = "two.sided")
  })
  
  output$modelSummary <- renderPrint({
    output<-data.frame(matrix(nrow = 4,ncol = 5))
    colnames(output)<-c("","Estimate","Std. Error","t-value","Pr(>|t|)")
    row.names(output)<-c(""," ","  ","   ")
    output[1,1:5]<-c("Intercept", round(summary(cq.mod1())$coefficients[1,1],3), round(summary(cq.mod1())$coefficients[1,2],3), round(summary(cq.mod1())$coefficients[1,3],3), signif(summary(cq.mod1())$coefficients[1,4],3))
    output[2,1:5]<-c(colnames(currdata()[2]), round(summary(cq.mod1())$coefficients[2,1],3), round(summary(cq.mod1())$coefficients[2,2],3), round(summary(cq.mod1())$coefficients[2,3],3), signif(summary(cq.mod1())$coefficients[2,4],3))
    output[4,1:2]<-c("Adjusted R^2:", round(summary(cq.mod1())$adj.r.squared,3))
    output[is.na(output)]<-" "
    return(output)
  })
  
  output$corSummary <- renderPrint({
    output.cor<-data.frame(matrix(nrow = 3,ncol = 4))
    colnames(output.cor)<-c(""," ","  ","   ")
    row.names(output.cor)<-c(""," ","  ")
    output.cor[1,1:4]<-c("StatSummary:", paste0("t = ", round(cq.Pearson()$statistic,3)), paste0("df = ",round(cq.Pearson()$parameter,3)), paste0("p = ", signif(cq.Pearson()$p.value,3)))
    output.cor[2,1:3]<-c("95CI:", paste0("lower = ", round(cq.Pearson()$conf.int[1],3)), paste0("upper = ", round(cq.Pearson()$conf.int[2],3)))
    output.cor[3,1:2]<-c("Pearson's r:", round(cq.Pearson()$estimate,3))
    output.cor[is.na(output.cor)]<-" "
    return(output.cor)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(seed.biomass, input$plot1_brush, xvar = input$xcol, yvar = input$ycol)
  })
  
  output$click_info <- renderPrint({
    nearPoints(seed.biomass, xvar=input$xcol, yvar=input$ycol, input$plot1_click, addDist = TRUE)
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(currdata(), hover, xvar=input$xcol, yvar=input$ycol, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(0, 0, 255, 0.6); color: white; ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Genotype: </b>", point$Genotype, "<br/>",
                    "<b>", input$xcol, ": </b>", point[2], "<br/>",
                    "<b>", input$ycol, ": </b>", point[3], "<br/>")))
    )
  })
  
  #downloading plots
  output$regressPlot <- downloadHandler(
      filename = function() { 
        paste0(input$xcol, "_", input$ycol, ".", input$plotType2)
        },
      content = function(file) {
        if (input$plotType2 == "png")
          png(file, width=8, height=5.5, units="in", res=300, pointsize = 10)
        else
          pdf(file, width=8, height=5.5, paper='special', pointsize = 10)
        
        print(ggplot(currdata(), aes(currdata()[,2], currdata()[,3])) + geom_point(size=2) +
              geom_smooth(method = lm, fullrange = TRUE, color = "black") +
              labs(x = input$xcol, y = input$ycol) + theme(axis.title=element_text(size=15), axis.text = element_text(size=12)))
        dev.off()
        })
}

##Shinyapp
shinyApp(ui = ui, server = server)