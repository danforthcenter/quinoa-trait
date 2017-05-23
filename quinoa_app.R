options(rgl.useNULL=TRUE)
library(shiny)
library(d3heatmap)
library(gplots)
library(reshape)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(shinythemes)
library(scatterplot3d)
library(rgl)
library(plotly)
library(gridExtra)


# load shoot data
cq.shoot<-readRDS('data/cqShootMean.rds')
raw.data<-readRDS('data/cqShootRaw.rds')
images.list<-readRDS('data/cqShootImage.rds')

# load panicle data
panicle<-readRDS('data/cqPanicle-Yield_20170421.rds')
panicle.pca<-readRDS('data/panicle_pca_scores.rds')
panicle.loading<-readRDS('data/panicle_pca_loading.rds')

# load seed data
gen0.seed.area<-readRDS('data/gen0.filtered_seed_biomass_hsv.rds')
gen0.seed.color<-readRDS('data/gen0_agg_seed_pca_scores.rds')

gen1.seed.area<-readRDS('data/gen1.filtered_seed_biomass_hsv.rds')
gen1.seed.color<-readRDS('data/gen1.pca.color.scores.rds')

#load comparison data
seed.0<-readRDS("data/cqSeedGen0_area.rds")
seed.1<-readRDS("data/cqSeedGen1_area.rds")
seed.biomass<-readRDS("data/cqSeedBiomass.rds")

#### user interface
ui <- navbarPage("Quinoa Phenotype Explorer",
                 theme = shinytheme("spacelab"),
                 tabPanel("About",
                          fluidRow(
                            column(6,
                                   h3(strong("About")),
                                   p("A total of 383 quinoa lines were obtained from the USDA Germplasm Research Information Network National Plant Germplasm System (GRIN),
                                      Leibniz-Institut Fur Pflanzengenetik Und Kulturpflanzenforschung (IPK) seed bank, and from collaborators at Washington State 
                                      University (Kevin Murphy), and Brigham Young University (Jeff Maughan and Rick Jellen)."),
                                   p("The following is a brief description of the data collected on the germplasm during our 2016 season. If you use data or code from Quinoa Phenotype Explorer
                                      please cite: DOIXXXXXXX"),
                                   p("CITATION INFORMATION HERE"),
                                   p("The code for this repository can be found on", a(href="https://github.com/danforthcenter/quinoa-trait/",target='_blank',"our Github."), 
                                     "The image data included in this repository can be found on", a(href="https://www.figshare.com",target='_blank',"Figshare."),
                                     "The raw image data can be found", a(href="https://github.com/danforthcenter/quinoa-trait/",target='_blank',"here.")),
                                   p("For more information about the Gehan lab go to ",a(href="https:/www.gehan-lab.org",target='_blank',"www.gehan-lab.org"))),
                            column(1),
                            column(4,
                                   img(src="example-images/20160822_115928.jpg",
                                       height="100%", width="100%"),
                                   tags$small(
                                     em("Chenopodium quinoa."),
                                     "Source: Steven Callen, Donald Danforth Plant Science Center"))),hr(),
                          fluidRow(
                            column(6,
                                   h4(strong("F0 Seed Phenotyping:")),
                                   p(" Seeds from each of the 383 quinoa lines were imaged using a Nikon Coolpix L830 camera. Images were then processed and analyzed for seed area 
                                     and color using PlantCV ",a(href="http://plantcv.danforthcenter.org/",target='_blank',"(plantcv.danforthcenter.org).")),
                                   p("Fifty of these accessions were selected as a diversity panel to examine differences in shoot growth and seed production among quinoa
                                     varieties. Accessions were selected based on country of origin, native elevation, seed color, and seed size. After germination, six seedlings 
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
                                   h4(strong("Shoot Phenotyping (50 Accessions):")),
                                   p("Plants were imaged in an imaging octacgon with Raspberry Pi computers and cameras every other day for two weeks. Images of quinoa shoots were taken simultaneously from four 
                                      camera angles: three side views and one top view. Images were processed and analyzed for area and height above bound using", 
                                      a(href="http://plantcv.danforthcenter.org/",target='_blank',"PlantCV.")," Above-ground fresh- and dry-weight biomass (g) 
                                     were estimated from linear regression models of shoot area (cm^2) and height (cm)."), 
                                    p("Model for fresh-weight (adjusted R^2 = 0.776): FW = 1.278e-04(all_area) + all_ht, where 'all_area' is the sum of above-ground plant 
                                      pixel area from all four camera angles and 'all_ht' is the mean of above-ground plant pixel height from all camera angles."),
                                    p("Model for dry-weight (adjusted R^2 = 0.742): DW = 8.684e-06(sides_area) + 2.723e-05(area_top), where 'sides_area' is the sum area of all three 
                                      sides and 'area-top' is the area from top camera angle only."),
                                   p(" Heatmap data can be explored on the 'Shoot' tab and is generated from data averaged over all replicates per time point and scaled by row."), 
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
                                   h4(strong("Panicle Phenotyping (50 Accessions):")),
                                   p("Panicles were collected at 111-146 days after germination, according to maturity. Panicles were imaged using a Nikon Coolpix L830 
                                     camera."),
                                   p("Panicle shape and density were scored using the descriptors for quinoa established by", 
                                     a(href="https://www.bioversityinternational.org/fileadmin/user_upload/online_library/publications/pdfs/Descriptors_for_quinoa__Chenopodium_quinoa_Willd___and_wild_relatives_1679.pdf",target='_blank',
                                       "Bioversity International et al. 2013.")), 
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
                                   h4(strong("F1 Seed Phenotyping (50 Accessions):")),
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
                 tabPanel("Seed",
                          fluidRow(
                            column(12,
                                   wellPanel(
                                     h4("Generation 0"),
                                     p("Phenotyping data collected on seed stocks from USDA, IPK, and collaborators. If a genotype appears more than once it indicates that 
                                       the genotype was acquired from more than one source."))),
                            column(6,
                                   wellPanel(
                                     p(plotlyOutput('seed0.plot',height="900px")))),
                            column(6,
                                   wellPanel(
                                     p(verbatimTextOutput('seed0.image.selected')),
                                     uiOutput(outputId = "seed0.image"),
                                     h4("Generation 0 Seed Area Data:"),
                                     DT::dataTableOutput('seed0.table'))),
                            column(12),
                            column(6,
                                   wellPanel(
                                     p(plotlyOutput('seed0.pca',height="900px")))),
                            column(6,
                                   wellPanel(
                                     p(verbatimTextOutput('seed0.pca.selected')),
                                     uiOutput(outputId = "seed0.pca.image"),
                                     h4("Generation 0 Color PCA Data:"),
                                     DT::dataTableOutput('seed0.pca.table'))),
                            column(12,
                                   wellPanel(
                                     h4("Generation 1"),
                                     p("Seed color and seed area phenotype data collected on the ~50 accessions that were shoot and panicle phenotyped"))),
                            column(6,
                                   wellPanel(
                                     p(plotlyOutput('seed1.plot',height="900px")))),
                            column(6,
                                   wellPanel(
                                     p(verbatimTextOutput('seed1.image.selected')),
                                     uiOutput(outputId = "seed1.image"),
                                     h4("Generation 1 Seed Area Data:"),
                                     DT::dataTableOutput('seed1.table'))),
                            column(12),
                            column(6,
                                   wellPanel(
                                     p(plotlyOutput('seed1.pca',height="900px")))),
                            column(6,
                                   wellPanel(
                                     p(verbatimTextOutput('seed1.pca.selected')),
                                     uiOutput(outputId = "seed1.pca.image"),
                                     h4("Generation 1 Color PCA Data:"),
                                     DT::dataTableOutput('seed1.pca.table'))))),
                 tabPanel("Shoot",
                          fluidRow(
                            column(12,
                                   h4("Graph Shoot Data and View Images"),
                                   wellPanel(
                                     h4("Datasets"),
                                     selectInput('data.choice', 'Select:', c("Height (cm)", "Area (cm^2)", "Modeled fresh weight (g)", "Modeled dry weight (g)"),selected="Height (cm)",selectize = FALSE))),
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
                                     tabPanel("Heatmap", 
                                              column(9, 
                                                     h4(textOutput("heatmap.title")),
                                                     d3heatmapOutput('heatmap', height="700px")), br(),
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
                            column(12,
                                   h4("Graph Panicle Data"),
                                   radioButtons("paniclebuttons",label=h4("Color By:"), 
                                               c("Harvest Day"=0, "Density"=1,"Shape"=2,"Yield"=3 ),inline=TRUE)),
                            column(6,
                                   wellPanel(
                                   p(plotlyOutput("panicle.plot",height="900px")))),
                            column(6,
                                   wellPanel(
                                   p(verbatimTextOutput('panicle.image.selected')),
                                   uiOutput(outputId = "panicle.image"),
                                   h4("Panicle Data Table:"),
                                   DT::dataTableOutput('panicle'))),
                            column(12,
                                   h4("Scoring Key:")),
                            column(6,
                                   img(src="panicle-images/panicleShape_vert.png",
                                       height="100%", width="100%"),
                                   tags$small("Example", em("Chenopodium quinoa"),
                                              "panicle shape images. Courtesy of Elizabeth Castillo, 
                                              Donald Danforth Plant Science Center.")),
                             column(6,
                                    img(src="panicle-images/panicleDens_vert.png",
                                        height="100%", width="100%"),
                                    tags$small("Example", em("Chenopodium quinoa"),
                                               "panicle density images. Courtesy of Elizabeth Castillo, 
                                               Donald Danforth Plant Science Center.")))),
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
server <- function(input, output,session) {

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
  
  
  #heatmap title
  
  output$heatmap.title<-({renderText(paste(input$data.choice))})
  
  #display heatmap
  plot.heatmap<-({
  output$heatmap <- renderD3heatmap({
    d3heatmap(heatmap(), dendrogram ="none", colors=input$palette,
              xaxis_font_size = "9pt", yaxis_font_size = "9pt")
  })})
  
  observeEvent(input$data.choice,{
      withProgress(message = 'Making plot', value =NULL, {
        incProgress()
        plot.heatmap()
      })
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


####################################### SEED ######################################## 

  seed0.area.graph<-reactive({
    p2 <- plot_ly(gen0.seed.area, x = ~hum.name, y = ~normalized.area, type = "box", color=~as.factor(country), source="seed0.area.click",
                  colors = c("blue","magenta","navy","palevioletred","darkgreen","red","chartreuse","mediumspringgreen","brown","darkorange","yellow","darkorchid","cyan")) %>%
      layout(title = "Chenopodium quinoa Seed Size",
             margin = list(b = 100, t=50),
             dragmode = "zoom",
             yaxis = list(title = "Mean Normalized Seed Size"),
             xaxis = list(title = "Genotype", categoryarray=~hum.name,categoryorder="array"))  
  })
  
  output$seed0.plot<-{
    renderPlotly({
      seed0.area.graph()
    })}
  

  output$seed0.image.selected <- renderPrint({
    d <- event_data("plotly_click", source="seed0.area.click")
    geno<-unique(d$x)
    meansize<-gen0.seed.area[gen0.seed.area$hum.name==geno,]$mean.norm.area[1]
    if (is.null(d)) "Click Barchart of Seed size to See image, You may need to zoom in" 
    else paste("The image selected is: ",unique(d$x)," average normalized seed size is ",meansize,sep="")
  })
  
  output$seed0.image<-renderUI({
    d<-event_data("plotly_click", source="seed0.area.click")
    geno<-unique(d$x)
    image.name<-paste("seed-images/gen0/",gen0.seed.area[gen0.seed.area$hum.name==geno,]$plantbarcode[1],"_downsized.jpg",sep="")
    print(image.name)
    if(image.name=="seed-images/gen0/NA_downsized.jpg"){tags$img(src = "seed-images/blankseedimg_downsized.jpg", height="500")}else{tags$img(src = image.name, height="500")}
  })
  
  output$seed0.table <- DT::renderDataTable(DT::datatable({
    unique(gen0.seed.area[,c(776,777,778,779)])},colnames=c('genotype','country','mean.seed.area','mean.seed.area.sd'), rownames = FALSE,
    options=list(lengthMenu = list(c(10,25,50,-1), c('10','25','50','All')),
                 pageLength = 5, scrollX = TRUE)))
  
  seed0.color.pca<-reactive({
    
    p<-plot_ly(source="seed0.pca.click")%>%
      add_markers(data=gen0.seed.color, x = ~PC1, y = ~PC2, z = ~PC3, color=~as.factor(country),
                  colors = c("blue","magenta","navy","palevioletred","darkgreen","red","chartreuse","mediumspringgreen","brown","darkorange","yellow","darkorchid","cyan"),
                  marker = list(symbol = 'circle'), showlegend=TRUE,
                  text = ~paste('Genotype:', hum.name,'<br>Norm. Mean Area',norm_area ,'<br>Country:', country))%>%
      layout(title = 'Seed Color',
             scene = list(xaxis = list(title = 'PC1 (POV:21.82%)'),
                          yaxis = list(title = 'PC2 (POV:14.39%)'),
                          zaxis = list(title = 'PC3 (POV:6.74%)'),
                          height = 800, units="px"))
  })
  
  output$seed0.pca<-{
    renderPlotly({
      seed0.color.pca()
    })}
  
  output$seed0.pca.table <- DT::renderDataTable(DT::datatable({
    unique(gen0.seed.color[,c(401,403,1,2,3)])}, colnames=c("genotype","country","PC1","PC2","PC3"),rownames = FALSE,
    options=list(lengthMenu = list(c(10,25,50,-1), c('10','25','50','All')),
                 pageLength = 5, scrollX = TRUE)))
  
  output$seed0.pca.selected <- renderPrint({
    d <- event_data("plotly_click", source="seed0.pca.click")
    if(is.null(d)){ "Click PCA of Seed Color to See image"}
    else{key<-signif(d$z,6)
    gen0.seed.color$key<-signif(gen0.seed.color$PC3,6)
    geno<-gen0.seed.color[gen0.seed.color$key==key,]$hum.name
    paste("The image selected is: ",geno,sep="")}
  })
  
  output$seed0.pca.image<-renderUI({
    d <- event_data("plotly_click", source="seed0.pca.click")
    if(is.null(d)){image.name="seed-images/gen0/NA.00_downsized.jpg"}
    else{key<-signif(d$z,6)
    gen0.seed.color$key<-signif(gen0.seed.color$PC3,6)
    geno<-gen0.seed.color[gen0.seed.color$key==key,]$genotype
    filename<-paste("./www/seed-images/gen0/",geno,".00_downsized.jpg",sep="")
    image.name<-paste("seed-images/gen0/",geno,".00_downsized.jpg",sep="")
    print(image.name)}
    if(image.name=="seed-images/gen0/NA.00_downsized.jpg"){tags$img(src = "seed-images/blankseedimg_downsized.jpg", height="500")}
    else{tags$img(src = image.name, height="500")}
  })
  
  
  seed1.area.graph<-reactive({
    p2 <- plot_ly(gen1.seed.area, x = ~hum.rep, y = ~normalized.area, type = "box", color=~as.factor(country), source="seed1.area.click",
                  colors = c("blue","navy","darkgreen","chartreuse","mediumspringgreen","darkorchid","cyan")) %>%
      layout(title = "Chenopodium quinoa Seed Size",
             margin = list(b = 100, t=50),
             dragmode = "zoom",
             yaxis = list(title = "Mean Normalized Seed Size"),
             xaxis = list(title = "Genotype", categoryarray=~hum.rep,categoryorder="array"))  
  })
  
  output$seed1.plot<-{
    renderPlotly({
      seed1.area.graph()
    })}
  

  output$seed1.table <- DT::renderDataTable(DT::datatable({
    unique(gen1.seed.area[,c(781,780,777,778)])},colnames=c('genotype','country','mean.seed.area','mean.seed.area.sd'), rownames = FALSE,
    options=list(lengthMenu = list(c(10,25,50,-1), c('10','25','50','All')),
                 pageLength = 5, scrollX = TRUE)))
  
  
  output$seed1.image.selected <- renderPrint({
    d <- event_data("plotly_click", source="seed1.area.click")
    geno<-unique(d$x)
    print(length(unique(gen1.seed.area$hum.rep)))
    meansize<-gen1.seed.area[gen1.seed.area$hum.rep==geno,]$mean.norm.area[1]
    if (is.null(d)) "Click Barchart of Seed size to See image, You may need to zoom in" 
    else paste("The image selected is: ",unique(d$x)," average normalized seed size is ",meansize,sep="")
  })
  
  output$seed1.image<-renderUI({
    d<-event_data("plotly_click", source="seed1.area.click")
    geno<-unique(d$x)
    image.name<-paste("seed-images/gen1/",gen1.seed.area[gen1.seed.area$hum.rep==geno,]$plantbarcode[1],"_downsized.jpg",sep="")
    print(image.name)
    print(gen1.seed.area[gen1.seed.area$hum.rep==geno,]$plantbarcode)
    if(image.name=="seed-images/gen1/NA_downsized.jpg"){tags$img(src = "seed-images/blankseedimg_downsized.jpg", height="500")}else{tags$img(src = image.name, height="500")}
  })
  
  seed1.color.pca<-reactive({
    
    p<-plot_ly(source="seed1.pca.click")%>%
      add_markers(data=gen1.seed.color, x = ~PC1, y = ~PC2, z = ~PC3, color=~as.factor(country),
                  colors = c("blue","navy","darkgreen","chartreuse","mediumspringgreen","darkorchid","cyan"),
                  marker = list(symbol = 'circle'), showlegend=TRUE,
                  text = ~paste('Genotype-Replicate:', hum.rep,'<br>Norm. Mean Area',mean.norm.area ,'<br>Country:', country))%>%
      layout(title = 'Seed Color',
             scene = list(xaxis = list(title = 'PC1 (POV:24.64%)'),
                          yaxis = list(title = 'PC2 (POV:16.75%)'),
                          zaxis = list(title = 'PC3 (POV:9.45%)'),
                          height = 800, units="px"))
  })
  
  output$seed1.pca<-{
    renderPlotly({
      seed1.color.pca()
    })}
  
  output$seed1.pca.selected <- renderPrint({
    d <- event_data("plotly_click", source="seed1.pca.click")
    if(is.null(d)){ "Click PCA of Seed Color to See image"}
    else{key<-signif(d$z,6)
    gen1.seed.color$key<-signif(gen1.seed.color$PC3,6)
    geno<-gen1.seed.color[gen1.seed.color$key==key,]$hum.rep
    paste("The image selected is: ",geno,sep="")}
  })
  
  output$seed1.pca.image<-renderUI({
    d <- event_data("plotly_click", source="seed1.pca.click")
    if(is.null(d)){tags$img(src = "seed-images/blankseedimg_downsized.jpg", height="500")}
    else{key<-signif(d$z,6)
    gen1.seed.color$key<-signif(gen1.seed.color$PC3,6)
    geno<-gen1.seed.color[gen1.seed.color$key==key,]$plantbarcode
    filename<-paste("./www/seed-images/gen1/",geno,"_downsized.jpg",sep="")
    image.name<-paste("seed-images/gen1/",geno,"_downsized.jpg",sep="")
    tags$img(src = image.name, height="500")}
  })
  
  output$seed1.pca.table <- DT::renderDataTable(DT::datatable({
    unique(gen1.seed.color[,c(188,189,1,2,3)])}, colnames=c("genotype","country","PC1","PC2","PC3"),rownames = FALSE,
    options=list(lengthMenu = list(c(10,25,50,-1), c('10','25','50','All')),
                 pageLength = 5, scrollX = TRUE)))

#####################################################################################

###################################### Panicle ###################################### 
  
  panicle.graph<-reactive({
  
    if(input$paniclebuttons==0){
    colorby=~DaysAtHarvest
    namelab='Harvest Day'
    }
    
    if(input$paniclebuttons==1){
      colorby=~as.factor(Density)
      namelab=~as.factor(density.h)
    }
    
    if(input$paniclebuttons==2){
      colorby=~as.factor(Shape)
      namelab=~as.factor(shape.h)
    }
    
    if(input$paniclebuttons==3){
      colorby=~SeedWT_All_g
      namelab='Yield'
    }
    
    Sys.setlocale(locale="C")
    
    p<-plot_ly(source="panicle.pca.click")%>%
      add_trace(data=panicle.loading, x=~PC1[1:2],y=~PC2[1:2],z=~PC3[1:2], color='red', mode='lines', type='scatter3d', showlegend=FALSE)%>%
      add_text(data=panicle.loading, x=~PC1[1],y=~PC2[1],z=~PC3[1], color='red',text="Harvest Day", showlegend=FALSE)%>%
      add_trace(data=panicle.loading,x=~PC1[3:4],y=~PC2[3:4],z=~PC3[3:4], color='blue', mode='lines', type='scatter3d', showlegend=FALSE)%>%
      add_text(data=panicle.loading, x=~PC1[3],y=~PC2[3],z=~PC3[3], color='blue', text="Yield", showlegend=FALSE)%>%
      add_trace(data=panicle.loading,x=~PC1[5:6],y=~PC2[5:6],z=~PC3[5:6], color='purple', mode='lines', type='scatter3d', showlegend=FALSE)%>%
      add_text(data=panicle.loading, x=~PC1[5],y=~PC2[5],z=~PC3[5], color='purple', text="Density", showlegend=FALSE)%>%
      add_trace(data=panicle.loading,x=~PC1[7:8],y=~PC2[7:8],z=~PC3[7:8], color='orange', mode='lines', type='scatter3d', showlegend=FALSE)%>%
      add_text(data=panicle.loading, x=~PC1[7],y=~PC2[7],z=~PC3[7], color='orange', text="Shape", showlegend=FALSE)%>%
      add_markers(data=panicle.pca, x = ~PC1, y = ~PC2, z = ~PC3, color=colorby, name=namelab,
                                marker = list(symbol = 'circle'), showlegend=TRUE,
                                text = ~paste('Genotype:', Genotype,'<br>Harvest Day',DaysAtHarvest ,'<br>Yield (g):', SeedWT_All_g, '<br>Density:', density.h,
                                              '<br>Shape:', shape.h))%>%
      layout(title = 'Panicle Phenotype Principal Components',
              scene = list(xaxis = list(title = 'PC1 (POV:73.14%)'),
                           yaxis = list(title = 'PC2 (POV:13.57%)'),
                           zaxis = list(title = 'PC3 (POV:9.74%)'),
                           height = 800, units="px"))
  
    
    })
  
  
  output$panicle.plot<-{
    renderPlotly({
      panicle.graph()
    })
  }
  
  output$panicle.image.selected <- renderPrint({
    d <- event_data("plotly_click", source="panicle.pca.click")
    linenum<-unique(d$pointNumber)
    geno<-panicle.pca[linenum+1,]$Genotype
    if (is.null(d)) "Click Barchart of Seed size to See image, You may need to zoom in" 
    else paste("The image selected is: ",geno,sep="")
  })
  
  output$panicle.image<-renderUI({
    d <- event_data("plotly_click", source="panicle.pca.click")
    linenum<-unique(d$pointNumber)
    geno<-panicle.pca[linenum+1,]$ID
    image.name<-paste("panicle-images/",geno,"_downsized.jpg",sep="")
    filename<-paste("./www/panicle-images/",geno,"_downsized.jpg",sep="")
    if(image.name=="panicle-images/_downsized.jpg"){tags$img(src = "panicle-images/blankpanicle_downsized.jpg", height="500")}
    else if(!file.exists(filename)){tags$img(src = "panicle-images/missing.panicle_downsized.jpg", height="500")}
    else{tags$img(src = image.name, height="500")}
  })
  
  #output panicle table
  output$panicle <- DT::renderDataTable(DT::datatable({
    panicle[,c(1,3,5,6,4)]}, rownames = FALSE,
    options=list(lengthMenu = list(c(10,25,50,-1), c('10','25','50','All')),
                 pageLength = 5, scrollX = TRUE)))
  
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
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
  
}

##Shinyapp
shinyApp(ui = ui, server = server)