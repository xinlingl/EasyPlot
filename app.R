library(shiny)
library(ggplot2)
library(FactoMineR)
library(DESeq2)
library(VennDiagram)
library(gplots) 
library(png)
library(calibrate)
library(enrichR)

ui <- navbarPage("EasyPlot",
                 tabPanel("Box Plot",
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Select a csv file as input. Note that entries can't contain spaces", style = "color:red"),
                              h5 ("The format of the input dataset needs to match the demo dataset on the right.", style = "color:red"),
                              fileInput("file1", "Choose a CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv"), 
                                        width='270px'
                              ),
                              h5 ("Press the button below to generate summary statistics"),
                              actionButton("goButton2", "Generate summary statistics"),
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("fontsize", label="Font size of x-axis and y-axis labels", value=1, width='270px'), 
                              textInput("titlesize", label="Font size of title", value=1, width='270px'),
                              textInput("axissize", label="Font size of x-axis", value=1, width='270px'),
                              textInput("boxnameangle", label="Angle of group names of boxes (eg. 1, 2, 3)", value=1, width='270px'),
                              textInput("xlabel", label="X-axis label", value="", width='270px'),
                              textInput("ylabel", label="Y-axis label", value="", width='270px'),
                              textInput("xlabelpos", label="X-axis label position", value=3, width='270px'),
                              textInput("ylabelpos", label="Y-axis label position", value=3, width='270px'),
                              textInput("title", label="Plot title", value="", width='270px'),
                              textInput("color", label="Colors of the boxes (comma delimited)", value="", width='270px'),
                              h5 ("Colors in HEX format can be chosen on http://colorbrewer2.org/"),
                              textInput("boxwidth", label="Width of the boxes", value=0.2, width='270px'), 
                              h5 ("Note: the length of group names need to be the same as the number of samples in the dataset", style = "color:red"),
                              textInput("boxname", label="Group names of boxes (comma delimited)", value="", width='270px'),
                              textInput("plotwidth", label="Width of the downloaded plot in pixels", value=1000, width='270px'), 
                              textInput("plotheight", label="Height of the downloaded plot in pixels", value=1000, width='270px'),
                              actionButton("goButton", "Generate a boxplot"),
                              downloadLink("downloadPlot", "Download the generated plot"),
                              tags$hr()
                            ),
                            mainPanel(
                              verbatimTextOutput("sum"),
                              img(src='BoxplotSampleData.png', align = "left"),  ### need to be in a folder called www which needs to be in the same folder as the R code 
                              plotOutput("coolplot")
                            )
                          )
                 ),
                 tabPanel("Bar Plot",
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Select a csv file as input. Note that entries can't contain spaces", style = "color:red"),
                              h5 ("The format of the input dataset needs to match the demo dataset on the right.", style = "color:red"),
                              fileInput("file2", "Choose a CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv"), 
                                        width='270px'
                              ), 
                              h5 ("Press the button below to generate summary statistics"),
                              actionButton("goButton4", "Generate summary statistics"),
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Required parameters:", style = "color:blue"),
                              textInput("fontsize2", label="Font size of x-axis and y-axis labels", value=1, width='270px'), 
                              textInput("titlesize2", label="Font size of title", value=1, width='270px'),
                              textInput("xaxissize", label="Font size of x-axis", value=1, width='270px'),
                              textInput("yaxissize", label="Font size of y-axis", value=1, width='270px'),
                              textInput("barnameangle", label="Angle of group names of bars (eg. 1, 2, 3)", value=1, width='270px'),
                              textInput("yaxislowerlimit", label="Lower limit of y-axis", value="", width='270px'),
                              textInput("yaxisupperlimit", label="Upper limit of y-axis", value="", width='270px'),
                              h5 ("Note: the length of colors of the bars need to be the same as the number of samples in the dataset", style = "color:red"),
                              textInput("color2", label="Colors of the bars (comma delimited)", value="", width='270px'),
                              h5 ("Colors in HEX format can be chosen on http://colorbrewer2.org/"),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("xlabel2", label="X-axis label", value="", width='270px'),
                              textInput("ylabel2", label="Y-axis label", value="", width='270px'),
                              textInput("xlabelpos2", label="X-axis label position", value=3, width='270px'),
                              textInput("ylabelpos2", label="Y-axis label position", value=3, width='270px'),
                              textInput("title2", label="Plot title", value="", width='270px'),
                              textInput("barwidth2", label="Width of the bars", value=1, width='270px'), 
                              textInput("plotrightmargin", label="Right margin of the plot", value=0, width='270px'), 
                              textInput("legendleftmargin", label="Left margin of the legend", value=0, width='270px'),
                              textInput("plotwidth2", label="Width of the downloaded plot in pixels", value=1000, width='270px'), 
                              textInput("plotheight2", label="Height of the downloaded plot in pixels", value=1000, width='270px'),
                              actionButton("goButton3", "Generate a barplot"),
                              downloadLink("downloadPlot2", "Download the generated plot"),
                              tags$hr()
                            ),
                            mainPanel(
                              verbatimTextOutput("sumBar"),
                              img(src='BarplotSampleData.png', align = "left"),
                              plotOutput("barplot")
                            )
                          )
                 ),
                 tabPanel("Scatter Plot",
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Select a csv file as input. Note that entries can't contain spaces", style = "color:red"),
                              h5 ("The format of the input dataset needs to match the demo dataset on the right.", style = "color:red"),
                              fileInput("file3", "Choose a CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ),
                              h5 ("Press the button below to generate summary statistics"),
                              actionButton("goButton6", "Generate summary statistics"),
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Required parameters:", style = "color:blue"),
                              textInput("xaxisvariable", label="Variable in x-axis (column number of table)", value=1, width='270px'),
                              textInput("yaxisvariable", label="Variable in y-axis (column number of table)", value=2, width='270px'), 
                              textInput("fontsize3", label="Font size of x-axis and y-axis labels", value=1, width='270px'), 
                              textInput("titlesize3", label="Font size of title", value=1, width='270px'),
                              textInput("axissize3", label="Font size of x-axis and y-axis", value=1, width='270px'),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("pointshape", label="shape of point", value=1, width='270px'),
                              h5 ("See numeric codes correspond to different shapes of points"),
                              textInput("xlabel3", label="Label of x-axis", value="", width='270px'),
                              textInput("ylabel3", label="Label of y-axis", value="", width='270px'), 
                              textInput("title3", label="Plot title", value="", width='270px'),
                              textInput("color3", label="Color of the dots", value="#000000", width='270px'),
                              h5 ("Colors in HEX format can be chosen on http://colorbrewer2.org/"),
                              textInput("plotwidth3", label="Width of the downloaded plot in pixels", value=1000, width='270px'), 
                              textInput("plotheight3", label="Height of the downloaded plot in pixels", value=1000, width='270px'),
                              actionButton("goButton5", "Generate a scatterplot"),
                              downloadLink("downloadPlot3", "Download the generated plot"),
                              tags$hr()
                            ),
                            mainPanel(
                              verbatimTextOutput("sumScatter"),
                              img(src='ScatterplotSampleData.png', align = "left"),
                              plotOutput("scatterplot")
                            )
                          )
                 ),
                 tabPanel("Histogram",
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Select a csv file as input. Note that entries can't contain spaces", style = "color:red"),
                              h5 ("The format of the input dataset needs to match the demo dataset on the right.", style = "color:red"),
                              fileInput("file4", "Choose a CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ),
                              h5 ("Press the button below to generate summary statistics"),
                              actionButton("goButton8", "Generate summary statistics"),
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Required parameters:", style = "color:blue"),
                              textInput("binnumber", label="Number of bins", value="", width='270px'),
                              textInput("axissize4", label="Font size of x-axis and y-axis", value=1, width='270px'),
                              textInput("fontsize4", label="Font size of x-axis and y-axis labels", value=1, width='270px'), 
                              textInput("titlesize4", label="Font size of title", value=1, width='270px'),
                              textInput("legendpos", label="Position of the legend", value="", width='270px'), 
                              h5 ("Position should be one of 'bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right' or 'center'"),
                              h5 ("Note: the length of color of the bars need to be the same as the number of samples in the dataset", style = "color:red"),
                              textInput("color4", label="Color of the bars", value="", width='270px'),
                              h5 ("Colors in HEX format can be chosen on http://colorbrewer2.org/"),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("xlabel4", label="Label of x-axis", value="", width='270px'),
                              textInput("ylabel4", label="Label of y-axis", value="", width='270px'), 
                              textInput("title4", label="Plot title", value="", width='270px'),
                              textInput("plotwidth4", label="Width of the downloaded plot in pixels", value=1000, width='270px'), 
                              textInput("plotheight4", label="Height of the downloaded plot in pixels", value=1000, width='270px'),
                              actionButton("goButton7", "Generate a histogram"), 
                              downloadLink("downloadPlot4", "Download the generated plot"),
                              tags$hr()
                            ),
                            mainPanel(
                              verbatimTextOutput("sumHistogram"),
                              img(src='HistogramSampleData.png', align = "left"),
                              plotOutput("histogram")
                            )
                          )
                 ),
                 tabPanel("Line Plot",
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Select a csv file as input. Note that entries can't contain spaces", style = "color:red"),
                              h5 ("The format of the input dataset needs to match the demo dataset on the right.", style = "color:red"),
                              fileInput("file5", "Choose a CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ), 
                              h5 ("Press the button below to generate summary statistics"),
                              actionButton("goButton10", "Generate summary statistics"),
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Required parameters:", style = "color:blue"),
                              h5 ("Note: the length of the colors need to match the number of the samples in the dataset", style = "color:red"),
                              textInput("color5", label="Colors of the lines (comma delimited)", value="", width='270px'),
                              h5 ("Colors in HEX format can be chosen on http://colorbrewer2.org/"),
                              textInput("fontsize5", label="Font size of x-axis and y-axis labels", value=1, width='270px'), 
                              textInput("titlesize5", label="Font size of title", value=1, width='270px'),
                              textInput("axissize5", label="Font size of x-axis and y-axis", value=1, width='270px'),
                              textInput("yaxislowerlimit2", label="Lower limit of y-axis", value="", width='270px'),
                              textInput("yaxisupperlimit2", label="Upper limit of y-axis", value="", width='270px'),
                              textInput("legendposition", label="Position of the legend", value="", width='270px'), 
                              h5 ("Position should be one of 'bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right' or 'center'"),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("xlabel5", label="Label of x-axis", value="", width='270px'),
                              textInput("ylabel5", label="Label of y-axis", value="", width='270px'), 
                              textInput("title5", label="Plot title", value="", width='270px'),
                              textInput("linethickness", label="Thickness of lines", value=1, width='270px'), 
                              textInput("plotwidth5", label="Width of the downloaded plot in pixels", value=1000, width='270px'), 
                              textInput("plotheight5", label="Height of the downloaded plot in pixels", value=1000, width='270px'),
                              actionButton("goButton9", "Generate a line plot"), 
                              downloadLink("downloadPlot5", "Download the generated plot"),
                              tags$hr()
                            ),
                            mainPanel(
                              verbatimTextOutput("sumLinegraph"),
                              img(src='LinegraphSampleData.png', align = "left"),
                              plotOutput("linegraph")
                            )
                          )
                 ),
                 tabPanel("PCA Plot", 
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Select a csv file as input. Note that entries can't contain spaces", style = "color:red"),
                              h5 ("The format of the input dataset needs to match the demo dataset on the right.", style = "color:red"),
                              fileInput("file6", "Choose a CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ), 
                              h5 ("Press the button below to generate summary statistics"),
                              actionButton("goButton12", "Generate summary statistics"),
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Note: it may take a few minutes to generate the plot and download it if the sample size is large", style = "color:red"),
                              h5 ("Required parameters:", style = "color:blue"),
                              h5 ("Note: there must be replicates within the samples", style = "color:red"),
                              h5 ("The length of group names of samples need to match the number of samples in the dataset", style = "color:red"),
                              textInput("groups", label="Group names of samples (comma delimited)", value="", width='270px'), 
                              textInput("xdim", label="Principal component in x-axis", value=1, width='270px'), 
                              textInput("ydim", label="Principal component in y-axis", value=2, width='270px'),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("plotwidth6", label="Width of the downloaded plot in pixels", value=400, width='270px'), 
                              textInput("plotheight6", label="Height of the downloaded plot in pixels", value=400, width='270px'),
                              actionButton("goButton11", "Generate a PCA plot below"), 
                              downloadLink("downloadPlot6", "Download the generated plot"),
                              tags$hr()
                            ),
                            mainPanel(
                              img(src='PCASampleData.png', align = "left"),
                              verbatimTextOutput("sumPCAplot"),
                              plotOutput("PCAplot")
                            )
                          )
                 ),
                 tabPanel("Venn diagram", 
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Select a csv file as input. Note that entries can't contain spaces and there can't be duplicates in the each column", style = "color:red"),
                              h5 ("The format of the input dataset needs to match the demo dataset on the right.", style = "color:red"),
                              fileInput("file7", "Choose a CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ),
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Required parameters:", style = "color:blue"),
                              h5 ("Note: the length of the names of sets, colors of the circles and names of sets need to match the number of samples in the dataset", style = "color:red"),
                              textInput("setname", label="Names of sets (comma delimited)", value="", width='270px'), 
                              textInput("color6", label="Colors of the circles (comma delimited)", value="", width='270px'),
                              textInput("setlabcol", label="Colors of names of sets", value="", width='270px'), 
                              h5 ("Colors in HEX format can be chosen on http://colorbrewer2.org/"),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("title6", label="Plot title", value="", width='270px'),
                              textInput("titlesize6", label="Font size of title", value=1, width='270px'),
                              textInput("fontsize6", label="Font size of numbers", value=1, width='270px'), 
                              textInput("setlabfontsize", label="Font size of names of sets", value=1, width='270px'), 
                              textInput("plotwidth7", label="Width of the downloaded plot in pixels", value=1000, width='270px'), 
                              textInput("plotheight7", label="Height of the downloaded plot in pixels", value=1000, width='270px'),
                              actionButton("goButton13", "Generate a Venn diagram"), 
                              downloadLink("downloadPlot7", "Download the generated plot"),
                              tags$hr()
                            ),
                            mainPanel(
                              img(src='VenndiagramSampleData.png', align="right"), 
                              plotOutput("Venndiagram")
                            )
                          )
                 ),
                 tabPanel("Heatmap", 
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Select a csv file as input. Note that entries can't contain spaces.", style = "color:red"),
                              h5("The data is pre-filetered and normalized", style="color:red"),
                              h5 ("The format of the input dataset needs to match the demo dataset on the right.", style = "color:red"),
                              fileInput("file8", "Choose a CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ), 
                              h5 ("Press the button below to generate summary statistics"),
                              actionButton("goButton15", "Generate summary statistics"),
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Note: it may take a few minutes to generate the plot and download it", style = "color:red"),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("title7", label="Plot title", value="", width='270px'),
                              textInput("topbottommargin", label="Margin on the top and bottom", value=16, width='270px'), 
                              textInput("leftrightmargin", label="Margin on the left and right", value=16, width='270px'), 
                              textInput("plotwidth8", label="Width of the downloaded plot in pixels", value=1000, width='270px'), 
                              textInput("plotheight8", label="Height of the downloaded plot in pixels", value=1000, width='270px'),
                              actionButton("goButton14", "Generate a Heatmap"), 
                              downloadLink("downloadPlot8", "Download the generated plot"),
                              tags$hr()
                            ),
                            mainPanel(
                              verbatimTextOutput("sumHeatmap"),
                              img(src='HeatmapSampleData.png', align="right"), 
                              plotOutput("Heatmap")
                            )
                          )
                 ), 
                 tabPanel("Volcano Plot", 
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Select a csv file as input. Note that entries can't contain spaces.", style = "color:red"),
                              h5 ("The format of the input dataset needs to match the demo dataset on the right.", style = "color:red"),
                              fileInput("file9", "Choose a CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ), 
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("color7", label="Color of the genes with log2FoldChange<=-1", value="orange", width='270px'), 
                              textInput("color8", label="Color of the genes with log2FoldChange>=1", value="blue", width='270px'),
                              textInput("color9", label="Color of the genes with padj<.05 and abs(log2FoldChange)>1", value="red", width='270px'),
                              h5 ("Colors in HEX format can be chosen on http://colorbrewer2.org/"),
                              textInput("title8", label="Plot title", value="", width='270px'),
                              textInput("plotwidth9", label="Width of the downloaded plot in pixels", value=1000, width='270px'), 
                              textInput("plotheight9", label="Height of the downloaded plot in pixels", value=1000, width='270px'),
                              actionButton("goButton16", "Generate a volcano plot"), 
                              downloadLink("downloadPlot9", "Download the generated plot"),
                              tags$hr()
                            ),
                            mainPanel(
                              img(src='VolcanoplotSampleData.png', align="right"), 
                              plotOutput("Volcanoplot")
                            )
                          )
                 ),
                 tabPanel("GO enrichment plot",
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Note: it may take a few minutes to generate the table if the number of genes is large", style = "color:red"),
                              h5 ("Required parameters:", style = "color:blue"),
                              textInput("genelist", label="Genes of interest (comma delimited)", value="", width='270px'),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("yaxisleftmargin", label="Left margin of yaxis", value=30, width='270px'),
                              textInput("plotwidth10", label="Width of the downloaded plot in pixels", value=1000, width='270px'), 
                              textInput("plotheight10", label="Height of the downloaded plot in pixels", value=1000, width='270px'),
                              actionButton("goButton17", "Generate a GO Enrichment plot"),
                              downloadLink("downloadPlot10", "Download the generated plot")
                            ),
                            mainPanel(
                              img(src='GoEnrichmentPlotSampleData.png', align="right"), 
                              plotOutput("GoEnrichment")
                            )
                          )
                 ),
                 tabPanel("Violin plot",
                          sidebarLayout(
                            sidebarPanel(
                              h5 ("Select a csv file as input.", style = "color:red"),
                              h5 ("The header of input file needs to match the demo dataset on the right.", style = "color:red"),
                              fileInput("file10", "Choose a CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ), 
                              h5 ("Set the parameters below and press generate figure button on the bottom"),
                              h5 ("Optional parameters:", style = "color:blue"),
                              textInput("xaxis", label="Variable on x axis", value="", width='270px'), 
                              textInput("yaxis", label="Variable on y axis", value="", width='270px'),
                              textInput("title9", label="Plot title", value="", width='270px'),
                              textInput("axissize6", label="Size of x and y axis", value=10, width='270px'),
                              textInput("titlesize7", label="Size of title", value=10, width='270px'),
                              textInput("labelsize", label="Size of label", value=10, width='270px'),
                              textInput("plotwidth11", label="Width of the downloaded plot in pixels", value=1000, width='270px'), 
                              textInput("plotheight11", label="Height of the downloaded plot in pixels", value=1000, width='270px'),
                              actionButton("goButton18", "Generate a violin plot"),
                              downloadLink("downloadPlot11", "Download the generated plot")
                            ),
                            mainPanel(
                              img(src='SampleImageViolinPlot.png', align="right"), 
                              plotOutput("Violinplot")
                            )
                          )
                 )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  # make a boxplot
  output$coolplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton
    isolate({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      data=read.csv(inFile$datapath, header=T)
      newdata=data[,-1]
      op <- par(mar=c(20,6,1,1))
      boxplot(newdata, names.arg=colnames(data)[-1], cex.axis=input$axissize, las=input$boxnameangle, main=input$title, col=unlist(strsplit(input$color,",")), boxwex=as.numeric(input$boxwidth), names=unlist(strsplit(input$boxname,",")), cex.main=input$titlesize)
      title(xlab = input$xlabel, line = input$xlabelpos, cex.lab=input$fontsize)
      title(ylab = input$ylabel, line = input$ylabelpos, cex.lab=input$fontsize)
      rm(op)
    })
  })
  
  output$sum <- renderPrint({
    input$goButton2
    isolate({
      inFile <- input$file1
      if (is.null(inFile))
        return(invisible("Data Visulization"))
      data2=read.csv(inFile$datapath, header=T)
      newdata2=data2[,-1]
      do.call(cbind, lapply(newdata2, summary))
    })
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste('boxplot', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width = as.numeric(input$plotwidth), height = as.numeric(input$plotheight), units = "px")
      # draw the plot
      op <- par(mar=c(20,6,1,1))
      boxplot(read.csv(input$file1$datapath, header=T)[,-1], names.arg=colnames(data)[-1], cex.axis=input$axissize, las=input$boxnameangle, main=input$title, col=unlist(strsplit(input$color,",")), boxwex=as.numeric(input$boxwidth), names=unlist(strsplit(input$boxname,",")), cex.main=input$titlesize)
      title(xlab = input$xlabel, line = input$xlabelpos, cex.lab=input$fontsize)
      title(ylab = input$ylabel, line = input$ylabelpos, cex.lab=input$fontsize)
      
      # turn the device off
      dev.off()
    }
  )
  
  #make a box plot for demo
  output$DemoBoxplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    data=read.csv("sample_data5.csv",header=T)
    newdata=data[,-1]
    op <- par(mar=c(20,6,1,1))
    boxplot(newdata, names.arg=colnames(data)[-1], cex.axis=1, las=1, main="Demo", col=c("light gray", "dark gray"), boxwex=0.25, names=c("sample_1", "sample_2"), cex.main=1)
    title(xlab = "Sample name", line = 2, cex.lab=1)
    title(ylab = "Gene expression", line = 2, cex.lab=1)
  })
  
  
  #make a barplot
  output$barplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton3
    isolate({
      inFile2 <- input$file2
      
      if (is.null(inFile2))
        return(NULL)
      print(inFile2)
      data=read.csv(inFile2$datapath, header=T)
      newdata=data[,-1]
      print("dimension")
      opar = par(oma = c(0,0,0,as.numeric(input$plotrightmargin))) # Large right margin for plot
      barplot(t(newdata), beside=FALSE, names=data[,1], col=unlist(strsplit(input$color2,",")), main=input$title2, space=as.numeric(input$barwidth2), ylim=c(as.numeric(input$yaxislowerlimit),as.numeric(input$yaxisupperlimit)), cex.axis=input$yaxissize, cex.main=input$titlesize2, las=input$barnameangle, cex.names=input$xaxissize)
      opar = par(oma = c(0,0,0,0), mar = c(0,0,0,as.numeric(input$legendleftmargin)), new = TRUE)
      legend(x = "right", legend = rownames(t(newdata)), fill = unlist(strsplit(input$color2,",")), bty = "n", y.intersp = 2)
      par(opar)
      title(xlab = input$xlabel2, line = input$xlabelpos2, cex.lab=input$fontsize2)
      title(ylab = input$ylabel2, line = input$ylabelpos2, cex.lab=input$fontsize2)
      rm(opar)
    })
  })
  
  output$sumBar <- renderPrint({
    input$goButton4
    isolate({
      inFile2 <- input$file2
      if (is.null(inFile2))
        return(invisible("Data Visulization"))
      data2=read.csv(inFile2$datapath, header=T)
      newdata2=data2[,-1]
      do.call(cbind, lapply(newdata2, summary))
    })
  })
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() { paste('barplot', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width = as.numeric(input$plotwidth2), height = as.numeric(input$plotheight2), units = "px")
      # draw the plot
      
      opar = par(oma = c(0,0,0,as.numeric(input$plotrightmargin))) # Large right margin for plot
      barplot(t(read.csv(input$file2$datapath)[,-1]), beside=FALSE, names=read.csv(input$file2$datapath)[,1], col=unlist(strsplit(input$color2,",")), main=input$title2, space=as.numeric(input$barwidth2), ylim=c(as.numeric(input$yaxislowerlimit),as.numeric(input$yaxisupperlimit)), cex.axis=input$yaxissize, cex.main=input$titlesize2, las=input$barnameangle, cex.names=input$xaxissize)
      opar = par(oma = c(0,0,0,0), mar = c(0,0,0,as.numeric(input$legendleftmargin)), new = TRUE)
      legend(x = "right", legend = rownames(t(read.csv(input$file2$datapath)[,-1])), fill = unlist(strsplit(input$color2,",")), bty = "n", y.intersp = 2)
      par(opar)
      title(xlab = input$xlabel2, line = input$xlabelpos2, cex.lab=input$fontsize2)
      title(ylab = input$ylabel2, line = input$ylabelpos2, cex.lab=input$fontsize2)
      rm(opar)
      # turn the device off
      dev.off()
    }
  )
  
  #make a bar plot for demo
  output$DemoBarplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    data=read.csv("sample_data5.csv",header=T)
    newdata=data[,-1]
    opar = par(oma = c(0,0,0,0))
    #print(names.arg)
    barplot(t(read.csv("sample_data5.csv")[,-1]), beside=FALSE, names=read.csv("sample_data5.csv")[,1], col=unlist(c("pink", "orange")), main="Demo", space=1, ylim=c(0,50), cex.axis=1, cex.main=1, las=1, cex.names=1)
    opar = par(oma = c(0,0,0,0), mar = c(0,0,0,as.numeric(input$legendleftmargin)), new = TRUE)
    legend(x = "right", legend = rownames(t(newdata)), fill = unlist(c("pink", "orange")), bty = "n", y.intersp = 2)
    par(opar)
    title(xlab = "Gene name", line = 3, cex.lab=1)
    title(ylab = "Expression level", line = 3, cex.lab=1)
    rm(opar)
  })
  
  #make a scatterplot
  output$scatterplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton5
    isolate({
      inFile3 <- input$file3
      
      if (is.null(inFile3))
        return(NULL)
      print(inFile3)
      data=read.csv(inFile3$datapath, header=T)
      newdata=data[,-1]
      print("dimension")
      plot(newdata[,as.numeric(input$xaxisvariable)], newdata[,as.numeric(input$yaxisvariable)], pch=as.numeric(input$pointshape), xlab = input$xlabel3, ylab = input$ylabel3, col=input$color3, main=input$title3, cex.axis=input$axissize3, cex.main=input$titlesize3, cex.lab=input$fontsize3)
    })
  })
  
  output$sumScatter <- renderPrint({
    input$goButton6
    isolate({
      inFile3 <- input$file3
      if (is.null(inFile3))
        return(invisible("Data Visulization"))
      data3=read.csv(inFile3$datapath, header=T)
      newdata3=data3[,-1]
      do.call(cbind, lapply(newdata3, summary))
    })
  })
  
  output$downloadPlot3 <- downloadHandler(
    filename = function() { paste('scatterplot', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width = as.numeric(input$plotwidth3), height = as.numeric(input$plotheight3), units = "px")
      # draw the plot
      plot(read.csv(input$file3$datapath, header=T)[,-1][,as.numeric(input$xaxisvariable)], read.csv(input$file3$datapath, header=T)[,-1][,as.numeric(input$yaxisvariable)], pch=as.numeric(input$pointshape), xlab = input$xlabel3, ylab = input$ylabel3, col=input$color3, main=input$title3, cex.axis=input$axissize3, cex.main=input$titlesize3, cex.lab=input$fontsize3)
      # turn the device off
      dev.off()
    }
  )
  
  #make a scatter plot for demo
  output$DemoScatterplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    data=read.csv("sample_data5.csv",header=T)
    newdata=data[,-1]
    opar = par(oma = c(0,0,0,0))
    plot(newdata[,1], newdata[,2], pch=1, xlab = "Gene expression in first selected sample", ylab = "Gene expression in second selected sample", col="black", main="Demo", cex.axis=1, cex.main=1, cex.lab=1)
  })
  
  #make a histogram
  output$histogram <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton7
    isolate({
      inFile4 <- input$file4
      
      if (is.null(inFile4))
        return(NULL)
      print(inFile4)
      data=read.csv(inFile4$datapath, header=T)
      newdata=data[,-1]
      print(newdata)
      for(tempcolumn in 1:ncol(newdata)){
        p<-hist(newdata[,tempcolumn], breaks=as.numeric(input$binnumber), plot=F)
        xlim = c(0, max(0, p$breaks))
        ylim = c(0, max(0, p$counts))
      }
      print(xlim)
      print(ylim)
      p1<-hist(newdata[,1], breaks=as.numeric(input$binnumber), plot=F)
      plot(p1, col=unlist(strsplit(input$color4,","))[1], xlab=input$xlabel4, ylab=input$ylabel4, main=input$title4, cex.axis=input$axissize4, cex.main=input$titlesize4, cex.lab=input$fontsize4, xlim=xlim, ylim=ylim)
      legend(x = input$legendpos, legend = rownames(t(newdata)), fill = unlist(strsplit(input$color4,",")), bty = "n", y.intersp = 2)
      for(column in 2:ncol(newdata)) {
        print("come here")
        ptemp<-hist(newdata[,column], breaks=as.numeric(input$binnumber), plot=F)
        plot(ptemp, col=unlist(strsplit(input$color4,","))[column], xlab=input$xlabel4, ylab=input$ylabel4, main=input$title4, cex.axis=input$axissize4, cex.main=input$titlesize4, cex.lab=input$fontsize4, add=T)
      }
    })
  })
  
  output$sumHistogram <- renderPrint({
    input$goButton8
    isolate({
      inFile4 <- input$file4
      if (is.null(inFile4))
        return(invisible("Data Visulization"))
      data4=read.csv(inFile4$datapath, header=T)
      newdata4=data4[,-1]
      do.call(cbind, lapply(newdata4, summary))
    })
  })
  
  output$downloadPlot4 <- downloadHandler(
    filename = function() { paste('histogram', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width = as.numeric(input$plotwidth4), height = as.numeric(input$plotheight4), units = "px")
      # draw the plot
      inFile4 <- input$file4
      data=read.csv(inFile4$datapath, header=T)
      newdata=data[,-1]
      for(tempcolumn in 1:ncol(newdata)){
        p<-hist(newdata[,tempcolumn], breaks=as.numeric(input$binnumber), plot=F)
        xlim = c(0, max(0, p$breaks))
        ylim = c(0, max(0, p$counts))
      }
      p1<-hist(newdata[,1], breaks=as.numeric(input$binnumber), plot=F)
      plot(p1, col=unlist(strsplit(input$color4,","))[1], xlab=input$xlabel4, ylab=input$ylabel4, main=input$title4, cex.axis=input$axissize4, cex.main=input$titlesize4, cex.lab=input$fontsize4, xlim=xlim, ylim=ylim)
      legend(x = input$legendpos, legend = rownames(t(newdata)), fill = unlist(strsplit(input$color4,",")), bty = "n", y.intersp = 2)
      for(column in 2:ncol(newdata)) {
        print("come here")
        ptemp<-hist(newdata[,column], breaks=as.numeric(input$binnumber), plot=F)
        plot(ptemp, col=unlist(strsplit(input$color4,","))[column], xlab=input$xlabel4, ylab=input$ylabel4, main=input$title4, cex.axis=input$axissize4, cex.main=input$titlesize4, cex.lab=input$fontsize4, add=T)
      }
      # turn the device off
      dev.off()
    }
  )
  
  #make a histogram for demo
  output$DemoHistogram <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    data=read.csv("sample_data5.csv",header=T)
    newdata=data[,-1]
    for(tempcolumn in 1:ncol(newdata)){
      p<-hist(newdata[,tempcolumn], breaks=5, plot=F)
      xlim = c(0, max(0, p$breaks))
      ylim = c(0, max(0, p$counts))
    }
    p1<-hist(newdata[,1], breaks=5, plot=F)
    plot(p1, col="dark gray", xlab="Gene expression", ylab="Cumulative count", main="Demo", cex.axis=1, cex.main=1, cex.lab=1, xlim=xlim, ylim=ylim)
    legend(x = "topright", legend = rownames(t(newdata)), fill = c("dark gray", "gray"), bty = "n", y.intersp = 2)
    for(column in 2:ncol(newdata)) {
      print("come here")
      ptemp<-hist(newdata[,column], breaks=5, plot=F)
      plot(ptemp, col="gray", xlab="Sample", ylab="Gene expression", main="Demo", cex.axis=1, cex.main=1, cex.lab=1, add=T)
    }
  })
  
  #make a line graph 
  output$linegraph <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton9
    isolate({
      inFile5 <- input$file5
      
      if (is.null(inFile5))
        return(NULL)
      print(inFile5)
      data=read.csv(inFile5$datapath, header=T)
      newdata=data[,-1]
      plot(newdata[,1], type="l", col=unlist(strsplit(input$color5,","))[1], xlab = input$xlabel5, ylab = input$ylabel5, main=input$title5, cex.axis=input$axissize5, cex.main=input$titlesize5, cex.lab=input$fontsize5, ylim=c(as.numeric(input$yaxislowerlimit2),as.numeric(input$yaxisupperlimit2)))
      for(column in 2:ncol(newdata)) {
        lines(newdata[column], col=unlist(strsplit(input$color5,","))[column], lwd=as.numeric(input$linethickness))
      }
      legend(x = input$legendposition, legend = rownames(t(newdata)), fill = unlist(strsplit(input$color5,",")), bty = "n", y.intersp = 2)
    })
  })
  
  output$sumLinegraph <- renderPrint({
    input$goButton10
    isolate({
      inFile5 <- input$file5
      if (is.null(inFile5))
        return(invisible("Data Visulization"))
      data5=read.csv(inFile5$datapath, header=T)
      newdata5=data5[,-1]
      do.call(cbind, lapply(newdata5, summary))
    })
  })
  
  output$downloadPlot5 <- downloadHandler(
    filename = function() { paste('Lineplot', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width = as.numeric(input$plotwidth5), height = as.numeric(input$plotheight5), units = "px")
      # draw the plot
      plot(read.csv(input$file5$datapath, header=T)[,-1][,1], type="l", col=unlist(strsplit(input$color5,","))[1], xlab = input$xlabel5, ylab = input$ylabel5, main=input$title5, cex.axis=input$axissize5, cex.main=input$titlesize5, cex.lab=input$fontsize5, ylim=c(as.numeric(input$yaxislowerlimit2),as.numeric(input$yaxisupperlimit2)))
      for(column in 2:ncol(read.csv(input$file5$datapath, header=T)[,-1])) {
        lines(read.csv(input$file5$datapath, header=T)[,-1][column], col=unlist(strsplit(input$color5,","))[column])
      }
      legend(x = input$legendposition, legend = rownames(t(read.csv(input$file5$datapath, header=T)[,-1])), fill = unlist(strsplit(input$color5,",")), bty = "n", y.intersp = 2)
      # turn the device off
      dev.off()
    }
  )
  
  #make a line graph for demo
  output$DemoLineGraph <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    data=read.csv("sample_data5.csv", header=T)
    newdata=data[,-1]
    plot(newdata[,1], type="l", col="black", xlab = "Sample", ylab = "Gene expression", main="Demo", cex.axis=1, cex.main=1, cex.lab=1, ylim=c(0, 12))
    for(column in 2:ncol(newdata)) {
      lines(newdata[column], col="gray", lwd=1)
    }
    legend(x = "topleft", legend = rownames(t(newdata)), fill = c("black", "gray"), bty = "n", y.intersp = 2)
  })
  
  #make a PCA plot
  output$PCAplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton11
    isolate({
      inFile6 <- input$file6
      
      if (is.null(inFile6))
        return(NULL)
      print(inFile6)
      a1=read.table(inFile6$datapath,sep=',',header=T,row.names=1,check=F,comment.char="")
      colnames(a1)<-sub("*_S\\d+_R1.trimmed.merged.genes.results","",colnames(a1))
      countdataraw=round(a1[rowSums(a1)>0,])
      countdata<-as.matrix(countdataraw)
      condition<-factor(c(unlist(strsplit(input$groups,","))))
      coldata <- data.frame(row.names=colnames(countdata), condition)
      coldata$condition<-as.factor(as.character(coldata$condition))
      dds <- DESeqDataSetFromMatrix(countData=countdata, colData=coldata, design=~condition)
      dds=DESeq(dds,minReplicatesForReplace = 50)
      rld=vst(dds, blind=TRUE)
      rv <- rowVars(assay(rld))
      select <- order(rv, decreasing = TRUE)[seq_len(min(length(rv),length(rv)))]
      dat.norm<-t(assay(rld)[select, ])
      dat.norm<-data.frame(dat.norm,condition)
      res.pca=PCA(dat.norm,ncp=5,scale.unit=T,graph=F,quali.sup=ncol(dat.norm))
      plot.PCA(res.pca,axes=c(as.numeric(input$xdim),as.numeric(input$ydim)),habillage=ncol(dat.norm),cex=1)
    })
  })
  
  output$sumPCAplot <- renderPrint({
    input$goButton12
    isolate({
      inFile6 <- input$file6
      if (is.null(inFile6))
        return(invisible("Data Visulization"))
      data6=read.csv(inFile6$datapath, header=T)
      newdata6=data6[,-1]
      do.call(cbind, lapply(newdata6, summary))
    })
  })
  
  output$downloadPlot6 <- downloadHandler(
    filename = function() { paste('PCAplot', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width = as.numeric(input$plotwidth6), height = as.numeric(input$plotheight6), units = "px")
      # draw the plot
      inFile6 <- input$file6
      a1=read.table(inFile6$datapath,sep=',',header=T,row.names=1,check=F,comment.char="")
      colnames(a1)<-sub("*_S\\d+_R1.trimmed.merged.genes.results","",colnames(a1))
      countdataraw=round(a1[rowSums(a1)>0,])
      countdata<-as.matrix(countdataraw)
      condition<-factor(c(unlist(strsplit(input$groups,","))))
      coldata <- data.frame(row.names=colnames(countdata), condition)
      coldata$condition<-as.factor(as.character(coldata$condition))
      dds <- DESeqDataSetFromMatrix(countData=countdata, colData=coldata, design=~condition)
      dds=DESeq(dds,minReplicatesForReplace = 50)
      rld=vst(dds, blind=TRUE)
      rv <- rowVars(assay(rld))
      select <- order(rv, decreasing = TRUE)[seq_len(min(length(rv),length(rv)))]
      dat.norm<-t(assay(rld)[select, ])
      dat.norm<-data.frame(dat.norm,condition)
      res.pca=PCA(dat.norm,ncp=5,scale.unit=T,graph=F,quali.sup=ncol(dat.norm))
      print(res.pca)
      plot.PCA(res.pca,axes=c(as.numeric(input$xdim),as.numeric(input$ydim)),habillage=ncol(dat.norm),cex=1)
      # turn the device off
      dev.off()
    }
  )
  
  #make a PCA plot for demo
  output$preImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('./images',
                                        paste('image', input$n, '.jpeg', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$n))
    
  }, deleteFile = FALSE)
  
  
  
  #make a Venn diagram 
  output$Venndiagram <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton13
    isolate({
      inFile7 <- input$file7
      if (is.null(inFile7))
      return(NULL)
      a1=read.table(inFile7$datapath,sep=',',header=T,row.names=1,check=F,comment.char="")
      venn.plot<-venn.diagram(
        tempx<-data.frame(a1),
        x<-as.list(tempx),
        category.names = unlist(strsplit(input$setname,",")),
        filename = NULL,
        output = TRUE ,
        imagetype="png" ,
        height = input$plotheight7, 
        width = input$plotwidth7, 
        col=unlist(strsplit(input$color6,",")),
        fill = unlist(strsplit(input$color6,",")),
        cex = input$fontsize6,
        cat.cex = input$setlabfontsize,
        cat.fontfamily = "sans", 
        cat.col = unlist(strsplit(input$setlabcol,",")), 
        main=input$title6, 
        main.cex=input$titlesize6
      )
      grid.draw(venn.plot)
    })
  })
  
  output$downloadPlot7 <- downloadHandler(
    filename = function() { paste('Venndiagram', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width = as.numeric(input$plotwidth7), height = as.numeric(input$plotheight7), units = "px")
      # draw the plot
      inFile7 <- input$file7
      a1=read.table(inFile7$datapath,sep=',',header=T,row.names=1,check=F,comment.char="")
      venn.plot<-venn.diagram(
        tempx<-data.frame(a1),
        x<-as.list(tempx),
        category.names = unlist(strsplit(input$setname,",")),
        filename = NULL,
        output = TRUE,
        imagetype="png",
        height = as.numeric(input$plotwidth7), 
        width = as.numeric(input$plotheight7), 
        col=unlist(strsplit(input$color6,",")),
        fill = unlist(strsplit(input$color6,",")),
        cex = input$fontsize6,
        cat.cex = input$setlabfontsize,
        cat.fontfamily = "sans", 
        cat.col = unlist(strsplit(input$setlabcol,",")), 
        main=input$title6, 
        main.cex=input$titlesize6
      )
      grid.draw(venn.plot)
      dev.off()
    }
  )
  
  output$VenndiagramInput <- renderTable(iris)
  
  #make a Venn diagram for demo
  output$DemoVenndiagram <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    a1=read.table("Sample_data_venndiagram_new.csv",sep=',',header=T,row.names=1,check=F,comment.char="")
    venn.plot2<-venn.diagram(
      tempx<-data.frame(a1),
      x<-as.list(tempx),
      category.names = c("Sample_1", "Sample_2"),
      filename = NULL,
      output = TRUE ,
      imagetype="png" ,
      height = 600, 
      width = 600, 
      col=c("pink", "orange"),
      fill = c("pink", "orange"),
      cex = 1,
      cat.cex = 1,
      cat.col = c("pink", "orange"), 
      main="Demo", 
      main.cex=1
    )
    grid.draw(venn.plot2)
  })
  
  #make a Heatmap 
  output$Heatmap <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton14
    isolate({
      inFile8 <- input$file8
      if (is.null(inFile8))
        return(NULL)
      a1=read.table(inFile8$datapath,sep=',',header=T,row.names=1,check=F,comment.char="")
      new_a1<-as.matrix(a1)
      heatmap(new_a1,col=colorRampPalette(c("blue","black","yellow"))(256),labRow=NULL,dendrogram='row',scale="row",cexRow =0.5,trace="none",density.info="none",keysize=.8,key.xlab="",margins = c(as.numeric(input$topbottommargin), as.numeric(input$leftrightmargin)),main=input$title7)
    })
  })
  
  output$sumHeatmap <- renderPrint({
    input$goButton15
    isolate({
      inFile8 <- input$file8
      if (is.null(inFile8))
        return(invisible("Data Visulization"))
      data7=read.csv(inFile8$datapath, header=T)
      newdata7=data7[,-1]
      do.call(cbind, lapply(newdata7, summary))
    })
  })
  
  output$downloadPlot8 <- downloadHandler(
    filename = function() { paste('Heatmap', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width = as.numeric(input$plotwidth8), height = as.numeric(input$plotheight8), units = "px")
      # draw the plot
      inFile8 <- input$file8
      a1=read.table(inFile8$datapath,sep=',',header=T,row.names=1,check=F,comment.char="")
      new_a1<-as.matrix(a1)
      plt <- heatmap.2(new_a1,col=colorRampPalette(c("blue","black","yellow"))(256),k_col=2,labRow=NULL,dendrogram='row',scale="row",cexRow =0.5,trace="none",density.info="none",keysize=.8,key.xlab="", margins = c(as.numeric(input$topbottommargin), as.numeric(input$leftrightmargin)), main=input$title7)
      dev.off()
    }
  )
  
  #make a volcano plot
  output$Volcanoplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton16
    isolate({
      inFile9 <- input$file9
      if (is.null(inFile9))
        return(NULL)
      a1=read.table(inFile9$datapath,sep=',',header=T,check=F,comment.char="")
      new_a1<-as.matrix(a1)
      
      # Make a basic volcano plot
      with(a1, plot(log2FoldChange, -log10(pvalue), pch=20, main=input$title8, xlim=c(-2.5,2)))
      
      # Add colored points: orange if log2FC<=-1, green of log2FC>=1, red if both)
      with(subset(a1, log2FoldChange<=-1), points(log2FoldChange, -log10(pvalue), pch=20, col=input$color7))
      with(subset(a1, log2FoldChange>=1), points(log2FoldChange, -log10(pvalue), pch=20, col=input$color8))
      with(subset(a1, padj<.05 & abs(log2FoldChange)>1), points(log2FoldChange, -log10(pvalue), pch=20, col=input$color9))
      
      # Label points with the textxy function from the calibrate plot
      with(subset(a1, padj<.05 & abs(log2FoldChange)>1), textxy(log2FoldChange, -log10(pvalue), labs=Gene, cex=.8))
    })
  })
  
  output$downloadPlot9 <- downloadHandler(
    filename = function() { paste('Volcano plot', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width=as.numeric(input$plotwidth9), height=as.numeric(input$plotwidth9), units="px")
      # draw the plot
      inFile9 <- input$file9
      a1=read.table(inFile9$datapath,sep=',',header=T,check=F,comment.char="")
      new_a1<-as.matrix(a1)
      with(a1, plot(log2FoldChange, -log10(pvalue), pch=20, main=input$title8, xlim=c(-2.5,2)))
      with(subset(a1, log2FoldChange<=-1), points(log2FoldChange, -log10(pvalue), pch=20, col=input$color7))
      with(subset(a1, log2FoldChange>=1), points(log2FoldChange, -log10(pvalue), pch=20, col=input$color8))
      with(subset(a1, padj<.05 & abs(log2FoldChange)>1), points(log2FoldChange, -log10(pvalue), pch=20, col=input$color9))
      
      # Label points with the textxy function from the calibrate plot
      with(subset(a1, padj<.05 & abs(log2FoldChange)>1), textxy(log2FoldChange, -log10(pvalue), labs=Gene, cex=.8))
      dev.off()
    }
  )
  
  #make a GO Enrichment table
  output$GoEnrichment <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton17
    isolate({
      if (input$genelist=="")
        return(invisible("Please enter gene names"))
      dbs <- c("GO_Molecular_Function_2015", "GO_Cellular_Component_2015", "GO_Biological_Process_2018" , "ChEA_2016" ,"KEGG_2016")
      enriched <- enrichr(unlist(strsplit(input$genelist,",")), dbs)
      bp <- enriched[["GO_Biological_Process_2018"]]
      bp <- bp[order(bp$P.value),]
      opar=par(1,1,mar=c(2,as.numeric(input$yaxisleftmargin),2,2))
      barplot( -log10(bp$P.value[15:1]), names.arg = bp$Term[15:1], las=1, main="GO terms ranked by -log10 of P-values", horiz = T)
    })
  })
  
  output$downloadPlot10 <- downloadHandler(
    filename = function() { paste('GO Enrichment plot', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width=as.numeric(input$plotwidth10), height=as.numeric(input$plotwidth10), units="px")
      # draw the plot
      dbs <- c("GO_Molecular_Function_2015", "GO_Cellular_Component_2015", "GO_Biological_Process_2018" , "ChEA_2016" ,"KEGG_2016")
      enriched <- enrichr(unlist(strsplit(input$genelist,",")), dbs)
      bp <- enriched[["GO_Biological_Process_2018"]]
      bp <- bp[order(bp$P.value),]
      opar=par(1,1,mar=c(2,as.numeric(input$yaxisleftmargin),2,2))
      barplot( -log10(bp$P.value[15:1]), names.arg = bp$Term[15:1], las=1, main="GO terms ranked by -log10 of P-values", horiz = T)
      # Label points with the textxy function from the calibrate plot
      dev.off()
    }
  )
  
  #make a violin plot
  output$Violinplot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    input$goButton18
    isolate({
      inFile10 <- input$file10
      if (is.null(inFile10))
        return(NULL)
      a1=read.table(inFile10$datapath,sep=',',header=T,check=F,comment.char="")
      ggplot(a1, aes(x=group, y=value, fill=group)) + # fill=name allow to automatically dedicate a color for each group
        geom_violin() +
        geom_jitter(shape=16, position=position_jitter(0.05)) +
        xlab(input$xaxis) +
        ylab(input$yaxis) +
        ggtitle(input$title9)+ 
        theme(plot.title = element_text(hjust = 0.5, size=input$titlesize7),axis.text=element_text(size=input$axissize6),axis.title=element_text(size=input$labelsize))
    })
  })
  
  output$downloadPlot11 <- downloadHandler(
    filename = function() { paste('Violin plot', ".png", sep='') },
    
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # open the png device
      png(filename=file, width=as.numeric(input$plotwidth11), height=as.numeric(input$plotwidth11), units="px")
      # draw the plot
      inFile10 <- input$file10
      a1=read.table(inFile10$datapath,sep=',',header=T,check=F,comment.char="")
      myplot <- ggplot(a1, aes(x=group, y=value, fill=group)) + # fill=name allow to automatically dedicate a color for each group
                geom_violin() +
                geom_jitter(shape=16, position=position_jitter(0.05)) +
                xlab(input$xaxis) +
                ylab(input$yaxis) +
                ggtitle(input$title9)+ 
                theme(plot.title = element_text(hjust = 0.5, size=input$titlesize7),axis.text=element_text(size=input$axissize6),axis.title=element_text(size=input$labelsize))
      print(myplot)
      dev.off()
    }
  )
}
shinyApp(ui, server)

