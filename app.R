library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
library(png)
library(EBImage)
library(lattice)


ksource <- function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)
}
ksource("01_Data.Rmd")


ui <- fluidPage(
  titlePanel("Sky Survey Data Clustering"),
  navbarPage(  
    h2("🌠", style = "font-family: 'Lobster', cursive;
       font-weight: 500; line-height: 0.7; color: #505050;"),
#-----------------------------------------About--------------------------------------
    navbarMenu(
      h4("About", style = "font-family: 'Lobster', cursive;
         font-weight: 500; line-height: 1.1; color: #505050;"),
      tabPanel(
        h5("The project", style = "font-family: 'Didot', cursive;
           font-weight: 500; line-height: 0.5; color: #505050;"),
        includeMarkdown("11_Introduction.md")
        ),
      tabPanel(
        h5(
          "Scisever",
          style = "font-family: 'Didot', cursive;
          font-weight: 500; line-height: 0.5; color: #505050;"),
        includeMarkdown("12_Scisever.md")
      ),
      tabPanel(
        h5(
          "References",
          style = "font-family: 'Didot', cursive;
          font-weight: 500; line-height: 0.5; color: #505050;"),
        includeMarkdown("13_Ref.md")
      )
      ),
#-------------------------------------------Clustering---------------------------------------
    navbarMenu(
      h4("Clustering",style = "font-family: 'Lobster', cursive;
         font-weight: 500; line-height: 1.1;color: #505050;"),
      tabPanel(
        h5("Datatable",style = "font-family: 'Didot', cursive;
           font-weight: 500; line-height: 0.7; color: #505050;"),
        DT::dataTableOutput("fulltable")
        ),
#-----------------------------------------Hierarchical clustering--------------------------------------
      tabPanel(
        h5("Hierarchical clustering",style = "font-family: 'Didot', cursive;
           font-weight: 500; line-height: 0.7; color: #505050;"), 
        sidebarPanel(
          numericInput("clst", "Number of clusters (2-15) :", 5, min = 2, max = 15),
          numericInput("n_clst", "Display cluster:", 5, min = 1, max = 5)
        ),
        mainPanel(
          # UI output
          #lapply(1:2, function(i) {
          #  imageOutput(paste0('b', i))
          #}),
          column(3, div(style = "height:10px;"), imageOutput(paste0('b', 1))),
          column(3, div(style = "height:10px;"), imageOutput(paste0('b', 2))),
          column(3, div(style = "height:10px;"), imageOutput(paste0('b', 3))),
          column(3, div(style = "height:10px;"), imageOutput(paste0('b', 4))),
          column(3, actionButton("refresh", "New images !"))
        )
        ),
#-----------------------------------------K-means--------------------------------------
      tabPanel(
        h5("K-means cluster",style = "font-family: 'Didot', cursive;
           font-weight: 500; line-height: 0.7; color: #505050;"),
        sidebarPanel(
          numericInput("n_clst_", "Number of clusters (1-15) :", 5, min = 1, max = 15)
        ),
        mainPanel(
          column(3, div(style = "height:10px;"), imageOutput(paste0('c', 1))),
          column(3, div(style = "height:10px;"), imageOutput(paste0('c', 2))),
          column(3, div(style = "height:10px;"), imageOutput(paste0('c', 3))),
          column(3, div(style = "height:10px;"), imageOutput(paste0('c', 4))),
          column(3, actionButton("refresh_", "New images !"))
        )
        ),
#-----------------------------------------Dbscan--------------------------------------
      tabPanel(
        h5("Dbscan",style = "font-family: 'Didot', cursive;
           font-weight: 500; line-height: 0.7; color: #505050;"),
        sidebarPanel(
          numericInput("n_clst", "Number of clusters (1-15) :", 5, min = 1, max = 15)
        )
        )
      ),
#-----------------------------------------Image analysis--------------------------------------
    navbarMenu(
      h4("Image analysis",style = "font-family: 'Lobster', cursive;
         font-weight: 500; line-height: 1.1; color: #505050;"),
      
      tabPanel(
        h5("Image characterization", style = "font-family: 'Didot', cursive;
           font-weight: 500; line-height: 0.6; color: #505050;"),
        sidebarPanel(
          textInput("ra", "Input ra value of galaxy", "154.439916732445"),
          textInput("dec", "Input dec value of galaxy", "1.9460713607363"),
          sliderInput("offset", "Number of observations:",
                      min = 0, max = 0.05, value = 0.01, step = 0.005
          ),
          h4("Image selected:"),
          imageOutput("imganalysis")
        ),
        mainPanel(
          column(10, selectInput("display", "Display:",
                                 c("Histogram of color" = "s_hist",
                                   "Image thresholding" = "s_bnw",
                                   "Objects datatable" = "s_table"))),
          
          conditionalPanel(
            condition = "input.display == 's_bnw'",
            column(10, imageOutput("bnw"))
            ),
          conditionalPanel(
            condition = "input.display == 's_hist'",
            column(10, plotOutput("hist_pic_on_side"))
          ),
            conditionalPanel(
              condition = "input.display == 's_table'",
              column(10, DT::dataTableOutput("nobj"))
              )
        )
      ))
    ))

#-----------
server <- function(input, output) {
  
  output$fulltable <- DT::renderDataTable({
    fulldata
  })
  
  possible_rows <- reactive({
    dt <- dt_h %>% filter(Cluster == input$n_clst)
    1:nrow(dt)
  })
  
  dynamic_seed <- reactive({
    input$refresh
    sample(1:10000, 1)
  })
  
  lapply(1:4, function(i) {
    output[[paste0('b', i)]] <- renderImage({
      outfile <- tempfile(fileext = ".png")
      dt <- dt_h %>% filter(Cluster == input$n_clst)
      set.seed(dynamic_seed()+i)
      sel_row <- sample(possible_rows(), 1)
      pic = SkyServer.getJpegImgCutout(ra = dt[sel_row, "ra"], dec=dt[sel_row, "dec"], 
                                       width=512, height=512, scale=0.05, 
                                       dataRelease=SkyServer_DataRelease,
                                       query="SELECT TOP 100 p.objID, p.ra, p.dec, p.r FROM fGetObjFromRectEq(197.6,18.4,197.7,18.5) n, PhotoPrimary p WHERE n.objID=p.objID")
      writePNG(pic, target = outfile)
      # Return a list containing information about the image
      list(src = outfile,
           contentType = "image/png",
           width = 150,
           height = 150,
           alt = "This is alternate text")
    }, deleteFile = TRUE)
  })
  
  possible_rows_ <- reactive({
    dt <- mydata_ %>% filter(fit.cluster == input$n_clst_)
    1:nrow(dt)
  })

  dynamic_seed_ <- reactive({
    input$refresh_
    sample(1:10000, 1)
  })

  lapply(1:4, function(j) {
    output[[paste0('c', j)]] <- renderImage({
      outfile <- tempfile(fileext = ".png")
      dt <- mydata_ %>% filter(fit.cluster == input$n_clst_)
      set.seed(dynamic_seed_()+j)
      sel_row <- sample(possible_rows_(), 1)
      pic = SkyServer.getJpegImgCutout(ra = dt[sel_row, "ra"], dec=dt[sel_row, "dec"],
                                       width=512, height=512, scale=0.05,
                                       dataRelease=SkyServer_DataRelease,
                                       query="SELECT TOP 100 p.objID, p.ra, p.dec, p.r FROM fGetObjFromRectEq(197.6,18.4,197.7,18.5) n, PhotoPrimary p WHERE n.objID=p.objID")
      writePNG(pic, target = outfile)
      # Return a list containing information about the image
      list(src = outfile,
           contentType = "image/png",
           width = 150,
           height = 150,
           alt = "This is alternate text")
    }, deleteFile = TRUE)
  })
  
  pic_on_side <- reactive({
    dt <- dt_h %>% filter(Cluster == input$n_clst)
    set.seed(sample(1:10000, 1))
    sel_row <- sample(possible_rows(), 1)
    SkyServer.getJpegImgCutout(ra = input$ra, dec=input$dec, 
                                     width=512, height=512, scale=0.05, 
                                     dataRelease=SkyServer_DataRelease,
                                     query="SELECT TOP 100 p.objID, p.ra, p.dec, p.r FROM fGetObjFromRectEq(197.6,18.4,197.7,18.5) n, PhotoPrimary p WHERE n.objID=p.objID")
  })

  output$imganalysis <- renderImage({
    outfile <- tempfile(fileext = ".png")
    writePNG(pic_on_side(), target = outfile)
    # Return a list containing information about the image
    list(src = outfile,
         contentType = "image/png",
         width = 150,
         height = 150,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$hist_pic_on_side <- renderPlot({
    hist(pic_on_side())
  })
  
  output$bnw <- renderPlot({
    #browser()
    img_gry <- 0.33*pic_on_side()[,,1]+0.33*pic_on_side()[,,2]+0.33*pic_on_side()[,,3] #0.21 0.71 0.07
    disc = makeBrush(31, "disc")
    disc = disc / sum(disc)
    offset = input$offset
    im_bg = filter2(img_gry, disc)
    im_th = img_gry > im_bg + offset
    im_th <- mapply(im_th, FUN=as.numeric)
    #heatmap(matrix(im_th,512,512),Rowv=NA,Colv=NA,col=paste("gray",1:99,sep=""))
    rotate <- function(x) t(apply(x, 2, rev))
    levelplot(rotate(matrix(im_th,512,512)), col.regions = gray(0:100/100))
  })
  
  output$nobj <- DT::renderDataTable({
    img_gry <- 0.21*pic_on_side()[,,1]+0.71*pic_on_side()[,,2]+0.07*pic_on_side()[,,3]
    disc = makeBrush(31, "disc")
    disc = disc / sum(disc)
    offset = input$offset
    im_bg = filter2(img_gry, disc)
    im_th = img_gry > im_bg + offset
    x = bwlabel(im_th)
    table(x)
    delete_these <- names(table(x))[table(x) < 10 ] %>% as.numeric()
    #computeFeatures.shape(x)
    tb <- computeFeatures.shape(rmObjects(x, delete_these))
    datatable(tb, options = list(
      lengthMenu = c(5, 10, 15, 20)
    ))
    
  })

}

# Run the application
shinyApp(ui = ui, server = server)


#https://shiny.rstudio.com/gallery/actionbutton-demo.html
#https://shiny.rstudio.com/articles/dynamic-ui.html