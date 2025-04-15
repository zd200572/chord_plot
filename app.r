library(shiny)
library(circlize)
library(RColorBrewer)

# UI 设计
ui <- fluidPage(
  titlePanel("连接弦图Chord生成器"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "上传 CSV 或 TXT 文件", accept = c(".csv", ".txt")),
      selectInput("format", "选择导出格式", choices = c("PDF", "PNG")),
      actionButton("plotBtn", "生成图像", icon = icon("play")),
      downloadButton("downloadPlot", "下载图像")
    ),
    mainPanel(
      tags$h4("预览图像"),
      plotOutput("chordPlot", height = "700px")
    )
  )
)

# Server 逻辑
server <- function(input, output, session) {
  
  dataInput <- reactive({
    req(input$datafile)
    df <- read.table(input$datafile$datapath, sep = ifelse(grepl(".csv$", input$datafile$name), ",", "\t"), header = TRUE, row.names = 1)
    as.matrix(df)
  })
  
  plotChord <- function(mat) {
    group_colors <- c(brewer.pal(8, "Set3"), brewer.pal(3, "Pastel1"))
    names(group_colors) <- c(rownames(mat), colnames(mat))
    circos.clear()
    chordDiagram(
      mat,
      grid.col = group_colors,
      transparency = 0.3,
      annotationTrack = "grid",
      preAllocateTracks = list(track.height = 0.05)
    )
    circos.trackPlotRegion(
      track.index = 1,
      panel.fun = function(x, y) {
        sector.name = get.cell.meta.data("sector.index")
        xlim = get.cell.meta.data("xlim")
        theta = mean(get.cell.meta.data("cell.start.degree") + get.cell.meta.data("cell.end.degree")) / 180 * pi
        facing = ifelse(cos(theta) > 0, "clockwise", "reverse.clockwise")
        circos.text(x = mean(xlim), y = 0.5, labels = sector.name,
                    facing = facing, niceFacing = TRUE, adj = c(0, 0.5),
                    cex = 0.6, col = "black")
      },
      bg.border = NA
    )
  }
  
  output$chordPlot <- renderPlot({
    req(input$plotBtn)
    mat <- dataInput()
    plotChord(mat)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("chord_diagram.", tolower(input$format))
    },
    content = function(file) {
      mat <- dataInput()
      if (input$format == "PDF") {
        pdf(file, width = 8, height = 8)
        plotChord(mat)
        dev.off()
      } else {
        png(file, width = 3000, height = 3000, res = 300)
        plotChord(mat)
        dev.off()
      }
    }
  )
}

# 启动 Shiny App
shinyApp(ui = ui, server = server)
