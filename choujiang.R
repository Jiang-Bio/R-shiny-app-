library(shiny)
library(wordcloud)
library(RColorBrewer)
library(later)

draw_lottery <- function(name_vec, n) {
  sample(name_vec, n, replace = FALSE)
}

ui <- fluidPage(
  
  titlePanel(
    h1("🎊 2026 年终抽奖 🎊", align = "center")
  ),
  
  fluidRow(
    column(
      3,
      wellPanel(
        h4("🎁 抽奖设置"),
        numericInput("userInput", "中奖人数", value = 1, min = 1),
        actionButton(
          "drawButton",
          "🎉 开始抽奖",
          class = "btn-danger btn-lg btn-block"
        )
      )
    ),
    
    column(
      9,
      wellPanel(
        h4("🏆 抽奖展示"),
        plotOutput("resultPlot", height = "600px"),
        br(),
        uiOutput("winnerText")   # 👈 新增
      )
    )
  ),
  
  tags$hr()
)

server <- function(input, output, session) {
  
  winners <- reactiveVal(character())
  stage   <- reactiveVal("idle")  # idle | rolling | result
  
  observeEvent(input$drawButton, {
    
    n <- input$userInput
    if (n <= 0) return(NULL)
    
    name <- read.table(
      "C:/Users/JIANGDK/Desktop/id1.txt",
      header = TRUE
    )$name
    
    if (n > length(name)) {
      showNotification("中奖人数超过剩余人数", type = "error")
      return(NULL)
    }
    
    stage("rolling")
    
    ## 2 秒后开奖
    later(function() {
      
      win <- draw_lottery(name, n)
      winners(win)
      
      ## 更新名单
      new_id <- setdiff(name, win)
      write.table(
        data.frame(name = new_id),
        "C:/Users/JIANGDK/Desktop/id1.txt",
        row.names = FALSE,
        quote = FALSE
      )
      
      write.table(
        win,
        "C:/Users/JIANGDK/Desktop/id1.log.txt",
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE
      )
      
      stage("result")
      
    }, delay = 2)
  })

  output$resultPlot <- renderPlot({
    
    if (stage() == "rolling") {
      
      invalidateLater(100, session)
      
      name <- read.table(
        "C:/Users/JIANGDK/Desktop/id1.txt",
        header = TRUE
      )$name
      
      show_names <- sample(name, min(30, length(name)))
      
      wordcloud(
        words = show_names,
        freq = rep(5, length(show_names)),
        colors = brewer.pal(8, "Dark2"),
        rot.per = 0
      )
      
    } else if (stage() == "result") {
      
      req(winners())
      
      wordcloud(
        words = winners(),
        freq = rep(10, length(winners())),
        colors = brewer.pal(8, "Set1"),
        rot.per = 0
      )
    }
  })
  
  ## 👇 中奖名单文本展示（下方）
  output$winnerText <- renderUI({
    
    req(stage() == "result", winners())
    
    tagList(
      h3("🎉 恭喜以下中奖者 🎉", align = "center"),
      div(
        style = "
          font-size: 28px;
          font-weight: bold;
          text-align: center;
          color: #D9534F;
          line-height: 1.8;
        ",
        paste(winners(), collapse = " 、 ")
      )
    )
  })
}

shinyApp(ui, server)
