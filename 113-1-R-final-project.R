library(shiny)
library(tidyverse)

# 假設已經載入 taxi 資料
taxi <- read_csv("taxi.csv")

# 定義 UI
ui <- fluidPage(
  titlePanel("計程車業統計展示"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("chart_type", "選擇圖表類型：",
                  choices = c("折線圖", "柱狀圖")),
      selectInput("data_type", "選擇資料類型：",
                  choices = c("家數", "車輛數"))
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

# 定義伺服器邏輯
server <- function(input, output) {
  
  # 根據使用者選擇的圖表類型繪製圖表
  output$plot <- renderPlot({
    # 根據選擇的資料類型來選擇顯示的數據
    if (input$data_type == "家數") {
      data_to_plot <- taxi %>%
        select(年度, `車行計程車客運業-家數`, `計程車運輸合作社-家數`, `個人計程車業-家數`)
    } else if (input$data_type == "車輛數") {
      data_to_plot <- taxi %>%
        select(年度, `車行計程車客運業-車輛數`, `計程車運輸合作社-車輛數`, `個人計程車業-車輛數`)
    }
    
    # 繪製圖表
    if (input$chart_type == "折線圖") {
      data_to_plot %>%
        pivot_longer(cols = -年度, names_to = "業別", values_to = "數量") %>%
        ggplot(aes(x = 年度, y = 數量, color = 業別)) +
        geom_line() +
        labs(title = paste(input$data_type, "折線圖"), x = "年度", y = input$data_type) +
        theme_minimal()
    } else if (input$chart_type == "柱狀圖") {
      data_to_plot %>%
        pivot_longer(cols = -年度, names_to = "業別", values_to = "數量") %>%
        ggplot(aes(x = 年度, y = 數量, fill = 業別)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste(input$data_type, "柱狀圖"), x = "年度", y = input$data_type) +
        theme_minimal()
    }
  })
}

# 啟動 Shiny 應用程式
shinyApp(ui = ui, server = server)

