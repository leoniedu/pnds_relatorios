#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("R/entrevistador_pnds.R")

ui_upload <- sidebarLayout(
    sidebarPanel(
        fileInput(inputId = "ultimo_movimento", label = "CSV com Último Movimento", buttonLabel = "Upload..."),
        fileInput(inputId = "gerencial_compilado_1", label = "CSV com Gerencial Compilado 1", buttonLabel = "Upload...")
    ),
    mainPanel(
        #h3("Planilha com último movimento:")
        #, tableOutput("ultimo_movimento")
    )
)

ui_download <- fluidRow(
    column(width = 12, downloadButton("download", "Download do Relatório por Entrevistador", class = "btn-block"))
)



server <- function(input, output, session) {
    # Upload ---------------------------------------------------------
    gerencial_up <- reactive({
        req(input$gerencial_compilado_1)
        readr::read_csv2(input$gerencial_compilado_1$datapath, col_types='ccccccccccccccccccccc')
    })
    ultimo_movimento_up <- reactive({
        req(input$ultimo_movimento)
        readr::read_csv2(input$ultimo_movimento$datapath, col_types='cccccccccccc')
    })
    # Clean ----------------------------------------------------------
    tidied <- reactive({
        producao_entrevistador(ultimo_movimento=ultimo_movimento_up(), gerencial_compilado_1=gerencial_up())
    }
    )
    # Download -------------------------------------------------------
    output$download <- downloadHandler(
        filename = function() {
            paste0("relatorio_por_entrevistador.xlsx")
        },
        content = function(file) {
            openxlsx::write.xlsx(tidied(), file)
        }
    )
}
# Run the application
shinyApp(ui = fluidPage(#ui_upload_base,
    ui_upload, ui_download), server = server)
