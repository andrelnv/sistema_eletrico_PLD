#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$h3(
        "Dashboard: Análise do PLD",
        style = "font-weight:700; letter-spacing:.2px; color:white; margin:0;"
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Início",       tabName = "home",          icon = icon("house")),
      menuItem("Variáveis",    tabName = "variaveis",     icon = icon("layer-group")),
      menuItem("Modelagem",    tabName = "modelagem_pld", icon = icon("calculator")),
      menuItem("Análises PLD", tabName = "analises_pld",  icon = icon("chart-area"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ===================== HOME =====================
      tabItem(
        tabName = "home",
        fluidRow(
          box(width = 4, status = "primary", solidHeader = TRUE, title = "Seleção",
              selectInput("home_ano", "Ano:", choices = NULL, width = "100%"),
              selectInput("home_reg", "Subsistema:",
                          choices = c("Sudeste/Centro-Oeste" = "SE",
                                      "Sul"                   = "S",
                                      "Nordeste"              = "NE",
                                      "Norte"                 = "N"),
                          selected = "SE", width = "100%")
          ),
          valueBoxOutput("vb_data", width = 4),
          valueBoxOutput("vb_pld",  width = 4)
        ),
        fluidRow(
          infoBoxOutput("ib_media",   width = 4),
          infoBoxOutput("ib_mediana", width = 4),
          infoBoxOutput("ib_sd",      width = 4)
        ),
        fluidRow(
          infoBoxOutput("ib_cv",      width = 4),
          infoBoxOutput("ib_minmax",  width = 4),
          infoBoxOutput("ib_n",       width = 4)
        ),
        fluidRow(
          box(width = 12, status = "warning", solidHeader = TRUE,
              title = "PLD ao longo do tempo",
              plotlyOutput("home_pld", height = 420)
          )
        )
      ),
      
      # ===================== VARIÁVEIS =====================
      tabItem(
        tabName = "variaveis",
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE, title = "Filtros",
              selectInput("var_ano", "Ano:", choices = NULL, width = "100%"),
              selectInput("var_regiao", "Região:",
                          choices = c("Todos",
                                      "Sudeste/Centro-Oeste" = "SE",
                                      "Sul"                   = "S",
                                      "Nordeste"              = "NE",
                                      "Norte"                 = "N"),
                          selected = "Todos", width = "100%")
          )
        ),
        fluidRow(
          box(title = "Custo Marginal de Operação (CMO)", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("graf_var_cmo", height = 360)),
          box(title = "Energia Armazenada (EAR)", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("graf_var_ear", height = 360))
        ),
        fluidRow(
          box(title = "Energia Natural Afluente (ENA)", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("graf_var_ena", height = 360)),
          box(title = "Vazão Turbinada – Usina Hidrelétrica Itaipu", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("graf_var_itaipu", height = 360))
        ),
        fluidRow(
          box(title = "Demanda de Energia", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("graf_var_demanda", height = 360)),
          box(title = "Carga", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("graf_var_carga", height = 360))
        ),
        fluidRow(
          box(title = "Geração Usinas Hidrelétricas", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("graf_var_hid", height = 360)),
          box(title = "Geração Usinas Termelétricas", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("graf_var_term", height = 360))
        ),
        fluidRow(
          box(title = "Geração Usinas Eólicas", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("graf_var_eol", height = 360)),
          box(title = "Geração Usinas Solares", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("graf_var_sol", height = 360))
        ),
        fluidRow(
          box(title = "Matriz de Geração por Fonte",
              status = "warning", solidHeader = TRUE, width = 12,
              plotlyOutput("graf_var_mix", height = 380))
        ),
        fluidRow(
          box(title = "Preço de Liquidação das Diferenças (PLD)", status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("graf_var_pld", height = 380))
        )
      ),
      
      
      # ================ MODELAGEM DO PLD ================
      tabItem(tabName = "modelagem_pld",
              fluidRow(
                infoBoxOutput("ib_r2",    width = 3),
                infoBoxOutput("ib_rse",  width = 3),
                infoBoxOutput("ib_rmse",  width = 3),
                infoBoxOutput("ib_fstat", width = 3)
              ),
              fluidRow(
                box(title = "Coeficientes do Modelo de Regressão Linear", status = "primary", solidHeader = TRUE, width = 6,
                    DTOutput("tabela_coef")),
                box(title = "Importância das Variáveis (Betas padronizados)",status = "warning", solidHeader = TRUE, width = 6,
                  plotOutput("plot_importancia", height = 360)
                )
              ),
              fluidRow(
                box(width = 6, title = "Dispersão: PLD Real vs. Previsto",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    plotOutput("graf_scatter_real_prev", height = 350)),
                box(width = 6, title = "Evolução Temporal: PLD Real vs. Previsto",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    plotOutput("graf_ts_real_prev", height = 350))
              ),
              fluidRow(
                box(title = "Resumo do Modelo (R)", status = "info", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("resumo_modelo"))
              ),
              fluidRow(
                box(title = "Testes Estatísticos", status = "danger", solidHeader = TRUE, width = 12,
                    DTOutput("tabela_testes"))
              )
      ),
      
 # ================= ANÁLISES DO PLD ===============
      tabItem(
        tabName = "analises_pld",
        
        fluidRow(
          box(
            title = tagList("Médias Móveis"),
            status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE,
            plotOutput("graf_mm", height = 380)
          ),
          box(
            title = "Volatilidade (Desvio-padrão e Coeficiente de Variação)",
            status = "warning", solidHeader = TRUE, width = 6, collapsible = TRUE,
            plotOutput("graf_vol", height = 380)
          )
        ),
        
        fluidRow(
          box(
            title = "Histograma com Densidade",
            status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE,
            plotOutput("graf_hist", height = 380)
          ),
          box(
            title = "Boxplot por Ano",
            status = "warning", solidHeader = TRUE, width = 6, collapsible = TRUE,
            plotOutput("graf_box", height = 380)
          )
        ),
        
         fluidRow(
          box(title = "Outliers pelo Método de Amplitude Interquartil (AIQ)",
              status = "danger", solidHeader = TRUE, width = 6, collapsible = TRUE,
              plotOutput("graf_out", height = 420)
          )
        ),
        fluidRow(
          box(title = "Resumo AIQ",
              status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
              div(style = "margin-top:6px;", DTOutput("tabela_aiq"))
          )
        )
      )
  )
))