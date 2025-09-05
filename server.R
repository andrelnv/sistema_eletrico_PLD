#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

server <- function(input, output, session) {
  
  # --------- Utilitários comuns ---------
  add_filtros <- function(df, col_data, col_regiao) {
    anos <- tryCatch(sort(unique(format(as.Date(df[[col_data]]), "%Y"))), error = function(e) NULL)
    regioes <- tryCatch(sort(unique(df[[col_regiao]])), error = function(e) NULL)
    list(anos = c("Todos", anos), regioes = c("Todos", regioes))
  }
  safe_date <- function(x) as.Date(x)
  
  # Paleta/labels da HOME
  home_cols   <- c(SE="#2E86C1", S="#27AE60", NE="#E67E22", N="#8E44AD")
  home_labels <- c(SE="Sudeste/Centro-Oeste", S="Sul", NE="Nordeste", N="Norte")
  scale_regioes <- function() {
    scale_color_manual(
      values = home_cols,
      breaks = names(home_labels),
      labels = unname(home_labels),
      drop   = FALSE
    )
  }
  fmt_num <- function(x, accuracy = NULL) {
    x <- as.numeric(x) # garante que não vem string
    if (is.null(accuracy)) {
      scales::number(x, big.mark = ".", decimal.mark = ",")
    } else {
      scales::number(x, accuracy = accuracy, big.mark = ".", decimal.mark = ",")
    }
  }
  
  fmt_pct <- function(x) {
    x <- as.numeric(x)
    scales::percent(x / 100, big.mark = ".", decimal.mark = ",", accuracy = 0.1)
  }
  
  
  # HOME 
  observe({
    validate(need(exists("pld_vf"), "Carregue 'pld_vf'."))
    anos <- pld_vf %>%
      mutate(d = as.Date(din_instante)) %>%
      filter(!is.na(d)) %>%
      transmute(ano = year(d)) %>%
      distinct() %>%
      arrange(desc(ano)) %>%
      pull(ano)
    updateSelectInput(session, "home_ano", choices = c("Todos", anos), selected = "Todos")
  })
  
  #  Data mais recente 
  output$vb_data <- renderValueBox({
    validate(need(exists("pld_vf"), "Carregue 'pld_vf'."))
    d <- suppressWarnings(as.Date(pld_vf$din_instante))
    if (all(is.na(d))) d <- suppressWarnings(lubridate::ymd(pld_vf$din_instante))
    ult <- max(d, na.rm = TRUE)
    valueBox(
      value    = format(ult, "%d/%m/%Y"),
      subtitle = "Data mais recente",
      icon     = icon("calendar"),
      color    = "blue"
    )
  })
  
  # PLD na data mais recente  
  output$vb_pld <- renderValueBox({
    validate(need(exists("pld_vf"), "Carregue 'pld_vf'."))
    reg <- req(input$home_reg)
    
    df_reg <- pld_vf %>%
      dplyr::filter(id_subsistema == reg) %>%
      dplyr::mutate(
        data = suppressWarnings(as.Date(din_instante)),
        pld  = suppressWarnings(as.numeric(pld))
      )
    if (all(is.na(df_reg$data))) {
      df_reg <- df_reg %>% dplyr::mutate(data = suppressWarnings(lubridate::ymd(din_instante)))
    }
    df_reg <- df_reg %>% dplyr::filter(!is.na(data), !is.na(pld))
    validate(need(nrow(df_reg) > 0, "Sem dados."))
    
    ult <- max(df_reg$data, na.rm = TRUE)
    val <- df_reg %>% dplyr::filter(data == ult) %>%
      dplyr::summarise(v = mean(pld, na.rm = TRUE)) %>% dplyr::pull(v)
    
    valueBox(
      value    = paste0(scales::number(val, big.mark=".", decimal.mark=",", accuracy = .01), " R$/MWh"),
      subtitle = paste0("PLD na última data (", format(ult, "%d/%m/%Y"), ")"),
      icon     = icon("dollar-sign"),
      color    = "green"
    )
  })
  
  # Estatísticas do PLD (ano/região) para infoBoxes
  contexto_filtro <- reactive({
    reg <- req(input$home_reg)
    ano <- input$home_ano
    paste0(home_labels[[reg]], " • ",
           if (is.null(ano) || ano == "Todos") "Série completa" else paste0("Ano: ", ano))
  })
  
  home_pld_stats <- reactive({
    validate(need(exists("pld_vf"), "Carregue 'pld_vf'."))
    reg <- req(input$home_reg)
    
    df <- pld_vf %>%
      dplyr::filter(id_subsistema == reg) %>%
      dplyr::mutate(data = as.Date(din_instante),
                    pld  = suppressWarnings(as.numeric(pld))) %>%
      dplyr::filter(!is.na(data), !is.na(pld))
    
    if (!is.null(input$home_ano) && input$home_ano != "Todos") {
      df <- df %>% dplyr::filter(lubridate::year(data) == input$home_ano)
    }
    
    if (nrow(df) == 0) {
      return(tibble::tibble(
        n = 0, media = NA_real_, mediana = NA_real_, sd = NA_real_,
        cv = NA_real_, minimo = NA_real_, maximo = NA_real_,
        d_min = as.Date(NA), d_max = as.Date(NA)
      ))
    }
    
    i_min <- which.min(df$pld)
    i_max <- which.max(df$pld)
    media   <- mean(df$pld, na.rm = TRUE)
    sd_val  <- sd(df$pld, na.rm = TRUE)
    cv_val  <- if (isTRUE(media > 0)) (sd_val / media) * 100 else NA_real_
    
    tibble::tibble(
      n       = nrow(df),
      media   = media,
      mediana = median(df$pld, na.rm = TRUE),
      sd      = sd_val,
      cv      = cv_val,
      minimo  = df$pld[i_min],
      maximo  = df$pld[i_max],
      d_min   = df$data[i_min],
      d_max   = df$data[i_max]
    )
  })
  
  output$ib_media <- renderInfoBox({
    s <- home_pld_stats()
    infoBox("MÉDIA (R$/MWh)", ifelse(is.na(s$media), "—", fmt_num(s$media)),
            contexto_filtro(), icon = icon("chart-line"),
            color = "light-blue", fill = TRUE)
  })
  output$ib_mediana <- renderInfoBox({
    s <- home_pld_stats()
    infoBox("MEDIANA (R$/MWh)", ifelse(is.na(s$mediana), "—", fmt_num(s$mediana)),
            contexto_filtro(), icon = icon("ruler-horizontal"),
            color = "aqua", fill = TRUE)
  })
  output$ib_sd <- renderInfoBox({
    s <- home_pld_stats()
    cor <- if (is.na(s$sd)) "purple" else if (s$sd < 50) "green" else if (s$sd < 150) "yellow" else "red"
    infoBox("DESVIO-PADRÃO (R$/MWh)", ifelse(is.na(s$sd), "—", fmt_num(s$sd)),
            contexto_filtro(), icon = icon("wave-square"),
            color = cor, fill = TRUE)
  })
  output$ib_cv <- renderInfoBox({
    s <- home_pld_stats()
    cor <- if (is.na(s$cv)) "purple" else if (s$cv < 20) "green" else if (s$cv < 50) "yellow" else "red"
    infoBox("CV (%)", ifelse(is.na(s$cv), "—", fmt_pct(s$cv)),
            "Volatilidade relativa", icon = icon("percent"),
            color = cor, fill = TRUE)
  })
  output$ib_minmax <- renderInfoBox({
    s <- home_pld_stats()
    val <- if (is.na(s$minimo) || is.na(s$maximo)) "—"
    else paste0(fmt_num(s$minimo), " – ", fmt_num(s$maximo), " R$/MWh")
    sub <- if (is.na(s$d_min) || is.na(s$d_max)) contexto_filtro()
    else paste0("Mín em ", format(s$d_min, "%d/%m/%Y"),
                " • Máx em ", format(s$d_max, "%d/%m/%Y"))
    infoBox("FAIXA (Mín–Máx)", val, sub, icon = icon("arrows-left-right"),
            color = "teal", fill = TRUE)
  })
  output$ib_n <- renderInfoBox({
    s <- home_pld_stats()
    infoBox("Amostragem (n)", ifelse(is.na(s$n) || s$n == 0, "—", format(s$n, big.mark=".")),
            contexto_filtro(), icon = icon("list-ol"),
            color = "olive", fill = TRUE)
  })
  
  # Gráfico do PLD (HOME; cor por região)
  output$home_pld <- renderPlotly({
    validate(need(exists("pld_vf"), "Carregue 'pld_vf'."))
    reg <- req(input$home_reg)
    df <- pld_vf %>%
      filter(id_subsistema == reg) %>%
      mutate(data = safe_date(din_instante),
             pld  = suppressWarnings(as.numeric(pld))) %>%
      filter(!is.na(data), !is.na(pld))
    
    if (!is.null(input$home_ano) && input$home_ano != "Todos") {
      df <- df %>% filter(year(data) == input$home_ano)
    }
    
    g <- ggplot(df, aes(x = data, y = pld)) +
      geom_line(color = home_cols[[reg]], linewidth = 1) +
      labs(x = "Tempo", y = "PLD [R$/MWh]",
           title = paste0("PLD – ", home_labels[[reg]],
                          if (input$home_ano != "Todos") paste0(" (", input$home_ano, ")")
                          else " (Série completa)")) +
      theme_minimal()
    
    ggplotly(g) %>% layout(hovermode = "x unified")
  })
  
  # ANÁLISES DO PLD 
  base_pld_mensal <- reactive({
    validate(need(exists("pld_vf"), "Carregue 'pld_vf'."))
    pld_vf %>%
      filter(id_subsistema == "SE") %>%
      mutate(
        din_instante = as.Date(din_instante),
        pld = as.numeric(pld)
      ) %>%
      arrange(din_instante) %>%
      filter(!is.na(din_instante), !is.na(pld)) %>%
      mutate(mes = floor_date(din_instante, "month")) %>%
      group_by(mes) %>%
      summarise(pld = mean(pld, na.rm = TRUE), .groups = "drop") %>%
      arrange(mes)
  })
  
  output$graf_mm <- renderPlot({
    pld_mensal <- base_pld_mensal()
    pld_ma <- pld_mensal %>%
      mutate(
        mm_3  = rollapply(pld, 3,  mean, align = "right", fill = NA, na.rm = TRUE),
        mm_6  = rollapply(pld, 6,  mean, align = "right", fill = NA, na.rm = TRUE),
        mm_12 = rollapply(pld, 12, mean, align = "right", fill = NA, na.rm = TRUE)
      )
    ggplot(pld_ma, aes(x = mes)) +
      geom_line(aes(y = pld,  color = "PLD (média mensal)"), linewidth = .7, alpha = .7) +
      geom_line(aes(y = mm_3,  color = "MM 3 meses"),  linewidth = 1) +
      geom_line(aes(y = mm_6,  color = "MM 6 meses"),  linewidth = 1) +
      geom_line(aes(y = mm_12, color = "MM 12 meses"), linewidth = 1) +
      scale_color_manual(NULL, values = c(
        "PLD (média mensal)" = "#7F8C8D",
        "MM 3 meses"         = "#2E86C1",
        "MM 6 meses"         = "#27AE60",
        "MM 12 meses"        = "#E67E22"
      )) +
      labs(title = "PLD com Médias Móveis (3, 6 e 12 meses)", x = "Ano", y = "PLD [R$/MWh]") +
      theme_minimal() + theme(legend.position = "bottom")
  })
  
  output$graf_vol <- renderPlot({
    pld_mensal <- base_pld_mensal()
    janela_vol <- 12
    pld_vol <- pld_mensal %>%
      mutate(
        sd_mov   = rollapply(pld, janela_vol, sd,   align = "right", fill = NA, na.rm = TRUE),
        mean_mov = rollapply(pld, janela_vol, mean, align = "right", fill = NA, na.rm = TRUE),
        cv_mov   = if_else(mean_mov > 0, sd_mov/mean_mov, NA_real_)
      )
    ggplot(pld_vol, aes(x = mes)) +
      geom_line(aes(y = sd_mov,      color = "Desvio-padrão móvel"), linewidth = 1) +
      geom_line(aes(y = cv_mov *100, color = "CV móvel (%)"),       linewidth = 1, linetype = "longdash") +
      scale_color_manual(NULL, values = c("Desvio-padrão móvel"="#C0392B","CV móvel (%)"="#7D3C98")) +
      labs(title = paste0("Volatilidade do PLD (janela = ", janela_vol, " meses)"),
           x = "Ano", y = "SD e CV (%) em eixo único") +
      theme_minimal() + theme(legend.position = "bottom")
  })
  
  output$graf_hist <- renderPlot({
    pld_mensal <- base_pld_mensal()
    ggplot(pld_mensal, aes(pld)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#5DADE2", color = "white") +
      geom_density(color = "#2C3E50", linewidth = 1) +
      labs(title = "Distribuição do PLD (Histograma + Densidade)", x = "PLD [R$/MWh]", y = "Densidade") +
      theme_minimal()
  })
  
  output$graf_box <- renderPlot({
    pld_mensal <- base_pld_mensal()
    pld_ano <- pld_mensal %>% mutate(ano = year(mes))
    ggplot(pld_ano, aes(x = factor(ano), y = pld)) +
      geom_boxplot(fill = "#5DADE2", outlier.color = "#CB4335") +
      labs(title = "Boxplot do PLD por Ano", x = "Ano", y = "PLD [R$/MWh]") +
      theme_minimal()
  })
# OUTLIERS via AIQ 
  output$graf_out <- renderPlot({
    pld_mensal <- base_pld_mensal()
    
    Q   <- quantile(pld_mensal$pld, probs = c(.25, .75), na.rm = TRUE)
    AIQ <- diff(Q); Q1 <- as.numeric(Q[1]); Q3 <- as.numeric(Q[2])
    
    lim_m_inf <- Q1 - 1.5 * AIQ
    lim_m_sup <- Q3 + 1.5 * AIQ
    lim_e_inf <- Q1 - 3   * AIQ
    lim_e_sup <- Q3 + 3   * AIQ
    
    df_plot <- dplyr::mutate(
      pld_mensal,
      tipo_out = dplyr::case_when(
        pld < lim_e_inf | pld > lim_e_sup ~ "Extremo (±3×AIQ)",
        pld < lim_m_inf | pld > lim_m_sup ~ "Moderado (±1,5×AIQ)",
        TRUE                              ~ "Normal"
      )
    )
    
    ggplot(df_plot, aes(x = mes, y = pld)) +
      # Série
      geom_line(color = "#5DADE2", linewidth = 0.9) +
      
      # Limites moderados  
      geom_hline(yintercept = c(lim_m_inf, lim_m_sup),
                 linetype = "dashed", color = "#F39C12", linewidth = 0.7,
                 show.legend = FALSE) +
      # Limites extremos 
      geom_hline(yintercept = c(lim_e_inf, lim_e_sup),
                 linetype = "dotted", color = "#C0392B", linewidth = 0.8,
                 show.legend = FALSE) +
      
            geom_point(
        data = dplyr::filter(df_plot, tipo_out == "Moderado (±1,5×AIQ)"),
        aes(color = tipo_out, shape = tipo_out),
        size = 3, alpha = 0.95
      ) +
      
      scale_color_manual(values = c("Moderado (±1,5×AIQ)" = "#F39C12"),
                         name = "Classificação") +
      scale_shape_manual(values = c("Moderado (±1,5×AIQ)" = 17),
                         name = "Classificação") +
      
      labs(x = "Ano", y = "PLD (R$/MWh)") +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })

  output$tabela_aiq <- DT::renderDT({
    pld_mensal <- base_pld_mensal()
    
    # Quartis e AIQ
    Q   <- quantile(pld_mensal$pld, probs = c(.25, .75), na.rm = TRUE)
    AIQ <- diff(Q)
    Q1  <- as.numeric(Q[1])
    Q3  <- as.numeric(Q[2])
    
    # Limites
    lim_m_inf <- Q1 - 1.5 * AIQ
    lim_m_sup <- Q3 + 1.5 * AIQ
    lim_e_inf <- Q1 - 3   * AIQ
    lim_e_sup <- Q3 + 3   * AIQ
    
    # Contagens
    flag_mod <- (pld_mensal$pld < lim_m_inf | pld_mensal$pld > lim_m_sup)
    flag_ext <- (pld_mensal$pld < lim_e_inf | pld_mensal$pld > lim_e_sup)
    
    n_ext       <- sum(flag_ext, na.rm = TRUE)
    n_mod_only  <- sum(flag_mod & !flag_ext, na.rm = TRUE)  # só moderados
    n_total_mod <- sum(flag_mod, na.rm = TRUE)              # moderados + extremos
    
    DT::datatable(
      data.frame(
        Q1                 = round(Q1, 2),
        Q3                 = round(Q3, 2),
        AIQ                = round(as.numeric(AIQ), 2),
        Lim_Moderado_Inf   = round(lim_m_inf, 2),
        Lim_Moderado_Sup   = round(lim_m_sup, 2),
        Lim_Extremo_Inf    = round(lim_e_inf, 2),
        Lim_Extremo_Sup    = round(lim_e_sup, 2),
        Outliers_Moderados = n_mod_only,
        Outliers_Extremos  = n_ext,
        Outliers_Total     = n_total_mod
      ),
      rownames = FALSE,
      options = list(dom = 't', paging = FALSE)
    )
  })
  
  # VARIÁVEIS 
  observe({
    if (exists("pld_vf")) {
      anos <- pld_vf |>
        dplyr::mutate(d = as.Date(din_instante)) |>
        dplyr::filter(!is.na(d)) |>
        dplyr::transmute(ano = lubridate::year(d)) |>
        dplyr::distinct() |>
        dplyr::arrange(dplyr::desc(ano)) |>
        dplyr::pull(ano)
      updateSelectInput(session, "var_ano", choices = c("Todos", anos), selected = "Todos")
    } else {
      updateSelectInput(session, "var_ano", choices = "Todos", selected = "Todos")
    }
  })
  
  filtra_por_ano_regiao <- function(df, col_data, col_regiao, ano, reg) {
    out <- df
    out[[col_data]] <- as.Date(out[[col_data]])
    out <- out[!is.na(out[[col_data]]), , drop = FALSE]
    if (!is.null(ano) && ano != "Todos") {
      out <- out[format(out[[col_data]], "%Y") == as.character(ano), , drop = FALSE]
    }
    if (!is.null(reg) && reg != "Todos") {
      out <- out[out[[col_regiao]] == reg, , drop = FALSE]
    }
    out
  }
  
  # CMO
  output$graf_var_cmo <- renderPlotly({
    req(exists("cmo_select"))
    df <- filtra_por_ano_regiao(cmo_select, "din_instante", "id_subsistema",
                                input$var_ano, input$var_regiao)
    p <- ggplot(df) +
      geom_line(aes(x = din_instante, y = val_cmomedia,
                    color = id_subsistema, group = id_subsistema), linewidth = 1) +
      scale_regioes() +
      labs( x = "Tempo", y = "CMO Médio [R$/MWh]", color = "Região")
    ggplotly(p) %>% layout(showlegend = is.null(input$var_regiao) || input$var_regiao == "Todos")
  })
  
  # EAR
  output$graf_var_ear <- renderPlotly({
    req(exists("ear_select"))
    df <- filtra_por_ano_regiao(ear_select, "ear_data", "id_subsistema",
                                input$var_ano, input$var_regiao)
    p <- ggplot(df) +
      geom_line(aes(x = ear_data, y = ear_verif_subsistema_mwmes,
                    color = id_subsistema, group = id_subsistema), linewidth = 1) +
      scale_regioes() +
      labs(x = "Tempo", y = "Energia Armazenada [MWmês]", color = "Região")
    ggplotly(p) %>% layout(showlegend = is.null(input$var_regiao) || input$var_regiao == "Todos")
  })
  
  # ENA
  output$graf_var_ena <- renderPlotly({
    req(exists("ena_select"))
    df <- filtra_por_ano_regiao(ena_select, "ena_data", "id_subsistema",
                                input$var_ano, input$var_regiao)
    p <- ggplot(df) +
      geom_line(aes(x = ena_data, y = ena_armazenavel_regiao_mwmed,
                    color = id_subsistema, group = id_subsistema), linewidth = 1) +
      scale_regioes() +
      labs(x = "Tempo", y = "ENA [MWmed]", color = "Região")
    ggplotly(p) %>% layout(showlegend = is.null(input$var_regiao) || input$var_regiao == "Todos")
  })
  
  # Carga
  output$graf_var_carga <- renderPlotly({
    req(exists("carga_select"))
    df <- filtra_por_ano_regiao(carga_select, "din_instante", "id_subsistema",
                                input$var_ano, input$var_regiao)
    p <- ggplot(df) +
      geom_line(aes(x = din_instante, y = val_cargaenergiamwmed,
                    color = id_subsistema, group = id_subsistema), linewidth = 1) +
      scale_regioes() +
      labs(x = "Tempo", y = "Carga [MWmed]", color = "Região")
    ggplotly(p) %>% layout(showlegend = is.null(input$var_regiao) || input$var_regiao == "Todos")
  })
  
  # Hidrelétrica
  output$graf_var_hid <- renderPlotly({
    req(exists("hidreletrica_vf"))
    df <- filtra_por_ano_regiao(hidreletrica_vf, "din_instante", "id_subsistema",
                                input$var_ano, input$var_regiao)
    p <- ggplot(df) +
      geom_line(aes(x = din_instante, y = geracaohidraulica,
                    color = id_subsistema, group = id_subsistema), linewidth = 1) +
      scale_regioes() +
      labs(x = "Tempo", y = "Geração Hidrelétrica [MWmed]", color = "Região")
    ggplotly(p) %>% layout(showlegend = is.null(input$var_regiao) || input$var_regiao == "Todos")
  })
  
  # Termelétrica
  output$graf_var_term <- renderPlotly({
    req(exists("termeletrica_vf"))
    df <- filtra_por_ano_regiao(termeletrica_vf, "din_instante", "id_subsistema",
                                input$var_ano, input$var_regiao)
    p <- ggplot(df) +
      geom_line(aes(x = din_instante, y = geracaotermeletrica,
                    color = id_subsistema, group = id_subsistema), linewidth = 1) +
      scale_regioes() +
      labs(x = "Tempo", y = "Geração Termelétrica [MWmed]", color = "Região")
    ggplotly(p) %>% layout(showlegend = is.null(input$var_regiao) || input$var_regiao == "Todos")
  })
  
  # Eólica
  output$graf_var_eol <- renderPlotly({
    req(exists("eolica_vf"))
    df <- filtra_por_ano_regiao(eolica_vf, "din_instante", "id_subsistema",
                                input$var_ano, input$var_regiao)
    p <- ggplot(df) +
      geom_line(aes(x = din_instante, y = geracaoeolica,
                    color = id_subsistema, group = id_subsistema), linewidth = 1) +
      scale_regioes() +
      labs(x = "Tempo", y = "Geração Eólica [MWmed]", color = "Região")
    ggplotly(p) %>% layout(showlegend = is.null(input$var_regiao) || input$var_regiao == "Todos")
  })
  
  # Solar
  output$graf_var_sol <- renderPlotly({
    req(exists("solar_vf"))
    df <- filtra_por_ano_regiao(solar_vf, "din_instante", "id_subsistema",
                                input$var_ano, input$var_regiao)
    p <- ggplot(df) +
      geom_line(aes(x = din_instante, y = geracaosolar,
                    color = id_subsistema, group = id_subsistema), linewidth = 1) +
      scale_regioes() +
      labs(x = "Tempo", y = "Geração Solar [MWmed]", color = "Região")
    ggplotly(p) %>% layout(showlegend = is.null(input$var_regiao) || input$var_regiao == "Todos")
  })
  
  output$graf_var_demanda <- renderPlotly({
    validate(need(exists("demanda_se"), "Carregue o objeto 'demanda_se' (data, Demanda)."))
    
    df <- demanda_se %>%
      dplyr::mutate(
        data    = as.Date(data),
        Demanda = suppressWarnings(as.numeric(Demanda))
      ) %>%
      dplyr::filter(!is.na(data), !is.na(Demanda))
    
    # Filtro de ano (quando não for "Todos")
    if (!is.null(input$var_ano) && input$var_ano != "Todos") {
      df <- dplyr::filter(df, lubridate::year(data) == as.integer(input$var_ano))
    }
   
    p <- ggplot(df, aes(x = data, y = Demanda)) +
      geom_line(linewidth = 1, color = "#2E86C1") +
      labs( x = "Tempo", y = "MWmed") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(hovermode = "x unified", showlegend = FALSE)
  })
  
    output$graf_var_itaipu <- renderPlotly({
    validate(need(exists("vazao_Itaipu"), "Carregue o objeto 'vazao_Itaipu' (data, vazaoItaipu)."))
    
    df <- vazao_Itaipu %>%
      dplyr::mutate(
        data        = as.Date(data),
        vazaoItaipu = suppressWarnings(as.numeric(vazaoItaipu))
      ) %>%
      dplyr::filter(!is.na(data), !is.na(vazaoItaipu))
    
    # Filtro de ano (quando não for "Todos")
    if (!is.null(input$var_ano) && input$var_ano != "Todos") {
      df <- dplyr::filter(df, lubridate::year(data) == as.integer(input$var_ano))
    }
    
    p <- ggplot(df, aes(x = data, y = vazaoItaipu)) +
      geom_line(linewidth = 1, color = "#2E86C1") +
      labs(x = "Tempo", y = "m³/s") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(hovermode = "x unified", showlegend = FALSE)
  })
  # PLD
  output$graf_var_pld <- renderPlotly({
    req(exists("pld_vf"))
    df <- filtra_por_ano_regiao(pld_vf, "din_instante", "id_subsistema",
                                input$var_ano, input$var_regiao)
    df$pld <- suppressWarnings(as.numeric(df$pld))
    p <- ggplot(df) +
      geom_line(aes(x = din_instante, y = pld,
                    color = id_subsistema, group = id_subsistema), linewidth = 1) +
      scale_regioes() +
      labs(x = "Tempo", y = "PLD [R$/MWh]", color = "Região")
    ggplotly(p) %>% layout(showlegend = is.null(input$var_regiao) || input$var_regiao == "Todos")
  })
  
  # GRAFICO PIZZA
  output$graf_var_mix <- renderPlotly({
    validate(
      need(exists("hidreletrica_vf"), "Carregue 'hidreletrica_vf'."),
      need(exists("termeletrica_vf"), "Carregue 'termeletrica_vf'."),
      need(exists("eolica_vf"),       "Carregue 'eolica_vf'."),
      need(exists("solar_vf"),        "Carregue 'solar_vf'.")
      
    )
    
    ano <- input$var_ano
    reg <- input$var_regiao
    
    # Filtra por ano/região e soma valores
    df_hid <- filtra_por_ano_regiao(hidreletrica_vf, "din_instante", "id_subsistema", ano, reg) |>
      dplyr::mutate(v = suppressWarnings(as.numeric(geracaohidraulica))) |>
      dplyr::summarise(valor = sum(v, na.rm = TRUE)) |>
      dplyr::pull(valor)
    
    df_term <- filtra_por_ano_regiao(termeletrica_vf, "din_instante", "id_subsistema", ano, reg) |>
      dplyr::mutate(v = suppressWarnings(as.numeric(geracaotermeletrica))) |>
      dplyr::summarise(valor = sum(v, na.rm = TRUE)) |>
      dplyr::pull(valor)
    
    df_sol <- filtra_por_ano_regiao(solar_vf, "din_instante", "id_subsistema", ano, reg) |>
      dplyr::mutate(v = suppressWarnings(as.numeric(geracaosolar))) |>
      dplyr::summarise(valor = sum(v, na.rm = TRUE)) |>
      dplyr::pull(valor)
    
    df_eol <- filtra_por_ano_regiao(eolica_vf, "din_instante", "id_subsistema", ano, reg) |>
      dplyr::mutate(v = suppressWarnings(as.numeric(geracaoeolica))) |>
      dplyr::summarise(valor = sum(v, na.rm = TRUE)) |>
      dplyr::pull(valor)
    
    mix <- tibble::tibble(
      fonte = c("Hidrelétrica", "Termelétrica", "Solar", "Eólica"),
      valor = c(df_hid, df_term, df_sol, df_eol)
    )
    
    validate(need(sum(mix$valor, na.rm = TRUE) > 0, "Sem dados para este filtro."))
    
    # título dinâmico 
    titulo_reg <- if (!is.null(reg) && reg != "Todos") {
      if (exists("home_labels")) home_labels[[reg]] else reg
    } else {
      "Brasil (todos subsistemas)"
    }
    titulo <- paste0("Matriz de Geração — ", titulo_reg,
                     if (!is.null(ano) && ano != "Todos") paste0(" – ", ano) else "")
    
    # paleta por fonte
    cores_fonte <- c(
      "Hidrelétrica" = "#3498db",
      "Termelétrica" = "#e74c3c",
      "Solar"        = "#f1c40f",
      "Eólica"       = "#2ecc71"
    )
    cores_ord <- cores_fonte[match(mix$fonte, names(cores_fonte))]
    
    plot_ly(
      data = mix,
      type = "pie",
      labels = ~fonte,
      values = ~valor,
      textinfo = "label+percent",
      insidetextfont = list(color = "white"),
      hovertemplate = paste0(
        "%{label}<br>",
        "Valor acumulado: %{value:.0f} MWmed<br>",
        "<extra></extra>"
      ),
      marker = list(colors = cores_ord)
    ) |>
      layout(
        title = list(text = titulo),
        showlegend = TRUE
      )
  })
  #  MODELAGEM
  modelo_ok <- reactive({
    exists("modelo_pld") && inherits(get("modelo_pld"), "lm") &&
      exists("dados_modelo") && is.data.frame(get("dados_modelo"))
  })
  
  output$resumo_modelo <- renderPrint({
    validate(need(modelo_ok(), "Carregue 'modelo_pld' (lm) e 'dados_modelo'."))
    summary(modelo_pld)
  })
  
  # InfoBoxes do modelo
  output$ib_r2 <- renderInfoBox({
    validate(need(modelo_ok(), ""))
    s <- summary(modelo_pld)
    infoBox("R² Ajustado", fmt_num(s$adj.r.squared, 0.001),
            "Qualidade de ajuste", icon = icon("circle-check"), color = "navy", fill = TRUE)
  })
  
  output$ib_rmse <- renderInfoBox({
    validate(need(modelo_ok(), ""))
    s <- summary(modelo_pld)
    rmse <- sqrt(mean(s$residuals^2))
    infoBox("RMSE (R$/MWh)", fmt_num(rmse, 0.01),
            "Erro médio quadrático", icon = icon("chart-area"), color = "aqua", fill = TRUE)
  })
  
  output$ib_rse <- renderInfoBox({
    validate(need(modelo_ok(), ""))
    s <- summary(modelo_pld)
    rse <- s$sigma
    infoBox("RSE (R$/MWh)", fmt_num(rse, 0.01),
            "Erro padrão residual", icon = icon("wave-square"), color = "light-blue", fill = TRUE)
  })
  
  output$ib_fstat <- renderInfoBox({
    validate(need(modelo_ok(), ""))
    s <- summary(modelo_pld)
    fval <- unname(s$fstatistic["value"])
    infoBox("F-stat", fmt_num(fval, 0.01),
            "Significância global", icon = icon("signal"), color = "teal", fill = TRUE)
  })
  
  # Coeficientes em tabela
  output$tabela_coef <- DT::renderDT({
    validate(need(modelo_ok(), ""))
    tc <- broom::tidy(modelo_pld) %>%
      mutate(across(where(is.numeric), ~round(.x, 6))) %>%
      rename(Termo = term, Estimativa = estimate, `Erro-padrão` = std.error,
             `t value` = statistic, `p-valor` = p.value)
    datatable(tc, rownames = FALSE, options = list(pageLength = 10, dom = 'tip'))
  })
  
  # Importância (betas padronizados)
  output$plot_importancia <- renderPlot({
    validate(need(modelo_ok(), ""))
    form  <- formula(modelo_pld)
    resp  <- all.vars(form)[1]
    preds <- attr(terms(form), "term.labels")
    
    df_std <- dados_modelo[, c(resp, preds), drop = FALSE]
    df_std <- dplyr::mutate_if(df_std, is.numeric, scale) %>% as.data.frame()
    fit_std <- lm(as.formula(paste(resp, "~", paste(preds, collapse = " + "))), data = df_std)
    
    betas <- broom::tidy(fit_std) %>%
      filter(term != "(Intercept)") %>%
      mutate(Variável = term, Importância = estimate) %>%
      arrange(desc(abs(Importância)))
    
    ggplot(betas, aes(x = reorder(Variável, abs(Importância)), y = Importância)) +
      geom_col(fill = "#3498db") +
      coord_flip() +
      geom_hline(yintercept = 0, linewidth = .5, color = "black") +
      labs(x = NULL, y = "Beta padronizado") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
  })
  # Dados previstos do modelo
  pred_df <- reactive({
    validate(need(modelo_ok(), "Carregue os dados e ajuste o modelo."))
    df <- dados_modelo
    df$Previsto <- predict(modelo_pld)
    df
  })
  
  # Gráfico Dispersão (PLD Real × Previsto)
  output$graf_scatter_real_prev <- renderPlot({
    dfp <- pred_df()
    df_predicao <- data.frame(
      PLD_Real     = dfp$pld_SE,
      PLD_Previsto = dfp$Previsto
    )
    
    ggplot(df_predicao, aes(x = PLD_Real, y = PLD_Previsto)) +
      geom_point(color = "#3498DB", alpha = 0.6, size = 2) +   # pontos em azul
      geom_abline(intercept = 0, slope = 1,                    # linha ideal y = x
                  linetype = "dashed", color = "black", size = 1) +
      labs(
        x = "PLD Real",
        y = "PLD Previsto") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5),
          )
  })
  output$graf_ts_real_prev <- renderPlot({
    df_ts <- pred_df()
    
    ggplot(df_ts, aes(x = data)) +
      geom_line(aes(y = pld_SE,    color = "PLD Real"), size = 1.1) +
      geom_line(aes(y = Previsto,  color = "PLD Previsto"), size = 1.1, linetype = "dashed") +
      scale_color_manual(values = c("PLD Real" = "#3498DB", "PLD Previsto" = "#E74C3C")) + # vermelho para destacar
      labs(x = "Tempo", y = "PLD (R$/MWh)", color = "") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  })
  
# Testes estatísticos (usa seu objeto pronto, se existir)
  output$tabela_testes <- DT::renderDT({
    validate(need(exists("resultados_testes"), "Carregue 'resultados_testes'."))
    datatable(resultados_testes, rownames = FALSE, options = list(dom = 't', paging = FALSE))
  })
}