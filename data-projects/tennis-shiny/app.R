library(shiny)
library(bslib)
library(tidyverse)
library(xgboost)
library(ggplot2)

# ── Load artifacts ────────────────────────────────────────────────────────────
# tennis_model.rds must sit in the same folder as app.R for shinyapps.io deployment.
# Generate it by rendering tennis-predictor.qmd, then copy:
#   cp data-projects/data/tennis_model.rds data-projects/tennis-shiny/
artifacts_path <- "tennis_model.rds"
if (!file.exists(artifacts_path)) {
  stop("Cannot find tennis_model.rds — render tennis-predictor.qmd then copy the file here.")
}

art        <- readRDS(artifacts_path)
fit_xgb    <- art$model_xgb
fit_glm    <- art$model_glm
FEAT_NAMES <- art$feat_names
elo_df     <- art$elo_df
h2h_map    <- art$h2h
h2h_surf   <- art$h2h_surf
recent_form <- art$recent_form
players    <- art$active_players

player_names <- sort(unique(players$player_name))

# ── Prediction helper ─────────────────────────────────────────────────────────
get_elo_vals <- function(nm) {
  r <- elo_df |> filter(player_name == nm) |> slice(1)
  if (nrow(r) == 0) list(elo=1500, hard=1500, clay=1500, grass=1500)
  else list(elo=r$elo, hard=r$elo_hard, clay=r$elo_clay, grass=r$elo_grass)
}

predict_match_app <- function(p1_name, p2_name, surface_str) {
  e1 <- get_elo_vals(p1_name); e2 <- get_elo_vals(p2_name)
  p1_id <- elo_df |> filter(player_name == p1_name) |> pull(player_id)
  p2_id <- elo_df |> filter(player_name == p2_name) |> pull(player_id)
  p1_id <- if (length(p1_id) > 0) p1_id[[1]] else ""
  p2_id <- if (length(p2_id) > 0) p2_id[[1]] else ""

  p1_elo  <- e1$elo;  p2_elo  <- e2$elo
  p1_selo <- switch(surface_str, Hard=e1$hard, Clay=e1$clay, Grass=e1$grass, e1$elo)
  p2_selo <- switch(surface_str, Hard=e2$hard, Clay=e2$clay, Grass=e2$grass, e2$elo)

  # Overall H2H
  h12  <- if (!is.null(h2h_map[[paste0(p1_id,"__",p2_id)]])) h2h_map[[paste0(p1_id,"__",p2_id)]] else 0L
  h21  <- if (!is.null(h2h_map[[paste0(p2_id,"__",p1_id)]])) h2h_map[[paste0(p2_id,"__",p1_id)]] else 0L
  ht   <- h12 + h21
  h2h_pct <- if (ht > 0) h12/ht - 0.5 else 0.0

  # Surface H2H
  sh12 <- if (!is.null(h2h_surf[[paste0(p1_id,"__",p2_id,"__",surface_str)]])) h2h_surf[[paste0(p1_id,"__",p2_id,"__",surface_str)]] else 0L
  sh21 <- if (!is.null(h2h_surf[[paste0(p2_id,"__",p1_id,"__",surface_str)]])) h2h_surf[[paste0(p2_id,"__",p1_id,"__",surface_str)]] else 0L
  sht  <- sh12 + sh21
  sh2h_pct <- if (sht > 0) sh12/sht - 0.5 else 0.0

  # Recent form
  p1_form <- coalesce(recent_form |> filter(player_name==p1_name) |> pull(form) |> first(), 0.5)
  p2_form <- coalesce(recent_form |> filter(player_name==p2_name) |> pull(form) |> first(), 0.5)

  # Feature matrix for XGBoost
  nd_xgb <- matrix(
    c(p1_elo-p2_elo, p1_selo-p2_selo, 0, h2h_pct, sh2h_pct,
      p1_form-p2_form, NA, 0,
      as.integer(surface_str=="Clay"), as.integer(surface_str=="Grass")),
    nrow=1, dimnames=list(NULL, FEAT_NAMES)
  )

  # Feature matrix for GLM (impute NA surf_wr with 0)
  nd_glm <- tibble(
    `Elo diff`          = p1_elo - p2_elo,
    `Surface Elo diff`  = p1_selo - p2_selo,
    `log-Rank diff`     = 0,
    `H2H share`         = h2h_pct,
    `Surface H2H share` = sh2h_pct,
    `Form diff`         = p1_form - p2_form,
    `Surface WR diff`   = 0,
    `Age diff`          = 0,
    surface             = factor(surface_str, levels=c("Hard","Clay","Grass"))
  )

  # SHAP values for XGBoost (for breakdown chart)
  shap_single <- predict(fit_xgb, xgb.DMatrix(nd_xgb), predcontrib=TRUE)[1, 1:length(FEAT_NAMES)]
  names(shap_single) <- FEAT_NAMES

  list(
    prob_xgb  = predict(fit_xgb, xgb.DMatrix(nd_xgb))[[1]],
    prob_glm  = predict(fit_glm, nd_glm, type="response")[[1]],
    p1_elo    = p1_elo,  p2_elo  = p2_elo,
    p1_selo   = p1_selo, p2_selo = p2_selo,
    p1_h2h    = h12,     p2_h2h  = h21,
    p1_sh2h   = sh12,    p2_sh2h = sh21,
    p1_form   = round(100 * p1_form, 1),
    p2_form   = round(100 * p2_form, 1),
    shap      = shap_single
  )
}

surf_colour <- c(Hard="#2171B5", Clay="#CB4335", Grass="#27AE60")

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_fluid(
  theme = bs_theme(bootswatch="cosmo", primary="#2171B5"),

  tags$head(tags$style(HTML("
    .prob-bar-wrap{background:#e9ecef;border-radius:8px;height:44px;overflow:hidden;margin:6px 0}
    .prob-bar-inner{height:100%;display:flex;align-items:center;justify-content:flex-end;
      padding-right:12px;color:white;font-weight:bold;font-size:1.1rem;transition:width .5s ease}
    .player-card{background:#f8f9fa;border-radius:10px;padding:14px;margin:6px 0}
    .stat-tbl{width:100%;font-size:.88rem}
    .stat-tbl td{padding:2px 4px}
    .winner-label{font-size:1.5rem;font-weight:bold}
    .up{color:#27AE60}.down{color:#CB4335}
  "))),

  titlePanel(
    div(span("🎾 ", style="font-size:1.8rem"),
        span("ATP Match Predictor", style="font-size:1.8rem;font-weight:bold"))
  ),
  p("Select two players and a surface. XGBoost model trained on ATP matches 2000–2021.",
    style="color:#6c757d"),
  hr(),

  layout_columns(
    col_widths = c(5, 2, 5),
    card(card_header("Player 1"),
         selectInput("p1", NULL, choices=player_names,
                     selected="Carlos Alcaraz", width="100%")),
    card(card_header("Surface"),
         radioButtons("surface", NULL,
                      choices=c("Hard","Clay","Grass"),
                      selected="Hard")),
    card(card_header("Player 2"),
         selectInput("p2", NULL, choices=player_names,
                     selected="Jannik Sinner", width="100%"))
  ),

  actionButton("go", "Predict", class="btn-primary btn-lg",
               style="margin:12px 0 4px 0"),

  uiOutput("result_panel"),

  hr(),
  p(em("Model: XGBoost (300 rounds, max_depth=4) + logistic regression comparison.",
       " Features: Elo, surface Elo, H2H, surface H2H, recent form, surface win rate, age, ranking.",
       " Test AUC reported on 2022–2025 held-out data."),
    style="color:#adb5bd;font-size:.83rem")
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  res <- eventReactive(input$go, {
    req(input$p1, input$p2, input$surface)
    if (input$p1 == input$p2) return(NULL)
    predict_match_app(input$p1, input$p2, input$surface)
  })

  output$result_panel <- renderUI({
    r <- res()
    if (is.null(r)) return(p("Select two different players.", style="color:red;margin-top:10px"))

    prob1 <- round(100 * r$prob_xgb, 1)
    prob2 <- 100 - prob1
    prob_glm1 <- round(100 * r$prob_glm, 1)
    sc <- surf_colour[[input$surface]]
    winner <- if (r$prob_xgb >= 0.5) input$p1 else input$p2

    # SHAP waterfall data
    shap_df <- tibble(feature=names(r$shap), value=r$shap) |>
      arrange(abs(value)) |>
      mutate(dir = if_else(value >= 0, "#2171B5", "#CB4335"))

    tagList(
      hr(),
      h4("Prediction"),

      # Probability bar
      div(style="margin-bottom:2px",
          layout_columns(col_widths=c(6,6),
            div(strong(input$p1)),
            div(strong(input$p2), style="text-align:right"))),
      div(class="prob-bar-wrap",
          div(class="prob-bar-inner",
              style=sprintf("width:%s%%;background:%s", prob1, sc),
              if (prob1 >= 15) paste0(prob1, "%") else "")),
      div(layout_columns(
        col_widths=c(6,6),
        div(span(paste0(prob1,"%"), style=sprintf("color:%s;font-weight:bold;font-size:1.3rem",sc))),
        div(span(paste0(prob2,"%"), style=sprintf("color:%s;font-weight:bold;font-size:1.3rem",sc)),
            style="text-align:right")
      )),

      # Implied odds + GLM comparison
      div(style="margin:10px 0",
          layout_columns(
            col_widths=c(6,6),
            div(tags$small("Decimal odds"),
                div(strong(round(1/r$prob_xgb,2)), style="font-size:1.1rem"),
                tags$small(sprintf("GLM: %.1f%%", prob_glm1), style="color:#999")),
            div(style="text-align:right",
                tags$small("Decimal odds"),
                div(strong(round(1/(1-r$prob_xgb),2)), style="font-size:1.1rem"),
                tags$small(sprintf("GLM: %.1f%%", 100-prob_glm1), style="color:#999"))
          )),

      # Winner highlight
      div(style=sprintf("background:%s22;border-left:5px solid %s;padding:10px 14px;border-radius:6px;margin:14px 0", sc, sc),
          span("Predicted winner: ", style="color:#666"),
          span(winner, class="winner-label", style=sprintf("color:%s", sc))),

      layout_columns(
        col_widths = c(6, 6),
        # Feature breakdown cards
        div(
          h6(strong(input$p1)),
          div(class="player-card",
              tags$table(class="stat-tbl",
                map(list(
                  c("Overall Elo",  round(r$p1_elo),  round(r$p2_elo)),
                  c(paste(input$surface,"Elo"), round(r$p1_selo), round(r$p2_selo)),
                  c("Overall H2H",  r$p1_h2h,        r$p2_h2h),
                  c(paste(input$surface,"H2H"), r$p1_sh2h, r$p2_sh2h),
                  c("Recent form",  paste0(r$p1_form,"%"), paste0(r$p2_form,"%"))
                ), function(row) {
                  mine <- as.numeric(gsub("%","",row[[2]]))
                  theirs <- as.numeric(gsub("%","",row[[3]]))
                  arrow <- if (!is.na(mine) && !is.na(theirs) && mine >= theirs) "▲" else "▼"
                  cls   <- if (!is.na(mine) && !is.na(theirs) && mine >= theirs) "up" else "down"
                  tags$tr(tags$td(row[[1]]), tags$td(strong(row[[2]])),
                          tags$td(class=cls, arrow))
                })
              )
          )
        ),
        div(
          h6(strong(input$p2)),
          div(class="player-card",
              tags$table(class="stat-tbl",
                map(list(
                  c("Overall Elo",  round(r$p2_elo),  round(r$p1_elo)),
                  c(paste(input$surface,"Elo"), round(r$p2_selo), round(r$p1_selo)),
                  c("Overall H2H",  r$p2_h2h,        r$p1_h2h),
                  c(paste(input$surface,"H2H"), r$p2_sh2h, r$p1_sh2h),
                  c("Recent form",  paste0(r$p2_form,"%"), paste0(r$p1_form,"%"))
                ), function(row) {
                  mine <- as.numeric(gsub("%","",row[[2]]))
                  theirs <- as.numeric(gsub("%","",row[[3]]))
                  arrow <- if (!is.na(mine) && !is.na(theirs) && mine >= theirs) "▲" else "▼"
                  cls   <- if (!is.na(mine) && !is.na(theirs) && mine >= theirs) "up" else "down"
                  tags$tr(tags$td(row[[1]]), tags$td(strong(row[[2]])),
                          tags$td(class=cls, arrow))
                })
              )
          )
        )
      ),

      # SHAP waterfall
      hr(),
      h5("Why this prediction? (SHAP contributions)"),
      p(em("Each bar shows how much that feature pushed the prediction toward player 1 (blue) or player 2 (red)."),
        style="color:#888;font-size:.85rem;margin-top:-6px"),

      plotOutput("shap_plot", height="300px")
    )
  })

  output$shap_plot <- renderPlot({
    r <- res(); req(r)
    sc <- surf_colour[[input$surface]]

    shap_df <- tibble(feature=names(r$shap), value=r$shap) |>
      filter(feature != "Surface: Clay" | input$surface == "Clay") |>
      filter(feature != "Surface: Grass"| input$surface == "Grass") |>
      arrange(abs(value)) |>
      mutate(feature=factor(feature, levels=feature),
             dir=if_else(value>=0, "Toward P1","Toward P2"))

    ggplot(shap_df, aes(value, feature, fill=dir)) +
      geom_col(alpha=0.9) +
      geom_vline(xintercept=0, linetype="dashed", colour="grey50") +
      scale_fill_manual(values=c("Toward P1"="#2171B5","Toward P2"="#CB4335"),
                        name=NULL) +
      theme_minimal(base_size=12) +
      theme(legend.position="bottom",
            panel.grid.minor=element_blank(),
            plot.margin=margin(8,8,8,8)) +
      labs(x="SHAP value (log-odds contribution)", y=NULL)
  })
}

shinyApp(ui, server)
