## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, out.width=550, out.height=400-------------------------------
knitr::include_graphics("../man/figures/app_germany_map.png")

## ----setup, message=FALSE,warning=FALSE---------------------------------------
library(leafdown)
library(leaflet)
library(shiny)
library(dplyr)
library(shinyjs)

## ---- echo=FALSE--------------------------------------------------------------
ger1 <- readRDS("../inst/extdata/ger1-005.R")
ger2 <- readRDS("../inst/extdata/ger2-005.R")

## ---- eval=FALSE--------------------------------------------------------------
#  ger1 <- raster::getData(country = "Germany", level = 1)
#  ger2 <- raster::getData(country = "Germany", level = 2)

## ---- message=FALSE-----------------------------------------------------------
ger2@data[c(76, 99, 136, 226), "NAME_2"] <- c(
  "Fürth (Kreisfreie Stadt)",
  "München (Kreisfreie Stadt)",
  "Osnabrück (Kreisfreie Stadt)",
  "Würzburg (Kreisfreie Stadt)"
)

## -----------------------------------------------------------------------------
spdfs_list <- list(ger1, ger2)

## -----------------------------------------------------------------------------
head(gdp_2014_federal_states)

## -----------------------------------------------------------------------------
head(gdp_2014_admin_districts)

## ---- echo=FALSE, fig.width=7, eval=FALSE-------------------------------------
#  DiagrammeR::grViz('
#  digraph {
#    rankdir=LR;
#    rank="same";
#    graph [fontname = "Segoe UI", fontsize = 36,
#           nodesep="2", ranksep="1", color = dimgrey];
#    node [fontname = "Segoe UI", fontsize = 30, color = dimgrey];
#    edge [fontname = "Segoe UI", fontsize = 30, color = dimgrey];
#    "Invisible"[style=invis];
#    c0 [
#      label = "Initialization \n ($new)", fillcolor  = palegreen,
#      style=filled, color = "grey"
#    ];
#  
#    subgraph cluster01 {
#      label="Map Level 1";
#      rank="same";
#      a0 [label="Add Data \n ($add_data)", fillcolor = OldLace, style=filled];
#      a1 [label="Draw Map \n ($draw_leafdown)",  fillcolor = Moccasin, style=filled];
#      a2 [label="Select Shapes", fillcolor = lightyellow, style=filled];
#      a3 [label="Drill Down \n ($drill_down)", fillcolor = PowderBlue, style=filled];
#      a0 -> a1;
#      a1 -> a2;
#      a2 -> a3;
#    };
#  
#    subgraph cluster02
#    {
#      label="Map Level 2";
#      rank="same";
#      b0 [label="    Drill Up   \n ($drill_up)", fillcolor = PowderBlue, style=filled];
#      b3 [label="   Add Data   \n ($add_data) ", fillcolor = OldLace, style=filled];
#      b1 [label="..."];
#      b2 [label="Draw Map \n ($draw_leafdown)", fillcolor = Moccasin, style=filled];
#  
#      b0 -> b1 [dir="back"];
#      b1 -> b2 [dir="back"];
#      b2 -> b3 [dir="back"];
#  
#    };
#  
#    edge[constraint=false];
#    a1->b1 [style=invis];
#    a2->b2 [style=invis];
#    a3->b3 [style=dotted];
#    b0->a0 [style=dotted];
#    edge[constraint=true];
#    Invisible -> b0[style=invis];
#    edge[constraint=true];
#    c0 -> a0;
#    Invisible -> a0[style=invis];
#  }
#  ') %>%
#    DiagrammeRsvg::export_svg() %>%
#    charToRaw() %>%
#    rsvg::rsvg_png("res/leafdown_workflow.png")
#  

## ---- echo=FALSE, out.width=700, out.height=200-------------------------------
knitr::include_graphics("../man/figures/leafdown_workflow.png")

## ---- echo=FALSE--------------------------------------------------------------
input <- reactiveValues(foo = "bar") # a little helper as we are currently not in a shiny session

## -----------------------------------------------------------------------------
my_leafdown <- Leafdown$new(spdfs_list, map_output_id = "leafdown", input = input)

## -----------------------------------------------------------------------------
metadata <- my_leafdown$curr_data
print(head(metadata))

## -----------------------------------------------------------------------------
new_data <- metadata %>% dplyr::left_join(gdp_2014_federal_states, by = c("NAME_1" = "Federal_State"))

## -----------------------------------------------------------------------------
my_leafdown$add_data(new_data)

## -----------------------------------------------------------------------------
print(head(my_leafdown$curr_data))

## ---- eval=FALSE--------------------------------------------------------------
#  map <- my_leafdown$draw_leafdown(
#    fillColor = ~ colorNumeric("Blues", GDP_2014)(GDP_2014)
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  map <- map %>%
#    addLegend(
#      pal = colorNumeric("Blues", data$GDP_2014),
#      values = data$GDP_2014
#    )

## ---- eval=FALSE--------------------------------------------------------------
#  my_leafdown$curr_sel_data()

## ---- echo=FALSE--------------------------------------------------------------
subset(my_leafdown$curr_data, NAME_1 %in% c("Bayern", "Hessen"))

## ---- eval=FALSE--------------------------------------------------------------
#  my_leafdown$drill_down()

## ---- eval=FALSE--------------------------------------------------------------
#  length(my_leafdown$curr_spdf)

## ---- echo=FALSE--------------------------------------------------------------
print(2)

## ----eval = FALSE-------------------------------------------------------------
#  my_leafdown$drill_down()
#  metadata <- my_leafdown$curr_data
#  head(metadata)

## ----echo=FALSE---------------------------------------------------------------
head(subset(ger2@data, GID_1 %in% c("DEU.2_1", "DEU.7_1")))

## ----eval = FALSE-------------------------------------------------------------
#  unique(metadata$NAME_1)

## ----echo=FALSE---------------------------------------------------------------
c("Bayern", "Hessen")

## ---- eval=FALSE--------------------------------------------------------------
#  new_data <- metadata %>%
#    dplyr::left_join(gdp_2014_admin_districts, by = c("NAME_2" = "Admin_District"))
#  my_leafdown$add_data(new_data)

## ---- eval=FALSE--------------------------------------------------------------
#  head(my_leafdown$curr_data)

## ---- echo=FALSE--------------------------------------------------------------
ger2@data %>% 
  filter(GID_1 %in% c("DEU.2_1", "DEU.7_1")) %>% 
  dplyr::left_join(gdp_2014_admin_districts, by = c("NAME_2" = "Admin_District")) %>% 
  slice_head(n = 5) %>% 
  as.data.frame()

## ---- eval=FALSE--------------------------------------------------------------
#  my_leafdown$draw_leafdown(
#    fillColor = ~ colorNumeric("Blues", GDP_2014)(GDP_2014)
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  my_leafdown$drill_down()

## -----------------------------------------------------------------------------
new_data <- metadata %>% left_join(gdp_2014_federal_states, by = c("NAME_1" = "Federal_State"))

## ---- eval=FALSE--------------------------------------------------------------
#  map <- my_leafdown$draw_leafdown(
#    fillColor = ~ colorNumeric("Blues", GDP_2014)(GDP_2014)
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  library(leafdown)
#  library(leaflet)
#  library(shiny)
#  library(dplyr)
#  library(shinyjs)
#  ger1 <- raster::getData(country = "Germany", level = 1)
#  ger2 <- raster::getData(country = "Germany", level = 2)
#  ger2@data[c(76, 99, 136, 226), "NAME_2"] <- c(
#    "Fürth (Kreisfreie Stadt)",
#    "München (Kreisfreie Stadt)",
#    "Osnabrück (Kreisfreie Stadt)",
#    "Würzburg (Kreisfreie Stadt)"
#  )
#  spdfs_list <- list(ger1, ger2)

## ---- eval=FALSE--------------------------------------------------------------
#  ui <- shiny::fluidPage(
#    tags$style(HTML(".leaflet-container {background: #ffffff;}")),
#    useShinyjs(),
#    actionButton("drill_down", "Drill Down"),
#    actionButton("drill_up", "Drill Up"),
#    leafletOutput("leafdown", height = 600),
#  )

## -----------------------------------------------------------------------------
# Little helper function for hover labels
create_labels <- function(data, map_level) {
  labels <- sprintf(
    "<strong>%s</strong><br/>%g € per capita</sup>",
    data[, paste0("NAME_", map_level)], data$GDP_2014
  )
  labels %>% lapply(htmltools::HTML)
}

## ---- eval=FALSE--------------------------------------------------------------
#  server <- function(input, output) {
#    my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)
#    update_leafdown <- reactiveVal(0)
#  
#    observeEvent(input$drill_down, {
#      my_leafdown$drill_down()
#      update_leafdown(update_leafdown() + 1)
#    })
#  
#    observeEvent(input$drill_up, {
#      my_leafdown$drill_up()
#      update_leafdown(update_leafdown() + 1)
#    })
#  
#    output$leafdown <- renderLeaflet({
#      update_leafdown()
#      meta_data <- my_leafdown$curr_data
#      curr_map_level <- my_leafdown$curr_map_level
#      if (curr_map_level == 1) {
#        data <- meta_data %>% left_join(gdp_2014_federal_states, by = c("NAME_1" = "Federal_State"))
#      } else {
#        data <- meta_data %>% left_join(gdp_2014_admin_districts, by = c("NAME_2" = "Admin_District"))
#      }
#  
#      my_leafdown$add_data(data)
#      labels <- create_labels(data, curr_map_level)
#      my_leafdown$draw_leafdown(
#        fillColor = ~ colorNumeric("Blues", GDP_2014)(GDP_2014),
#        weight = 2, fillOpacity = 0.8, color = "grey", label = labels,
#        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)
#      ) %>%
#        addLegend("topright",
#          pal = colorNumeric("Blues", data$GDP_2014),
#          values = data$GDP_2014,
#          title = "GDP per capita (2014)",
#          labFormat = labelFormat(suffix = "€"),
#          opacity = 1
#        )
#    })
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  shinyApp(ui, server)

