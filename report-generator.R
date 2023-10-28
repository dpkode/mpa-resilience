library(quarto)
library(DBI)
library(duckdb)
library(tidyverse)

if (exists(x = "con")) dbDisconnect(con, shutdown=TRUE)
con <- DBI::dbConnect(duckdb::duckdb(),
                      dbdir = "data/sim_out.duckdb",
                      read_only = FALSE) # change to TRUE???

avail_tables <- DBI::dbListTables(con)

exp_tabs <- avail_tables[!str_detect(avail_tables, "enso_75|white_75|mei_135_25|white_135_25")]

DBI::dbDisconnect(conn = con, shutdown = TRUE)

exp_tabs <- exp_tabs[str_detect(exp_tabs, "blue_rockfish")]

run_html <- function(experiment) {
  quarto::quarto_render(
    "recs-abund-yield-parameters.qmd",
    output_format = "html",
    execute_params = list(experiment = experiment),
    output_file = glue::glue("ts-contour-plots-{experiment}.html")
  )
}

purrr::map(exp_tabs, run_html)

# 
con <- DBI::dbConnect(duckdb::duckdb(),
                      dbdir = "data/sim_out.duckdb",
                      read_only = FALSE) # change to TRUE???
nd_qry <- "select * from 'no_dispersal_blue_rockfish_mei_135_75' limit 2000"
lp_qry <- "select * from 'larval_pool_blue_rockfish_mei_135_75' limit 2000"
nd_df <- dbGetQuery(con, nd_qry)
lp_df <- dbGetQuery(con, lp_qry)
all.equal(nd_df, lp_df)
DBI::dbDisconnect(con, shutdown = TRUE)
