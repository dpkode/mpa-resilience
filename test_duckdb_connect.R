# Install and load packages
# Uncomment next 5 lines first time the project is run
# install.packages(c("DBI", "ggplot2"))
library(remotes)
# ## duckdb note: newer package versions cannot read databases written with older versions
# ## until the duckdb (R package) version reaches 1.0
install_version("duckdb", version = "0.7.0", repos = "https://cloud.r-project.org/")

library(DBI)
library(duckdb)
library(ggplot2)

# connect to the database
con <- DBI::dbConnect(duckdb::duckdb(),
                      dbdir = "data/sim_out.duckdb",
                      read_only = FALSE)
# list the names of tables in the database
tables <- dbListTables(con)

# query one of the tables # larval_pool_blue_rockfish_mei_135_75
for (i in 1:length(tables)) {
  out <- dbGetQuery(conn = con, paste0("SELECT reserve_frac, flep_ratio, sum(number) as total_nums, sum(yield) as total_yield 
                               from '", tables[i], "' 
                               where year > 500
                               group by reserve_frac, flep_ratio
                               order by reserve_frac asc, flep_ratio asc"))
  
  print(tail(out))
  
}

# make some quick plots that are kind of like the real plots for the last table of the loop
ggplot(out, aes(x = as.numeric(reserve_frac), 
                y = as.numeric(flep_ratio),
                z = total_nums)) +
  geom_contour_filled()

ggplot(out, aes(x = as.numeric(reserve_frac), 
                y = as.numeric(flep_ratio),
                z = total_yield)) +
  geom_contour_filled()

# close the connection
DBI::dbDisconnect(con, shutdown = TRUE)
