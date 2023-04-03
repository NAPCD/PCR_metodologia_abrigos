install.packages("brclimr")

install.packages("remotes")

remotes::install_github(repo = "rfsaldanha/brclimr")

library(brclimr)

rj <- fetch_data(
  code_muni = 3304557,
  date_start = as.Date("2010-10-15"),
  date_end = as.Date("2010-10-15")
)

product_info("brdwgd")
product_info("terraclimate")
