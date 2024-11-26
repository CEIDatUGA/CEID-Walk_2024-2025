read_forecast <- function(file) {
  read_csv(
    file,
    col_types =
      cols(
        forecast_date = col_date(format = "%Y-%m-%d"),
        target = col_character(),
        target_end_date = col_date(format = "%Y-%m-%d"),
        location = col_character(),
        type = col_character(),
        quantile = col_double(),
        value = col_double()
      )
  )
}

quant <- function(x, p){
  quantile(x, prob = p, names = FALSE, type = 8, na.rm = TRUE)
}

vardf <- function(var, samp){
  cname <- switch(var,
                  "inc death" = "deaths",
                  "inc case" = "cases")
  if (var != "inc case") {
    prob <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  } else {
    prob <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  }
  t1 <- tibble(
    target = var,
    type  = "quantile",
    quantile = prob,
    value = quant(samp[[cname]], prob)
  )
  t2 <-
    tibble(
      target = var,
      type = "point",
      quantile = NA,
      value = quant(samp[[cname]], 0.5)
      
    )
  bind_rows(t1, t2)
}

samp_to_df <-
  function(sampdf,
           vars = c("inc death", "inc case")) {
    purrr::map_dfr(vars, vardf, samp = sampdf)
  }

# Take simulation trajectories and output a data frame in the format described
# here: https://github.com/reichlab/covid19-forecast-hub/blob/6a7e5624ef540a55902770b7c17609d19e1f593a/data-processed/README.md
paths_to_forecast <- function(out, loc = "13", wks_ahead = 1:6, hop, fdt) {
  if(any(wks_ahead > 20)){
    stop("Max weeks ahead accepted is 20", .call = FALSE)
  }
  
  out2 <- 
    out %>% group_by(Rep) %>% arrange(Date) %>% 
    # ERIC MARTY RESTORED CREATION OF day_diff
    mutate(day_diff = c(NA, diff(Date))) # %>% 
    #mutate(cum_deaths = cumsum(deaths),
    #       day_diff = c(NA, diff(Date)))
  
  #prior_deaths <- hop %>% 
   # filter(target_end_date < fdt & location == loc & 
    #         target_type == "wk ahead cum death") %>%
  #  arrange(target_end_date) %>%
   # pull("value") %>%
    #tail(n = 1)
  #out2$cum_deaths <- out2$cum_deaths + prior_deaths
  
  take_back_step <- lubridate::wday(fdt, label = TRUE) %in% c("Sun", "Mon")
  fdt_yr <- lubridate::year(fdt)
  fdt_wk <- lubridate::epiweek(fdt)
  fdt_sun <- MMWRweek::MMWRweek2Date(fdt_yr, fdt_wk)
  if (take_back_step){
    week0_sun <- fdt_sun - 7
  } else {
    week0_sun <- fdt_sun
  }
  forecast_epiweeks <- (week0_sun + wks_ahead * 7) %>% lubridate::epiweek()
  if(any(na.omit(out2$day_diff) > 7)){
    stop("Non-continuous series of weeks, unable to compute cumulative forecasts", 
         .call = FALSE)
  }
  
  weekly <- out2 %>%
    as_tibble() %>%
    mutate(epiweek = lubridate::epiweek(Date)) %>%
    filter(epiweek %in% forecast_epiweeks) %>%
    rename("target_end_date" = Date) %>%
    nest(data = c(Rep, cases, deaths)) %>%
    mutate(pred_df = purrr::map(data, samp_to_df, 
                                vars = c("inc death", "inc case"))) %>%
    select(-data) %>%
    unnest(pred_df) %>%
    mutate(target = paste((target_end_date - (week0_sun + 6)) / lubridate::ddays(7), 
                          "wk ahead", target)) %>%
    add_column(location = loc) %>%
    mutate(quantile = round(quantile, digits = 3),
           value = round(value, digits = 3)) %>%
    add_column(forecast_date = fdt) %>%
    select(forecast_date, target, target_end_date, location, type, quantile, 
           value)
  
  #weekly %>% 
   # filter(!is.na(value)) %>%
    #filter((nchar(location)) <= 2 | str_detect(target, "inc case$")) ## only case forecasts accepted for counties
}