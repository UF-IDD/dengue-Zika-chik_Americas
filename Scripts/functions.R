## Helper functions and related citations


## copied from EpiWeek package (version 1.1) which was discontinued
##  @Manual{,
##    title = {EpiWeek: Conversion Between Epidemiological Weeks and Calendar Dates},
##    author = {Xiahong Zhao},
##    year = {2016},
##    note = {R package version 1.1},
##    url = {https://CRAN.R-project.org/package=EpiWeek},
##  }
epiweekToDate = function (year, weekno, firstday = "Sunday") 
{
  if (!(firstday == "Sunday" || firstday == "Monday")) {
    print("Wrong firstday!")
    break
  }
  if (year < 0 || weekno < 0) {
    print("Wrong Input!")
    break
  }
  jan4 = strptime(paste(year, 1, 4, sep = "-"), format = "%Y-%m-%d")
  wday = jan4$wday
  wday[wday == 0] = 7
  wdaystart = ifelse(firstday == "Sunday", 7, 1)
  if (wday == wdaystart) 
    weekstart = jan4
  if (wday != wdaystart) 
    weekstart = jan4 - (wday - ifelse(firstday == "Sunday", 
                                      0, 1)) * 86400
  jan4_2 = strptime(paste(year + 1, 1, 4, sep = "-"), format = "%Y-%m-%d")
  wday_2 = jan4_2$wday
  wday_2[wday_2 == 0] = 7
  wdaystart_2 = ifelse(firstday == "Sunday", 7, 1)
  if (wday_2 == wdaystart_2) 
    weekstart_2 = jan4_2
  if (wday_2 != wdaystart_2) 
    weekstart_2 = jan4_2 - (wday_2 - ifelse(firstday == "Sunday", 
                                            0, 1)) * 86400
  if (weekno > ((weekstart_2 - weekstart)/7)) {
    print(paste("There are only ", (weekstart_2 - weekstart)/7, 
                " weeks in ", year, "!", sep = ""))
    break
  }
  d0 = weekstart + (weekno - 1) * 7 * 86400
  d1 = weekstart + (weekno - 1) * 7 * 86400 + 6 * 86400
  return(list(d0 = strptime(d0, format = "%Y-%m-%d"), d1 = strptime(d1, 
                                                                    format = "%Y-%m-%d")))
}



## Convergence functions from the ShinyStan R package
##
## Stan Development Team. 2018. ShinyStan: Interactive Visual and Numerical 
## Diagnostics and Posterior Analysis for Bayesian Models. R package version 2.5.0.   
## http://mc-stan.org


# n_eff_warnings -----------------------------------------------------------
.n_eff_warnings <- function(summary, threshold = 10,
                            N_total = NULL) {
  n_eff <- summary[,"n_eff"]
  warn_params <- names(which(n_eff / N_total < threshold / 100))
  ll <- length(warn_params)
  if (ll == 0) "None"
  else paste0(warn_params, collapse = ", ")
}

# rhat_warnings -----------------------------------------------------------
.rhat_warnings <- function(summary, threshold = 1.10) {
  rhat <- summary[,"Rhat"]
  warn_params <- names(which(rhat > threshold))
  ll <- length(warn_params)
  if (ll == 0) "None"
  else paste0(warn_params, collapse = ", ")
}

# mcse_over_sd_warnings -----------------------------------------------------------
.mcse_over_sd_warnings <- function(summary, threshold = 10) {
  dat <- summary[,c("se_mean", "sd")]
  warn_params <- names(which(dat[,1] > (threshold/100) * dat[,2]))
  
  ll <- length(warn_params)
  if (ll == 0) "None"
  else paste0(warn_params, collapse = ", ")
}

#############
# bivariate plot ----------------------------------------------------------
.bivariate_plot <- function(samps, sp = NULL, max_td = NULL,
                            param, param2,
                            pt_alpha = 0.10,
                            pt_size = 2,
                            pt_shape = 10,
                            pt_color = "gray20",
                            ellipse_color = "black",
                            ellipse_lev = "None",
                            ellipse_lty = 1,
                            ellipse_lwd = 1,
                            ellipse_alpha = 1,
                            lines = "back",
                            lines_color = "gray",
                            lines_alpha,
                            points = TRUE,
                            transform_x = "identity",
                            transform_y = "identity"
){
  
  shape_translator <- function(x) {
    shape <- if (x >= 6) x + 9 else x
    shape
  }
  
  params <- c(param, param2)
  nParams <- 2
  nIter <- dim(samps)[1] * dim(samps)[2]
  samps_use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps_use) <- params
  
  t_x <- get(transform_x)
  # t_x <- function(x) eval(parse(text = transform_x))
  t_y <- get(transform_y)
  x_lab <- if (transform_x != "identity")
    paste0(transform_x, "(", param, ")") else param
  y_lab <- if (transform_y != "identity")
    paste0(transform_y, "(", param2, ")") else param2
  param_labs <- labs(x = x_lab, y = y_lab)
  
  dat <- data.frame(
    x = if (transform_x == "identity")
      samps_use[,param] else t_x(samps_use[,param]),
    y = if (transform_y == "identity")
      samps_use[,param2] else t_y(samps_use[,param2]))
  if (!is.null(sp)) {
    dat$divergent <- c(sapply(sp, FUN = function(y) y[, "divergent__"]))
    dat$hit_max_td <- if (is.null(max_td)) 0 else
      c(sapply(sp, FUN = function(y) as.numeric(y[, "treedepth__"] == max_td)))
  } else {
    dat$divergent <- 0
    dat$hit_max_td <- 0
  }
  graph <- ggplot(dat, aes(x = x, y = y, xend=c(tail(x, n=-1), NA),
                           yend=c(tail(y, n=-1), NA)))
  
  if (lines == "hide") {
    graph <- graph + geom_point(alpha = pt_alpha, size = pt_size,
                                shape = shape_translator(pt_shape),
                                color = pt_color)
  } else { # if lines = "back" or "front"
    if (lines == "back") {
      graph <- graph +
        geom_path(alpha = lines_alpha, color = lines_color) +
        geom_point(alpha = pt_alpha, size = pt_size,
                   shape = shape_translator(pt_shape), color = pt_color)
    } else { # lines = "front"
      graph <- graph +
        geom_point(alpha = pt_alpha, size = pt_size,
                   shape = shape_translator(pt_shape), color = pt_color) +
        geom_path(alpha = lines_alpha, color = lines_color)
    }
  }
  if (ellipse_lev != "None")
    graph <- graph + stat_ellipse(level = as.numeric(ellipse_lev), color = ellipse_color,
                                  linetype = ellipse_lty, size = ellipse_lwd, alpha = ellipse_alpha)
  if (!all(dat$divergent == 0))
    graph <- graph + geom_point(data = subset(dat, divergent == 1), aes(x,y),
                                size = pt_size + 0.5, shape = 21,
                                color = "#570000", fill = "#ae0001")
  if (!all(dat$hit_max_td == 0))
    graph <- graph + geom_point(data = subset(dat, hit_max_td == 1), aes(x,y),
                                size = pt_size + 0.5, shape = 21,
                                color = "#5f4a13", fill = "#eeba30")
  graph + param_labs +
    theme_classic() %+replace% (no_lgnd + axis_labs + fat_axis + axis_color + transparent)
}
