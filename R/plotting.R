# Plotting

# Data
#x <- c(15, 9, 75, 90, 1, 1, 11, 5, 9, 8, 33, 11, 11, 20, 14, 13, 10, 28, 33, 21, 24, 25, 11, 33)

# Clock plot function
#' Taken from \url{http://www.r-graph-gallery.com/49-clock-plot/}
#' 
# clock.plot <- function (self, col = col=viridisLite::viridis(24), ...) {
#   
#   x <- self$data_table %>% select(datetext) %>%
#     rowwise() %>%
#     mutate(hour=gsub(":","",
#                      regmatches(datetext,regexpr("([0-9]{2})\\:",datetext)))) %>%
#     group_by(hour) %>%
#     summarize(count=n()) %>% select(count)
#   
#   if( min(x)<0 ) x <- x - min(x)
#   if( max(x)>1 ) x <- x/max(x)
#   n <- length(x)
#   if(is.null(names(x))) names(x) <- 0:(n-1)
#   m <- 1.05
#   plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = '', ylab = '', ...)
#   a <- pi/2 - 2*pi/200*0:200
#   polygon( cos(a), sin(a) )
#   v <- .02
#   a <- pi/2 - 2*pi/n*0:n
#   segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a) )
#   segments( cos(a), sin(a),0, 0, col = 'light grey', lty = 3) 
#   ca <- -2*pi/n*(0:50)/50
#   for (i in 1:n) {
#     a <- pi/2 - 2*pi/n*(i-1)
#     b <- pi/2 - 2*pi/n*i
#     polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[i] )
#     v <- .1
#     text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
#   }
# }

# Use the function on the created data
#clock.plot(x$count, main = "Number of visitors to a web site for each hour of the day",)
#' Clock plot function
#' Taken from \url{http://www.r-graph-gallery.com/49-clock-plot/}
#' @name clock.plot
#' @examples 
#' 
#' test_data <- UserData$new("zappingseb","b25b959554ed76058ac220b7b2e0a026","test")
#' 
#' test_data$clock.plot()
#' 
UserData$set("public", "clock.plot", function(...) {
  
  # Derive the play time per hour by a regular expression match for the two digits before
  # the ":" and grouping by them
  x <- self$data_table %>% select(datetext) %>%
    rowwise() %>%
    mutate(hour=as.numeric(gsub(":","",
                     regmatches(datetext,regexpr("([0-9]{2})\\:",datetext))))+self$timezone) %>%
    group_by(hour) %>%
    summarize(count=n())
  col=viridisLite::viridis(length(x$hour))
  
  x <- 
    x %>% rowwise() %>% mutate(hour=ifelse(hour>=24,hour-24,hour))
  #print(x)
  time_vec <- c()
  for(time in 0:23){
    
    
    
    index <- which(as.numeric(x$hour)==time)
    #browser()
    if(length(index)>0){
      time_vec <- c(time_vec,x[index,"count"])
    }else{
      time_vec <- c(time_vec,0)
    }
    
  }
  x <- unlist(time_vec)
  #browser()
  names(x)<-0:23
  
  if( min(x)<0 ) x <- x - min(x)
  if( max(x)>1 ) x <- x/max(x)
  n <- length(x)
  if(is.null(names(x))) names(x) <- 0:(n-1)
  m <- 1.05
  plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = '', ylab = '', ...)
  a <- pi/2 - 2*pi/200*0:200
  polygon( cos(a), sin(a) )
  v <- .02
  a <- pi/2 - 2*pi/n*0:n
  segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a) )
  segments( cos(a), sin(a),0, 0, col = 'light grey', lty = 3) 
  ca <- -2*pi/n*(0:50)/50
  for (i in 1:n) {
    a <- pi/2 - 2*pi/n*(i-1)
    b <- pi/2 - 2*pi/n*i
    polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[i] )
    v <- .1
    text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
  }
  
},overwrite=TRUE)

#' Get a simple barplot by week
#' 
#' @name barplots
#' @param choice_of_aggregation \code{character} How you would like to aggregate the listened tracks
UserData$set("public", "barplots", function(choice_of_aggregation=c("day","month","week","weekdays")) {
  
data_with_month <- self$data_table %>% rowwise() %>% mutate(real_date = 
                                                              as.POSIXct(as.numeric(uts),origin="1970-01-01",tz="GMT"))

if(choice_of_aggregation=="weekdays"){
  data_by_month <- data_with_month %>% group_by(month=format(as.Date(real_date), "%a")) %>%
    summarize(amount=n())
  
  if("Di" %in% data_by_month$month){
    data_by_month$month <-  factor(data_by_month$month, levels= c("Mo", "Di", 
                                          "Mi", "Do", "Fr", "Sa", "So"))
  }else{
    data_by_month$month <- factor(data_by_month$month, levels= c("Su", "Mo", 
                                             "Tu", "We", "Th", "Fr", "Sa"))
    
    
  }
  data_by_month <- data_by_month[order(data_by_month$month),]
  naming <- data_by_month %>% .$month
}else{
  
  data_by_month <- data_with_month %>% group_by(month=lubridate::floor_date(real_date, choice_of_aggregation)) %>%
    summarize(amount=n())
  naming <- data_by_month %>% .$month
}

if(choice_of_aggregation=="month"){
  naming <- data_by_month %>% mutate(month=months(month)) %>% select(month) %>% .$month
}else if(choice_of_aggregation=="week"){
  naming <- data_by_month %>% mutate(month=strftime(month,"%V")) %>% select(month) %>% .$month
}
my_vector <- data_by_month %>% select(amount) %>% flatten() %>% .$amount

names(my_vector) <- naming

barplot(my_vector)
},overwrite=TRUE)
  

