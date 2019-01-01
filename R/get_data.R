#' Last.FM last.year data
#' 
#' This class can be used to import data for a specific  \url{http://www.last.fm} user
#' from a specific year by calling the user.getRecentTracks (\url{https://www.last.fm/api/show/user.getRecentTracks})
#' function. The whole data will be stored inside the data_table \code{data.frame} of the 
#' class.
#' 
#' @name UserData
#' @field username \code{character} The \url{http://www.last.fm} user whos data shall be taken
#' @field apikey \code{character} The \url{http://www.last.fm} API key of your app or any app
#' @field timezone \code{numeric} Compared time to GMT, for Warsaw or Berlin it's +1
#' @field data_table \code{data.frame} A data table containing the last years tracks of the
#' user in the columns
#' \itemize{
#'\item{artist:}{ Artist Name}
#'\item{track:}{ Track Name}
#'\item{album:}{ Album Name}
#'\item{uts:}{ Time as a Javascript time starting 1970-01-01}
#'\item{datetext:}{ Time as Human readible text}
#'}
#' @usage data <- UserData$new(username, API_KEY, year)
#' 
#' album_analysis <- data$albumstats()
#' 
#' @docType class
#' @param username \code{character} username \code{character} The \url{http://www.last.fm} user whos data shall be taken
#' @param API_KEY \code{character} The \url{http://www.last.fm} API key of your app or any app
#' @param year \code{integer} Year which shall be analyzed
#' @param timezone \code{numeric} Compared time to GMT, for Warsaw or Berlin it's +1
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @section Methods:
#' \describe{
#'   \item{\code{create_api(page)}}{This method uses \code{page} to create the API json call needed for last.fm}
#'   \item{\code{get_data(year)}}{This method downloads the last.fm data of a specific year into the data_table field}
#'   \item{\code{album_stats(exclude_artist="Die drei ???",exclude_album="",min_tracks=5,sort_by=c("by_total_count","by_album_count"))}}{
#'   This method creates a table with album statistics for the specific year}

#' }
UserData <- R6Class("UserData",
                  public = list(
                    username = NULL,
                    apikey = NULL,
                    json_data=NULL,
                    timezone=1,
                    year=numeric(0),
                    data_table=data.frame(
                      "artist.#text"=character(0),
                      "name"=character(0),
                      "album.#text"=character(0),
                      "date.uts"=character(0),
                      "date.#text"=character(0)
                    ),
                    initialize = function(username = NA, API_KEY = NA, year=NULL, timezone=1) {
                      self$username <- username
                      self$timezone <- timezone
                      self$apikey <- API_KEY
                      self$year <- year
                      self$get_data(year)
                    },
                    # Function to create the API call
                    create_api = function(page=1){
                      
                      paste0(
                        "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=",
                        self$username,
                        "&api_key=",
                        self$apikey,
                        "&format=json&limit=200&page=",
                        page)
                    },
                    get_data = function(year) {
                      
                      # Function to get the data of a specific year
                      stop_sign = FALSE
                      is_test = FALSE
                      page=1
                      
                      if(year=="test"){
                        year = as.numeric(gsub("\\-[0-9]{2}\\-[0-9]{2}","",Sys.Date()))
                        is_test <- TRUE
                      }
                      
                      max_time <- as.POSIXct(paste0("31 12 ",year,", 23:59"),origin="1899-12-30",format="%d %m %Y, %H:%M")
                      min_time <- as.POSIXct(paste0("01 01 ",year,", 00:00"),origin="1899-12-30",format="%d %m %Y, %H:%M")
                      
                      while(!stop_sign){
                        
                        json_data <- fromJSON(self$create_api(page))$recenttracks
                        
                        x_test <- json_data$track %>% flatten(recursive=TRUE) %>% select(c("artist.#text","name","album.#text","date.uts","date.#text"))
                        
                        x_test$date.uts <- sapply(x_test$date.uts,function(x){
                          as.POSIXct(as.numeric(x),origin="1970-01-01",tz="GMT")
                        })
                        
                        self$data_table <- rbind(self$data_table,x_test)
                        
                        if(is_test){
                          stop_sign=TRUE
                          
                        }
                        
                        max_index <- which(self$data_table$date.uts < max_time)
                        min_index <- which(self$data_table$date.uts > min_time)
                        
                        print(paste("Getting data until: ",self$data_table[max(min_index),"date.#text"],"including",dim(self$data_table)[1],"Tracks"))
                        
                        if(dim(self$data_table)[1] > max(min_index)){
                          stop_sign=TRUE
                        }
                         page=page+1
                      }
                      
                      max_index <- which(self$data_table$date.uts < max_time)
                      min_index <- which(self$data_table$date.uts > min_time)
                      take_index <- intersect(max_index,min_index)
                      
                      self$data_table <-self$data_table[take_index,]
                      colnames(self$data_table)<-c("artist","track","album","uts","datetext")
                    },
                    albumstats = function(exclude_artist="Die drei ???",exclude_album="",min_tracks=5,sort_by=c("by_total_count","by_album_count")) {
                      
                      
                      albumlabel <- self$data_table %>% 
                        group_by(album) %>%
                        select(album,artist)%>% distinct(album,artist)
                      
                      albumstats <- self$data_table %>% 
                        group_by(album) %>%
                        summarise(n = n(),
                                  count = n_distinct(track),
                                  count_by_track=n()/n_distinct(track))
                        
                      if(sort_by=="by_total_count"){
                        album_data <- albumlabel %>% left_join(albumstats) %>% filter(count>min_tracks) %>%
                          arrange(desc(count)) %>% filter(artist!=exclude_artist) %>% filter(album!=exclude_album) 
                      }else{
                        
                      album_data <- albumlabel %>% left_join(albumstats) %>% filter(count>min_tracks) %>%
                        arrange(desc(count_by_track)) %>% filter(artist!=exclude_artist) %>% filter(album!=exclude_album) 
                      }
                      
                      return(album_data)
                    }
                  )
)

#as.POSIXct("01 Jan 2018, 00:58", origin="1899-12-30",format="%d %b %Y, %H:%M") 
