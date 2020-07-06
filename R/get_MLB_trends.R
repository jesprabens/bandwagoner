#' Grabs MLB Google Trends data from 2004 to present
#'
#'
#' @return A data frame of containing search hits for each team
#'
#' @import dplyr
#' @import gtrendsR

get_MLB_trends <- function(){

  # use url as search term in order to get the teams as "Baseball Team" instead of "Search Term"
  # These are same order as the names that I use on teams
  keyword <- c("%2Fm%2F01yjl","%2Fm%2F051vz","%2Fm%2F061xq","%2Fm%2F06x68","%2Fm%2F01ypc","%2Fm%2F0x2p","%2Fm%2F02__x","%2Fm%2F05g76","%2Fm%2F05xvj", "%2Fm%2F03lpp_","%2Fm%2F0x0d","%2Fm%2F01ync","%2Fm%2F04mjl","%2Fm%2F07147","%2Fm%2F0713r","%2Fm%2F01d6g","%2Fm%2F01d5z","%2Fm%2F0cqt41","%2Fm%2F07l8f","%2Fm%2F07l4z","%2Fm%2F01slc","%2Fm%2F01yhm","%2Fm%2F02d02","%2Fm%2F049n7","%2Fm%2F0512p","%2Fm%2F03m1n","%2Fm%2F04wmvz","%2Fm%2F05m_8","%2Fm%2F06wpc","%2Fm%2F07l8x")

  teams <- c("Chicago Cubs", "Milwaukee Brewers", "Pittsburgh Pirates", "St. Louis Cardinals", "Cincinnati Reds", "Atlanta Braves", "Miami Marlins", "New York Mets", "Philadelphia Phillies", "Washington Nationals", "Arizona Diamondbacks", "Colorado Rockies", "Los Angeles Dodgers", "San Diego Padres", "San Francisco Giants", "Baltimore Orioles", "Boston Red Sox", "New York Yankees", "Tampa Bay Rays", "Toronto Blue Jays", "Chicago White Sox", "Cleveland Indians", "Detroit Tigers", "Kansas City Royals", "Minnesota Twins", "Houston Astros", "Los Angeles Angels", "Oakland Athletics", "Seattle Mariners", "Texas Rangers")

  #data frame with both teams and urls
  teams_with_url <- as.data.frame(cbind(teams, keyword))

  # dataframe with cubs as first column and every other team as second column
  all_urls <- cbind("%2Fm%2F01yjl", keyword)

  # creates a data set with all 30 MLB teams and hits from Jan 2004 to the current month
  trends <- NULL
  for(i in 1:nrow(all_urls)){
    res <- gtrends(keyword = all_urls[i,], geo = "US", time = "all")$interest_over_time
    trends <- rbind(trends, res)
  }

  # makes the date and each team a variable
  dat <- trends %>%
    select(date, hits, keyword) %>%
    distinct()

  final <- left_join(dat, teams_with_url, by = "keyword") %>%
    select(-"keyword")

  #ISSUE: takes out duplicate rows for cubs, not sure why they're in there, sometimes change location
  final <- final[-c(200:216, 2605:2621),]

  # converts data frame to csv
  write.csv(final, "C:/Users/espra/OneDrive/Documents/CP/Research - Bandwagon/MLB_Google_Trends.csv")

  return(final)
}


