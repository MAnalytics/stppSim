#' @title Pareto Probability distribution
#' @description Given a number of points, this function
#' assign probability values (signifying
#' their strength) to the points in accordance with
#' the pareto principle (i.e. 80% of the events
#' come from 20% of the locations).
#' @param npoints (an integer) Number of points
#' @usage p_prob(npoints)
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' @importFrom magrittr %>%
#'
#'
#' @export

p_prob <- function(npoints =  50){

  mutate <- area <- total <-  NULL

  n <- npoints

  q <- 0.8/(n/10)       #+#decay (denting) parameter
  #plot(y,exp(q*y))

  a_table <- NULL #area table
  #steps <- n

  for(i in 1:n){ #i<-0.5

    if(i == 1){
      area_i <- exp(q*i) - exp(q*(0))
      a_table <- c(a_table, area_i)
    }
    if(i != 1){
      area_i <- exp(q*i) - exp(q*(i-1))
      a_table <- c(a_table, area_i)
    }
  }

    a_table <- data.frame(sn=1:n, area=a_table)

    a_table <- a_table %>%
      mutate(total=sum(area))%>%
      mutate(prob=round(area/total, digits = 8))

  return(a_table)
}


# 0.2*50
# sum(a_table[1:40])/sum(a_table[1:50])

0.7 * 50# sum(a_table[41:50])/sum(a_table[1:50])

