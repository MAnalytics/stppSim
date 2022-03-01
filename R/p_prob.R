#' @title Pareto (probability) distribution
#' @description Generates an \code{n} probability
#' values in accordance with a specified Pareto ratio.
#' @param n (an integer) Number of data points.
#' @param p_ratio (an integer) The smaller of the
#' terms of a Pareto ratio. For instance, for a
#' \code{20:80} ratio, `p_ratio` is \code{20}.
#' Default value is set as
#' \code{20}. Acceptable `p_ratio` values
#' are: (\code{5, 10, 20, 30, 40}).
#' @usage p_prob(n,  p_ratio = 20)
#' @examples
#' p_prob(n = 15,  p_ratio = 20)
#' @details Produces a list probability values
#' based on the specified Pareto ratio. Each ratio
#' term is assigned the proportion of area under
#' an exponential curve that is equal to
#' the value of the other ratio term. For example,
#' for a \code{20:80} ratio, 20% of data points
#' covers 80% of areas under the curve, and vice
#' versa.
#' @return Returns a vector of probability values
#' @importFrom magrittr %>%
#' @export

p_prob <- function(n = 50, p_ratio = 20){

  mutate <- area <- total <-  NULL

  #Table showing the Pareto ratio value and the corresponding
  #pre-determined exponential constant for different
  #Pareto ratio
  exp_param_table <- data.frame(ratio=c(5, seq(10, 40, 10)),
                                constant = c(6.0, 2.3, 0.8, 0.39,
                                             0.17))

  if(!p_ratio %in% exp_param_table$ratio){
    stop(paste(" 'p_ratio' needs to be one of the",
               "following values: [5, 10, 20, 30, 40]", sep = " "))
  }


  #get the constant
  constant<-exp_param_table[which(exp_param_table$ratio ==
                                    p_ratio),2]

  #determine q, the decay (expo. denting) parameter
  q <- constant/(n/10)

  p_table <- NULL #area table

  for(i in 1:n){ #i<-0.5

    if(i == 1){
      area_i <- exp(q*i) - exp(q*(0))
      p_table <- c(p_table, area_i)
    }
    if(i != 1){
      area_i <- exp(q*i) - exp(q*(i-1))
      p_table <- c(p_table, area_i)
    }
  }

    p_table <- data.frame(sn=1:n, area=p_table)

    p_table <- p_table %>%
      mutate(total=sum(area))%>%
      mutate(prob=round(area/total, digits = 8))

  return(p_table)

}
