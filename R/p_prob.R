#' @title Pareto Probability distribution
#' @description Given a specified number of points \code{n},
#' this function generates an \code{n} probability values
#' in accordance with a specified Pareto ratio.
#' @param npoints (an integer) Number of points. Default is
#' \code{50}.
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio. For instance, for a
#' \code{20:80} ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Input values must be \code{10}, \code{20},
#' \code{30}, \code{40}, or \code{50}. The 'p_ratio'
#' determines the proportion of points that are the most
#' dominant event generators.
#' @usage p_prob(npoints,  p_ratio = 30)
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' @importFrom magrittr %>%
#' @export

p_prob <- function(npoints =  50, p_ratio = 30){

  mutate <- area <- total <-  NULL

  #Table showing the Pareto ratio value and the corresponding
  #pre-determined
  #exponential constant for different
  #Pareto ratio
  exp_param_table <- data.frame(ratio=seq(10, 40, 10),
                                constant = c(2.3, 0.8, 0.39,
                                             0.17, 0.004))

  if(!p_ratio %in% exp_param_table$ratio){
    stop(paste(" 'p_ratio' needs to be one of the",
               "following values: [10, 20, 30, 40]", sep = " "))
  }

#}

  n <- npoints

  #get the constant
  constant<-exp_param_table[which(exp_param_table$ratio == p_ratio),
                              2]

  #determine q, the decay (expo. denting) parameter
  q <- constant/(n/10)
  #plot(y,exp(q*y))

  p_table <- NULL #area table
  #steps <- n

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


# 0.2*50
# sum(p_table[1:40])/sum(p_table[1:50])

#0.7 * 50# sum(p_table[41:50])/sum(p_table[1:50])

