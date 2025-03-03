#' Execute Activity 7
#'
#' Individual execution of Activity 7 of Playing With Statistics
#'
#' @param rand randomise settings? (TRUE/FALSE)
#' @param seed value of seed for rnadom number generator
#'
#' @returns  NULL
#' @examples
#' activity7_execute()
#'
#' @export
#'
#'
activity7_execute <- function(rand = FALSE, seed = NULL){

    if(!is.null(seed)) set.seed(seed)

    #base mean....
    mu <- 193

    if(rand == TRUE){
        weight <- gtools::rdirichlet(1, rep(10, 3)) * 9
        sd <- exp(rnorm(1, log(10), 0.25))
    }
    else{
        # default weights...
        weight <- c(4, 2, 3)
        # default sd...
        sd <- 10
    }



    options(warn = -1)
    bank_0 <- 10
    data_cost <- 10

    cat(paste0('Initial bank: ', bank_0, ' units\n'), fill=T)
    cat('You can buy data for analysis\n')
    cat(paste0('1 unit buys ', data_cost, ' rows of data\n'), fill=T)
    ndata_1 <- readline(prompt = "How many rows of data would you like to buy? \n ")  %>% as.numeric

    spend_1 <- ndata_1 / data_cost
    bank <- (bank_0 - spend_1) %>% round(1)

    while(bank < 0){
        cat("Not enough money", fill=T)
        ndata_1 <- readline(prompt = "How many rows of data would you like to buy? ")  %>% as.numeric
        data_spend_1  <- ndata_1 / data_cost
        bank <- (bank_0 - data_spend_1) %>% round(1)
    }


    if(ndata_1 > 0) {
        d1 <- pws:::mski_sim(ndata_1, weight = weight, mu = mu, sd = sd)
        cat('\nThese are your data:\n')
        print(d1)

        d1_r <- reshape2::melt(d1, id.vars = c("jump_length"))

        l1 <- lm(jump_length ~ technique, data = d1)$coefficients
        l2 <- lm(jump_length ~ materials, data = d1)$coefficients
        l3 <- lm(jump_length ~ fitness, data = d1)$coefficients

        ta <- rbind(l1,l2,l3)
        colnames(ta) <- c('inctercept', 'gradient')
        rownames(ta) <- c('technique', 'materials', 'fitness')

        cat("\n")
        cat('Jump Length Regressions:')
        cat("\n")
        cat("\n")

        (ggplot2::ggplot(d1_r, ggplot2::aes(value, jump_length)) +ggplot2::facet_wrap(~variable) +
                ggplot2::geom_point(colour = "steelblue") + ggplot2::geom_smooth(method=lm, colour = "red")) %>%
            print %>% suppressMessages

        l1 <- lm(jump_length ~ technique, data = d1)$coefficients
        l2 <- lm(jump_length ~ materials, data = d1)$coefficients
        l3 <- lm(jump_length ~ fitness, data = d1)$coefficients

        ta <- rbind(l1,l2,l3)
        colnames(ta) <- c('inctercept', 'gradient')
        rownames(ta) <- c('technique', 'materials', 'fitness')

        print(ta %>% round(2))
        cat("\n")
        cat("\n")
    }

    else{
        d1 <- NULL
        cat('\n You have no data to analyse\n')
    }

    choice <- -1

    cat("\n")
    cat(paste0('Your remaining bank is: ', bank,  ' units\n'), fill=T)

    fail <- TRUE

    while(fail){
        fail <- FALSE
        spend1 <- readline(prompt = "Enter 3 values separated by a comma: ") %>% strsplit(',') %>% unlist %>% as.numeric
        spend1 <- round(spend1, 2)
        if(length(spend1)!=3){
            fail <- TRUE
            cat('3 values needed\n')
        }
        else if(any(is.na(spend1))){
            fail <- TRUE
            cat('all values must be numeric\n')
        }
        else if(any(spend1 < 0)){
            fail <- TRUE
            cat('all values must be positive\n')
        }
        else if(any(spend1 > 10)){
            fail <- TRUE
            cat('maximum spend for each category is 10\n')
        }
        else if(sum(spend1) > bank){
            fail <- TRUE
            cat(paste0('maximum available total spend is ',bank),fill=T)
            cat("\n")
        }
    }

    tech_spend1 <- spend1[1]
    mat_spend1 <- spend1[2]
    fit_spend1 <- spend1[3]

    cat("\n")
    go <- readline(prompt = "Press any key for training jump\n")

    j1 <- pws:::ski_jump(c(tech_spend1, mat_spend1, fit_spend1), njump = 1, weight = weight, mu = mu, sd = sd)
    cat('your training jump distance is...\n \n', j1, ' metres',fill = T)

    cat("\n")
    go <- readline(prompt = "Press any key to continue\n")


    bank_0 <- 10
    cat(paste0('Additional funds available: ', bank_0, ' units\n'), fill=T)
    cat('You can now buy additional data for analysis\n')
    cat("\n")
    cat(paste0('1 unit buys ', data_cost, ' rows of data'), fill=T)
    cat("\n")
    ndata_2 <- readline(prompt = "How many rows of data would you like to buy? ")  %>% as.numeric
    cat("\n")

    spend_2 <- ndata_2 / data_cost
    bank <- (bank_0 - spend_2) %>% round(1)

    while(bank < 0){
        cat("Not enough money", fill=T)
        ndata_2 <- readline(prompt = "How many rows of data would you like to buy? ")  %>% as.numeric
        spend_2  <- ndata_2 / data_cost
        bank <- (bank_0 - spend_2) %>% round(1)
    }


    d2 <- pws:::mski_sim(ndata_2, weight = weight, mu = mu, sd = sd)
    d2_r <- reshape2::melt(rbind(d1,d2), id.vars = c("jump_length"))

    if((ndata_1 + ndata_2) > 0){
        cat('These are your complete data:', fill=T)
        cat("\n")

        d1 <- rbind(d1, d2)
        print(d1)
        cat("\n")

        l1 <- lm(jump_length ~ technique, data = d1)$coefficients
        l2 <- lm(jump_length ~ materials, data = d1)$coefficients
        l3 <- lm(jump_length ~ fitness, data = d1)$coefficients

        ta <- rbind(l1,l2,l3)
        colnames(ta) <- c('inctercept', 'gradient')
        rownames(ta) <- c('technique', 'materials', 'fitness')

        cat("\n")
        cat('Jump Length Regressions:')
        cat("\n")
        cat("\n")

        (ggplot2::ggplot(d2_r, ggplot2::aes(value, jump_length)) +ggplot2::facet_wrap(~variable) +
                ggplot2::geom_point(colour = "steelblue") + ggplot2::geom_smooth(method=lm, colour = "red")) %>%
            print %>% suppressMessages

        l1 <- lm(jump_length ~ technique, data = d1)$coefficients
        l2 <- lm(jump_length ~ materials, data = d1)$coefficients
        l3 <- lm(jump_length ~ fitness, data = d1)$coefficients

        ta <- rbind(l1, l2, l3)
        colnames(ta) <- c('inctercept', 'gradient')
        rownames(ta) <- c('technique', 'materials', 'fitness')

        print(ta %>% round(2))
        cat("\n")
        cat("\n")
    }
    else{
        cat("\n")
        cat('You have no data to analyse\n')
        cat("\n")
    }

    cat(paste0('Your remaining bank is: ', bank, ' units\n'), fill=T)

    cat("\n")

    cat(paste0('Choose additional spend on the 3 categories: Technique, Materials and Fitness: \n'), fill=T)
    cat("\n")

    cat(paste0('Choose spend on the 3 categories: Technique, Materials and Fitness: \n'), fill=T)
    cat(paste0('Maximum allowable remaining for each category is: \n'))
    cat(paste0('Technique: ', 10 - tech_spend1,  '\n'))
    cat(paste0('Materials: ', 10 - mat_spend1,  '\n'))
    cat(paste0('Fitness: ', 10 - fit_spend1,  '\n'))
    cat("\n")

    fail <- TRUE

    while(fail){
        fail <- FALSE
        spend2 <- readline(prompt = "Enter 3 values separated by a comma: ") %>% strsplit(',') %>% unlist %>% as.numeric
        cat("\n")
        spend2 <- round(spend2, 2)
        if(length(spend2)!=3){
            fail <- TRUE
            cat('3 values needed\n')
        }
        else if(any(is.na(spend2))){
            fail <- TRUE
            cat('all values must be numeric\n')
        }
        else if(any(spend2 < 0)){
            fail <- TRUE
            cat('all values must be positive\n')
        }
        else if(any((spend1 + spend2) > 10)){
            fail <- TRUE
            cat('maximum spend for each category is 10\n')
        }
        else if(sum(spend2) > bank){
            fail <- TRUE
            cat(paste0('maximum available total spend is ',bank),fill=T)
            cat("\n")
        }
    }

    tech_spend2 <- spend2[1]
    mat_spend2 <- spend2[2]
    fit_spend2 <- spend2[3]


    go <- readline(prompt = "Press any key for competition jump\n")

    j2 <- pws:::ski_jump(c(tech_spend1 + tech_spend2, mat_spend1 + mat_spend2, fit_spend1 + fit_spend2), njump = 1,  weight = weight, mu = mu, sd = sd)
    cat('your competition jump distance is...', j2, ' metres', fill = T)

    cat("\n")
    if(j2 > 253.5){
        cat('*** CONGRATULATIONS ***')
        cat("\n")
        cat("\n")
        cat(crayon::green('you beat the world record'))
    }
    else cat(crayon::red('you failed to beat the world record'))
    cat("\n")
    cat("\n")

    if(rand == TRUE){
        cat("\n")
        cat("weights were:\n")
        cat("Technique: ", weight[1] %>% round(1), fill=T)
        cat("Materials: ", weight[2] %>% round(1), fill=T)
        cat("Fitness: ", weight[3] %>% round(1), fill=T)
        cat("Standard deviation: ", sd %>% round(1), fill=T)
    }
    options(warn = 0)
}






