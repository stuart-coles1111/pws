ski_strengths <- function(p1, weight = rep(10, 3)){
    p1 <- as.numeric(p1)

    s1_1 <- pws:::ski_contribution(p1[1], weight[1])
    s2_1 <- pws:::ski_contribution(p1[2], weight[2])
    s3_1 <- pws:::ski_contribution(p1[3], weight[3])


    c_1 <- (s1_1 + s2_1 + s3_1)

    list(
        a_1=c(s1_1, s2_1, s3_1),
        s=c_1
    )
}
