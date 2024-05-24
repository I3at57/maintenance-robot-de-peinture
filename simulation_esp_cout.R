alpha <- 225.61302
beta <- 3.5031918
lambda <- 0.01405405
T <- 400
tp <- 10
cc <- 30
ci <- 20

esp_Tf <- alpha * gamma(1 + 1/beta)
esp_D <- 1/lambda

FT <- pweibull(T, shape = beta, scale = alpha)

S <- 0
for (p in 1:1000) {
    list_traj <- c()
    for (i in 1:1000) {
        Tf <- rweibull(1, shape = beta, scale = alpha)
        D <- rexp(1, rate = lambda)
        if (Tf <= T) {
            list_traj <- c(list_traj, cc + D * ci)
        } else {
            list_traj <- c(list_traj, cc + tp * ci)
        }
    }
    S <- S + mean(list_traj)
}
print(S/1000)

S <- 0
for (p in 1:1000) {
    D <- rexp(1, rate = lambda)
    S <- S + (FT * (cc + D * ci) + (1-FT) * (cc + tp * ci))
}
print(S/1000)

sec_T <- seq(1,500,1)
res_A <- seq(1,500,1)
res_B <- seq(1,500,1)
for (t in sec_T) {
    list_traj <- c()
    for (i in 1:10000) {
        Tf <- rweibull(1, shape = beta, scale = alpha)
        D <- rexp(1, rate = lambda)
        if (Tf <= t) {
            list_traj <- c(list_traj, cc + D * ci)
        } else {
            list_traj <- c(list_traj, cc + tp * ci)
        }
    }
    res_A[t] <- mean(list_traj)
    
    S <- 0
    FT <- pweibull(t, shape = beta, scale = alpha)
    for (p in 1:10000) {
        D <- rexp(1, rate = lambda)
        S <- S + (FT * (cc + D * ci) + (1-FT) * (cc + tp * ci))
    }
    res_B[t] <- S/10000
}

ggplot() +
    geom_line(aes(x = sec_T, y = res_A, color = "Par simulation")) +
    geom_line(aes(x = sec_T, y = res_B, color = "Analytique")) +
    labs(x = "Valeur T : temps avant inspection",
         y = "Coût total CCr",
         title = "Comparaison de l'évolution de CCr en fonction
         de T") +
    scale_color_manual(name = "Mode de calcul",
                       values = c("Par simulation" = "blue",
                                  "Analytique" = "red")) +
    custom_theme














##############################
replicate(n = 100,
    as.numeric(
        table(
            as.vector(rweibull(10000, scale = alpha, shape = beta) <= 200)
        )[2]/10000
    )
) %>% mean()
pweibull(200, scale = alpha, shape = beta)

res_for_p <- c()
for (p in 1:10000) {
    res_for_p <- c(res_for_p, mean(    
        replicate(n = 100,
                  as.numeric(
                      table(
                          as.vector(rweibull(p, scale = alpha, shape = beta) <= 200)
                      )[2]/p
                  )
        )
    )
    )
}

ggplot() +
    geom_line(aes(x = 1:10000, y = res_for_p)) +
    geom_line(aes(x = 1:10000, pweibull(200, scale = alpha, shape = beta)))
