# par_sys <- list(
#     alpha = 225.61302,
#     beta = 3.5031918,
#     lambda = 0.01405405,
#     T = 200,
#     tp = 10
# )
# 
# simuler_trajectoire <- function(param, info = FALSE) {
#     date_panne <- rweibull(1, scale = param$alpha, shape = param$beta)
#     if (info) {print(date_panne)}
#     if (date_panne <= param$T) {
#         if (info) {print(date_panne + rexp(1, rate = param$lambda))}
#         return(date_panne + rexp(1, rate = param$lambda))
#     } else {
#         if (info) {print(param$T + param$tp)}
#         return(param$T + param$tp)
#     }
# }
# 
# list_traj <- c()
# for (i in 1:100000) {
#     list_traj <- c(list_traj,
#                    simuler_trajectoire(par_sys))
# }
# print(mean(list_traj))
# 
# real <- pweibull(par_sys$T, scale = par_sys$alpha, shape = par_sys$beta)
# print(
#     real *
#         ((par_sys$alpha * gamma(1+(1/par_sys$beta))) + (1/par_sys$lambda)) +
#         (1-real) *
#         (par_sys$T + par_sys$tp)
# )

alpha = 225.61302
beta = 3.5031918
lambda = 0.01405405
T = 400
tp = 10

esp_Tf <- alpha * gamma(1 + 1/beta)
esp_D <- 1/lambda

list_traj <- c()
for (i in 1:10000) {
    Tf <- rweibull(1, shape = beta, scale = alpha)
    D <- rexp(1, rate = lambda)
    if (Tf <= T) {
        list_traj <- c(list_traj, Tf + D)
    } else {
        list_traj <- c(list_traj, T + tp)
    }
}
mean(list_traj)

FT <- pweibull(T, shape = beta, scale = alpha)
print(FT * (esp_Tf + esp_D) + (1-FT) * (T + tp))

sec_T <- seq(1,500,1)
res_A <- seq(1,500,1)
res_B <- seq(1,500,1)
for (t in sec_T) {
    list_traj <- c()
    for (i in 1:10000) {
        Tf <- rweibull(1, shape = beta, scale = alpha)
        D <- rexp(1, rate = lambda)
        if (Tf <= t) {
            list_traj <- c(list_traj, Tf + D)
        } else {
            list_traj <- c(list_traj, t + tp)
        }
    }
    res_A[t] <- mean(list_traj)
    
    FT <- pweibull(t, shape = beta, scale = alpha)
    res_B[t] <- (FT * (esp_Tf + esp_D) + (1-FT) * (t + tp))
}

ggplot() +
    geom_line(aes(x = sec_T, y = res_A, color = "Par simulation")) +
    geom_line(aes(x = sec_T, y = res_B, color = "Analytique")) +
    labs(x = "Valeur T : temps avant inspection",
         y = "Espérance de Tr",
         title = "Comparaison de l'évolution de l'espérance de Tr en fonction
         de T") +
    scale_color_manual(name = "Mode de calcul",
                       values = c("Par simulation" = "blue",
                                  "Analytique" = "red")) +
    custom_theme