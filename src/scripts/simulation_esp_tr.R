# Paramètre de mon système en heures
# Loi de durée de vie du système (ici données pour le composant E)
alpha <- 225.61302
beta <- 3.5031918
# Loi de durée de réparation (données pour E)
lambda <- 0.8432432
# Périodes de remplacement périodique
T <- 200
# Temps de remplacement périodique
tp <- 10 / 60

# MTTF du composant
esp_Tf <- alpha * gamma(1 + 1/beta)
# Durée moyenne de remplacement périodique du composant
esp_D <- 1/lambda

# Routine pour calculer par simulation l'espérance de la date de remise en
# service E[Tr]
# Simuler une trajectoire revient à simuler le système et récupérer la date
# de remise en service Tr
# Pour obtenir la moyenne, on recommence le processus un grand nombre de fois et
# on prend la moyenne

# contient les trajectoires du système.
list_traj <- c()
p <- 10000 # nombre de réplications
for (i in 1:p) {
    # simule une date de panne aléatoire
    Tf <- rweibull(1, shape = beta, scale = alpha)
    # simule un temps de réparation corrective aléatoire
    D <- rexp(1, rate = lambda)
    if (Tf <= T) {
        # Dans ce cas maintenance corrective
        list_traj <- c(list_traj, Tf + D)
    } else {
        # Dans ce cas maintenance préventive
        list_traj <- c(list_traj, T + tp)
    }
}
mean(list_traj)

# Probabilité d'obtenir une maintenance corrective
FT <- pweibull(T, shape = beta, scale = alpha)
# calcul théorique
print(FT * (esp_Tf + esp_D) + (1-FT) * (T + tp))