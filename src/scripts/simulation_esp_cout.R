# Paramètre de mon système en heures
# Loi de durée de vie du système (ici données pour le composant E)
alpha <- 225.61302
beta <- 3.5031918
# Loi de durée de réparation (données pour E)
lambda <- 0.8432432
# Périodes de remplacement périodique
T <- 800
# Temps de remplacement périodique en heures
tp <- 10 / 60
# coût de changement fixe
cc <- 30
# coût d'indisponibilité par heures
ci <- 20 * 60

# Durée moyenne de remplacement périodique du composant
esp_D <- 1/lambda

# Routine pour calculer par simulation l'espérance du coût à date de remise en
# service E[CCr]
# Simuler une trajectoire revient à simuler le système et récupérer le coût total
# à date de remplacement
# Pour obtenir la moyenne, on recommence le processus un grand nombre de fois et
# on prend la moyenne

# nombre de réplications
p <- 20000
# contient les trajectoires du système.
list_traj <- c()
for (i in 1:p) {
    # Simule une date de panne
    Tf <- rweibull(1, shape = beta, scale = alpha)
    # Simule une durée de réparation
    D <- rexp(1, rate = lambda)
    if (Tf <= T) {
        # Maintenance corrective
        list_traj <- c(list_traj, cc + D * ci)
    } else {
        # Maintenance préventive
        list_traj <- c(list_traj, cc + tp * ci)
    }
}
mean(list_traj)

# Probabilité d'obtenir une maintenance corrective
FT <- pweibull(T, shape = beta, scale = alpha)
# calcul théorique
print(FT * (cc + esp_D * ci) + (1-FT) * (cc + tp * ci))
