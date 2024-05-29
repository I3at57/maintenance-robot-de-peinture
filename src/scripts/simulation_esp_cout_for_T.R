# Calcule par simulation l'espérance du coût à date de remise en service E[CCr]
# pour plusieurs valeurs de T

# Paramètre de mon système en heures
# Loi de durée de vie du système (ici données pour le composant E)
alpha <- 225.61302
beta <- 3.5031918
# Loi de durée de réparation (données pour E)
lambda <- 0.8432432
# Périodes de remplacement périodique
T <- 200
# Temps de remplacement périodique en heures
tp <- 10 / 60
# coût de changement fixe
cc <- 30
# coût d'indisponibilité par heures
ci <- 20 * 60

# Durée moyenne de remplacement périodique du composant
esp_D <- 1/lambda

# On va faire varier T tout en appliquant la logique précédente pour calculer
# par simulation sur un seul T

T_lim <- 500
sec_T <- seq(1,T_lim,1)
res_A <- seq(1,T_lim,1)
res_B <- seq(1,T_lim,1)
for (t in sec_T) {
	# nombre de réplications
	p <- 5000
	# contient les trajectoires du système.
	list_traj <- c()
	for (i in 1:p) {
		# Simule une date de panne
		Tf <- rweibull(1, shape = beta, scale = alpha)
		# Simule une durée de réparation
		D <- rexp(1, rate = lambda)
		if (Tf <= t) {
			# Maintenance corrective
			list_traj <- c(list_traj, cc + D * ci)
		} else {
			# Maintenance préventive
			list_traj <- c(list_traj, cc + tp * ci)
		}
	}
	res_A[t] <- mean(list_traj)
	
	# Probabilité d'obtenir une maintenance corrective
	FT <- pweibull(t, shape = beta, scale = alpha)
	# calcul théorique
	res_B[t] <- FT * (cc + esp_D * ci) + (1-FT) * (cc + tp * ci)
}

# Afficher les résultats
ggplot() +
	geom_line(aes(x = sec_T, y = res_A, color = "Par simulation")) +
	geom_line(aes(x = sec_T, y = res_B, color = "Analytique")) +
	labs(x = "Valeur T : temps avant inspection",
		 y = "Espérance du coût total CCr",
		 title = "Comparaison de l'évolution de l'espérance de CCr en fonction
         de T") +
	scale_color_manual(name = "Mode de calcul",
					   values = c("Par simulation" = "blue",
					   		   "Analytique" = "red")) +
	custom_theme