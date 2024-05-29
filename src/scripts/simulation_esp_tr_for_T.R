# Routine pour calculer par simulation l'espérance de la date de remise en
# service E[Tr] pour plusieurs valeurs de T

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

# On va faire varier T tout en appliquant la logique précédente pour calculer
# par simulation sur un seul T

T_lim <- 500
sec_T <- seq(1,T_lim,1)
res_A <- seq(1,T_lim,1)
res_B <- seq(1,T_lim,1)
for (t in sec_T) {
	# contient les trajectoires du système.
	list_traj <- c()
	p <- 2000 # nombre de réplications
	for (i in 1:p) {
		# simule une date de panne aléatoire
		Tf <- rweibull(1, shape = beta, scale = alpha)
		# simule un temps de réparation corrective aléatoire
		D <- rexp(1, rate = lambda)
		if (Tf <= t) {
			# Dans ce cas maintenance corrective
			list_traj <- c(list_traj, Tf + D)
		} else {
			# Dans ce cas maintenance préventive
			list_traj <- c(list_traj, t + tp)
		}
	}
	res_A[t] <- mean(list_traj)
	
	
	# Probabilité d'obtenir une maintenance corrective
	FT <- pweibull(t, shape = beta, scale = alpha)
	# calcul théorique
	res_B[t] <- FT * (esp_Tf + esp_D) + (1-FT) * (t + tp)
}

# Affichage des résultats
ggplot() +
	geom_line(aes(x = sec_T, y = res_A, color = "Par simulation")) +
	geom_line(aes(x = sec_T, y = res_B, color = "Analytique")) +
	# geom_line(aes(x = sec_T, y = sec_T)) +
	# ylim(0,300) +
	# xlim(0,200) +
	labs(x = "Valeur T : temps avant inspection",
		 y = "Espérance de Tr",
		 title = "Comparaison de l'évolution de l'espérance de Tr en fonction
         de T") +
	scale_color_manual(name = "Mode de calcul",
					   values = c("Par simulation" = "blue",
					   		   "Analytique" = "red")) +
	custom_theme