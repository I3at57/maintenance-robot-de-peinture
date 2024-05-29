# =====================================================
# Fonction pour simuler l'espérance de Tr en fonction :
# - du composant choisi
# - de la période d'inspection T choisie
# - du nombre de réplication p pour le calcul de la moyenne
#
# Les paramètres doivent être passés en argument au format
# param = list(
#	alpha = X, # Loi de durée de vie du composant
#	beta = X, # Loi de durée de vie du composant
#	lambda = X, # Loi de remise en service
#	T = X, # Période d'inspection
#	tp = X # durée de remplacement périodique
# )
#
# /!\/!\/!\ Attention aux unités, elles doivent être cohérentes /!\/!\/!\

simulation_esp_Tr <- function(param, p = 1000) { 
	# MTTF du composant
	esp_Tf <- param$alpha * gamma(1 + 1/param$beta)
	# Durée moyenne de remplacement périodique du composant
	esp_D <- 1/param$lambda
	
	# Routine pour calculer par simulation l'espérance de la date de remise en
	# service E[Tr]
	# Simuler une trajectoire revient à simuler le système et récupérer la date
	# de remise en service Tr
	# Pour obtenir la moyenne, on recommence le processus un grand nombre de fois et
	# on prend la moyenne
	
	# contient les trajectoires du système.
	list_traj <- c()
	for (i in 1:p) {
		# simule une date de panne aléatoire
		Tf <- rweibull(1, shape = param$beta, scale = param$alpha)
		# simule un temps de réparation corrective aléatoire
		D <- rexp(1, rate = param$lambda)
		if (Tf <= T) {
			# Dans ce cas maintenance corrective
			list_traj <- c(list_traj, Tf + D)
		} else {
			# Dans ce cas maintenance préventive
			list_traj <- c(list_traj, T + param$tp)
		}
	}
	
	# Renvoi la valeur moyenne E[Tr]
	return(mean(list_traj))
}

# =====================================================
# Fonction pour simuler l'espérance de CCr en fonction :
# - du composant choisi
# - de la période d'inspection T choisie
# - du nombre de réplication p pour le calcul de la moyenne
#
# Les paramètres doivent être passés en argument au format
# param = list(
#	alpha = X, # Loi de durée de vie du composant
#	beta = X, # Loi de durée de vie du composant
#	lambda = X, # Loi de remise en service
#	T = X, # Période d'inspection
#	tp = X # durée de remplacement périodique
#	ci = X, # coût d'interruption
#	cc = X, # coût de changement
#	
# )
#
# /!\/!\/!\ Attention aux unités, elles doivent être cohérentes /!\/!\/!\

simulation_esp_CCr <- function(param, p = 1000) {
	
	# Routine pour calculer par simulation l'espérance du coût à date de remise
	# en service E[CCr]
	# Simuler une trajectoire revient à simuler le système et récupérer le coût total
	# à date de remplacement
	# Pour obtenir la moyenne, on recommence le processus un grand nombre
	# de fois et on prend la moyenne
	
	# contient les trajectoires du système.
	list_traj <- c()
	for (i in 1:p) {
		# Simule une date de panne
		Tf <- rweibull(1, shape = param$beta, scale = param$alpha)
		# Simule une durée de réparation
		D <- rexp(1, rate = param$lambda)
		if (Tf <= param$T) {
			# Maintenance corrective
			list_traj <- c(list_traj, param$cc + D * param$ci)
		} else {
			# Maintenance préventive
			list_traj <- c(list_traj, param$cc + param$tp * param$ci)
		}
	}
	return(mean(list_traj))
}

# =====================================================
# Fonction pour simuler l'espérance de CUT en fonction :
# - du composant choisi
# - de la période d'inspection T choisie
# - du nombre de réplication p pour le calcul de la moyenne
#
# Les paramètres doivent être passés en argument au format
# param = list(
#	alpha = X, # Loi de durée de vie du composant
#	beta = X, # Loi de durée de vie du composant
#	lambda = X, # Loi de remise en service
#	T = X, # Période d'inspection
#	tp = X # durée de remplacement périodique
#	ci = X, # coût d'interruption
#	cc = X, # coût de changement
#	
# )
#
# /!\/!\/!\ Attention aux unités, elles doivent être cohérentes /!\/!\/!\

simulation_esp_cut <- function(param, p = 1000) {
	
	# Routine pour calculer par simulation l'espérance du coût à date de remise en
	# service E[CCr]
	# Simuler une trajectoire revient à simuler le système et récupérer le coût
	# total à date de remplacement
	# Pour obtenir la moyenne, on recommence le processus un grand nombre de fois
	# et on prend la moyenne
	
	# contient les trajectoires du système.
	list_traj <- list(temps = c(), cout = c())
	for (i in 1:p) {
		# Simule une date de panne
		Tf <- rweibull(1, shape = param$beta, scale = param$alpha)
		# Simule une durée de réparation
		D <- rexp(1, rate = param$lambda)
		if (Tf <= param$T) {
			# Maintenance corrective
			list_traj$temps = c(list_traj$temps, Tf + D)
			list_traj$cout = c(list_traj$cout, param$cc + D * param$ci)
		} else {
			# Maintenance préventive
			list_traj$temps = c(list_traj$temps, param$T + param$tp)
			list_traj$cout = c(list_traj$cout,
							   param$cc + param$tp * param$ci)
		}
	}
	
	return(mean(list_traj$cout)/mean(list_traj$temps))
}