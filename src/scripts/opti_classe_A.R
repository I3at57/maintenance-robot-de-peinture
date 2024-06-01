repere <- c("E", "D", "A")
t_opti <- c()
cut_opti <- c()
for (w in repere) {
	
	# Les paramètres
	alpha = estim_max_likelihood[estim_max_likelihood$Repère == w,]$Alpha_h
	beta = estim_max_likelihood[estim_max_likelihood$Repère == w,]$Beta
	lambda = estim_param_duree_remplacement[
		estim_param_duree_remplacement$Repère == w,
	]$lambda_h
	tp = 10 / 60 # en heures
	T = 0 # en heures
	# Temps de remplacement périodique en heures
	tp = 10 / 60
	# coût de changement fixe
	cc = 30
	# coût d'indisponibilité par heures
	ci = 20 * 60
	
	p <- 20000
	T_set <- seq(50,400,1)
	n <- length(T_set)
	res_list <- c()
	for (t in T_set) {
		sumT <- 0
		sumC <- 0
		for (i in 1:p) {
			Tf <- rweibull(1, shape = beta, scale = alpha)
			D <- rexp(1, rate = lambda)
			if (Tf <= t) {
				# Maintenance corrective
				sumT = sumT + Tf + D
				sumC = sumC + cc + D * ci
			} else {
				# Maintenance préventive
				sumT = sumT + t + tp
				sumC = sumC + cc + tp * ci
			}
		}
		res_list <- append(res_list, sumC/sumT)
	}
	
	moyenne_mobile <- c()
	for (i in 3:(n-2)) {
		moyenne_mobile <- c(moyenne_mobile,
							mean(res_list[seq(i-2,i+2,1)]))
	}
	
	t_opti <- c(t_opti, T_set[which(moyenne_mobile == min(moyenne_mobile))])
	cut_opti <- c(cut_opti, min(moyenne_mobile))
}
politique_maintenance <- tibble(repere,t_opti,cut_opti)
politique_maintenance