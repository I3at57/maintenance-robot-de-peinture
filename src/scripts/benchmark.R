source("../scripts/functions.R")
library(tictoc)

vals <- matrix(
	c(
		estim_max_likelihood[estim_max_likelihood$Repère == "A",]$Alpha_h,
		estim_max_likelihood[estim_max_likelihood$Repère == "A",]$Beta,
		estim_param_duree_remplacement[
			estim_param_duree_remplacement$Repère == "A",
		]$lambda_h,
		estim_max_likelihood[estim_max_likelihood$Repère == "E",]$Alpha_h,
		estim_max_likelihood[estim_max_likelihood$Repère == "E",]$Beta,
		estim_param_duree_remplacement[
			estim_param_duree_remplacement$Repère == "E",
		]$lambda_h
	),
	ncol = 3,
	byrow = TRUE
) %>% tibble()

rownames(vals) <- c("A", "E")
colnames(vals) <- c("alpha", "beta", "lambda")

vals

parametres = list(
	alpha = estim_max_likelihood[estim_max_likelihood$Repère == "E",]$Alpha_h,
	beta = estim_max_likelihood[estim_max_likelihood$Repère == "E",]$Beta,
	lambda = estim_param_duree_remplacement[
		estim_param_duree_remplacement$Repère == "E",
	]$lambda_h,
	tp = 10 / 60, # en heures
	T = 60, # en heures
	# Temps de remplacement périodique en heures
	tp = 10 / 60,
	# coût de changement fixe
	cc = 30,
	# coût d'indisponibilité par heures
	ci = 20 * 60
)