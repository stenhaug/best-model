# n_items <- 10
# disc_ln_sd <- 0
# n_ref <- 10
# n_foc <- 10
# mean_foc <- 0
#
# out <-
# 	sim(
# 		n_items = 10,
# 		disc_ln_sd = 0,
# 		n_ref = 10,
# 		n_foc = 10,
# 		mean_foc = 0
# 	)

sim <- function(n_items, disc_ln_sd, n_ref, n_foc, mean_foc){

	students <-
		tibble(
			group = c(rep("a_ref", n_ref), rep("b_foc", n_foc)),
			theta = c(rnorm(n_ref, 0, 1), rnorm(n_foc, mean_foc, 1))
		)

	items <-
		tibble(
			d = rnorm(n_items, 0, 0.5), # notice low sd here
			a1 = rlnorm(n_items, 0, disc_ln_sd)
		) %>%
		mutate(g = 0, u = 1)

	data <-
		simdata(
			a = items$a1,
			d = items$d,
			itemtype = "2PL",
			Theta = students$theta %>% as.matrix()
		)

	list(students = students, items = items, data = data)
}
