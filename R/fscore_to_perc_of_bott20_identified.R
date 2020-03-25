fscore_to_perc_of_bott20_identified <- function(fscore, simmed){

	# get who we're looking for
	ids_truly_in_bottom20 <-
		which(simmed$students$theta <= quantile(sort(simmed$students$theta), 0.2))

	stopifnot(
		length(ids_truly_in_bottom20) == length(simmed$students$theta) * 0.2
	)

	# get students definitly in there
	ids_pred_lower <-
		which(
			fscore[ , 1] <
				quantile(sort(fscore[ , 1]), 0.2)
		)

	length(ids_pred_lower)

	# sample the right amount on the edge
	ids_on_edge <-
		sample(
			which(
				fscore[ , 1] ==
					quantile(sort(fscore[ , 1]), 0.2)
			),
			size = length(ids_truly_in_bottom20) - length(ids_pred_lower),
			replace = FALSE
		)

	stopifnot(
		length(c(ids_pred_lower, ids_on_edge)) == length(simmed$students$theta) * 0.2
	)

	mean(ids_truly_in_bottom20 %in% c(ids_pred_lower, ids_on_edge))
}
