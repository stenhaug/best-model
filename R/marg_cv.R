marg_cv <- function(in_model, out_data){
	make_fulldata <- function(data){
		wrong <- 1 - data
		right <- data
		colnames(wrong) <- glue::glue("Item.{1:ncol(data)}_1")
		colnames(right) <- glue::glue("Item.{1:ncol(data)}_2")
		cbind(wrong, right)[, order(c(seq(ncol(wrong)), seq(ncol(right))))]
	}

	mirt:::Estep.mirt(
		pars = in_model@ParObjects$pars,
		tabdata = make_fulldata(out_data),
		freq = rep(1, nrow(out_data)),
		CUSTOM.IND = in_model@Internals$CUSTOM.IND,
		Theta = in_model@Model$Theta,
		prior = in_model@Internals$Prior[[1]],
		itemloc = in_model@Model$itemloc,
		full = FALSE,
		Etable = TRUE
	)$expected
}
