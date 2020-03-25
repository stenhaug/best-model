sumlog <- function(x) sum(log(x))

get_p_from_anova <- function(mod1, mod2){

	if("logical" %in% c(class(mod1), class(mod2))) {return(NA_real_)} # catches missing

	anova(mod1, mod2)$p[2]
}

get_p <- function(model, fscores){
	n_items <- length(model@Data$K)

	1:n_items %>%
		map(~ probtrace(extract.item(model, .), fscores)[ , 2]) %>%
		do.call(cbind, .)
}
