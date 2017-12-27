#	Estimation of a sample variance, correction using Student distribution is applied
EstimVariance<-function(vect,conf_probability=0.95) {
	vect_mean<-mean(vect)
	n_obs<-length(vect)
	sigma_sum<-(vect_mean-vect[1])^2
	for (i_run in (2:n_obs)) {
		sigma_sum<-sigma_sum+(vect_mean-vect[i_run])^2
	}	
	sigma2<-sigma_sum/(n_obs-1)
	#return(sigma_sum)
	delta_estim<-(sigma2^0.5)*(qt(p=conf_probability,df=(n_obs-1))/(n_obs^0.5))
	sigma2_left<-sigma2*((n_obs-1)/qchisq(p=0.95, df=n_obs-1))
	sigma2_right<-sigma2*((n_obs-1)/qchisq(p=0.05, df=n_obs-1))
	return(c(Delta=delta_estim,SigmaMin=sigma2_left,
		Sigma=sigma2,Sigma_max=sigma2_right))
	# return(S2_av)
	#return(qt(p=conf_probability,df=(n_obs-1)))
	#return(qt(p=conf_probability,df=(n_obs-1))/(n_obs^0.5))
	#return(c(delta_estim,S2_left,S2_av,S2_right))
}
# #	verification: an example from [Bendat,pp.97-98]
# #	NB a mistake in the example: the probability 0.95 was assumed instead an indicated 0.9
# test_vct<-c(60,61,47,56,61,63,
# 			65,69,54,59,43,61,
# 			55,61,56,48,67,65,
# 			60,58,57,62,57,58,
# 			53,59,58,61,67,62,
# 		54)
# mean(test_vct)
# estim_variance(test_vct,conf_probability=0.95)
# # values to compare
# 58.61			# mean			
# (60.37-56.85)/2	# delta
# 22.91			# sigma2 left
# 33.43			# sigma2	
# 54.22			# sigma2 right
# #
#
BuildVect<-function(List,i_mx,j_mx) {
	vect<-List[[1]][i_mx,j_mx]
	for (k in seq(along.with=List)[-1]) {
		vect<-c(vect,List[[k]][i_mx,j_mx])
	}
	return(vect)
}
EstimListVariance<-function(List,i_l,j_l) {
	VectFromList<-BuildVect(List=List,i_mx=i_l,j_mx=j_l)
	ij_Variance<-EstimVariance(vect=VectFromList,conf_probability=0.95)
	return(ij_Variance["Delta"])
}