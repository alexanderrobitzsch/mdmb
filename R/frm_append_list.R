## File Name: frm_append_list.R
## File Version: 0.04
## File Last Change: 2017-01-23 19:35:11

frm_append_list <- function(list1, list2)
{
	N2 <- length(list2)
	if (N2>0){
		for (nn in 1:N2){
			list1[[ names(list2)[nn] ]] <- list2[[nn]]
		}
	}
	return(list1)
}
